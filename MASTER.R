
rm(list = ls())

# SETUP -------------------------------------------------------------------

# Handle packages
pack <- c('shiny', 'shinydashboard', 'tidyverse', 'plotly', 'httr', 'DT')
pack_new <- pack[!pack %in% installed.packages()[,'Package']]
if (length(pack_new)) install.packages(pack_new, dependencies = TRUE)
lapply(X = pack, library, character.only = TRUE)

# Youtube channel id
channel_id <- 'UCGERnPMUkIrDlNf7XMmk8Vg'

file.remove('.httr-oauth') #remove current token


# Generate an OAuth2.0 token
oauth_app <- oauth_app(
  appname      = 'Pew Research',
  key          = '516340538354-0hmf6jtbr8djig3jst0k29emdbsalcjf.apps.googleusercontent.com',
  secret       = '0slGArv5u3BTeI1IWZOGiwls'
)
google_token <- oauth2.0_token(
  endpoint = oauth_endpoints('google'),
  app      = oauth_app,
  scope    = c(
    # https://developers.google.com/youtube/analytics/reference/
    'https://www.googleapis.com/auth/yt-analytics.readonly',
    'https://www.googleapis.com/auth/yt-analytics-monetary.readonly',
    'https://www.googleapis.com/auth/youtube',
    'https://www.googleapis.com/auth/youtubepartner'),
  use_oob = TRUE,
  cache = TRUE
)

# YOUTUBE DATA API --------------------------------------------------------

# LOGIC
# We need video IDs for each video uploaded by the channel
# We do this by identifying the 'uploads' playlist ID
# Then, scrape the upload playlist ID to get a list of video IDs
# Then, we pass the video IDs through Youtube Analytics API to get stats

# 1 - Get Playlist ID

# Base URL for channels report
base_url <- parse_url('https://www.googleapis.com/youtube/v3/channels')

# Generate an http query
url <- base_url
url$query <- list(
  part = 'contentDetails',
  id   = channel_id
)
url <- build_url(url)

# Submit a GET request using the google token
req <- GET(url, config = httr::config(token = google_token))
response <- content(req)

# Extract playlist id from response
playlist_id <- response$items[[1]]$contentDetails$relatedPlaylists[['uploads']]

# 2 - Use Playlist ID to get video IDs for all uploads

# Base URL for playlist items report
base_url <- parse_url('https://www.googleapis.com/youtube/v3/playlistItems')

# Generate an http query
url <- base_url
url$query <- list(
  part       = 'snippet,contentDetails',
  playlistId = playlist_id,
  maxResults = 50
)
url <- build_url(url)

# Submit a GET request using the google token
req <- GET(url, config = httr::config(token = google_token))
response <- content(req)

# Extract video info from response
video_info <- lapply(X = seq_len(length(response$items)), FUN = function(i) {
  list(
    videoId = response$items[[i]]$snippet$resourceId$videoId,
    publishedAt = response$items[[i]]$snippet$publishedAt,
    title = response$items[[i]]$snippet$title
  )
}) %>% bind_rows()

# Repeat for each page
while (!is.null(response$nextPageToken)) {

  # Generate a new http query using next page token
  url <- base_url
  url$query <- list(
    part       = 'snippet',
    playlistId = playlist_id,
    maxResults = 50,
    pageToken  = response$nextPageToken
  )
  url <- build_url(url)
  req <- GET(url, config = httr::config(token = google_token))
  response <- content(req)

  # Extract video info from response
  video_info <- lapply(X = seq_len(length(response$items)), FUN = function(i) {
    list(
      videoId = response$items[[i]]$snippet$resourceId$videoId,
      publishedAt = response$items[[i]]$snippet$publishedAt,
      title = response$items[[i]]$snippet$title
    )
  }) %>%
    # Bind to full data
    bind_rows(video_info, .)

  writeLines(paste('Successfully scraped', length(response$items), 'videos.'))

}

# Clean video info
video_info <- video_info %>%
  mutate(
    url          = paste0('https://www.youtube.com/watch?v=', videoId),
    publish_dt   = as.POSIXct(publishedAt, format = '%Y-%m-%dT%H:%M:%S.000', tz = 'zulu'),
    publish_date = substr(publish_dt, 1, 10) %>% as.Date(),
    publish_time = substr(publish_dt, 12, 19) %>% as.character()
  ) %>%
  select(id = videoId, url, title, publish_date, publish_time)

# This saves as csv file
# TODO: Save into Redshift database
write.csv(x = video_info, file = 'video_info.csv', row.names = FALSE)

# YOUTUBE ANALYTICS API ---------------------------------------------------

# Initialize object to store all data
stats_all <- data.frame()

# Create group factor to split data into groups of 200
groups <- ceiling(1:nrow(video_info) / 200)
# EQUIVALENT TO:
# groups <- ceiling(1:length(video_info$id) / 200)

for (group in unique(groups)) {

   # Get the video IDs for group
  video_ids <- as.character(video_info$id[which(groups == group)])

  filters <- paste0('video==', paste(video_ids, collapse = ','))

  url <- parse_url('https://youtubeanalytics.googleapis.com/v2/reports')
  url$query <- list(
    ids        = paste0('channel==', channel_id),
    dimensions = 'video',
    metrics    = 'views,averageViewDuration,averageViewPercentage',
    filters    = filters,
    sort       = 'views',
    startDate  = '2000-01-01',
    endDate    = Sys.Date()
  )
  url <- build_url(url)
  req <- GET(url, config = httr::config(token = google_token))
  response <- content(req)

  stats2 <- data.frame(
    matrix(unlist(response$rows), ncol = 4, byrow = TRUE)
  ) %>%
    set_names(c('id', 'views', 'averageViewDuration', 'averageViewPercentage'))
  stats_all <- bind_rows(stats_all, stats2)
    writeLines(paste('Successfully scraped', length(response$rows), 'videos.'))
  Sys.sleep(2)
}

# MERGE -------------------------------------------------------------------

data_merged <- merge(
  x = video_info, y = stats_all,
  by = 'id', all = TRUE
)

write.csv(x = data_merged, file = 'data_merged.csv', row.names = FALSE)
