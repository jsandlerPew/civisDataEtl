


# SETUP -------------------------------------------------------------------

# function to convert avg view duration to MMSS from S
get_min_sec <- function(x){
  x <- as.numeric(x)
  min <- floor(x / 60)
  rem <- (x / 60) - min
  sec <- rem * 60
  paste0(min, 'M', sec, 'S')
} 

# Handle packages
pack <- c('tidyverse', 'httr', 'civis')
pack_new <- pack[!pack %in% installed.packages()[,'Package']]
if (length(pack_new)) install.packages(pack_new, dependencies = TRUE)
lapply(X = pack, library, character.only = TRUE)

# Youtube channel id
channel_id <- 'UCGERnPMUkIrDlNf7XMmk8Vg'

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
    'https://www.googleapis.com/auth/youtubepartner'
  )
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
stats_main <- data.frame()

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
    startDate  = '2019-01-01',
    endDate    = '2019-03-31'
  )
  url <- build_url(url)
  req <- GET(url, config = httr::config(token = google_token))
  response <- content(req)
  
  # Clean
  stats_this <- data.frame(
    matrix(unlist(response$rows), ncol = 4, byrow = TRUE)
  ) %>%
    set_names(c('id', 'views', 'averageViewDuration', 'averageViewPercentage')) %>%
    mutate(averageViewDuration = get_min_sec(averageViewDuration))
  stats_main <- bind_rows(stats_main, stats_this)
  writeLines(paste('Successfully scraped', length(response$rows), 'videos.'))
  Sys.sleep(2)
}

# VIDEO DURATION ----------------------------------------------------------

# Initialize object to store all data
stats_duration <- data.frame()

# Create group factor to split data into groups of 50
groups <- ceiling(1:nrow(video_info) / 50)
# EQUIVALENT TO:
# groups <- ceiling(1:length(video_info$id) / 50)

for (group in unique(groups)) {
  
  # Get the video IDs for group
  video_ids <- as.character(video_info$id[which(groups == group)])
  video_ids <- paste(video_ids, collapse = ',')
  
  url <- parse_url('https://www.googleapis.com/youtube/v3/videos')
  url$query <- list(
    part       = "contentDetails",
    id         = video_ids,
    startDate  = '2019-01-01',
    endDate    = '2019-03-31'
  )
  url <- build_url(url)
  req <- GET(url, config = httr::config(token = google_token))
  response <- content(req)
  
  # Clean
  stats_this <- data.frame(
    matrix(unlist(response$items), ncol = 10, byrow = TRUE)
  ) %>%
    select(3:4) %>%
    set_names(c('id', 'videoDuration')) %>%
    mutate(videoDuration = gsub(x           = videoDuration,
                                pattern     = "^PT",
                                replacement = ""))
  
  stats_duration <- bind_rows(stats_duration, stats_this)
  writeLines(paste('Successfully scraped', length(response$items), 'videos.'))
  Sys.sleep(2)
}

# MERGE -------------------------------------------------------------------

# Merge main and duration stats
stats_all <- merge(x   = stats_main,
                   y   = stats_duration,
                   by  = "id",
                   all = TRUE)

# Merge Data and Analytics API responses
data_merged <- merge(x = video_info,
                     y = stats_all,
                     by = 'id',
                     all = TRUE)

# Save merged data
write.csv(x = data_merged, file = 'data_merged_q1.csv', row.names = FALSE)

# TODO: Figure out why this doesn't work:
# civis::write_civis(
#   x = data_merged,
#   tablename,
#   database
# )



