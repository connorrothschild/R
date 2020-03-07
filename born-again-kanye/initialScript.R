library(spotifyr)
library(tidyverse)

Sys.setenv(SPOTIFY_CLIENT_ID = '040a9199706d4ee5a1729414f7fa4a3e')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'bc369271b5164ff3a601b923e2e22e25')

access_token <- get_spotify_access_token()

kanye <- get_artist_audio_features('kanye west')
kanye

song_data <- kanye %>% 
  select(album_release_year, danceability:tempo, duration_ms, explicit, track_name, external_urls.spotify, album_name:key_mode)

song_data <- song_data %>% 
  mutate(album_name = ifelse(album_name == "Kanye West Presents Good Music Cruel Summer", "Good Music Cruel Summer", album_name)) %>% 
  filter(track_name != str_detect(track_name, "Skit *"),
         album_name != "Graduation (Alternative Business Partners)") %>% 
  distinct(track_name, .keep_all = TRUE)

write.csv(song_data, "song_data.csv")

grouped <- song_data %>% 
  group_by(album_name) %>% 
  summarise_at(vars(danceability:tempo), mean) 

grouped <- grouped %>% 
  filter(album_name != "Graduation (Alternative Business Partners)" & album_name != "Late Orchestration") %>% 
  mutate(album_name = ifelse(album_name == "Kanye West Presents Good Music Cruel Summer", "Good Music Cruel Summer", album_name))

write.csv(grouped, "kanye_data.csv")