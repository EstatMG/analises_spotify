#####################################################################################
######################## Script disponibilizado por EstatMG #########################
#####################################################################################

# Primeiros ajustes -------------------------------------------------------------------------

library(tidyverse)
library(spotifyr)
library(knitr)
library(kableExtra)

Sys.setenv(SPOTIFY_CLIENT_ID = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")
Sys.setenv(SPOTIFY_CLIENT_SECRET = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")

access_token <- get_spotify_access_token()

# Testes ------------------------------------------------------------------------------------

get_artist("0epOFNiUfyON9EYx7Tpr6V") %>% 
  unlist() %>% .[2:5] %>% as.data.frame() %>% # Transformando de lista para um data-frame;
  rownames_to_column(var = "Vari?veis") %>% 
  kbl() %>% kable_styling()

get_artist_audio_features("Billie Eilish") %>% 
  select(track_name, danceability, energy, album_name, album_release_date) %>% 
  sample_n(6) %>% 
  kbl() %>% kable_styling()

get_my_recently_played(limit = 5) %>% 
  select(track.name, track.album.name, played_at) %>% 
  kbl() %>% kable_styling()

get_my_top_artists_or_tracks(type = "artists", limit = 5) %>% 
  select(name, genres) %>% 
  kbl() %>% kable_styling()

get_my_top_artists_or_tracks(type = "tracks", limit = 5) %>% 
  select(name, album.name, album.release_date) %>% 
  kbl() %>% kable_styling()

get_playlist_audio_features("Integrantes - 2021", "7eZreLsVfaNRFB5W0pZQHN") %>% 
  select(playlist_owner_name, track.name, track.popularity) %>% 
  head() %>% kbl() %>% kable_styling()


