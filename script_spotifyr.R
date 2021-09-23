#####################################################################################
######################## Script disponibilizado por EstatMG #########################
#####################################################################################

# Primeiros ajustes ------------------------------------------------------------------

library(tidyverse)
library(spotifyr)
library(wordcloud2)
library(gghighlight)
library(lubridate)
library(patchwork)

theme_set(theme_minimal())

Sys.setenv(SPOTIFY_CLIENT_ID = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")
Sys.setenv(SPOTIFY_CLIENT_SECRET = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")

access_token <- get_spotify_access_token()

# Banco de Dados ---------------------------------------------------------------------

dados <- get_playlist_audio_features("Integrantes - 2021",
                                     "7eZreLsVfaNRFB5W0pZQHN") %>% 
  unnest(track.artists) %>% # Variável a principío como lista
  distinct(track.id, .keep_all = TRUE) %>% 
  select(added_at, track.id, track.name, artist.id = id, artist.name = name, 
         track.album.name,  track.album.release_date, track.album.release_date_precision,
         track.popularity, danceability, energy, loudness, speechiness, 
         acousticness, instrumentalness, liveness, valence, tempo) %>% 
  rowwise() %>% 
  mutate(genres = str_flatten(get_artist(artist.id)$genres, collapse = ","),
         artist.popularity = get_artist(artist.id)$popularity)

#  Gêneros mais comuns ---------------------------------------------------------------

dados %>% separate_rows(genres, sep = ",") %>% 
  group_by(genres) %>% count() %>%
  wordcloud2(minSize = 5)
  
dados %>% separate_rows(genres, sep = ",") %>% 
  group_by(genres) %>% count() %>% ungroup() %>% 
  slice_max(order_by = n, n = 10) %>%
  mutate(prop = n/nrow(dados),
         genres = fct_reorder(as.factor(genres), prop)) %>% 
  ggplot(aes(y = genres, x = prop)) + 
  geom_col(fill = "#4fb6a7") + labs(y = "", x = "Proporção")

# Data de lançamento dos álbuns/músicas ----------------------------------------------

dados %>% 
  mutate(track.album.release_date = str_sub(track.album.release_date, end = 4) %>% 
           as.numeric()) %>% 
  ggplot(aes(track.album.release_date)) +
  geom_histogram(color = "white", fill = "#652177", bins = 15) +
  labs(y = "Contagem", x = "")

musicas_mais_recentes <- dados %>% ungroup() %>% 
  mutate(track.album.release_date = str_sub(track.album.release_date, end = 4) %>% 
           as.numeric()) %>% 
  slice_max(track.album.release_date, n = 1) %>% 
  select(track.name, track.album.release_date)

musicas_mais_antigas <- dados %>% ungroup() %>% 
  mutate(track.album.release_date = str_sub(track.album.release_date, end = 4) %>% 
           as.numeric()) %>% 
  slice_min(track.album.release_date, n = 1) %>% 
  select(track.name, track.album.release_date)

# Boxplots - Variáveis Numéricas -----------------------------------------------------

dados %>% select(-ends_with("popularity"), -liveness) %>% 
  pivot_longer(cols = where(is.numeric), names_to = "Variáveis", values_to = "Valores") %>% 
  ggplot(aes(x = `Variáveis`, y = Valores)) + 
  geom_boxplot(color = "#4FB6A7") + geom_jitter(color = "#4FB6A7", title = NA) +
  facet_wrap(vars(`Variáveis`), scales = "free", nrow = 2)

# Correlação entre variáveis ---------------------------------------------------------

g1 <- dados %>% 
  mutate(label = paste(track.name, artist.name, sep = " - ")) %>% 
  ggplot(aes(danceability, energy)) +
  geom_point()

g2 <- dados %>% 
  mutate(label = paste(track.name, artist.name, sep = " - ")) %>% 
  ggplot(aes(danceability, energy, color = label)) +
  geom_point() +
  gghighlight(danceability < 0.3 | danceability > 0.85 | energy < 0.15 | energy > 0.97, 
              label_key = label)

g1 + g2

g1 <- dados %>% 
  mutate(label = paste(track.name, artist.name, sep = " - ")) %>% 
  ggplot(aes(valence, tempo)) +
  geom_point()

g2 <- dados %>% 
  mutate(label = paste(track.name, artist.name, sep = " - ")) %>% 
  ggplot(aes(valence, tempo, color = label)) +
  geom_point() +
  gghighlight(valence < 0.07 | valence > 0.95 | tempo > 185 | tempo < 75, 
              label_key = label)

g1 + g2

