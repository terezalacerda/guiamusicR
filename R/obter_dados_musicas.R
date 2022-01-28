#' Obter Dados de Musicas
#'
#' Essa função obtem dados de musicas. Tais como:
#'
#' @param ids_musicas um vetor com os ID's das músicas que deseja obter informação
#'
#' @return uma tibble com as seguintes informações sobre as músicas especificadas: c("track_id","explicit", "album", "album_id", "artist_id", "track_popularity", "track_name", "artist_name", "genres", "artist_popularity", "followers", "danceability", "energy", "key", "loudness", "mode", "speechiness", "acousticness", "instrumentalness", "liveness", "valence", "tempo", "uri", "track_href", "analysis_url", "duration_ms", "time_signature" )
#' @export
#'
#' @examples # obter_dados_musicas(c("3Wrjm47oTz2sjIgck11l5e",
#' # "02MWAaffLxlfxAUY7c5dvx", "0VjIjW4GlUZAMYd2vXMi3b",
#' # "37BZB0z9T8Xu7U3e65qxFy", "07MDkzWARZaLEdKxo6yArG"))
obter_dados_musicas <- function(ids_musicas){
  infos_musicas <- NULL
  infos_artistas <- NULL
  infos_features <- NULL
  for (track_id in ids_musicas){
    infos_musicas <- infos_musicas %>% rbind(
      Rspotify::getTrack(track_id, token = Rspotify::spotifyOAuth(Sys.getenv("MY_ID"),
                                                                  Sys.getenv("SPOTIFY_CLIENT_ID"),
                                                                  Sys.getenv("SPOTIFY_CLIENT_SECRET"))) %>%
        dplyr::mutate(artists_id = stringr::str_split(artists_id, pattern = ";"),
                      artists = stringr::str_split(artists, pattern = ";")) %>%
        tidyr::unnest(cols = c(artists, artists_id)) %>% utils::head(1)
    )
    infos_features <- infos_features %>% rbind(
      spotifyr::get_track_audio_features(track_id,
                                         authorization = spotifyr::get_spotify_access_token(
                                           client_id = Sys.getenv("SPOTIFY_CLIENT_ID"),
                                           client_secret = Sys.getenv("SPOTIFY_CLIENT_SECRET")))
    )
  }
  for (artist_id in infos_musicas$artists_id){
    infos_artistas <- infos_artistas %>% rbind(
      spotifyr::get_artists(artist_id,
                            authorization = spotifyr::get_spotify_access_token(
                              client_id = Sys.getenv("SPOTIFY_CLIENT_ID"),
                              client_secret = Sys.getenv("SPOTIFY_CLIENT_SECRET")))
    )
  }

  infos_musicas_final <- infos_musicas %>%
    dplyr::mutate(artist_id = artists_id, track_popularity = popularity,
                  track_name = name, artist_name = artists) %>%
    dplyr::select(-name, -popularity, -artists, -artists_id)
  infos_artistas_final <- infos_artistas %>%
    dplyr::mutate(artist_id = id, artist_popularity = popularity,
                  artist_name = name, followers = followers.total) %>%
    dplyr::select(-id, -popularity, -name, -followers.total, -href,
                  -images, -type, -uri, -external_urls.spotify,
                  -followers.href)
  infos_features_final <- infos_features %>%
    dplyr::mutate(track_id = id) %>%
    dplyr::select(-id, -type)
  infos_final <- dplyr::inner_join(infos_musicas_final,
                                   infos_artistas_final) %>%
    dplyr::inner_join(infos_features_final) %>% unique()
  colnames(infos_final) <- colnames(infos_final) %>%
    stringr::str_replace_all(pattern = "\\.", replacement = "_")
  return(infos_final)
}
