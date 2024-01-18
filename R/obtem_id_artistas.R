obter_id_artistas <- function(artistas){

  ids <- c()

  for (artist in artistas){

    procurando_artista <- spotifyr::search_spotify(q = artist,
                                                   type = c("artist"),
                                                   market = "BR",
                                                   limit = 1,
                                                   offset = 0,
                                                   include_external = "audio",
                                                   authorization = spotifyr::get_spotify_access_token(
                                                     client_id = Sys.getenv("SPOTIFY_CLIENT_ID"),
                                                     client_secret = Sys.getenv("SPOTIFY_CLIENT_SECRET")),
                                                   include_meta_info = TRUE)

    ids <- procurando_artista$artists$items |>
      head(1) |>
      dplyr::pull(id) |>
      append(ids)

  }

  return(ids)

}
