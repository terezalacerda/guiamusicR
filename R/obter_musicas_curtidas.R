#' Obtém Suas Músicas Curtidas no Spotify
#'
#' Essa função pega informações de todas as suas músicas curtida no Spotify.
#'
#' @return uma tibble com todas suas músicas curtidas, e as seguintes informações sobre elas: c("added_at", "artists_href", "artists_id", "artists_name", "artists_type", "artists_uri", "artists_external_urls_spotify", "available_markets", "disc_number", "duration_ms", "explicit", "is_local", "preview_url", "track_number", "type", "uri", "album_available_markets", "album_href", "album_id", "album_images", "album_name", "album_release_date", "album_release_date_precision", "album_total_tracks", "album_type", "album_uri", "album_external_urls_spotify", "external_ids_isrc", "external_urls_spotify", "track_rhef", "track_id", "track_name", "track_popularity")
#' @export
#'
#' @examples # obtem_musicas_curtidas()
obtem_musicas_curtidas <- function(){
  biblioteca = NULL
  n <- 1
  i <-  1
  while (n > 0){
    salvas <- spotifyr::get_my_saved_tracks(limit = 50,
                                            offset = (50*(i-1)),
                                            authorization = spotifyr::get_spotify_authorization_code(
                                              client_id = Sys.getenv("SPOTIFY_CLIENT_ID"),
                                              client_secret = Sys.getenv("SPOTIFY_CLIENT_SECRET"),
                                              scope = c("user-library-read")))
    n <- length(salvas)
    biblioteca <- rbind(biblioteca, salvas)
    i <- i + 1
  }
  colnames(biblioteca) <- colnames(biblioteca) %>%
    stringr::str_remove("track\\.") %>%
    stringr::str_replace_all(pattern = "\\.", replacement = "_")
  biblioteca <- biblioteca %>%
    dplyr::select(-album_artists) %>%
    tidyr::unnest(cols = artists, names_sep = "_") %>%
    dplyr::mutate(track_rhef = href, track_id = id, track_name = name,
                  track_popularity = popularity, album_type = album_album_type) %>%
    dplyr::select(-href, -id, -name, -popularity, -album_album_type)
  return(biblioteca)
}

#definir_ids('therezzza', 'd31252a2880546b3ba640d5d64b7e652', '3ff2301f995d4319b0977c77133792dc')
#obter_dados_musicas(c("3Wrjm47oTz2sjIgck11l5e", "02MWAaffLxlfxAUY7c5dvx", "0VjIjW4GlUZAMYd2vXMi3b", "37BZB0z9T8Xu7U3e65qxFy", "07MDkzWARZaLEdKxo6yArG"))
#ele me pede pra digitar alguma coisa no console, sera que é esse o erro?
