#' Obtém Suas Músicas Curtidas no Spotify
#'
#' Essa função pega informações de todas as suas músicas curtidas no Spotify.
#'
#' @return uma tibble com todas suas músicas curtidas, e as seguintes informações sobre elas: c("added_at", "artists_href", "artists_id", "artists_name", "artists_type", "artists_uri", "artists_external_urls_spotify", "available_markets", "disc_number", "duration_ms", "explicit", "is_local", "preview_url", "track_number", "type", "uri", "album_available_markets", "album_href", "album_id", "album_images", "album_name", "album_release_date", "album_release_date_precision", "album_total_tracks", "album_type", "album_uri", "album_external_urls_spotify", "external_ids_isrc", "external_urls_spotify", "track_rhef", "track_id", "track_name", "track_popularity")
#' @export
#'
#' @examples # obter_musicas_curtidas()
obter_musicas_curtidas <- function(){

  # criando data frame vazio
  biblioteca = NULL

  # critério de parada
  n <- 1

  # contador para o looping
  i <-  1
  while (n > 0){

    # pega 50 musicas por vez do seu spotify
    salvas <- spotifyr::get_my_saved_tracks(limit = 50,
                                            offset = (50*(i-1)),
                                            authorization = spotifyr::get_spotify_authorization_code(
                                              client_id = Sys.getenv("SPOTIFY_CLIENT_ID"),
                                              client_secret = Sys.getenv("SPOTIFY_CLIENT_SECRET"),
                                              scope = c("user-library-read")))
    # conta quantas musicas foram obtidas. se for 0, o looping para.
    n <- length(salvas)

    # adiciona as ultimas musicas obtidas no data frame criado inicialmente
    biblioteca <- rbind(biblioteca, salvas)

    # adiciona +1 no contador
    i <- i + 1
  }

  # arrumando nomes das colunas
  colnames(biblioteca) <- colnames(biblioteca) %>%
    stringr::str_remove("track\\.") %>%
    stringr::str_replace_all(pattern = "\\.", replacement = "_")

  biblioteca <- biblioteca %>%
    dplyr::select(-album_artists) %>%
    tidyr::unnest(cols = artists, names_sep = "_") %>%
    dplyr::mutate(track_rhef = href, track_id = id, track_name = name,
                  track_popularity = popularity, album_type = album_album_type) %>%
    dplyr::select(-href, -id, -name, -popularity, -album_album_type)

  # retorna o data frame com todas as musicas salvas
  return(biblioteca)
}
