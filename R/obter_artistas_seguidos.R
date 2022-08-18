#' Obtém Seus Arittas Seguidos no Spotify
#'
#' Essa função pega informações de todas as seus artistas seguidos no Spotify.
#'
#' @return uma tibble com todos os seus artistas seguidos, e as seguintes informações sobre eles: c("name", "id", "genres", "images", "popularity", "followers_total")
#' @export
#'
#' @examples # obter_artistas_seguidos()
obter_artistas_seguidos <- function(){

  # criando data frame vazio
  artistas_seguindo <-  NULL

  # critério de parada
  n <- 1

  # contador para o looping
  i <-  1
  while (n > 0){

    if(i == 1){

      # pega 50 artistas por vez do seu spotify
      salvos <- spotifyr::get_my_followed_artists(limit = 50,
                                                  authorization = spotifyr::get_spotify_authorization_code(
                                                    client_id = Sys.getenv("SPOTIFY_CLIENT_ID"),
                                                    client_secret = Sys.getenv("SPOTIFY_CLIENT_SECRET"),
                                                    scope = "user-follow-read"
                                                  ))
    } else {

      # pega 50 artistas por vez do seu spotify
      salvos <- spotifyr::get_my_followed_artists(limit = 50,
                                                  after = depois,
                                                  authorization = spotifyr::get_spotify_authorization_code(
                                                    client_id = Sys.getenv("SPOTIFY_CLIENT_ID"),
                                                    client_secret = Sys.getenv("SPOTIFY_CLIENT_SECRET"),
                                                    scope = "user-follow-read"
                                                  ))
    }
    # conta quantos artistas foram obtidos. se for 0, o looping para.
    n <- length(salvos)

    # pega o id do ultimo artista
    if (n != 0){
      depois <- salvos |>
        dplyr::pull(id) |>
        tail(n = 1)}

    # adiciona os ultimos artistas no data frame criado inicialmente
    artistas_seguindo <- rbind(artistas_seguindo, salvos)

    # adiciona +1 no contador
    i <- i + 1
  }

  # arrumando colunas

  artistas <- artistas_seguindo |>
    dplyr::select(name, id, genres, images,
                  popularity, followers_total = followers.total)

  generos <- artistas |>
    dplyr::mutate(n_genres = purrr::map(genres, length))

  generos1 <- generos |>
    dplyr::filter(n_genres == 0) |>
    dplyr::mutate(genres = NA_character_)

  generos2 <- generos |>
    dplyr::filter(n_genres != 0) |>
    dplyr::mutate(genres = purrr::map_chr(genres, ~paste(.x, collapse = ", ")))

  infos_final <- rbind(generos2, generos1) |>
    dplyr::select(-n_genres)

  return(infos_final)
}
