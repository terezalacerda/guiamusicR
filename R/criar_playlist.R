#' Criar Playlist no Spotify
#'
#' Essa função cria uma playlist no Spotify, e adiciona as músicas especificadas.
#'
#' @param musicas_para_adicionar um data frame que tem uma das colunas nomeada "uri", essa coluna deve conter os URI's das músicas que deseja adicionar na playlist.
#' @param nome_playlist um nome para a playlist que será criada
#' @param descricao uma descrição para a playlist que será criada. opcional
#' @param publica TRUE se quer que a playlist seja pública. FALSE se quer que a playlist seja privada. Valor default: TRUE
#' @param colaborativa TRUE se quer que a playlist seja colaborativa. FALSE caso contrário. Valor default: FALSE
#'
#' @return retorna o link da playlist no Spotify
#' @export
#'
#' @examples # criar_playlist(data.frame(
#' # uri = c("spotify:track:3Wrjm47oTz2sjIgck11l5e",
#' # "spotify:track:02MWAaffLxlfxAUY7c5dvx",
#' # "spotify:track:0VjIjW4GlUZAMYd2vXMi3b",
#' # "spotify:track:37BZB0z9T8Xu7U3e65qxFy",
#' # "spotify:track:07MDkzWARZaLEdKxo6yArG")),
#' # nome_playlist = "Nome da Playlist",
#' # descricao = "descricao da playlist",
#' # publica = FALSE, colaborativa = FALSE)
criar_playlist <- function(musicas_para_adicionar,
                          nome_playlist,
                          descricao = NULL,
                          publica = TRUE,
                          colaborativa = FALSE){

  # definindo os 'scopes' de autorização que devem ser utilizados
  if (publica == TRUE){
    autorizacao <- c("playlist-modify-public")
  } else {
    autorizacao <- c("playlist-modify-private")
  }

  # cria playlist com a funcao create_playlist do spotifyr
  dados_playlist <- spotifyr::create_playlist(
    Sys.getenv("MY_ID"),
    nome_playlist,
    public = publica,
    collaborative = colaborativa,
    description = descricao,
    authorization = spotifyr::get_spotify_authorization_code(
      client_id = Sys.getenv("SPOTIFY_CLIENT_ID"),
      client_secret = Sys.getenv("SPOTIFY_CLIENT_SECRET"),
      scope = autorizacao
    )
  )

  # cria uma coluna de numeração para as musicas
  # remove o "spotify:track:" dos uri's das músicas
  n_musicas <- musicas_para_adicionar %>% nrow()
  numero <- 1:n_musicas
  musicas <- musicas_para_adicionar %>%
    dplyr::mutate(uri = stringr::str_remove_all(uri, "spotify:track:")) %>%
    cbind(numero)

  # adiciona 50 músicas por vez na playlist criada anteriormente
  # através da funcao add_tracks_to_playlist
  for (i in 1:50){
    n = 50*i+1
    m = 50*i
    adicionar <- musicas %>% dplyr::filter(numero < n)
    spotifyr::add_tracks_to_playlist(
      playlist_id = dados_playlist$id,
      adicionar$uri,
      authorization = spotifyr::get_spotify_authorization_code(
        client_id = Sys.getenv("SPOTIFY_CLIENT_ID"),
        client_secret = Sys.getenv("SPOTIFY_CLIENT_SECRET"),
        scope = autorizacao
      ) )
    musicas <- musicas %>% dplyr::filter(numero > m)
    if(length(musicas$uri) == 0){
      return(dados_playlist$external_urls$spotify)
    }
  }
  return(dados_playlist$external_urls$spotify)
}

