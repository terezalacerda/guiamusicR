#' Criar Playlist no Spotify com Top 10 musicas de cada artista
#'
#' Essa função cria uma playlist no Spotify, e adiciona 10 musicas mais famosas de cada banda especificada.
#'
#' @param artistas vetor com nome dos artistas
#' @param nome um nome para a playlist que será criada
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
cria_playlist_artistas <- function(artistas,
                              nome,
                              descricao = NULL,
                              publica = TRUE,
                              colaborativa = FALSE){

  ids <- obter_id_artistas(artistas)

  top_musicas <- NULL

  for (id in ids){

    top_musicas <- top_musicas |> rbind(
      spotifyr::get_artist_top_tracks(id = id,
                                      market = "BR",
                                      authorization = spotifyr::get_spotify_access_token(
                                        client_id = Sys.getenv("SPOTIFY_CLIENT_ID"),
                                        client_secret = Sys.getenv("SPOTIFY_CLIENT_SECRET")))
    )

  }

  # arrumar pq a descricao nao ta indo
  top_musicas |>
    criar_playlist(
      nome, descricao, publica, colaborativa
    )

}
