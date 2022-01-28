#' Define ID's
#'
#' Para acessar a API do Spotify, são necessários tokens de autenticação.
#' Essa função vai definir esses tokens/ID'S como variáveis globais, para que não
#' precisem ser especificados sempre que uma consulta for realizada.
#'
#' @param user_id é o seu nome de usuário (id único) no Spotify. Você pode consultá-lo nesse link: https://www.spotify.com/br/account/overview/
#' @param client_id o client_id é um dos tokens de autenticação da API do Spotify. Você pode obtê-lo nesse link: https://developer.spotify.com/dashboard/applications
#' @param client_secret o client_secret é um dos tokens de autenticação da API do Spotify. Você pode obtê-lo nesse link: https://developer.spotify.com/dashboard/applications
#'
#' @return não retorna nada
#' @export
#'
#' @examples definir_ids("meu_nome_de_usuario", "xxxxxxxxxxxxxxxxxxxxx", "xxxxxxxxxxxxxxxxxxxxx")
definir_ids <- function(user_id, client_id, client_secret){
  Sys.setenv(SPOTIFY_CLIENT_ID = client_id)
  Sys.setenv(SPOTIFY_CLIENT_SECRET = client_secret)
  Sys.setenv(MY_ID = user_id)
}
