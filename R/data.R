#' Músicas curtidas
#'
#' Essa base contém as músicas curtidas da Cherry
#' no Spotify, com diversas informações sobre elas.
#'
#' @format Um data frame, contendo:
#' \describe{
#'   \item{added_at}{momento em que a musica foi curtida}
#'   \item{track_name}{nome da musica}
#'   \item{artist_name}{nome do artista}
#'   \item{album}{nome do album}
#'   \item{track_popularity}{popularidade da musica}
#'   \item{genres}{generos do artista}
#'   \item{artist_popularity}{popularidade do artista}
#'   \item{followers}{numero de seguidores do artista}
#'   \item{danceability}{o quão dançável uma música é}
#'   \item{energy}{uma medida entre 0.0 e 1.0 representando a intensidade e atividade}
#'   \item{key}{escala estimada da música. São valores inteiros mapeados na notação musical. E.g. 0 = C, 1 = C#/Db 2 = D, e assim por diante}
#'   \item{loudness}{altura da música em decibéis (dB)}
#'   \item{mode}{modalidade (maior ou menor), da escala da qual a música é derivada}
#'   \item{speechiness}{detecta a presença de palavras faladas}
#'   \item{acousticness}{o quanto a música é acústica}
#'   \item{instrumentalness}{se a música contém vocais}
#'   \item{liveness}{presença de audiência na gravação}
#'   \item{valence}{a "positividade" musical medida entre 0 e 1}
#'   \item{tempo}{tempo estimado em batidas por minuto (BPM)}
#'   \item{duration_ms}{duração da música em milisegundos}
#'   \item{time_signature}{indica quantas batidas existem por compasso}
#'   \item{album_images}{imagens da capa do album}
#'   \item{album_release_date}{data de lançamento do album}
#'   \item{explicit}{se é explicita}
#'   \item{track_id}{id da música}
#'   \item{album_id}{id do álbum}
#'   \item{artist_id}{id do artista}
#'   \item{track_url}{link da musica}
#' }
#' @name musicas_curtidas
#' @source Spotify - \url{https://developer.spotify.com/documentation/web-api/}
#' @examples head(musicas_curtidas)
"musicas_curtidas"
