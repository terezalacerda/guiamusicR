
# proximos passos:

# agrupar essas musicas de acordo com o genero, ou com as outras
# caracteristicas musicais

# pegar musicas das playlists que tenho criadas pra cada genero,
# ver quais musicas das curtidas nao estao em nenhuma, e adicioná-las
# em playlists que fazem sentido

# funcao que ordena musicas de acordo com alguma variavel



grilo <- spotifyr::get_artist_audio_features("O Grilo",
                                             c("album", "single","compilation"),
                                             authorization = spotifyr::get_spotify_access_token(
                                               client_id = Sys.getenv("SPOTIFY_CLIENT_ID"),
                                               client_secret = Sys.getenv("SPOTIFY_CLIENT_SECRET")
                                             )
)


# atualizando playlists artistas automaticamente --------------------------

# data frame com infos dos artistas

artistas <- get_my_followed_artists(limit = 50,
                                    authorization = spotifyr::get_spotify_authorization_code(
                                      client_id = Sys.getenv("SPOTIFY_CLIENT_ID"),
                                      client_secret = Sys.getenv("SPOTIFY_CLIENT_SECRET"),
                                      scope = "user-follow-read"
                                    ))


# analisando playlists por genero -----------------------------------------

# criar playlists tematicas com 20 musicas cada
# analisar essas musicas e ao ter uma musica
# nova, conseguir alocar em uma dessas playlists

# adicionar musicas novas na playlist de novidades ------------------------

# (dos artistas q vc segue)


# seguir tds artistas q vc ouve mt ----------------------------------------


# ideia: ------------------------------------------------------------------

# fazer shiny que meus amigos podem mexer, sendo q eles
# tem que me autorizar antes de usar pela primeira vez, q
# aí supostamente as coisas só vao funcionar normal


# eles podem ter acesso ao que eu to escutando no momento -----------------

# (ultimas musicas curtidas)

# albuns salvos com fotinha


# eles podem me recomendar musicas e ai serao adicionadas numa pla --------


# fazer playlists pra eles automaticas ------------------------------------

# com base em musicas q ele gosta sla
library(guiamusicR)
definir_ids('therezzza', 'd31252a2880546b3ba640d5d64b7e652',
            '964f9c50f2254b19a5ab0e7f333b3c49')

grilo <- spotifyr::get_artist_audio_features("0L8ExT028jH3ddEcZwqJJ5",
                                             c("album", "single","compilation"),
                                             authorization = spotifyr::get_spotify_access_token(
                                               client_id = Sys.getenv("SPOTIFY_CLIENT_ID"),
                                               client_secret = Sys.getenv("SPOTIFY_CLIENT_SECRET")
                                             )
)


source("data-raw/gera_musicas_curtidas.R")

# criar playlist com o setlist do setlist.fm

dados |>
  dplyr::group_by(track_id) |>
  dplyr::mutate(
    rowid = min(rowid),
    genres = dplyr::if_else(is.na(genres), "", genres),
    genres = paste(genres, collapse = ", ") |>
      stringr::str_remove("(, )+$"),
    genres = dplyr::if_else(genres == "", NA_character_, genres)
  ) |> View()
