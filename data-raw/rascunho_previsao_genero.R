# esqueleto
devtools::load_all()

# defino ids
definir_ids('therezzza', 'd31252a2880546b3ba640d5d64b7e652',
            '964f9c50f2254b19a5ab0e7f333b3c49')

nome_musica <- "infinito -1"

nome_artista <- "o grilo"

# obtendo dados da musica
dados_musica <- procurar_musica_spotify(nome_musica, nome_artista)

# arrumando nomes das variaveis para entrar no modelo
dados_prontos <- dados_musica |>
  dplyr::rename(
    track.duration_ms = duration_ms,
    track.popularity = track_popularity
  )

# lendo modelo
modelo <- readr::read_rds("modelos_genero/modelo_genero_rf.rds")

library(tidymodels)
faz_grafico_generos_radar(dados_prontos, modelo)

nome_musica_encontrada <- dados_prontos |>
  dplyr::select(track_name) |>
  dplyr::pull()

nome_artista_encontrado <- dados_prontos |>
  dplyr::select(artist_name) |>
  dplyr::pull()
