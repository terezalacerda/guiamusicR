library(shiny)
library(tidymodels)
library(ranger)


# funcoes -----------------------------------------------------------------

definir_ids <- function(user_id, client_id, client_secret){
  Sys.setenv(SPOTIFY_CLIENT_ID = client_id)
  Sys.setenv(SPOTIFY_CLIENT_SECRET = client_secret)
  Sys.setenv(MY_ID = user_id)
}

faz_grafico_generos_radar <- function(dados_uma_musica, modelo){

  probs <- stats::predict(modelo, dados_uma_musica, type = "prob") |>
    dplyr::rename_with(
      ~ stringr::str_remove(.x, "\\.pred_") |>
        stringr::str_replace("_", " ") |>
        stringr::str_to_title()
    )

  n <- ncol(probs)

  data <- rbind(rep(1, n), rep(0, n), probs)

  par(mar = c(0, 0, 0, 0))

  fmsb::radarchart(data,
                   axistype = 1,
                   pcol = rgb(0.2, 0.5, 0.5, 0.9),
                   pfcol = rgb(0.2, 0.5, 0.5, 0.4),
                   plwd = 2,
                   cglcol = "grey",
                   cglty = 2,
                   axislabcol = "grey",
                   caxislabels = seq(0, 1, 4),
                   cglwd = 0.9,
                   vlcex = 0.8)

}

procurar_musica_spotify <- function(nome_musica, nome_artista){

  nomes_juntos <- paste(nome_musica, nome_artista)

  procurando_musica <- spotifyr::search_spotify(q = nomes_juntos,
                                                type = c("track"),
                                                market = "BR",
                                                limit = 1,
                                                offset = 0,
                                                include_external = "audio",
                                                authorization = spotifyr::get_spotify_access_token(
                                                  client_id = Sys.getenv("SPOTIFY_CLIENT_ID"),
                                                  client_secret = Sys.getenv("SPOTIFY_CLIENT_SECRET")),
                                                include_meta_info = TRUE)

  musica_candidata <- procurando_musica$tracks$items$id[1]

  infos_novas <- obter_dados_musicas(musica_candidata)

}

obter_dados_musicas <- function(ids_musicas){

  # cria vetores vazios
  infos_musicas <- NULL
  infos_artistas <- NULL
  infos_features <- NULL

  # para cada id, a função irá:
  for (track_id in ids_musicas){

    # obter informações sobre essa musica atraves da funcao getTrack do Rspotify
    infos_musicas <- infos_musicas %>% rbind(
      Rspotify::getTrack(track_id, token = Rspotify::spotifyOAuth(Sys.getenv("MY_ID"),
                                                                  Sys.getenv("SPOTIFY_CLIENT_ID"),
                                                                  Sys.getenv("SPOTIFY_CLIENT_SECRET"))) %>%
        # algumas musicas tem + de 1 artista. quero manter apenas o primeiro
        dplyr::mutate(artists_id = stringr::str_split(artists_id, pattern = ";"),
                      artists = stringr::str_split(artists, pattern = ";")) %>%
        tidyr::unnest(cols = c(artists, artists_id)) #%>% utils::head(1)
    )

    # obtendo informações sobre essa musica atraves da função get_track_audio_features do spotifyr
    infos_features <- infos_features %>% rbind(
      spotifyr::get_track_audio_features(track_id,
                                         authorization = spotifyr::get_spotify_access_token(
                                           client_id = Sys.getenv("SPOTIFY_CLIENT_ID"),
                                           client_secret = Sys.getenv("SPOTIFY_CLIENT_SECRET")))
    )
  }

  # para cada musica dada, obtemos informações do artista através da função get_artists do spotifyr
  # fazemos isso em um novo looping pois inicialmente não tinhamos o id do artista, só
  # foi obtido no primeiro looping
  for (artist_id in infos_musicas$artists_id){
    infos_artistas <- infos_artistas %>% rbind(
      spotifyr::get_artists(artist_id,
                            authorization = spotifyr::get_spotify_access_token(
                              client_id = Sys.getenv("SPOTIFY_CLIENT_ID"),
                              client_secret = Sys.getenv("SPOTIFY_CLIENT_SECRET")))
    )
  }

  # arrumando nomes das colunas
  infos_musicas_final <- infos_musicas %>%
    dplyr::mutate(artist_id = artists_id, track_popularity = popularity,
                  track_name = name, artist_name = artists) %>%
    dplyr::select(-name, -popularity, -artists, -artists_id)

  infos_artistas_final <- infos_artistas %>%
    dplyr::mutate(artist_id = id, artist_popularity = popularity,
                  artist_name = name, followers = followers.total) %>%
    dplyr::select(-id, -popularity, -name, -followers.total, -href,
                  -images, -type, -uri, -external_urls.spotify,
                  -followers.href)

  infos_features_final <- infos_features %>%
    dplyr::mutate(track_id = id) %>%
    dplyr::select(-id, -type)

  # fazendo joins para juntas todas as informações
  infos_final <- dplyr::inner_join(infos_musicas_final,
                                   infos_artistas_final) %>%
    dplyr::inner_join(infos_features_final) %>% unique() |>
    tibble::rowid_to_column()

  # arrumando nomes das colunas novamente
  colnames(infos_final) <- colnames(infos_final) %>%
    stringr::str_replace_all(pattern = "\\.", replacement = "_")

  generos <- infos_final |>
    dplyr::mutate(n_genres = purrr::map(genres, length))

  generos1 <- generos |>
    dplyr::filter(n_genres == 0) |>
    dplyr::mutate(genres = NA_character_)

  generos2 <- generos |>
    dplyr::filter(n_genres != 0) |>
    dplyr::mutate(genres = purrr::map_chr(genres, ~paste(.x, collapse = ", ")))

  infos_final_final <- rbind(generos1, generos2) |>
    dplyr::select(-n_genres) |>
    dplyr::arrange(rowid) |>
    dplyr::group_by(track_id) |>
    dplyr::mutate(
      rowid = min(rowid),
      genres = dplyr::if_else(is.na(genres), "", genres),
      genres = paste(genres, collapse = ", ") |>
        stringr::str_remove("(, )+$"),
      genres = dplyr::if_else(genres == "", NA_character_, genres),
      artist_id = paste(artist_id, collapse = ",,,") |>
        stringr::str_remove(",,,.+"),
      artist_name = paste(artist_name, collapse = ",,,") |>
        stringr::str_remove(",,,.+"),
      artist_popularity = paste(artist_popularity, collapse = ",,,") |>
        stringr::str_remove(",,,.+") |>
        as.numeric(),
      followers = paste(followers, collapse = ",,,") |>
        stringr::str_remove(",,,.+") |>
        as.numeric()
    ) |>
    dplyr::ungroup() |>
    unique()


  # retorna data frame com todas as informações das musicas dadas.
  return(infos_final_final)
}

# app ---------------------------------------------------------------------



definir_ids('therezzza', 'd31252a2880546b3ba640d5d64b7e652',
            '964f9c50f2254b19a5ab0e7f333b3c49')

modelo <- readr::read_rds("modelo_genero_rf.rds")

ui <- fluidPage(
  fluidRow(
    column(
      width = 8,
      h2("Descobrindo gêneros de músicas!")
    )
  ),
  br(),
  fluidRow(
    column(
      width = 12,
      p("Olá, usuário!"),
      p("Esse aplicativo funciona de maneira bem simples!"),
      p("Basta preencher nos campos abaixo o nome de uma música e de seu respectivo artista (vale qualquer uma!),
        apertar o botão, e então, será gerado um gráfico com os gêneros dessa música!"),
      p("Quer experimentar?"),
      br()
    )
  ),
  fluidRow(
    column(
      width = 12,
      textInput("nome_musica", "Digite o nome de uma música!", width = 500),
      textInput("nome_artista", "Digite o nome do respectivo artista/banda!", width = 500),
      actionButton("gerar_grafico", "Gerar gráfico!")
    ),
    br()
  ),
  fluidRow(
    column(
      width = 12,
      br(),
      textOutput(outputId = "musica_artista"),
      br(),
      plotOutput(outputId = "grafico")
    )
  ),
  fluidRow(
    column(
      width = 12,
      br(),
      br(),
      p("Esse aplicativo utiliza um modelo de Random Forest para fazer as predições,
        sendo que cada eixo representa a probabilidade de uma música pertencer
        ao respectivo gênero."),
      br(),
      p("Autoria: Tereza Lacerda")
    )
  )
              )

server <- function(input, output, session) {

  observeEvent(input$gerar_grafico, ignoreNULL = TRUE, {
    nome_musica <- input$nome_musica

    nome_artista <- input$nome_artista

    dados_musica <- procurar_musica_spotify(nome_musica, nome_artista)

    dados_prontos <- dados_musica |>
      dplyr::rename(
        track.duration_ms = duration_ms,
        track.popularity = track_popularity
      )

    output$grafico <- renderPlot({
      faz_grafico_generos_radar(dados_prontos, modelo)
    })

    nome_musica_encontrada <- dados_prontos |>
      dplyr::select(track_name) |>
      dplyr::pull()

    nome_artista_encontrado <- dados_prontos |>
      dplyr::select(artist_name) |>
      dplyr::pull()

    output$musica_artista <- renderText({
      paste0('A música encontrada foi: "',
            nome_musica_encontrada,
            '", de "',
            nome_artista_encontrado,
            '"')
    })
  })


}

shinyApp(ui, server)
