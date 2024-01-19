
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
