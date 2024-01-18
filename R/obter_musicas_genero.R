# dado um genero, pega playlists do spotify desse genero, e pega dados de todas
# as musicas dessas playlists

obter_musicas_genero <- function(genero){

  playlists_nomes <- spotifyr::search_spotify(q = genero,
                                              type = "playlist",
                                              market = "BR",
                                              include_external = "audio",
                                              limit = 50) |>
    dplyr::filter(owner.id %in% c("spotify", "Spotify"))

  uris <- playlists_nomes |>
    dplyr::select(uri) |>
    dplyr::mutate(uri = stringr::str_remove(uri, "spotify:playlist:")) |>
    dplyr::pull() |>
    head(10)

  playlists_musicas <- spotifyr::get_playlist_audio_features("spotify", uris)

}


