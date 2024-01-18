## code to prepare `musicas_curtidas` dataset goes here

definir_ids('therezzza', 'bf97cb31956a48159ea6b8524cdaa02a',
            '3c909290b04f4e65afb0958e9bd188bf')

curtidas <- obter_musicas_curtidas()

ultima_data <- musicas_curtidas$added_at[1]

curtidas_novas <- curtidas |>
  dplyr::filter(added_at > ultima_data) |>
  dplyr::arrange(added_at) |> head(500)

if (nrow(curtidas_novas) > 0){

dados <- obter_dados_musicas(curtidas_novas$track_id)

musicas_curtidas2 <- curtidas_novas |>
  dplyr::select(added_at, album_images, album_release_date, track_id, track_url = external_urls_spotify) |>
  unique() |>
  dplyr::left_join(dados, by = "track_id") |>
  dplyr::select(-rowid, -track_href, -analysis_url) |>
  dplyr::select(added_at, track_name, artist_name,
         album, track_popularity, genres,
         artist_popularity, followers:time_signature,
         album_images, album_release_date, explicit,
         track_id, album_id, artist_id, uri, track_url)

musicas_curtidas <- dplyr::bind_rows(musicas_curtidas2,
                           musicas_curtidas) |>
  unique() |>
  dplyr::arrange(desc(added_at))

usethis::use_data(musicas_curtidas, overwrite = TRUE)

}
