test_that("obter_musicas_curtidas works", {

  # definindo ids para que as funções funcionem
  definir_ids('therezzza', 'd31252a2880546b3ba640d5d64b7e652',
              '3ff2301f995d4319b0977c77133792dc')

  # teste que deve funcionar
  teste <- obter_musicas_curtidas()

  # valores esperados para o dataframe que será gerado
  expect_equal(colnames(teste), c("added_at", "artists_href", "artists_id", "artists_name", "artists_type", "artists_uri", "artists_external_urls.spotify", "available_markets", "disc_number", "duration_ms", "explicit", "is_local", "preview_url", "track_number", "type", "uri", "album_available_markets", "album_href", "album_id", "album_images", "album_name", "album_release_date", "album_release_date_precision", "album_total_tracks", "album_type", "album_uri", "album_external_urls_spotify", "external_ids_isrc", "external_urls_spotify", "track_rhef", "track_id", "track_name", "track_popularity"))

})
