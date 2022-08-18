test_that("obter_dados_musicais works", {

  # definindo ids para que as funções funcionem
  definir_ids('therezzza', 'd31252a2880546b3ba640d5d64b7e652',
              '964f9c50f2254b19a5ab0e7f333b3c49')

  # codigo que deve funcionar
  teste1 <- obter_dados_musicas(c("3Wrjm47oTz2sjIgck11l5e",
                        "02MWAaffLxlfxAUY7c5dvx",
                        "0VjIjW4GlUZAMYd2vXMi3b",
                        "37BZB0z9T8Xu7U3e65qxFy",
                        "07MDkzWARZaLEdKxo6yArG"))

  # valores esperados para o dataframe que será gerado
  expect_equal(teste1$album[1], "Chosen")
  expect_equal(teste1$track_name[2], "Heat Waves")
  expect_equal(nrow(teste1), 5)
  expect_equal(ncol(teste1), 28)
  expect_equal(colnames(teste1), c("rowid","track_id","explicit", "album", "album_id", "artist_id", "track_popularity", "track_name", "artist_name", "genres", "artist_popularity", "followers", "danceability", "energy", "key", "loudness", "mode", "speechiness", "acousticness", "instrumentalness", "liveness", "valence", "tempo", "uri", "track_href", "analysis_url", "duration_ms", "time_signature"))

})
