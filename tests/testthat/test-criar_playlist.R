test_that("criar_playlist works", {

  # definindo ids para que as funções funcionem
  definir_ids('therezzza', 'd31252a2880546b3ba640d5d64b7e652',
              '964f9c50f2254b19a5ab0e7f333b3c49')

  # teste que deve funcionar normalmente
  teste1 <- criar_playlist(data.frame(
  uri = c("spotify:track:3Wrjm47oTz2sjIgck11l5e",
  "spotify:track:02MWAaffLxlfxAUY7c5dvx",
  "spotify:track:0VjIjW4GlUZAMYd2vXMi3b",
  "spotify:track:37BZB0z9T8Xu7U3e65qxFy",
  "spotify:track:07MDkzWARZaLEdKxo6yArG")),
  nome_playlist = "Nome da Playlist",
  descricao = "descricao da playlist",
  publica = FALSE, colaborativa = FALSE)

  expect_true(stringr::str_detect(teste1, "https://open\\.spotify\\.com/playlist"))


  # passar vetor em vez de df não funciona
  expect_error(criar_playlist(c("spotify:track:3Wrjm47oTz2sjIgck11l5e",
                                "spotify:track:02MWAaffLxlfxAUY7c5dvx",
                                "spotify:track:0VjIjW4GlUZAMYd2vXMi3b",
                                "spotify:track:37BZB0z9T8Xu7U3e65qxFy",
                                "spotify:track:07MDkzWARZaLEdKxo6yArG"),
                              nome_playlist = "Nome da Playlist",
                              descricao = "descricao da playlist",
                              publica = FALSE, colaborativa = FALSE))

  # nao passou nome da playlist
  expect_error(criar_playlist(data.frame(
    uri = c("spotify:track:3Wrjm47oTz2sjIgck11l5e",
                                "spotify:track:02MWAaffLxlfxAUY7c5dvx",
                                "spotify:track:0VjIjW4GlUZAMYd2vXMi3b",
                                "spotify:track:37BZB0z9T8Xu7U3e65qxFy",
                                "spotify:track:07MDkzWARZaLEdKxo6yArG")),
                              descricao = "descricao da playlist",
                              publica = FALSE, colaborativa = FALSE))

  # nao passou uris das musicas
  expect_error(criar_playlist(nome_playlist = "Nome da Playlist",
                              descricao = "descricao da playlist",
                              publica = FALSE, colaborativa = FALSE))




})
