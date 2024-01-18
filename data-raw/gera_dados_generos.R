## code to prepare `dados_generos` dataset goes here

definir_ids('therezzza', 'bf97cb31956a48159ea6b8524cdaa02a',
            '3c909290b04f4e65afb0958e9bd188bf')

generos <- list(
  rock = "Rock",
  pop = "Pop",
  hip_hop = "Hip Hop",
  classical = "Classical",
  jazz = "Jazz",
  eletronic = "eletronic")

dados_generos <- purrr::map(generos, obter_musicas_genero)

dados_generos <- dados_generos |>
  purrr::list_rbind(names_to = "genero") |>
  dplyr::distinct(track.id, .keep_all = TRUE)

usethis::use_data(dados_generos, overwrite = TRUE)
