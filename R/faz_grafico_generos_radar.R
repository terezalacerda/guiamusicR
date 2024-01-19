# recebe uma nova observação e faz o grafico de generos

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

