# modelo de classificacao de genero

# pacotes -----------------------------------------------------------------

library(tidymodels)
library(ISLR)
library(tidyverse)
library(modeldata)
library(pROC)
library(vip)
library(yardstick)

# dados -------------------------------------------------------------------

# balanceando os dados

devtools::load_all()

n_max <- dados_generos |>
  dplyr::count(genero) |>
  dplyr::slice_min(n) |>
  dplyr::pull(n)

dados <- dados_generos |>
  group_by(genero) |>
  dplyr::slice_sample(
    n = n_max,
    replace = FALSE
  ) |>
  dplyr::select(genero, danceability, energy, key, loudness, mode, speechiness,
                acousticness, instrumentalness, liveness, valence, tempo,
                time_signature, track.duration_ms, track.popularity)
  # dplyr::mutate(
  #   genero = as.factor(genero)
  # )


# base de treino e teste --------------------------------------------------

set.seed(1)
initial_split <- initial_split(dados, prop = 0.8)

train <- training(initial_split)
test  <- testing(initial_split)

# Reamostragem ------------------------------------------------------------

resamples <- vfold_cv(train, v = 20)

# exploratoria ------------------------------------------------------------

glimpse(dados)

dados |>
  dplyr::group_by(genero) |>
  dplyr::summarise(
    dplyr::across(
      .cols = danceability:track.popularity,
      .fns = median
    )
  )

# data prep ---------------------------------------------------------------

rf_recipe <- recipe(genero ~ ., data = train)

# dar uma olhada no resultado da receita.
rec_prep <- rf_recipe |>
  prep()

rec_prep |>
  bake(new_data = train)

# modelo ------------------------------------------------------------------

rf_model <- rand_forest(mtry = 5, trees = 70, min_n = 5) |>
  set_mode("classification") |>
  set_engine("ranger")

# workflow ----------------------------------------------------------------

rf_wf <- workflow() |>
  add_model(rf_model) |>
  add_recipe(rf_recipe)

# tune --------------------------------------------------------------------

grid_rf <- grid_regular(
  min_n(range = c(5, 20)),
  levels = 12
)

rf_tune_grid <- tune_grid(
  rf_wf,
  resamples = resamples,
  grid = grid_rf,
  metrics = metric_set(precision)
)

autoplot(rf_tune_grid)
show_best(rf_tune_grid, n = 1, "precision")

collect_metrics(rf_tune_grid)

# Desempenho do modelo final -------------------------------------------------

rf_best_hiperparams <- select_best(rf_tune_grid, "precision")
rf_wf <- rf_wf |>  finalize_workflow(rf_best_hiperparams)
rf_last_fit <- last_fit(rf_wf, initial_split)

collect_metrics(rf_last_fit)
test_preds <- collect_predictions(rf_last_fit)
precision(test_preds, genero, .pred_class)

# salvando modelo ---------------------------------------------------------

genero_final_rf_model <- rf_wf %>% fit(dados)

write_rds(genero_final_rf_model, "modelos_genero/modelo_genero_rf.rds")


# estatisticas ------------------------------------------------------------

test_preds |>
  dplyr::mutate(acertou = dplyr::if_else(.pred_class == genero, 1, 0)) |>
  dplyr::group_by(genero) |>
  count(acertou)


# predicao ----------------------------------------------------------------

dados_novos <- dados_generos

probs <- predict(genero_final_rf_model, dados_novos, type = "prob")

classe_predita <- predict(genero_final_rf_model, dados_novos)

dados_novos |>
  dplyr::select(genero, track.name) |>
  dplyr::bind_cols(classe_predita) |>
  dplyr::bind_cols(probs) |>
  View()
