# generos

# rap

musicas_para_recomendar <- musicas_curtidas |>
  tibble::rowid_to_column()

# agrupar essas musicas em grupos
# -------------------------------------------------------------------------

# primeiro, vamos pegar as variaveis numericas

numericos <- musicas_para_recomendar |>
  dplyr::select(where(is.numeric)) |>
  dplyr::select(danceability:tempo, time_signature) |>
  dplyr::select(-key)

# a escolha das variaveis é importante
# ver se faz sentido pegar a popularidade, e a duração das musicas,
# por ex

# dados padronizados
padronizados <- scale(numericos)

# biblioteca para fazer os agrupamentos
library(cluster)

d <-dist(padronizados, method = "euclidean")

hc1 <- hclust(d,method="ward.D")
hc2 <- hclust(d,method="complete")
hc3 <- hclust(d,method="ward.D2")
hc4 <- hclust(d,method="single")
hc5 <- hclust(d,method="average")
hc6 <- hclust(d,method="mcquitty")
hc7 <- hclust(d,method="median")
hc8 <- hclust(d,method="centroid")

# nao vou fazer dendrograma porque o banco é gigante

# auxilia na escolha dos grupos
library(NbClust)

agrupamento <- NbClust(data=padronizados, min.nc=7, max.nc=15, index="all", method="complete")
#mostra varios indices para auxiliar na escolha do numero de grupos

grupos <- agrupamento$Best.partition

d.coph <- cophenetic(hc2)
cor(d, d.coph)

agrupados <- bind_cols(musicas_para_recomendar, grupos)


table(agrupados$...29)

dados_grupos <- agrupados |>
  group_by(...29) |>
  summarise(across(
    .cols = where(is.numeric),
    .fns = mean
  ))


library(skimr)

info <- skim(musicas_para_recomendar)

#---------------------------------------------------------------------
# algoritmo distancia por genero ------------------------------------------

cro <- expand.grid(musicas_para_recomendar$genres, musicas_para_recomendar$genres)

vamove <- cro |>
  dplyr::mutate(
    distancia = purrr::map2_dbl(Var1, Var2, calcula_distancia)
  )

m_d <- matrix(vamove$distancia, ncol = 369, nrow = 369)

new_d <- m_d/4 + as.matrix(d)

arruma_string <- function(string){
  string_aux <- string |>
    stringr::str_remove_all(",") |>
    stringr::str_squish() |>
    stringr::str_split(" ")

  return(string_aux[[1]] |> unique())
}

calcula_distancia <- function(string1, string2){

genero1 <- arruma_string(string1)
genero2 <- arruma_string(string2)

combinacoes <- purrr::cross2(genero1, genero2)

i = 1
matches = 0

for (i in 1:length(combinacoes)){

  f <- combinacoes[[i]]

  if (isTRUE(f[[1]] == f[[2]])){
    matches <-  matches + 1
  }

}

prop <- 1 - matches/mean(c(length(genero1), length(genero2)))

}

# coisas a se pensar:

# que peso dar para a distancia por genero?

# casos como: funk metal e funk carioca
# se quebrar em todas as palavras, isso vai dar um match,
# mas nao tem nada a ver... ou sera q tem?

# separar os generos só pela virgula e ver no que da


vetor <- new_d[124,]
vetor[25]
dplyr::bind_cols(musicas_para_recomendar, vetor) |>
  dplyr::arrange(vetor) |>
  View()

d[1183,]
dim(d)


# simulando o programa ----------------------------------------------------

# vai pedir pra pessoa falar uma musica

obter_recomendacoes <- function(musica_artista){

procurando_musica <- spotifyr::search_spotify(q = musica_artista,
                                     type = c("track"),
                                     market = "BR",
                                     limit = 1,
                                     offset = 0,
                                     include_external = "audio",
                                     authorization = spotifyr::get_spotify_access_token(
                                       client_id = Sys.getenv("SPOTIFY_CLIENT_ID"),
                                       client_secret = Sys.getenv("SPOTIFY_CLIENT_SECRET")),
                                     include_meta_info = TRUE)

borave <- procurando_musica$tracks$items$id[1]

infos_novas <- obter_dados_musicas(borave) |>
  dplyr::select(track_name, artist_name, danceability:tempo, time_signature, uri, genres)

para_recomendar <- musicas_curtidas |>
  dplyr::select(track_name, artist_name, danceability:tempo, time_signature, uri, genres) |>
  dplyr::bind_rows(infos_novas)

numericos <- para_recomendar |>
  dplyr::select(danceability:tempo, time_signature) |>
  dplyr::select(-key)

padronizados <- scale(numericos)

d <-dist(padronizados, method = "euclidean")

cro <- expand.grid(para_recomendar$genres |>
                     tail(1), para_recomendar$genres)

vamove <- cro |>
  dplyr::mutate(
    distancia = purrr::map2_dbl(Var1, Var2, calcula_distancia)
  )

m_d <- vamove$distancia

matriz_d <- as.matrix(d)

n <- length(m_d)

new_d <- m_d/4 + matriz_d[n, ]

name <- paste("Recomendações:",
              para_recomendar$track_name[n],
              "-",
              para_recomendar$artist_name[n])

descric <- paste("Recomendações da Guia dos Musiqueiros com base na música:",
                 para_recomendar$track_name[n],
                 "-",
                 para_recomendar$artist_name[n],
                 ":)")

link <- dplyr::bind_cols(para_recomendar, dist = new_d) |>
  dplyr::arrange(dist) |>
  dplyr::filter(dist != 0) |>
  head(30) |>
  criar_playlist(name, descric, FALSE, FALSE)

return(link)
}

obter_recomendacoes("bonde to tigrao")
