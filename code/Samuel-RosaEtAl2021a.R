# SAMUEL-ROSA, A.; HORST, T. Z.
# Disponibilidade de dados abertos do solo no Estado do Paraná.
# VII Reunião Paranaense de Ciência do Solo. Anais... In: VII REUNIÃO PARANAENSE DE CIÊNCIA DO SOLO.
# Guarapuava, Paraná: Sociedade Brasileira de Ciência do Solo - Núcleo Estadual Paraná, 2021.
# Homepage: https://www.even3.com.br/rpcs2021/
# Resumo: https://docs.google.com/document/d/1GIiiS1p_MnKV4G9krUHnjuH8W5bu2UCq/
# Pôster: https://docs.google.com/presentation/d/1YLXPRFLS0fDEzR603JaWTXmG_wnrdef3CMRZFZzUBNo/

# Carregar índice de conjuntos de dados publicados no FEBR
febr_index <- data.table::fread("~/ownCloud/febr-repo/publico/febr-indice.txt")

# Quantos conjuntos de dados já foram publicados?
publicados <- nrow(febr_index)
publicados

# Carregar super conjunto de dados padronizados e harmonizados
febr_dados <- data.table::fread("~/ownCloud/febr-repo/publico/febr-superconjunto.txt", dec = ",")
head(febr_dados)

# Quantas amostras já foram padronizadas e harmonizadas?
amostras_brasil <- nrow(febr_dados)
amostras_brasil

# Quantos conjuntos de dados padronizados e harmonizados?
padronizados <- length(unique(febr_dados[, dataset_id]))
padronizados; round(padronizados / publicados * 100)

# Quantos eventos já foram padronizados e harmonizados?
eventos_brazil <- nrow(unique(febr_dados[, dataset_id, observacao_id]))
eventos_brazil

# Quantas amostras padronizadas e harmonizadas no Paraná?
uf <- "PR"
febr_estado <- febr_dados[estado_id == uf]
amostras_parana <- nrow(febr_estado)
amostras_parana; round(amostras_parana / amostras_brasil * 100)

# Quantos eventos padronizados e harmonizados no Paraná?
length(unique(febr_estado[, dataset_id]))
eventos_parana <- nrow(unique(febr_estado[, dataset_id, observacao_id]))
eventos_parana; round(eventos_parana / eventos_brazil * 100)

# Quantas amostras não possuem data de coleta?
data_faltante <- febr_dados[estado_id == uf & is.na(observacao_data), dataset_id, observacao_id]
data_faltante <- nrow(unique(data_faltante))
data_faltante; round(data_faltante / eventos_parana * 100)

# Para as demais amostras, quando elas foram coletadas?
data_presente <-
  febr_dados[
    estado_id == uf & !is.na(observacao_data),
    c("dataset_id", "observacao_id", "observacao_data")
  ]
data_presente <- unique(data_presente)
data_interesse <- as.Date(as.character(c(1900, seq(1960, 2020, 10))), format = "%Y")
data_contagem <- sapply(data_interesse, function(x) sum(data_presente[, observacao_data] < x, na.rm = TRUE))
data_contagem <- diff(data_contagem)
names(data_contagem) <- data_interesse[-1]
data_contagem; round(data_contagem / sum(data_contagem) * 100)

# Quantos eventos não possuem coordenadas espaciais?
coord_faltante <- febr_dados[estado_id == uf & is.na(coord_x), dataset_id, observacao_id]
coord_faltante <- nrow(unique(febr_coord))
coord_faltante; round(coord_faltante / eventos_parana * 100)

# FIGURA MOSTRANDO DISTRIBUIÇÃO ESPACIAL E TEMPORAL DOS EVENTOS
# Preparar dados dos eventos
which_cols <- c("dataset_id", "observacao_id", "coord_x", "coord_y", "observacao_data")
estado_espacial <- febr_estado[, ..which_cols]
estado_espacial <- estado_espacial[!is.na(coord_x)]
estado_espacial <- estado_espacial[!is.na(coord_y)]
estado_espacial <- sf::st_as_sf(estado_espacial, coords = c("coord_x", "coord_y"), crs = 4674)
estado_espacial[["observacao_data"]] <- as.Date(estado_espacial[["observacao_data"]])
estado_espacial[["observacao_data"]] <- format(estado_espacial[["observacao_data"]], "%Y")
estado_espacial[["observacao_data"]] <- as.numeric(estado_espacial[["observacao_data"]])
# Descarregar limite do estado e filtrar eventos
estado <- geobr::read_state()
qual_estado <- estado[["name_state"]] == "Paraná"
estado <- estado[qual_estado, "name_state"]
points_in_state <- sf::st_intersects(estado, estado_espacial)
estado_espacial <- estado_espacial[points_in_state[[1]], ]
year_seq <- seq(1920, 2020, length.out = 6)
# Criar figura
dev.off()
png("res/fig/parana-espaco-tempo.png", width = 480 * 5, height = 480 * 5, res = 72 * 5)
plot_matrix <- c(1, 2, 2, 3, 2, 2, 4, 5, 6)
plot_matrix <- matrix(plot_matrix, ncol = 3)
plot_matrix <- layout(plot_matrix)
for (i in 1:6) {
  if (i == 2) {
    plot(estado, reset = FALSE, axes = TRUE, graticule = TRUE, main = "", col = "white")
    plot(estado_espacial["observacao_id"], add = TRUE, col = "black")
  } else {
    if (i < 2) {
      main <- paste0(year_seq[i], "-", year_seq[i + 1])
      idx <- {
              year_seq[i] < estado_espacial[["observacao_data"]]
            } & {
              estado_espacial[["observacao_data"]] < year_seq[i + 1]
            }
    } else {
      main <- paste0(year_seq[i - 1], "-", year_seq[i])
      idx <- {
              year_seq[i - 1] < estado_espacial[["observacao_data"]]
            } & {
              estado_espacial[["observacao_data"]] < year_seq[i]
            }
    }
    plot(estado, reset = FALSE, axes = FALSE, graticule = TRUE, main = main,
      col = "white", col_graticule = "white")
    plot(estado_espacial[idx, "observacao_id"], add = TRUE, col = "black")
  }
}
dev.off()
