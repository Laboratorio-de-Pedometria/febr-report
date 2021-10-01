# SAMUEL-ROSA, A.; HORST, T. Z.
# Disponibilidade de dados abertos do solo no Estado do Paraná.
# VII Reunião Paranaense de Ciência do Solo. Anais... In: VII REUNIÃO PARANAENSE DE CIÊNCIA DO SOLO.
# Guarapuava, Paraná: Sociedade Brasileira de Ciência do Solo - Núcleo Estadual Paraná, 2021.

# Carregar índice de conjuntos de dados publicados no FEBR
febr_index <- data.table::fread("~/ownCloud/febr-repo/publico/febr-indice.txt")

# Quantos conjuntos de dados já foram publicados?
publicados <- nrow(febr_index)
publicados

# Carregar super conjunto de dados padronizados e harmonizados
febr_dados <- data.table::fread("~/ownCloud/febr-repo/publico/febr-superconjunto.txt")
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
