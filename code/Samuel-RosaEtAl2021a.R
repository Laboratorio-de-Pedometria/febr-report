# SAMUEL-ROSA, A.; HORST, T. Z.
# Disponibilidade de dados abertos do solo no Estado do Paraná.
# VII Reunião Paranaense de Ciência do Solo. Anais... In: VII REUNIÃO PARANAENSE DE CIÊNCIA DO SOLO.
# Guarapuava, Paraná: Sociedade Brasileira de Ciência do Solo - Núcleo Estadual Paraná, 2021.

# Carregar índice de conjuntos de dados publicados
# Quantos conjuntos de dados publicados?
febr_index <- data.table::fread("~/ownCloud/febr-repo/publico/febr-indice.txt")
publicados <- nrow(febr_index)
publicados

# Carregar conjunto de dados padronizados e harmonizados
febr_dados <- data.table::fread("~/ownCloud/febr-repo/publico/febr-superconjunto.txt")
head(febr_dados)

# Quantos conjuntos de dados padronizados e harmonizados?
padronizados <- length(unique(febr_dados[, dataset_id]))
padronizados; padronizados / publicados

# Quantos eventos padronizados e harmonizados?
eventos_brazil <- nrow(unique(febr_dados[, dataset_id, observacao_id]))

# Quantos eventos padronizados e harmonizados por estado?
uf <- "PR"
febr_estado <- febr_dados[estado_id == uf]
length(unique(febr_estado[, dataset_id]))
febr_estado <- nrow(unique(febr_estado[, dataset_id, observacao_id]))
febr_estado; febr_estado / eventos_brazil

# 
febr_data <- as.Date(febr_dados[estado_id == uf, observacao_data])
datas <- as.Date(c("1900-01-01", "1960-01-01", "1990-01-01", "2010-01-01", "2020-01-01"))
datas <- sapply(datas, function (x) sum(febr_data < x, na.rm = TRUE))
datas <- diff(datas)

