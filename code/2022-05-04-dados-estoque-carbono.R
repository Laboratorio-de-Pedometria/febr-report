
febr_dados <- data.table::fread(
  input = "https://cloud.utfpr.edu.br/index.php/s/nEXaoXIE0nZ1AqG/download", dec = ",")
head(febr_data)
nrow(febr_data)
length(unique(febr_data[["dataset_id"]]))
nrow(unique(febr_data[, c("dataset_id", "observacao_id")]))

# CONCENTRAÇÃO DE CARBONO ORGÂNICO

## Total de amostras e disponibilidade de dados de concentração de carbono orgânico
sum(!is.na(febr_data[["carbono"]]))

## Histograma de frequência
dev.off()
png("febr-report/res/fig/concentracao-de-carbono-organico-histograma.png",
  width = 480 * 2, height = 480 * 2, res = 72 * 2)
par(mar = c(5, 4, 1, 2) + 0.1)
hist(febr_data[["carbono"]],
  main = "", xlab = "Concentração de carbono orgânico, g/kg", ylab = "Frequência",
  sub = format(Sys.time(), "%Y-%m-%d"))
dev.off()

# PROFUNDIDADE AMOSTRADA

## Cálculo da profundidade amostrada
febr_data[["profundidade"]] <- rowMeans(febr_data[, c("profund_sup", "profund_inf")])
idx <- which(febr_data[["profundidade"]] < 400)

## Total de amostras e disponibilidade de dados de profundidade amostrada
sum(!is.na(febr_data[["profundidade"]][idx]))

## Histograma de frequência
dev.off()
png("febr-report/res/fig/profundidade-amostrada-histograma.png",
  width = 480 * 2, height = 480 * 2, res = 72 * 2)
par(mar = c(5, 4, 1, 2) + 0.1)
hist(febr_data[["profundidade"]][idx],
  main = "", xlab = "Profundidade amostrada, cm", ylab = "Frequência",
  sub = format(Sys.time(), "%Y-%m-%d"))
dev.off()

# DENSIDADE DA TERRA FINA

## Total de amostras e disponibilidade de dados de densidade do solo
sum(!is.na(febr_data[["dsi"]]))

## Histograma de frequência
dev.off()
png("febr-report/res/fig/densidade-da-terra-fina-histograma.png",
  width = 480 * 2, height = 480 * 2, res = 72 * 2)
par(mar = c(5, 4, 1, 2) + 0.1)
hist(febr_data[["dsi"]],
  main = "", xlab = "Densidade da terra fina, g/cm^3", ylab = "Frequência",
  sub = format(Sys.time(), "%Y-%m-%d"))
dev.off()

# PROPORÇÃO DE TERRA FINA

## Total de amostras e disponibilidade de dados de proporção de terra fina
sum(!is.na(febr_data[["terrafina"]]))

## Histograma de frequência
dev.off()
png("febr-report/res/fig/proporcao-de-terra-fina.png",
  width = 480 * 2, height = 480 * 2, res = 72 * 2)
par(mar = c(5, 4, 1, 2) + 0.1)
hist(febr_data[["terrafina"]] / 1000,
  main = "", xlab = "Proporção de terra fina", ylab = "Frequência",
  sub = format(Sys.time(), "%Y-%m-%d"))
dev.off()

# ESTOQUE DE CARBONO

## Disponibilidade de dados de estoque de carbono orgânico
vars <- c("carbono", "dsi", "terrafina", "profundidade")
solo <- sum(rowSums(!is.na(febr_data[, ..vars]))  == length(vars))
vars <- c("carbono", "dsi", "terrafina", "profundidade", "coord_x")
coord <- sum(rowSums(!is.na(febr_data[, ..vars]))  == length(vars))
vars <- c("carbono", "dsi", "terrafina", "profundidade", "coord_x", "observacao_data")
tempo <- sum(rowSums(!is.na(febr_data[, ..vars]))  == length(vars))
vars <- c(Solo = solo, Coordenadas = coord, Data = tempo)

# Gráfico de colunas
dev.off()
png("febr-report/res/fig/barplot-solo-coord-tempo.png",
  width = 480 * 2, height = 480 * 2, res = 72 * 2)
par(mar = c(5, 4, 1, 2) + 0.1)
barplot(vars,
  main = "", xlab = "Variáveis utilizadas no modelo espaço-temporal", ylab = "Frequência",
  sub = format(Sys.time(), "%Y-%m-%d"))
dev.off()

# COORDENADAS ESPACIAIS
which_rows <- (febr_data[, !is.na(coord_x)] + febr_data[, !is.na(coord_y)])
febr_data_sp <- febr_data[which_rows == 2, ]
febr_data_sp <- sf::st_as_sf(febr_data_sp, coords = c("coord_x", "coord_y"), crs = 4674)
brazil <- geobr::read_country()
points_in_country <- sf::st_intersects(brazil, febr_data_sp)
febr_data_sp <- febr_data_sp[points_in_country[[1]], ]

## Disponibilidade de dados de coordenadas espaciais
nrow(unique(febr_data[, c("coord_x", "coord_y")]))

# Distribuição espacial
dev.off()
png("febr-report/res/fig/distribuicao-espacial.png",
  width = 480 * 2, height = 480 * 2, res = 72 * 2)
par(mar = c(5, 4, 1, 2) + 0.1)
plot(brazil, reset = FALSE, axes = TRUE, graticule = TRUE, main = "", col = "white",
  xlab = "Longitude", ylab = "Latitude", sub = format(Sys.time(), "%Y-%m-%d"))
plot(febr_data_sp["observacao_id"], add = TRUE, col = "black", pch = 20, cex = 0.5)
dev.off()

# COORDENADA TEMPORAL

## Disponibilidade de dados da coordenada temporal
tmp <- unique(febr_data[, c("dataset_id", "observacao_id", "observacao_data")])
tmp[["observacao_data"]] <- format(as.Date(tmp[["observacao_data"]], "%Y-%m-%d"), "%Y")
tmp[["observacao_data"]] <- as.numeric(tmp[["observacao_data"]])
idx <- tmp[["observacao_data"]] > 1960
sum(!is.na(tmp[["observacao_data"]][idx]))

## Histograma de frequência
dev.off()
png("febr-report/res/fig/histogram-ano-de-amostragem.png",
  width = 480 * 2, height = 480 * 2, res = 72 * 2)
par(mar = c(5, 4, 1, 2) + 0.1)
hist(tmp[["observacao_data"]][idx],
  main = "", xlab = "Ano de amostragem", ylab = "Frequência",
  sub = format(Sys.time(), "%Y-%m-%d"))
dev.off()
