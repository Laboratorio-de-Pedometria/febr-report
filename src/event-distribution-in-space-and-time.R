# SAMUEL-ROSA, A.
# Distribuição espacial e temporal de eventos no território brasileiro

# Instalar e carregar pacotes necessários
if (!require(data.table)) {
  install.packages("data.table", dependencies = TRUE)
}
if (!require(sf)) {
  install.packages("sf", dependencies = TRUE)
}
if (!require(geobr)) {
  install.packages("geobr", dependencies = TRUE)
}
if (!require(rnaturalearth)) {
  install.packages("rnaturalearth", dependencies = TRUE)
}

# Carregar super conjunto de dados padronizados e harmonizados
febr_data <- data.table::fread(
  input = "https://cloud.utfpr.edu.br/index.php/s/nEXaoXIE0nZ1AqG/download", dec = ",")
head(febr_data)

# Filtrar eventos
which_cols <- c("dataset_id", "observacao_id", "coord_x", "coord_y", "observacao_data")
which_rows <- duplicated(febr_data[, dataset_id, observacao_id])
febr_data <- febr_data[!which_rows, ..which_cols]
which_rows <- (febr_data[, !is.na(coord_x)] + febr_data[, !is.na(coord_y)])
febr_data <- febr_data[which_rows == 2, ]

# Criar objeto espacial (sf)
febr_data <- sf::st_as_sf(febr_data, coords = c("coord_x", "coord_y"), crs = 4674)

# Identificar ano de observação do evento
febr_data[["observacao_data"]] <- as.Date(febr_data[["observacao_data"]])
febr_data[["observacao_data"]] <- format(febr_data[["observacao_data"]], "%Y")
febr_data[["observacao_data"]] <- as.numeric(febr_data[["observacao_data"]])

# Descarregar limites do Brasil, biomas brasileiros e países sul americanos,
# inclusive Guiana Francesa
brazil <- geobr::read_country()
biomes <- geobr::read_biomes()
biomes <- biomes[biomes$name_biome != "Sistema Costeiro", "code_biome"]
biomes$code_biome <- 0
southamerica <- rnaturalearth::ne_countries(continent = c("south america", "europe"),
  returnclass = "sf", scale = "medium")
southamerica <- southamerica[, "iso_a2"]

# Filtrar eventos contidos dentro dos biomas brasileiros
points_in_country <- sf::st_intersects(brazil, febr_data)
febr_data <- febr_data[points_in_country[[1]], ]

# Definir fatias temporais
time_slices <- seq(1920, 2020, length.out = 6)

# Criar figura
dev.off()
png("febr-report/res/fig/event-distribution-in-space-and-time.png",
  width = 480 * 6, height = 480 * 6, res = 72 * 6)
# x11()
par(mar = rep(1.9, 4))
plot_matrix <- c(1, 2, 2, 3, 2, 2, 4, 5, 6)
plot_matrix <- matrix(plot_matrix, ncol = 3)
plot_matrix <- layout(plot_matrix)
for (i in 1:6) {
  if (i == 2) {
    plot(brazil, reset = FALSE, main = "BRAZIL", col = "transparent",
      axes = TRUE, graticule = TRUE, lwd = 0.01)
    plot(southamerica, reset = FALSE, col = "gray96", add = TRUE, lwd = 0.5)
    plot(biomes, reset = FALSE, add = TRUE, main = "", col = "#eeece1", lwd = 0.5)
    plot(febr_data["observacao_id"], add = TRUE, col = "firebrick", cex = 0.5)
  } else {
    j <- ifelse(i < 2, i, i - 1)
    main <- paste0(time_slices[j], "-", time_slices[j + 1])
    idx <- (time_slices[j] <= febr_data[["observacao_data"]]) +
      (febr_data[["observacao_data"]] < time_slices[j + 1])
    idx <- (idx == 2)
    plot(brazil, reset = FALSE, main = main, col = "transparent",
      axes = TRUE, graticule = TRUE, lwd = 0.01)
    plot(southamerica, reset = FALSE, col = "gray96", add = TRUE, lwd = 0.5)
    plot(biomes, reset = FALSE, add = TRUE, col = "#eeece1", lwd = 0.5)
    plot(febr_data[idx, "observacao_id"], add = TRUE, col = "firebrick", cex = 0.5)
  }
}
dev.off()
