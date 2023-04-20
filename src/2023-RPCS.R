# VIII Reunião Paranaense de Ciência do Solo | Dois Vizinhos | Paraná
# Processamento de Dados no FEBR para Cálculo de Estoques de Carbono do Solo
# Alessandro Samuel-Rosa, Taciara Z. Horst, Graziele F.D. Wendling, Aline M. Huf dos Reis
# 2023 CC-BY
rm(list = ls())

# Install and load required packages
if (!require("data.table")) {
  install.packages("data.table")
}
if (!require("sf")) {
  install.packages("sf")
}
if (!require("geobr")) {
  install.packages("geobr")
}
brazil <- geobr::read_country()

# Read processed data (2021 FEBR snapshot)
url <- "http://cloud.utfpr.edu.br/index.php/s/QpG6Tcr6x1NBOcI/download"
temp <- tempfile(fileext = ".zip")
download.file(url = url, destfile = temp)
febr_data <- data.table::fread(unzip(temp), sep = ";", dec = ",")
febr_data[, id := paste0(dataset_id, "-", observacao_id)]
febr_data[, data_coleta_ano := as.integer(format(observacao_data, "%Y"))]
febr_data[data_coleta_ano < 1950, data_coleta_ano := NA_integer_]
febr_data[, espessura := profund_inf - profund_sup]
febr_data[, profundidade := profund_sup + (espessura / 2)]
nrow(unique(febr_data[, "id"])) # 14 043 events
nrow(febr_data) # 50 470 layers
colnames(febr_data)

# Figura 1. Distribuição de frequência empírica dos dados com coordenada temporal (n = 9207)
# disponíveis no FEBR (N = 14.043).
dev.off()
png("febr-report/res/fig/2023-RPCS-time-histogram.png",
    width = 480 * 3, height = 480 * 3, res = 72 * 3
  )
par(mar = c(5, 4, 2, 2) + 0.1)
x <- febr_data[!duplicated(id) & !is.na(data_coleta_ano), data_coleta_ano]
y_max <- max(hist(x, plot = FALSE)$counts)
k <- 10^(nchar(y_max) - 1)
y_max <- ceiling(y_max / k) * k
hist(x,
  panel.first = grid(nx = FALSE, ny = NULL),
  xlab = "Ano de amostragem",
  ylab = paste0("Frequência absoluta (n = ", length(x), ")"),
  ylim = c(0, y_max),
  xlim = c(min(x), as.integer(format(Sys.time(), "%Y"))),
  main = ""
)
rug(x)
dev.off()

# Figura 2. Distribuição de frequência empírica dos dados de proporção de terra fina
# (n = 50.470) disponíveis no FEBR (N = 50.470).
dev.off()
png("febr-report/res/fig/2023-RPCS-fine-earth-histogram.png",
    width = 480 * 3, height = 480 * 3, res = 72 * 3
  )
par(mar = c(5, 4, 2, 2) + 0.1)
x <- febr_data[!is.na(terrafina), terrafina] / 1000
y_max <- max(hist(x, plot = FALSE)$counts)
k <- 10^(nchar(y_max) - 1)
y_max <- ceiling(y_max / k) * k
hist(x,
  panel.first = grid(nx = FALSE, ny = NULL),
  xlab = "Proporção de terra fina",
  ylab = paste0("Frequência absoluta (n = ", length(x), ")"),
  ylim = c(0, y_max),
  xlim = c(0, 1),
  main = ""
)
rug(x)
dev.off()

# Figura 3. Distribuição de frequência empírica dos dados de densidade da terra fina (n = 5.775)
# disponíveis no FEBR (N = 50.470).
dev.off()
png("febr-report/res/fig/2023-RPCS-bulk-density-histogram.png",
    width = 480 * 3, height = 480 * 3, res = 72 * 3
  )
par(mar = c(5, 4, 2, 2) + 0.1)
x <- febr_data[!is.na(dsi), dsi]
y_max <- max(hist(x, plot = FALSE)$counts)
k <- 10^(nchar(y_max) - 1)
y_max <- ceiling(y_max / k) * k
hist(x,
  panel.first = grid(nx = FALSE, ny = NULL),
  xlab = expression("Densidade do solo, g cm"^-3),
  ylab = paste0("Frequência absoluta (n = ", length(x), ")"),
  ylim = c(0, y_max),
  # xlim = c(0, 1000),
  main = ""
)
rug(x)
dev.off()

# Figura 4. Distribuição de frequência empírica dos dados de profundidade amostrada (n = 49.232)
# disponíveis no FEBR (N = 50.470).
dev.off()
png("febr-report/res/fig/2023-RPCS-depth-histogram.png",
    width = 480 * 3, height = 480 * 3, res = 72 * 3
  )
par(mar = c(5, 4, 2, 2) + 0.1)
x <- febr_data[!is.na(profundidade) & profundidade < 400, profundidade] / 100
y_max <- max(hist(x, plot = FALSE)$counts)
k <- 10^(nchar(y_max) - 1)
y_max <- ceiling(y_max / k) * k
hist(x,
  panel.first = grid(nx = FALSE, ny = NULL),
  xlab = "Profundidade, m",
  ylab = paste0("Frequência absoluta (n = ", length(x), ")"),
  ylim = c(0, y_max),
  main = ""
)
rug(x)
dev.off()

# Figura 5. Distribuição de frequência dos dados de concentração de carbono orgânico (n = 32.656)
# disponíveis no FEBR (N = 50.470).
dev.off()
png("febr-report/res/fig/2023-RPCS-carbon-histogram.png",
    width = 480 * 3, height = 480 * 3, res = 72 * 3
  )
par(mar = c(5, 4, 2, 2) + 0.1)
x <- febr_data[!is.na(carbono), carbono]
y_max <- max(hist(x, plot = FALSE)$counts)
k <- 10^(nchar(y_max) - 1)
y_max <- ceiling(y_max / k) * k
hist(x,
  panel.first = grid(nx = FALSE, ny = NULL),
  xlab = expression("Concentração de carbono orgânico, g kg"^-1),
  ylab = paste0("Frequência absoluta (n = ", length(x), ")"),
  ylim = c(0, y_max),
  main = ""
)
rug(x)
dev.off()

# Figura 6. Distribuição espacial empírica dos dados aptos para cálculo dos estoques de carbono
# orgânico do solo (n = 6187).
febr_data_sf <- sf::st_as_sf(
  febr_data[!duplicated(id) & !is.na(coord_x) & !is.na(coord_y) & !is.na(carbono)],
  coords = c("coord_x", "coord_y"), crs = 4623
)
nrow(febr_data_sf)
dev.off()
png("febr-report/res/fig/2023-RPCS-carbon-spatial.png",
  width = 480 * 3, height = 480 * 3, res = 72 * 3
)
par(mar = c(2, 2, 2, 2) + 0.1)
plot(brazil, reset = FALSE,
  main = "", axes = TRUE, lwd = 0.5,
  graticule = TRUE)
plot(febr_data_sf["carbono"], add = TRUE, pch = 21, cex = 0.5, col = "black")
dev.off()