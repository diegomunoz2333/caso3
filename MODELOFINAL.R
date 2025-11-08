library(tidyverse)
library(FactoClass)
library(ade4)


datos_analisis <- Base_2022_filtrada %>%
  filter(Pais != "Luxembourg") %>% 
  select(-Codigo) %>% 
  column_to_rownames("Pais")

cat("Países:", nrow(datos_analisis), "\n")
cat("Variables:", ncol(datos_analisis), "\n\n")

# ACP
acp_temp <- dudi.pca(datos_analisis, center = TRUE, scale = TRUE, scannf = FALSE, nf = ncol(datos_analisis))
varianza_acum <- cumsum(acp_temp$eig) / sum(acp_temp$eig) * 100
n_componentes <- which(varianza_acum >= 80)[1]

cat("=== SELECCIÓN DE COMPONENTES ===\n")
cat("Componentes necesarios para ≥80% varianza:", n_componentes, "\n")
cat("Varianza explicada:", round(varianza_acum[n_componentes], 2), "%\n\n")

acp_resultado <- dudi.pca(
  df = datos_analisis,
  center = TRUE,
  scale = TRUE,
  scannf = FALSE,
  nf = n_componentes
)

# Extraer los factores 
factores <- acp_resultado$li  # cada país con sus coordenadas 

#  CLUSTERING SOBRE LOS FACTORES
distancia <- dist(factores)
arbol <- hclust(distancia, method = "ward.D2")

# Método del codo (WSS)
wss <- sapply(2:10, function(k) {
  grupos <- cutree(arbol, k = k)
  sum(tapply(1:nrow(factores), grupos, function(idx) {
    if (length(idx) > 1) {
      sum(dist(factores[idx, ])^2) / (2 * length(idx))
    } else 0
  }))
})

# Detectar número óptimo
diff_wss <- diff(wss)
diff_diff <- diff(diff_wss)
k_optimo <- which.max(diff_diff) + 2
if (k_optimo > 6) k_optimo <- 5
if (k_optimo < 2) k_optimo <- 2

cat("=== CLUSTERING ===\n")
cat("Número óptimo de clusters según el método del codo:", k_optimo, "\n\n")

# ASIGNAR CLUSTERS
clusters <- cutree(arbol, k = k_optimo)

# Nueva base con clusters
NuevaBase <- data.frame(Cluster = clusters, datos_analisis)

# Promedios por cluster
carac_cont <- NuevaBase %>%
  group_by(Cluster) %>%
  summarise(across(everything(), mean, na.rm = TRUE), .groups = "drop") %>%
  as.data.frame()

# RESULTADOS
resultado_ACP <- list(
  dudi = acp_resultado,
  cluster = clusters,
  tree = arbol,
  carac.cont = carac_cont
)

cat("Clusters formados:", length(unique(clusters)), "\n\n")


# Círculo de correlaciones
s.corcircle(acp_resultado$co)

# Gráfico de países en espacio de factores
s.class(factores, as.factor(clusters), sub = "Clusters en el espacio factorial", possub = "bottomright")

# Dendrograma
plot(arbol, labels = FALSE, main = "Dendrograma (método de Ward)", xlab = "", sub = "")
rect.hclust(arbol, k = k_optimo, border = 2:5)

