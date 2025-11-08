
library(tidyverse)
library(FactoClass)
library(ade4)




datos_analisis <- Base_2022_filtrada%>%
  select(-Codigo) %>%         # si no tienes columna "Codigo", elimina esta línea
  column_to_rownames("Pais")

cat("Países:", nrow(datos_analisis), "\n")
cat("Variables:", ncol(datos_analisis), "\n\n")

# --- 2. DETERMINAR NÚMERO DE COMPONENTES PARA ≥80% VARIANZA ---

acp_temp <- dudi.pca(datos_analisis, center = TRUE, scale = TRUE, scannf = FALSE, nf = ncol(datos_analisis))
varianza_acum <- cumsum(acp_temp$eig) / sum(acp_temp$eig) * 100
n_componentes <- which(varianza_acum >= 80)[1]

cat("=== SELECCIÓN DE COMPONENTES ===\n")
cat("Componentes necesarios para ≥80% varianza:", n_componentes, "\n")
cat("Varianza explicada:", round(varianza_acum[n_componentes], 2), "%\n\n")

# --- 3. ACP FINAL ---

acp_resultado <- dudi.pca(
  df = datos_analisis,
  center = TRUE,
  scale = TRUE,
  scannf = FALSE,
  nf = n_componentes
)

# --- 4. CLUSTERING JERÁRQUICO (Ward) ---

distancia <- dist(acp_resultado$li)
arbol <- hclust(distancia, method = "ward.D2")

# Calcular within sum of squares (WSS) para 2–10 clusters
wss <- sapply(2:10, function(k) {
  clusters_temp <- cutree(arbol, k = k)
  sum(tapply(1:nrow(acp_resultado$li), clusters_temp, function(idx) {
    if(length(idx) > 1) {
      sum(dist(acp_resultado$li[idx, ])^2) / (2 * length(idx))
    } else {
      0
    }
  }))
})

# Detectar el "codo" en la curva WSS
diff_wss <- diff(wss)
diff_diff <- diff(diff_wss)
k_optimo <- which.max(diff_diff) + 2
if(k_optimo > 6) k_optimo <- 5
if(k_optimo < 2) k_optimo <- 2

cat("=== CLUSTERING ===\n")
cat("Número óptimo de clusters según el método del codo:", k_optimo, "\n\n")

# --- 5. ASIGNACIÓN DE CLUSTERS ---

clusters <- cutree(arbol, k = k_optimo)
NuevaBase <- data.frame(Cluster = clusters, datos_analisis)

carac_cont <- NuevaBase %>%
  group_by(Cluster) %>%
  summarise(across(everything(), mean, na.rm = TRUE), .groups = "drop") %>%
  as.data.frame()

# --- 6. RESULTADO FINAL ---

resultado_ACP <- list(
  dudi = acp_resultado,
  cluster = clusters,
  tree = arbol,
  carac.cont = carac_cont
)

cat("Clusters formados:", length(unique(clusters)), "\n\n")

# --- 7. OPCIONAL: VISUALIZACIONES ---

# Círculo de correlaciones
s.corcircle(resultado_ACP$dudi$co)

# Gráfico de individuos (países)
s.label(resultado_ACP$dudi$li, label = rownames(datos_analisis))

# Gráfico de variables
s.label(resultado_ACP$dudi$co, xax = 1, yax = 2, sub = "Componente 1 y 2", possub = "bottomright")

# Gráfico conjunto
scatter(resultado_ACP$dudi, xax = 1, yax = 2)

# Gráfico por grupos
Grupo <- as.factor(NuevaBase$Cluster)
s.class(resultado_ACP$dudi$li, Grupo, sub = "Componentes 1 y 2", possub = "bottomright")
