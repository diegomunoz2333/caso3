library(tidyverse)
library(readxl)
library(factoextra)
library(cluster)
library(dendextend)
library(pheatmap)
library(mclust)

datos23 <- "P_Data_Extract_From_World_Development_Indicators (1).xlsx"
datosfinal <- read_excel(datos23)

datos <- datosfinal %>%
  select(`Country Name`, `Series Name`, `2022 [YR2022]`) %>%
  mutate(`2022 [YR2022]` = as.numeric(gsub("[^0-9\\.-]", "", as.character(`2022 [YR2022]`)))) %>%
  group_by(`Country Name`, `Series Name`) %>%
  summarise(valor = mean(`2022 [YR2022]`, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = `Series Name`, values_from = valor)

# Eliminar variables con >50% NA
datos <- datos %>%
  filter(!is.na(`Country Name`)) %>%
  select(where(~ mean(is.na(.x)) < 0.5))

# Reemplazar NA por la media
datos_imputed <- datos %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)))

# Matriz numérica y vector de países
datos_num <- datos_imputed %>%
  select(-`Country Name`) %>%
  mutate(across(everything(), as.numeric))

paises <- datos_imputed$`Country Name`

# Eliminar variables constantes
datos_num <- datos_num %>%
  select(where(is.numeric)) %>%
  select(where(~ sd(.x, na.rm = TRUE) > 0))

# =======================
# 2) PCA
# =======================
res.pca <- prcomp(datos_num, scale. = TRUE)

fviz_eig(res.pca, addlabels = TRUE, main = "Varianza explicada por componentes")
summary(res.pca)

# =======================
# 3) Seleccionar número de componentes
# =======================
eig.val <- get_eigenvalue(res.pca)
cum_var <- cumsum(eig.val[,2])
num_pcs <- which(cum_var >= 80)[1]
if (is.na(num_pcs)) num_pcs <- 3
cat("Número de PC seleccionadas (>=80% varianza):", num_pcs, "\n")

pca_scores <- as.data.frame(res.pca$x[, 1:num_pcs])
rownames(pca_scores) <- paises

# =======================
# 4) Determinar número óptimo de clusters
# =======================
set.seed(123)
fviz_nbclust(pca_scores, kmeans, method = "wss") + labs(title = "Método del codo (WSS)")
fviz_nbclust(pca_scores, kmeans, method = "silhouette") + labs(title = "Método de la silueta")

# Escogemos k automático según silhouette
sil_scores <- sapply(2:8, function(k) {
  km <- kmeans(pca_scores, centers = k, nstart = 10)
  ss <- silhouette(km$cluster, dist(pca_scores))
  mean(ss[, 3])
})
k_opt <- which.max(sil_scores) + 1
cat("Número óptimo de clusters por silhouette:", k_opt, "\n")

# =======================
# 5) K-means final
# =======================
set.seed(123)
km_res <- kmeans(pca_scores, centers = k_opt, nstart = 50)
clusters_km <- km_res$cluster

fviz_cluster(list(data = pca_scores, cluster = clusters_km),
             geom = "point", ellipse.type = "norm", main = paste("K-means k =", k_opt))

sil <- silhouette(clusters_km, dist(pca_scores))
fviz_silhouette(sil) + ggtitle(paste("Silhouette - k =", k_opt))

# =======================
# 6) Jerárquico (Ward)
# =======================
distancias <- dist(pca_scores)
hclust_res <- hclust(distancias, method = "ward.D2")
clusters_hc <- cutree(hclust_res, k = k_opt)

fviz_dend(hclust_res, k = k_opt, rect = TRUE, main = "Dendrograma (Ward.D2)")

# Comparar resultados
ari <- adjustedRandIndex(clusters_km, clusters_hc)
cat("ARI entre K-means y Jerárquico:", ari, "\n")

# =======================
# 7) Resumen de clusters
# =======================
tabla_clusters <- data.frame(País = paises, Cluster = clusters_km)
write.csv(tabla_clusters, "Resultados_clusters.csv", row.names = FALSE)

summary_clusters <- datos_imputed %>%
  filter(`Country Name` %in% tabla_clusters$País) %>%
  left_join(tabla_clusters, by = c("Country Name" = "País")) %>%
  group_by(Cluster) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))

write.csv(summary_clusters, "Resumen_por_cluster.csv", row.names = FALSE)
print(summary_clusters)

# =======================
# 8) Heatmap (variables originales estandarizadas)
# =======================
mat <- scale(datos_num)
rownames(mat) <- paises
annotation_row <- data.frame(Cluster = factor(clusters_km))
rownames(annotation_row) <- paises

pheatmap(mat, annotation_row = annotation_row, show_rownames = FALSE,
         main = "Heatmap de países según variables estandarizadas")

# =======================
# 9) Guardar resultados
# =======================
save(res.pca, km_res, clusters_km, clusters_hc, file = "pca_clustering_results.RData")
cat("Listo: análisis completo ejecutado y guardado.\n")

