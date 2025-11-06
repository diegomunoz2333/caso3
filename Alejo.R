library(tidyverse)
library(factoextra)
library(dendextend)
library(cluster)
library(ggrepel)
library(readr)
Base_2022_filtrada <- read_csv("Base_2022_filtrada.csv")

vars_numericas <- Base_2022_filtrada %>% select(3:ncol(.))
vars_escaladas <- scale(vars_numericas)
fviz_nbclust(vars_escaladas, FUN = hcut, method = "silhouette") +
  ggtitle("Número óptimo de clusters jerárquicos")

# 4) Calcular matriz de distancias usando los datos escalados
dist_entre_Base <- dist(vars_escaladas, method = "euclidean")

# 5) Ajustar clustering jerárquico sobre la matriz de distancias (Ward.D2 recomendado)
modelo_jerarquico <- hclust(dist_entre_Base, method = "ward.D2")

# 6) Convertir a dendrograma (para colorear y graficar)
dend_modelo <- as.dendrogram(modelo_jerarquico)

# 7) Graficar dendrograma básico
plot(dend_modelo, main = "Dendrograma - Ward.D2", ylab = "Altura")


clusters_deseados <- 2
dend_modelo_col_k <- dend_modelo %>% color_branches(k = clusters_deseados) %>% color_labels(k = clusters_deseados)
plot(dend_modelo_col_k, main = paste0("Dendrograma coloreado (k = ", clusters_deseados, ")"))

# 9) Cortar dendrograma y asignar clusters 
cluster_hc_k <- cutree(modelo_jerarquico, k = clusters_deseados)


# 10) Añadir clusters al dataframe original (identificadores + variables originales)
resultados_hc <- Base_2022_filtrada %>%
  mutate(cluster_hc = factor(cluster_hc_k))
view(resultados_hc)
# 11) Resumen por cluster (tamaño y medias)
perfil_clusters_hc <- resultados_hc %>%
  group_by(cluster_hc) %>%
  summarise(n = n(),
            across(.cols = names(vars_numericas),
                   .fns = ~ mean(.x, na.rm = TRUE),
                   .names = "mean_{col}")) %>%
  arrange(cluster_hc)

print(table(resultados_hc$cluster_hc))
print(perfil_clusters_hc)

# 12) Visualización de verificación: PCA 2D (PC1 vs PC2) con clusters coloreados
pca_tmp <- prcomp(vars_escaladas, center = FALSE, scale. = FALSE)  # ya escalado
scores_pca <- as.data.frame(pca_tmp$x[, 1:2]) %>%
  bind_cols(resultados_hc %>% select(1:2, cluster_hc)) 

ggplot(scores_pca, aes(x = PC1, y = PC2, color = cluster_hc)) +
  geom_point(size = 3) +
  geom_text_repel(aes(label = !!sym(names(resultados_hc)[1])), size = 2.2, max.overlaps = 30) +
  labs(title = paste("PC1 vs PC2 - clusters Ward (k =", clusters_deseados, ")")) +
  theme_minimal()

# 13) Silhouette para evaluar calidad del agrupamiento
sil <- silhouette(as.numeric(resultados_hc$cluster_hc), dist_entre_Base)
cat("Silhouette promedio:", mean(sil[, 3]), "\n")
plot(sil, main = "Silhouette por observación - clustering jerárquico")
