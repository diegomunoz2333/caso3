library(tidyverse)
library(factoextra)
library(dendextend)
library(cluster)
library(ggrepel)
library(readr)

Base_2022_filtrada <- read_csv("Base_2022_filtrada.csv")

# 1) Variables numéricas y escalado
vars_numericas <- Base_2022_filtrada %>% select(3:ncol(.))
vars_escaladas <- scale(vars_numericas)

# 2) Sugerencia de k (silhouette para hcut)
fviz_nbclust(vars_escaladas, FUN = hcut, method = "silhouette") +
  ggtitle("Número óptimo de clusters jerárquicos")

# 3) Matriz de distancias (usar las variables escaladas)
dist_entre_Base <- dist(vars_escaladas, method = "euclidean")

# 4) Clustering jerárquico (usar la matriz de distancias)
modelo_jerarquico <- hclust(dist_entre_Base, method = "ward.D2")

# 5) Dendrograma
dend_modelo <- as.dendrogram(modelo_jerarquico)
plot(dend_modelo, main = "Dendrograma - Ward.D2", ylab = "Altura")

# -----------------------
# Comparación k = 2 y k = 3
# -----------------------

# k = 2
clusters_deseados <- 2
dend_modelo_col_k <- dend_modelo %>% color_branches(k = clusters_deseados) %>% color_labels(k = clusters_deseados)
plot(dend_modelo_col_k, main = paste0("Dendrograma coloreado (k = ", clusters_deseados, ")"))
cluster_hc_k2 <- cutree(modelo_jerarquico, k = clusters_deseados)

resultados_hc_k2 <- Base_2022_filtrada %>% mutate(cluster_hc = factor(cluster_hc_k2))
View(resultados_hc_k2)

perfil_k2 <- resultados_hc_k2 %>%
  group_by(cluster_hc) %>%
  summarise(n = n(),
            across(.cols = names(vars_numericas),
                   .fns = ~ mean(.x, na.rm = TRUE),
                   .names = "mean_{col}")) %>%
  arrange(cluster_hc)

cat("Tamaños k=2:\n"); print(table(resultados_hc_k2$cluster_hc))
cat("Silhouette promedio k=2:\n")
sil_k2 <- silhouette(as.numeric(resultados_hc_k2$cluster_hc), dist_entre_Base)
print(mean(sil_k2[,3]))
plot(sil_k2, main = "Silhouette - k = 2")

# PC1 vs PC2 (k=2)
pca_tmp <- prcomp(vars_escaladas, center = FALSE, scale. = FALSE)
scores_pca <- as.data.frame(pca_tmp$x[, 1:2]) %>% bind_cols(resultados_hc_k2 %>% select(1:2, cluster_hc))
ggplot(scores_pca, aes(x = PC1, y = PC2, color = cluster_hc)) +
  geom_point(size = 3) +
  geom_text_repel(aes(label = !!sym(names(resultados_hc_k2)[1])), size = 2.2, max.overlaps = 30) +
  labs(title = paste("PC1 vs PC2 - k =", 2)) + theme_minimal()

# -----------------------
# k = 3
# -----------------------
clusters_deseados <- 3
dend_modelo_col_k3 <- dend_modelo %>% color_branches(k = clusters_deseados) %>% color_labels(k = clusters_deseados)
plot(dend_modelo_col_k3, main = paste0("Dendrograma coloreado (k = ", clusters_deseados, ")"))
cluster_hc_k3 <- cutree(modelo_jerarquico, k = clusters_deseados)

resultados_hc_k3 <- Base_2022_filtrada %>% mutate(cluster_hc = factor(cluster_hc_k3))
View(resultados_hc_k3)

perfil_k3 <- resultados_hc_k3 %>%
  group_by(cluster_hc) %>%
  summarise(n = n(),
            across(.cols = names(vars_numericas),
                   .fns = ~ mean(.x, na.rm = TRUE),
                   .names = "mean_{col}")) %>%
  arrange(cluster_hc)

cat("Tamaños k=3:\n"); print(table(resultados_hc_k3$cluster_hc))
cat("Silhouette promedio k=3:\n")
sil_k3 <- silhouette(as.numeric(resultados_hc_k3$cluster_hc), dist_entre_Base)
print(mean(sil_k3[,3]))
plot(sil_k3, main = "Silhouette - k = 3")

# PC1 vs PC2 (k=3)
scores_pca_k3 <- as.data.frame(pca_tmp$x[, 1:2]) %>% bind_cols(resultados_hc_k3 %>% select(1:2, cluster_hc))
ggplot(scores_pca_k3, aes(x = PC1, y = PC2, color = cluster_hc)) +
  geom_point(size = 3) +
  geom_text_repel(aes(label = !!sym(names(resultados_hc_k3)[1])), size = 2.2, max.overlaps = 30) +
  labs(title = paste("PC1 vs PC2 - k =", 3)) + theme_minimal()

# Imprimir perfiles para comparar
print("Perfil k=2 (primeras filas):"); print(head(perfil_k2))
print("Perfil k=3 (primeras filas):"); print(head(perfil_k3))
