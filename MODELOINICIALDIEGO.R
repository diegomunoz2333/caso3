library(tidyverse)
library(readxl)
library(FactoMineR)
library(factoextra)
library(dendextend)
library(psych)
library(pheatmap)
library(NbClust)
set.seed(28)

Base_2022 <- read_csv("Base_2022_filtrada.csv", show_col_types = FALSE)

datos_num <- Base_2022 %>%
  select(where(is.numeric)) %>%
  as.data.frame()


#  Reducción de dimensionalidad: Análisis de Componentes Principales (ACP)


## 2.1 Matriz de correlaciones
cor_mat <- cor(datos_num, use = "pairwise.complete.obs")
pheatmap(
  cor_mat,
  main = "Matriz de correlaciones (2022)",
  fontsize = 8,
  color = colorRampPalette(c("red", "white", "blue"))(100),
  clustering_method = "complete"
)

## 2.2 Varianza explicada y componentes principales
res.pca <- prcomp(datos_num, center = TRUE, scale. = TRUE)
eig.val <- get_eigenvalue(res.pca)
print(eig.val)

# Número de componentes según criterios
n_kaiser <- sum(eig.val$eigenvalue > 1)
n_80 <- which(cumsum(eig.val$variance.percent) >= 80)[1]
ncomp <- if (!is.na(n_80)) n_80 else max(2, n_kaiser)

cat("Componentes (Kaiser):", n_kaiser, "\n")
cat("Componentes (>=80%):", n_80, "\n")
cat("Dimensiones seleccionadas:", ncomp, "\n")

# Varianza explicada por las primeras 8 dimensiones
eig.val[1:8, c("variance.percent", "cumulative.variance.percent")]

## 2.3 Biplot PCA
pca_scores <- as.data.frame(res.pca$x)
pca_scores$Pais <- Base_2022$Pais

fviz_pca_biplot(
  res.pca, repel = TRUE,
  col.var = "#2E9FDF", col.ind = "#696969",
  title = "Biplot PCA - Países e Indicadores"
)

# Interpretación de variables más relevantes
# (Título solo, sin código)


# ========================================================
# 3. Integración ACP–Clusterización
# ========================================================

## 3.1 Agrupamiento sobre los factores principales
pca_for_cluster <- pca_scores %>%
  select(starts_with("PC")) %>%
  select(1:ncomp)
rownames(pca_for_cluster) <- Base_2022$Pais


# ========================================================
# 4. Determinación del número óptimo de clusters
# ========================================================

## 4.1 Método del codo (WSS)
fviz_nbclust(pca_for_cluster, FUN = hcut, method = "wss") +
  ggtitle("Método del Codo (WSS) - Ward.D2")

## 4.2 Método de la silueta promedio
fviz_nbclust(pca_for_cluster, FUN = hcut, method = "silhouette") +
  ggtitle("Método de la Silueta (Ward.D2)")

## 4.3 Determinación mediante NbClust
set.seed(123)
nb <- NbClust(
  pca_for_cluster, distance = "euclidean",
  min.nc = 2, max.nc = 8, method = "ward.D2", index = "all"
)

table(nb$Best.nc[1, ])
k_opt <- as.integer(names(sort(table(nb$Best.nc[1, ]), decreasing = TRUE))[1])
if (is.na(k_opt)) k_opt <- 4
cat("k sugerido por NbClust:", k_opt, "\n")


# ========================================================
# 5. Agrupamiento jerárquico (Método de Ward)
# ========================================================

## 5.1 Dendrograma y definición de cortes
dist_pca <- dist(pca_for_cluster, method = "euclidean")
hc_ward <- hclust(dist_pca, method = "ward.D2")

plot(as.dendrogram(hc_ward), main = "Dendrograma (Ward.D2) sobre PCs")

# Comparación con K-means



# ========================================================
# 6. Agrupamiento K-medias (K-means)
# ========================================================

## 6.1 Descripción del método


## 6.2 Aplicación sobre los datos
kmeans_model <- kmeans(pca_for_cluster, centers = k_opt, nstart = 25)

## 6.3 Visualización de los grupos obtenidos
fviz_cluster(
  list(data = pca_for_cluster, cluster = kmeans_model$cluster),
  geom = "point", repel = TRUE,
  main = paste("K-medias sobre", ncomp, "PCs (k =", k_opt, ")")
)


# ========================================================
# 7. Descripción de los clusters
# ========================================================

## 7.1 Asignar clusters (ejemplo con Ward)
clusters <- cutree(hc_ward, k = k_opt)
table(clusters)

resultado_paises <- data.frame(
  Pais = rownames(pca_for_cluster),
  cluster = factor(clusters),
  pca_for_cluster
)

## 7.2 Estadísticos descriptivos por grupo
Base_cluster <- Base_2022 %>%
  mutate(cluster = factor(clusters[match(Pais, rownames(pca_for_cluster))])) %>%
  filter(!is.na(cluster))

cluster_profiles <- Base_cluster %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE), n = n()) %>%
  arrange(cluster)

print(cluster_profiles)

## 7.3 Heatmap de perfiles estandarizados por cluster
profiles_mat <- cluster_profiles %>%
  select(-n) %>%
  column_to_rownames("cluster") %>%
  as.matrix()
profiles_scaled <- t(scale(t(profiles_mat)))

pheatmap(
  profiles_scaled,
  main = "Perfiles estandarizados por cluster"
)


# ========================================================
# 8. Visualización final de los clusters
# ========================================================
fviz_cluster(
  list(data = pca_for_cluster, cluster = clusters),
  geom = "point", repel = TRUE,
  main = paste("Clusters finales sobre", ncomp, "PCs (k =", k_opt, ")")
)


# ========================================================
# 9. Comparaciones adicionales
# ========================================================

## Ejemplo: Luxemburgo vs promedio mundial
promedios <- colMeans(Base_2022[, -c(1, 2)], na.rm = TRUE)
lux <- Base_2022[Base_2022$Pais == "Luxembourg", -c(1, 2)]
comparacion <- t(lux - promedios)
comparacion

  