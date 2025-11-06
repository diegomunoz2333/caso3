
library(tidyverse)
library(readxl)
library(FactoMineR)
library(factoextra)
library(dendextend)
library(psych)
library(pheatmap)
library(NbClust)

Base_2022 <- read_csv("Base_2022_filtrada.csv", show_col_types = FALSE)

datos_num <- Base_2022 %>%
  select(where(is.numeric)) %>%
  as.data.frame()
#1)k-medias 






# --------------------------------------------------------
# 2. Matriz de correlaciones
# --------------------------------------------------------
cor_mat <- cor(datos_num, use = "pairwise.complete.obs")
pheatmap(
  cor_mat,
  main = "Matriz de correlaciones (2022)",
  fontsize = 8,
  color = colorRampPalette(c("red", "white", "blue"))(100),
  clustering_method = "complete"
)

# --------------------------------------------------------
# 3. Análisis de Componentes Principales (PCA)
# --------------------------------------------------------
res.pca <- prcomp(datos_num, center = TRUE, scale. = TRUE)
eig.val <- get_eigenvalue(res.pca)
print(eig.val)

# Número de componentes
n_kaiser <- sum(eig.val$eigenvalue > 1)
n_80 <- which(cumsum(eig.val$variance.percent) >= 80)[1]
ncomp <- if (!is.na(n_80)) n_80 else max(2, n_kaiser)

cat("Componentes (Kaiser):", n_kaiser, "\n")
cat("Componentes (>=80%):", n_80, "\n")
cat("Componentes seleccionados:", ncomp, "\n")

# Varianza explicada por las 8 primeras dimensiones
eig.val[1:8, c("variance.percent", "cumulative.variance.percent")]

# Biplot PCA
pca_scores <- as.data.frame(res.pca$x)
pca_scores$Pais <- Base_2022$Pais

fviz_pca_biplot(
  res.pca, repel = TRUE,
  col.var = "#2E9FDF", col.ind = "#696969",
  title = "Biplot PCA - Países e Indicadores"
)

# --------------------------------------------------------
# 4. Cluster jerárquico (Ward.D2) sobre PCs
# --------------------------------------------------------
pca_for_cluster <- pca_scores %>%
  select(starts_with("PC")) %>%
  select(1:ncomp)
rownames(pca_for_cluster) <- Base_2022$Pais

dist_pca <- dist(pca_for_cluster, method = "euclidean")
hc_ward <- hclust(dist_pca, method = "ward.D2")

plot(as.dendrogram(hc_ward), main = "Dendrograma (Ward.D2) sobre PCs")

# Determinar número óptimo de clusters
fviz_nbclust(pca_for_cluster, FUN = hcut, method = "silhouette") + ggtitle("Silhouette - hcut")
fviz_nbclust(pca_for_cluster, FUN = hcut, method = "wss") + ggtitle("WSS - hcut")

set.seed(123)
nb <- NbClust(
  pca_for_cluster, distance = "euclidean",
  min.nc = 2, max.nc = 8, method = "ward.D2", index = "all"
)

table(nb$Best.nc[1, ])
k_opt <- as.integer(names(sort(table(nb$Best.nc[1, ]), decreasing = TRUE))[1])
if (is.na(k_opt)) k_opt <- 4
cat("k sugerido por NbClust:", k_opt, "\n")

# --------------------------------------------------------
# 5. Asignar clusters y visualizar
# --------------------------------------------------------
clusters <- cutree(hc_ward, k = k_opt)
table(clusters)

resultado_paises <- data.frame(
  Pais = rownames(pca_for_cluster),
  cluster = factor(clusters),
  pca_for_cluster
)

fviz_cluster(
  list(data = pca_for_cluster, cluster = clusters),
  geom = "point", repel = TRUE,
  main = paste("Clusters sobre", ncomp, "PCs (k =", k_opt, ")")
)

# --------------------------------------------------------
# 6. Análisis de perfiles por cluster
# --------------------------------------------------------
Base_cluster <- Base_2022 %>%
  mutate(cluster = factor(clusters[match(Pais, rownames(pca_for_cluster))])) %>%
  filter(!is.na(cluster))

cluster_profiles <- Base_cluster %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE), n = n()) %>%
  arrange(cluster)

print(cluster_profiles)

# --------------------------------------------------------
# 7. Comparación Luxemburgo vs promedio mundial
# --------------------------------------------------------
promedios <- colMeans(Base_2022[ , -c(1,2)], na.rm = TRUE)
lux <- Base_2022[Base_2022$Pais == "Luxembourg", -c(1,2)]
comparacion <- t(lux - promedios)
comparacion

#  Heatmap de perfiles estandarizados por cluster

profiles_mat <- cluster_profiles %>%
  select(-n) %>%
  column_to_rownames("cluster") %>%
  as.matrix()

profiles_scaled <- t(scale(t(profiles_mat)))

pheatmap(
  profiles_scaled,
  main = "Perfiles estandarizados por cluster"
)
