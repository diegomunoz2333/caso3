library(tidyverse)
library(readxl)
library(FactoMineR)
library(factoextra)
library(dendextend)
library(psych)
library(pheatmap)
library(NbClust)

# Cargar base de datos
Base_2022 <- read_csv("Base_2022_filtrada.csv", show_col_types = FALSE)

datos_raw <- Base_2022

datos_wide2 <- Base_2022

datos_imputed <- datos_wide2 %>%
  select(where(is.numeric)) %>%
  as.data.frame()

# Heatmap de correlaciones
cor_mat <- cor(datos_imputed, use = "pairwise.complete.obs")
pheatmap(cor_mat,
         main = "Matriz de correlaciones (2022)",
         fontsize = 8,
         color = colorRampPalette(c("red", "white", "blue"))(100),
         clustering_method = "complete")

# PCA
res.pca <- prcomp(datos_imputed, center = TRUE, scale. = TRUE)

eig.val <- get_eigenvalue(res.pca)
print(eig.val)

# Biplot PCA
pca_scores <- as.data.frame(res.pca$x)
pca_scores$Country <- datos_wide2$Pais
fviz_pca_biplot(res.pca, repel = TRUE, col.var = "#2E9FDF", col.ind = "#696969",
                title = "Biplot PCA - Países e Indicadores")

# Selección de número de componentes
n_kaiser <- sum(eig.val[, "eigenvalue"] > 1)
n_80 <- which(cumsum(eig.val[, "variance.percent"]) >= 80)[1]
ncomp <- if (!is.na(n_80)) n_80 else max(2, n_kaiser)

# Clustering jerárquico (Ward.D2)
pca_for_cluster <- pca_scores %>% select(starts_with("PC")) %>% select(1:ncomp)
pca_for_cluster <- pca_for_cluster[!is.na(pca_scores$Country), ]
rownames(pca_for_cluster) <- na.omit(pca_scores$Country)

dist_pca <- dist(pca_for_cluster, method = "euclidean")
hc_ward <- hclust(dist_pca, method = "ward.D2")
plot(as.dendrogram(hc_ward), main = "Dendrograma (Ward.D2) sobre PCs")

# Determinar número de clusters
fviz_nbclust(pca_for_cluster, FUN = hcut, method = "silhouette") + ggtitle("Silhouette - hcut")
fviz_nbclust(pca_for_cluster, FUN = hcut, method = "wss") + ggtitle("WSS - hcut")

set.seed(123)
nb <- NbClust(pca_for_cluster, distance = "euclidean", min.nc = 2, max.nc = 8, method = "ward.D2", index = "all")

table(nb$Best.nc[1, ])
k_opt <- as.integer(names(sort(table(nb$Best.nc[1, ]), decreasing = TRUE))[1])
if (is.na(k_opt)) k_opt <- 4
cat("k sugerido por NbClust:", k_opt, "\n")

# Asignar clusters y analizar perfiles
clusters <- cutree(hc_ward, k = 3)
table(clusters)

resultado_paises <- data.frame(Country = rownames(pca_for_cluster),
                               cluster = factor(clusters),
                               pca_for_cluster)
view(resultado_paises)
# Visualización
fviz_cluster(list(data = pca_for_cluster, cluster = clusters),
             geom = "point", repel = TRUE,
             main = paste("Clusters sobre", ncomp, "PCs (k=", k_opt, ")"))

# Perfiles por cluster
datos_clustered <- datos_wide2 %>%
  filter(Pais %in% rownames(pca_for_cluster)) %>%
  mutate(cluster = factor(clusters[match(Pais, rownames(pca_for_cluster))]))

cluster_profiles <- datos_clustered %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), n = n()) %>%
  arrange(cluster)

print(cluster_profiles)

# Calcular medias de cada variable (sin las columnas de texto)
promedios <- colMeans(Base_2022[ , -c(1,2)], na.rm = TRUE)

# Extraer los valores de Luxemburgo
lux <- Base_2022[Base_2022$Pais == "Luxembourg", -c(1,2)]

# Comparar Luxemburgo vs promedio mundial
comparacion <- t(lux - promedios)
comparacion

# Heatmap de perfiles estandarizados
profiles_mat <- cluster_profiles %>%
  select(-n) %>%
  column_to_rownames("cluster") %>%
  as.matrix()

profiles_scaled <- t(scale(t(profiles_mat)))
pheatmap(profiles_scaled, main = "Perfiles estandarizados por cluster")
