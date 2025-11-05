library(tidyverse)
library(readxl)
library(FactoMineR)
library(factoextra)
library(dendextend)
library(psych)
library(pheatmap)
library(NbClust)

ruta <- "P_Data_Extract_From_World_Development_Indicators (1).xlsx"
raw <- read_excel(ruta)

# 2022
raw <- raw %>%
  mutate(`2022 [YR2022]` = as.character(`2022 [YR2022]`),
         `2022 [YR2022]` = gsub("[^0-9\\.-]", "", `2022 [YR2022]`),
         `2022 [YR2022]` = as.numeric(`2022 [YR2022]`))

# 3) pivot a formato wide (Country Name x Series Name)
datos_wide <- raw %>%
  select(`Country Name`, `Series Name`, `2022 [YR2022]`) %>%
  pivot_wider(
    names_from = `Series Name`,
    values_from = `2022 [YR2022]`,
    values_fn = mean,
    values_fill = NA_real_
  )

# 4) parámetros: número de variables que quieres conservar
n_vars <- 15

# 5) calcula proporción de NA por variable (excluyendo Country Name)
na_prop <- sapply(datos_wide %>% select(-`Country Name`), function(x) mean(is.na(x)))

# 6) selecciona las n_vars con menor NA
#   si hay menos de n_vars disponibles, toma todas las que existan y avisa
if(length(na_prop) < n_vars){
  warning("Hay menos variables que n_vars; se usarán todas las variables disponibles.")
  vars_top15 <- names(na_prop)
} else {
  vars_top15 <- names(sort(na_prop, decreasing = FALSE))[1:n_vars]
}

# 7) construye dataset con Country Name + esas variables
df_top15 <- datos_wide %>% select(`Country Name`, all_of(vars_top15))

# 8) elimina filas con cualquier NA (casos completos)
df_top15_complete <- df_top15 %>% drop_na()

# 9) resumen en consola
cat("Variables seleccionadas (", length(vars_top15), "):\n")
print(vars_top15)
cat("\nPaíses en df_top15 (con NA permitidos):", nrow(df_top15), "\n")
cat("Países después de drop_na():", nrow(df_top15_complete), "\n")

# 10) inspección rápida (ver / head)
print(head(df_top15_complete, 6))
# Si estás en RStudio puedes usar:
View(df_top15_complete)


Base_datos=datos_wide

#hatmap correlacion
cor_mat <- cor(datos_imputed, use = "pairwise.complete.obs")
pheatmap(cor_mat,
         main = "Matriz de correlaciones (2022)",
         fontsize = 8,
         color = colorRampPalette(c("red", "white", "blue"))(100),
         clustering_method = "complete")

#´PCA
res.pca <- prcomp(datos_imputed, center = TRUE, scale. = TRUE)

eig.val <- get_eigenvalue(res.pca)
print(eig.val)


# Biplot PCA
pca_scores <- as.data.frame(res.pca$x)
pca_scores$Country <- datos_wide2$`Country Name`
fviz_pca_biplot(res.pca, repel = TRUE, col.var = "#2E9FDF", col.ind = "#696969",
                title = "Biplot PCA - Países e Indicadores")

# Selección de número de componentes

n_kaiser <- sum(eig.val[, "eigenvalue"] > 1)
n_80 <- which(cumsum(eig.val[, "variance.percent"]) >= 80)[1]
ncomp <- if (!is.na(n_80)) n_80 else max(2, n_kaiser)

#Clustering jerárquico (Ward.D2)

pca_for_cluster <- pca_scores %>% select(starts_with("PC")) %>% select(1:ncomp)

# eliminar países sin nombre
pca_for_cluster <- pca_for_cluster[!is.na(pca_scores$Country), ]
rownames(pca_for_cluster) <- na.omit(pca_scores$Country)

# Distancia y dendrograma
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

clusters <- cutree(hc_ward, k = k_opt)
table(clusters)

resultado_paises <- data.frame(Country = rownames(pca_for_cluster),
                               cluster = factor(clusters),
                               pca_for_cluster)

# Visualización
fviz_cluster(list(data = pca_for_cluster, cluster = clusters),
             geom = "point", repel = TRUE,
             main = paste("Clusters sobre", ncomp, "PCs (k=", k_opt, ")"))


#  Perfiles por cluster

datos_clustered <- datos_wide2 %>%
  filter(`Country Name` %in% rownames(pca_for_cluster)) %>%
  mutate(cluster = factor(clusters[match(`Country Name`, rownames(pca_for_cluster))]))

cluster_profiles <- datos_clustered %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), n = n()) %>%
  arrange(cluster)

print(cluster_profiles)

# 15) Heatmap de perfiles estandarizados

profiles_mat <- cluster_profiles %>%
  select(-n) %>%
  column_to_rownames("cluster") %>%
  as.matrix()

profiles_scaled <- t(scale(t(profiles_mat)))
pheatmap(profiles_scaled, main = "Perfiles estandarizados por cluster")

