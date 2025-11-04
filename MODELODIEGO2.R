library(tidyverse)
library(readxl)
library(factoextra)
library(cluster)
library(dendextend)
library(pheatmap)

#base
datos23 <- "P_Data_Extract_From_World_Development_Indicators.xlsx"
datosfinal <- read_excel(datos23)

datos <- datosfinal %>%
  select(`Country Name`, `Series Name`, `2022 [YR2022]`) %>%
  mutate(`2022 [YR2022]` = as.numeric(gsub("[^0-9\\.-]", "", as.character(`2022 [YR2022]`)))) %>%
  group_by(`Country Name`, `Series Name`) %>%
  summarise(valor = mean(`2022 [YR2022]`, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = `Series Name`, values_from = valor)

#elimino 50% na
datos <- datos %>%
  filter(!is.na(`Country Name`)) %>%
  select(where(~ mean(is.na(.x)) < 0.5))

# na por media

datos_imputed <- datos %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)))

datos_num <- datos_imputed %>%
  select(-`Country Name`) %>%
  mutate(across(everything(), as.numeric))

# Eliminar variables constantes
datos_num <- datos_num %>%
  select(where(is.numeric)) %>%
  select(where(~ sd(.x, na.rm = TRUE) > 0))

# nombres
paises <- datos_imputed$`Country Name`
if (nrow(datos_num) != length(paises)) {
  paises <- paises[1:nrow(datos_num)]
}

# PCA
res.pca <- prcomp(datos_num, scale. = TRUE)
fviz_eig(res.pca, addlabels = TRUE, main = "Varianza explicada por componentes")
summary(res.pca)

# clustering
pca_scores <- as.data.frame(res.pca$x[, 1:3])
rownames(pca_scores) <- paises

distancias <- dist(pca_scores)
hclust_res <- hclust(distancias, method = "ward.D2")

fviz_dend(hclust_res, k = 2, 
          cex = 0.6,
          palette = "jco", 
          rect = TRUE, 
          rect_border = "steelblue", 
          rect_fill = TRUE,
          main = "Dendrograma - Clustering Jerárquico")

# =========================
# 8) Determinar número óptimo de clusters
# =========================
set.seed(123)
fviz_nbclust(pca_scores, kmeans, method = "wss") +
  labs(title = "Número óptimo de clusters (Método del Codo)")

k_opt <- 2 

# =========================
# 9) Clustering final con K-means
# =========================
set.seed(123)
km_res <- kmeans(pca_scores, centers = k_opt, nstart = 25)
clusters <- km_res$cluster

# =========================
# 10) Resultados y visualización
# =========================
tabla_clusters <- data.frame(País = paises, Cluster = clusters)
write.csv(tabla_clusters, "Resultados_clusters.csv", row.names = FALSE)

cat("\nNúmero de países:", nrow(tabla_clusters),
    "\nNúmero de variables:", ncol(datos_num),
    "\nNúmero de clusters:", k_opt, "\n")

fviz_cluster(list(data = pca_scores, cluster = clusters),
             geom = "point",
             ellipse.type = "norm",
             main = "Clusters según PCA")
