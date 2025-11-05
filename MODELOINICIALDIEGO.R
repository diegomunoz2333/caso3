library(tidyverse)
library(readxl)
library(FactoMineR)
library(factoextra)
library(dendextend)
library(psych)
library(pheatmap)
library(NbClust)

P_Data_Extract_From_World_Development_Indicators_1_ <- read_excel("P_Data_Extract_From_World_Development_Indicators (1).xlsx")
View(P_Data_Extract_From_World_Development_Indicators_1_)

datos_raw <- P_Data_Extract_From_World_Development_Indicators_1_

datos_raw <- datos_raw %>%
  mutate(`2022 [YR2022]` = as.character(`2022 [YR2022]`),
         `2022 [YR2022]` = gsub("[^0-9\\.-]", "", `2022 [YR2022]`),
         `2022 [YR2022]` = as.numeric(`2022 [YR2022]`))

# 
datos_wide <- datos_raw %>%
  select(`Country Name`, `Series Name`, `2022 [YR2022]`) %>%
  pivot_wider(
    names_from = `Series Name`,
    values_from = `2022 [YR2022]`,
    values_fn = mean,
    values_fill = NA_real_
  )

install.packages("countrycode")

library(countrycode)
paises_onu <- countrycode::codelist$country.name.en %>% unique() %>% na.omit()

# Filtrar solo los países   segun la onu
datos_filtrados <- datos_raw %>%
  filter(`Country Name` %in% paises_onu)


view(datos_wide)
# Filtrar columnas con menos de 40% de NA

# 
na_prop <- sapply(datos_wide %>% select(-`Country Name`), function(x) mean(is.na(x)))

# Creamos un vector con las columnas que tienen menos del 40% de NA
cols_keep <- names(na_prop[na_prop < 0.4])

# Filtramos el dataset para mantener solo esas columnas + el país
datos_filtrados <- datos_wide %>%
  select(`Country Name`, all_of(cols_keep))

# Verificamos cuántas columnas quedaron
cat("Columnas retenidas:", length(cols_keep), "\n")
print(cols_keep)

# Opcional: ver un resumen
summary(datos_filtrados)
view(datos_filtrados)
# Quitar columna "NA" si existe
#if ("NA" %in% names(datos_wide)) datos_wide <- datos_wide %>% select(-all_of("NA"))

#cat("Países:", nrow(datos_wide), " Indicadores:", ncol(datos_wide) - 1, "\n")

#variables ocn mas de 50% na
#na_prop <- colMeans(is.na(datos_wide %>% select(-`Country Name`)))
#umbral <- 0.5
#vars_keep <- names(na_prop)[na_prop <= umbral]
#datos_wide2 <- datos_wide %>% select(`Country Name`, all_of(vars_keep))
#cat("Variables retenidas:", length(vars_keep), "\n")

# matriz
#datos_num <- datos_wide2 %>% select(-`Country Name`) %>%
#  mutate(across(everything(), ~ as.numeric(.x)))

# Eliminar columnas totalmente NA
#tot_na_cols <- names(which(colMeans(is.na(datos_num)) == 1))
#if (length(tot_na_cols) > 0) {
#  datos_num <- datos_num %>% select(-all_of(tot_na_cols))
#}

# media por na
#datos_imputed <- datos_num %>%
 # mutate(across(everything(), ~ ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)))

# Eliminar variables constantes

#zero_var_cols <- names(which(apply(datos_imputed, 2, sd, na.rm = TRUE) == 0))
#if (length(zero_var_cols) > 0) {
#  datos_imputed <- datos_imputed %>% select(-all_of(zero_var_cols))
#}

#cat("Dimensiones matriz final:", dim(datos_imputed)[1], "x", dim(datos_imputed)[2], "\n")


#Base de datos


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

