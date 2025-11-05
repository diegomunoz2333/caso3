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

na_prop_col <- sapply(datos_wide %>% select(-`Country Name`), function(x) mean(is.na(x)))

A
cols_keep <- names(na_prop_col[na_prop_col < 0.4])


datos_filtrados <- datos_wide %>%
  select(`Country Name`, all_of(cols_keep))


na_prop_row <- apply(datos_filtrados %>% select(-`Country Name`), 1, function(x) mean(is.na(x)))

#  Filtrar países con < 40% NA
datos_filtrados_final <- datos_filtrados[na_prop_row < 0.4, ]


cat("Indicadores retenidos:", length(cols_keep), "\n")
cat("Países retenidos:", nrow(datos_filtrados_final), "\n")

# Revisar resumen general
summary(datos_filtrados_final)
view(datos_filtrados_final)


###########################
method <- "auto"   # "auto" o "top15"

# 1) proporción de NA por columna (excluyendo Country Name)
na_prop <- sapply(datos_wide %>% select(-`Country Name`), function(x) mean(is.na(x)))

# Si method == "auto": busco el menor umbral thr (desde 0.00 a 0.99) con >= 15 columnas
if(method == "auto"){
  thr_found <- NA
  for(thr in seq(0, 0.99, by = 0.01)){
    n_keep <- sum(na_prop <= thr)
    if(n_keep >= 15){
      thr_found <- thr
      break
    }
  }
  # Si no encontró (poco probable), entonces usa top15 como fallback
  if(is.na(thr_found)){
    message("No se encontró umbral que deje >=15 columnas; se usará top15 como fallback.")
    method <- "top15"
  } else {
    cols_keep <- names(na_prop[na_prop <= thr_found])
    message("Método AUTO: umbral NA <= ", thr_found, " -> columnas retenidas: ", length(cols_keep))
  }
}

# Si method == "top15": tomar 15 indicadores con menor NA proportion
if(method == "top15"){
  cols_keep <- names(sort(na_prop, decreasing = FALSE))[1:15]
  message("Método TOP15: seleccionadas 15 variables con menor proporción de NA.")
}

# Mostrar columnas elegidas
cat("Número columnas elegidas:", length(cols_keep), "\n")
print(cols_keep)

# Construir tabla filtrada (solo país + columnas)
datos_filtrados <- datos_wide %>% select(`Country Name`, all_of(cols_keep))

# 2) Filtrar países con muchos NA (opcional)
# Calcula NA por fila
na_prop_row <- apply(datos_filtrados %>% select(-`Country Name`), 1, function(x) mean(is.na(x)))

# Umbral de países: puedes ajustar (ej. 0.4). Lo dejamos en 0.4 por defecto.
row_thr <- 0.4
datos_filtrados_final <- datos_filtrados[na_prop_row < row_thr, ]

# Si filtrando así te quedas con muy pocos países, relaja el umbral automáticamente:
min_countries <- 30  # mínimo deseado de países (ajusta según tu caso)
if(nrow(datos_filtrados_final) < min_countries){
  message("Quedaste con < ", min_countries, " países. Se relaja row_thr automáticamente.")
  # busca el row_thr máximo que deja al menos min_countries
  possible_thrs <- seq(0, 0.9, by = 0.01)
  chosen_row_thr <- NA
  for(rt in possible_thrs){
    n_country <- sum(na_prop_row < rt)
    if(n_country >= min_countries){
      chosen_row_thr <- rt
      break
    }
  }
  if(!is.na(chosen_row_thr)){
    row_thr <- chosen_row_thr
    datos_filtrados_final <- datos_filtrados[na_prop_row < row_thr, ]
    message("Nuevo row_thr: ", row_thr, " -> países retenidos: ", nrow(datos_filtrados_final))
  } else {
    message("No se consiguió mantener ", min_countries, " países con estos filtros; mantengo resultado actual.")
  }
}

# Informar resultados
cat("Indicadores retenidos:", ncol(datos_filtrados_final) - 1, "\n") # -1 porque Country Name
cat("Países retenidos:", nrow(datos_filtrados_final), "\n")

view(datos_filtrados_final)

## HASTA AQUI VA MIS FILTROS DE BASE 
###################HASTA AQIO FILTROS 
###################HASTA AQIO FILTROS 
###################HASTA AQIO FILTROS 


# Quitar columna "NA" si existe
#if ("NA" %in% names(datos_wide)) datos_wide <- datos_wide %>% select(-all_of("NA"))

#cat("Países:", nrow(datos_wide), " Indicadores:", ncol(datos_wide) - 1, "\n")


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

