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

# ---------------------------
# PCA / ACP sobre la base filtrada
# ---------------------------
library(factoextra)   # visualizaciones PCA

# 1) Ejecutar PCA (prcomp usa la matriz original y estandariza con scale = TRUE)
res.pca <- prcomp(vars_numericas, scale. = TRUE)

# 2) Eigenvalues / varianza explicada
eig.val <- (res.pca$sdev)^2
prop_var <- eig.val / sum(eig.val)
cum_var  <- cumsum(prop_var)
eig_table <- tibble(PC = paste0("PC", 1:length(eig.val)),
                    eigenvalue = eig.val,
                    prop_var = prop_var,
                    cum_var = cum_var)
print(eig_table)       # mira cuánta varianza explica cada componente

# Scree plot (gráfico de barras de eigenvalues) — fviz_eig sirve con prcomp
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 60)) + ggtitle("Scree plot (varianza explicada por PC)")

fviz_pca_ind(res.pca,
             col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE) + ggtitle("Individuos (países) — calidad (cos2)")

fviz_pca_var(res.pca,
             col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             axes = c(1,2)) + ggtitle("Variables — contribución a PC1 y PC2")

fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "#2E9FDF",
                col.ind = "#696969",
                axes = c(1,2)) + ggtitle("Biplot PCA (PC1 vs PC2)")

# ---------------------------
# ACCEDER A LOS RESULTADOS DEL PCA (res.pca)
# ---------------------------
eig_df <- get_eigenvalue(res.pca)
print(eig_df)

res.var <- get_pca_var(res.pca)
res.var$coord 
res.var$contrib
res.var$cos2
# Ver rápidamente las contribuciones de las primeras 6 variables a los primeros 4 PCs
View(round(res.var$contrib[, 1:4], 2))
# 3) Resultados para los INDIVIDUOS (scores)
res.ind <- get_pca_ind(res.pca)

# Coordenadas (scores) de los individuos (países) en las PCs
res.ind$coord    # filas = países, columnas = PCs

# Contribuciones (%) de individuos a las PCs
res.ind$contrib

# cos2 (calidad de representación) de individuos
res.ind$cos2

# Ver las contribuciones de los primeros países en PC1-PC3
View(round(res.ind$contrib[, 1:3], 2))

# 4) (Opcional) Proyectar nuevos casos sobre el PCA
# Ejemplo: usar las primeras 3 observaciones como "nuevos individuos"
ind.test <- vars_numericas[1:3, , drop = FALSE]   # solo ejemplo
ind.test.coord <- predict(res.pca, newdata = ind.test)
ind.test.coord[, 1:2]   # coordenadas en PC1 y PC2

# 5) (Opcional) Añadir clusters al gráfico PCA (si ya tienes resultados_hc$cluster_hc)
if (exists("resultados_hc") && "cluster_hc" %in% names(resultados_hc)) {
  fviz_pca_ind(res.pca,
               habillage = resultados_hc$cluster_hc,  # colorea por cluster
               addEllipses = TRUE,
               repel = TRUE) +
    ggtitle("PCA: individuos coloreados por cluster (Ward)")
}

# ---------------------------
# Qué mirar / interpretar
# ---------------------------
# - 'eig_df' : ver % de varianza explicada por cada PC y decidir cuántas retener (p.ej. PCs hasta ~70-85% acum.)
# - 'res.var$contrib' : variables con mayor contribución a PC1/PC2 -> te dicen qué conceptos resumen cada PC.
# - 'colSums(res.var$contrib[,1:2])' : suma de contribuciones (comprobar qué proporción de "importancia" explican PC1+PC2).
# - 'res.ind$cos2' : identifica países bien representados en PC1/PC2 (alto cos2) y países mal representados (bajo cos2).
# - El gráfico coloreado por cluster te muestra visualmente si los clusters se separan en el espacio de las PCs.
