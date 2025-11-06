library(tidyverse)
library(readr)

# Cargar la base
Base <- read_csv("f36a5086-3311-4b1a-9f0c-bda5cd4718df_Series - Metadata.csv")

# Ver estructura
glimpse(Base)

# Seleccionar solo las variables relevantes y renombrarlas a español
Base_2022 <- Base %>%
  select(
    Pais = `Country Name`,
    Codigo = `Country Code`,
    PIB_per = `GDP per capita (current US$) [NY.GDP.PCAP.CD]`,
    Poblacion = `Population, total [SP.POP.TOTL]`,
    `Esperanza vida` = `Life expectancy at birth, total (years) [SP.DYN.LE00.IN]`,
    `Acceso electricidad` = `Access to electricity (% of population) [EG.ELC.ACCS.ZS]`,
    `Area boscosa` = `Forest area (% of land area) [AG.LND.FRST.ZS]`,
    `Suscripciones movil` = `Mobile cellular subscriptions (per 100 people) [IT.CEL.SETS.P2]`,
    `Crecimiento PIB` = `GDP growth (annual %) [NY.GDP.MKTP.KD.ZG]`,
    `Mortalidad infantil` = `Mortality rate, infant (per 1,000 live births) [SP.DYN.IMRT.IN]`,
    `Inversion extranjera` = `Foreign direct investment, net inflows (% of GDP) [BX.KLT.DINV.WD.GD.ZS]`,
    `Gasto salud` = `Current health expenditure (% of GDP) [SH.XPD.CHEX.GD.ZS]`,
    `Uso internet` = `Individuals using the Internet (% of population) [IT.NET.USER.ZS]`,
    `Importaciones` = `Imports of goods and services (% of GDP) [NE.IMP.GNFS.ZS]`,
    `Exportaciones` = `Exports of goods and services (% of GDP) [NE.EXP.GNFS.ZS]`,
    `Tierra cultivable` = `Arable land (% of land area) [AG.LND.ARBL.ZS]`,
    `Crecimiento poblacion` = `Population growth (annual %) [SP.POP.GROW]`,
    Industria = `Industry (including construction), value added (% of GDP) [NV.IND.TOTL.ZS]`,
    Remesas = `Personal remittances, received (% of GDP) [BX.TRF.PWKR.DT.GD.ZS]`
  )

# Reemplazar valores ":" por NA
Base_2022[Base_2022 == ".."] <- NA

# Eliminar filas con NA en cualquiera de las variables numéricas
Base_2022 <- Base_2022 %>%
  drop_na()

# Filtrar solo países reales (no regiones o grupos)
Base_2022 <- Base_2022 %>%
  filter(!Pais %in% c("World", "High income", "Low income", "European Union",
                      "Latin America & Caribbean", "Middle income", "OECD members",
                      "East Asia & Pacific", "Sub-Saharan Africa", "South Asia",
                      "North America", "Euro area", "Arab World", "West Bank and Gaza"))

# Ver resultado
glimpse(Base_2022)

View(Base_2022)

library(dendextend)   # para colorear el dendrograma
library(factoextra)   
datos_hc <- Base_2022 %>%
  select(Pais, Codigo, everything()) %>%
  select(Pais, Codigo, names(Base_2022)[3:ncol(Base_2022)])

# Convertir a num (por si hay factores o characters) y comprobar NAs
datos_hc <- datos_hc %>%
  mutate(across(-c(Pais, Codigo), ~ as.numeric(.x)))

# Mostrar un resumen rápido para verificar
glimpse(datos_hc)
summary(datos_hc %>% select(-Pais, -Codigo))

# Si hay NA: opción simple -> eliminar filas con NA (ya lo habías hecho),
# si prefieres imputar por mediana:
# datos_hc <- datos_hc %>% mutate(across(-c(Pais, Codigo), ~ ifelse(is.na(.x), median(.x, na.rm = TRUE), .x)))

# ---------------------------
# 1) Estandarizar las variables
# ---------------------------
# Ward se hace normalmente con variables escaladas para evitar que una variable domine por su escala.
mat_num <- datos_hc %>% select(-Pais, -Codigo)
mat_scaled <- scale(mat_num)   # media 0, sd 1

# Guardar una tabla de comprobación
head(mat_scaled)

# ---------------------------
# 2) Distancia y clustering jerárquico (Ward.D2)
# ---------------------------
# Distancia Euclídea entre observaciones (países)
dist_mat <- dist(mat_scaled, method = "euclidean")

# hclust con método Ward (ward.D2 recomendado)
hc_ward <- hclust(dist_mat, method = "ward.D2")

# Imprimir resumen del objeto hclust (altura, merge)
print(hc_ward)

# ---------------------------
# 3) Dendrograma básico y coloreado
# ---------------------------
# Convertir a dendrograma (para manipular y pintar)
dend <- as.dendrogram(hc_ward)

# Gráfico base del dendrograma
plot(dend, main = "Dendrograma — Clustering Jerárquico (Ward.D2)", ylab = "Altura")

# Colorear por un corte de k clusters
k_elegido <- 4   # <- cambia este número según lo que te sugieran WSS/silhouette o tu criterio
dend %>%
  color_branches(k = k_elegido) %>%
  color_labels(k = k_elegido) %>%
  plot(main = paste0("Dendrograma coloreado - k = ", k_elegido))

# También puedes colorear por altura (h) en lugar de k:
h_corte <- 30  # ajusta según la escala de tu dendrograma
dend %>% color_branches(h = h_corte) %>% color_labels(h = h_corte) %>%
  plot(main = paste0("Dendrograma coloreado - h = ", h_corte))

# ---------------------------
# 4) Cortar el dendrograma y asignar clusters
# ---------------------------
cluster_hc <- cutree(hc_ward, k = k_elegido)  # vector con número de cluster por país

# Añadir cluster al data.frame original
resultados_hc <- datos_hc %>%
  mutate(cluster_hc = factor(cluster_hc))

# Ver primeros 10 países con su cluster
print(head(resultados_hc %>% select(Pais, Codigo, cluster_hc), 10))

# ---------------------------
# 5) Resumen por cluster (tamaño + medias)
# ---------------------------
perfil_clusters_hc <- resultados_hc %>%
  group_by(cluster_hc) %>%
  summarise(n = n(),
            across(.cols = where(is.numeric), .fns = ~ mean(.x, na.rm = TRUE), .names = "mean_{col}")) %>%
  arrange(cluster_hc)

print(perfil_clusters_hc)

# Mostrar tamaños de cluster
table(resultados_hc$cluster_hc)

# ---------------------------
# 6) Validación visual (PCA + scatter de PC1 vs PC2 coloreado por cluster)
# ---------------------------
# Hacemos una PCA rápida para tener coordenadas 2D visualizables
pca_tmp <- prcomp(mat_scaled, scale. = FALSE)  # ya escalado, así que scale.=FALSE ok
scores_pca <- as.data.frame(pca_tmp$x[, 1:2])  # PC1 y PC2
scores_pca <- bind_cols(resultados_hc %>% select(Pais, Codigo, cluster_hc), scores_pca)

# Graficar PC1 vs PC2
ggplot(scores_pca, aes(x = PC1, y = PC2, color = cluster_hc, label = Pais)) +
  geom_point(size = 3) +
  geom_text(aes(label = Pais), size = 2.5, vjust = 1.6, show.legend = FALSE) +
  labs(title = "PC1 vs PC2 — países coloreados por cluster (Ward)",
       subtitle = paste("k =", k_elegido)) +
  theme_minimal()

# ---------------------------
# 7) Medida de silhouette para el clustering jerárquico (opcional)
# ---------------------------
# Calculamos silhouette sobre los clusters y la distancia usada
library(cluster)
sil <- silhouette(as.numeric(resultados_hc$cluster_hc), dist_mat)
summary(sil)            # ver promedio de silhouette
plot(sil, main = "Silhouette — clustering jerárquico")

# Alternativa: usar factoextra::fviz_nbclust con hcut para comparar k antes de cortar
fviz_nbclust(mat_scaled, FUN = hcut, method = "silhouette") + ggtitle("Silhouette (hcut) — elegir k")