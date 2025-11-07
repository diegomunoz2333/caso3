library(tidyverse)
library(readxl)
library(FactoMineR)
library(factoextra)

# 1) Cargar y limpiar
ruta <- "P_Data_Extract_From_World_Development_Indicators.xlsx"
datos_raw <- read_excel(ruta)

datos <- datos_raw %>%
  select(`Country Name`, `Series Name`, `2022 [YR2022]`) %>%
  mutate(`2022 [YR2022]` = as.numeric(gsub("[^0-9\\.-]", "", as.character(`2022 [YR2022]`)))) %>%
  group_by(`Country Name`, `Series Name`) %>%
  summarise(valor = mean(`2022 [YR2022]`, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = `Series Name`, values_from = valor)

# Eliminar variables con menos de 50 NA 
datos <- datos %>% select(where(~ mean(is.na(.x)) < 0.5)) %>%
  filter(!is.na(`Country Name`)) %>%
  distinct(`Country Name`, .keep_all = TRUE)

# cambiar na por media
datos_imputed <- datos %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)))

paises <- datos_imputed$`Country Name`
datos_num <- datos_imputed %>% select(-`Country Name`) %>%
  mutate(across(everything(), as.numeric))

# PCA
res.pca <- prcomp(datos_num, scale. = TRUE)
fviz_eig(res.pca, addlabels = TRUE, main = "Varianza explicada por componentes")
summary(res.pca)

#Clustering 
pca_scores <- as.data.frame(res.pca$x[, 1:3])
rownames(pca_scores) <- paises

dist_pca <- dist(pca_scores)
modelo_hc <- hclust(dist_pca, method = "ward.D2")
plot(modelo_hc, main = "Dendrograma - Ward D2", cex = 0.7)

clusters <- cutree(modelo_hc, k = 4)

#Resultados
resultados <- data.frame(País = paises, Cluster = clusters)
table(resultados$Cluster)
head(resultados)

perfil_cluster <- datos_num %>%
  mutate(Cluster = clusters) %>%
  group_by(Cluster) %>%
  summarise(across(everything(), \(x) mean(x, na.rm = TRUE)))

print(perfil_cluster)

