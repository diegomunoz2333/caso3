library(tidyverse)
library(FactoClass)
library(ade4)

# CARGAR Y PREPARAR DATOS

Base <- read_csv("f36a5086-3311-4b1a-9f0c-bda5cd4718df_Series - Metadata.csv",
                 show_col_types = FALSE)

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
    Importaciones = `Imports of goods and services (% of GDP) [NE.IMP.GNFS.ZS]`,
    Exportaciones = `Exports of goods and services (% of GDP) [NE.EXP.GNFS.ZS]`,
    `Tierra cultivable` = `Arable land (% of land area) [AG.LND.ARBL.ZS]`,
    `Crecimiento poblacion` = `Population growth (annual %) [SP.POP.GROW]`,
    Industria = `Industry (including construction), value added (% of GDP) [NV.IND.TOTL.ZS]`,
    Remesas = `Personal remittances, received (% of GDP) [BX.TRF.PWKR.DT.GD.ZS]`
  )

Base_2022[Base_2022 == ".."] <- NA

Base_2022 <- Base_2022 %>%
  mutate(across(-c(Pais, Codigo), as.numeric)) %>%
  drop_na() %>%
  filter(!Pais %in% c("World", "High income", "Low income", "European Union",
                      "Latin America & Caribbean", "Middle income", "OECD members",
                      "East Asia & Pacific", "Sub-Saharan Africa", "South Asia",
                      "North America", "Euro area", "Arab World", "West Bank and Gaza",
                      "Lower middle income", "Upper middle income", 
                      "Least developed countries: UN classification",
                      "Fragile and conflict affected situations", 
                      "Heavily indebted poor countries (HIPC)",
                      "IDA total", "Low & middle income", "Middle East & North Africa",
                      "Pacific island small states", "Small states", 
                      "Caribbean small states", "Other small states", 
                      "IDA & IBRD total", "IDA only", "IBRD only",
                      "Pre-demographic dividend", "Post-demographic dividend", 
                      "Early-demographic dividend", "Late-demographic dividend"))

datos_analisis <- Base_2022 %>%
  select(-Codigo) %>%
  column_to_rownames("Pais")

cat("Países:", nrow(datos_analisis), "\n")
cat("Variables:", ncol(datos_analisis), "\n\n")

# DETERMINAR NÚMERO DE COMPONENTES (para ≥80% varianza)

# ACP preliminar para ver varianza
acp_temp <- dudi.pca(datos_analisis, center = TRUE, scale = TRUE, scannf = FALSE, nf = ncol(datos_analisis))
varianza_acum <- cumsum(acp_temp$eig) / sum(acp_temp$eig) * 100

# Número de componentes para 80% de varianza
n_componentes <- which(varianza_acum >= 80)[1]

cat("=== Selección de Componentes ===\n")
cat("Componentes necesarios para ≥80% varianza:", n_componentes, "\n")
cat("Varianza explicada con", n_componentes, "componentes:", 
    round(varianza_acum[n_componentes], 2), "%\n\n")

# ACP CON COMPONENTES SELECCIONADOS


acp_resultado <- dudi.pca(
  df = datos_analisis,
  center = TRUE,
  scale = TRUE,
  scannf = FALSE,
  nf = n_componentes
)

# CLUSTERING JERÁRQUICO (Ward) - DETERMINAR NÚMERO ÓPTIMO

# Distancias euclídeas sobre los FACTORES del ACP (constructos)
distancia <- dist(acp_resultado$li)

# Clustering Ward
arbol <- hclust(distancia, method = "ward.D2")

# Determinar número óptimo de clusters (método del codo)
# Calcular within sum of squares para diferentes k
wss <- sapply(2:10, function(k) {
  clusters_temp <- cutree(arbol, k = k)
  sum(tapply(1:nrow(acp_resultado$li), clusters_temp, function(idx) {
    if(length(idx) > 1) {
      sum(dist(acp_resultado$li[idx, ])^2) / (2 * length(idx))
    } else {
      0
    }
  }))
})

# Elegir k óptimo (donde hay un codo - aproximación simple)
# Usamos diferencias para detectar el codo
diff_wss <- diff(wss)
diff_diff <- diff(diff_wss)
k_optimo <- which.max(diff_diff) + 2  # +2 porque perdemos 2 índices con diff

# Si el método del codo da un valor muy alto o bajo, limitarlo
if(k_optimo > 6) k_optimo <- 5
if(k_optimo < 2) k_optimo <- 2

# Cortar en k_optimo clusters
clusters <- cutree(arbol, k = k_optimo)

# CREAR resultado_ACP 

# Calcular caracterización (medias por cluster)
NuevaBase <- data.frame(Cluster = clusters, datos_analisis)

carac_cont <- NuevaBase %>%
  group_by(Cluster) %>%
  summarise(across(everything(), mean, na.rm = TRUE), .groups = "drop") %>%
  as.data.frame()

# Crear objeto resultado_ACP 
resultado_ACP <- list(
  dudi = acp_resultado,
  cluster = clusters,
  tree = arbol,
  carac.cont = carac_cont
)

# GRÁFICOS 

# Gráfico del análisis
plot(resultado_ACP$dudi)


# Círculo de correlaciones
s.corcircle((resultado_ACP$dudi)$co)

# Gráfico de individuos
s.label((resultado_ACP$dudi)$li, label = row.names(datos_analisis))

# Gráfico de variables
s.label((resultado_ACP$dudi)$co, xax = 1, yax = 2, 
        sub = "Componente 1 y 2", possub = "bottomright")

# Gráfico conjunto
scatter(resultado_ACP$dudi, xax = 1, yax = 2)

# Gráfico por grupos (requiere que sea factor)
Grupo <- as.factor(NuevaBase$Cluster)
s.class((resultado_ACP$dudi)$li, Grupo, 
        sub = "Componentes 1 y 2", 
        possub = "bottomright",
        xax = 1, yax = 3, 
        col = c(1, 2, 3, 4, 5, 6))

# DESCRIPCIÓN DE LOS GRUPOS

print(resultado_ACP$carac.cont)
