library(tidyverse)
library(FactoClass)
library(ade4)
library(factoextra)
library(cluster)
library(dendextend)
library(ggrepel)
library(kableExtra)
library(ggforce)
library(plotly)
library(sf)
library(leaflet)
library(rnaturalearth)
library(RColorBrewer)
library(corrplot)

## CARGA Y LIMPIEZA ------------------------------------------------------------
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
                      "Early-demographic dividend", "Late-demographic dividend")) %>%
  filter(Pais != "Luxembourg")

datos_analisis <- Base_2022 %>%
  select(-Codigo) %>%
  column_to_rownames("Pais")

write_csv(Base_2022, "Base_2022_limpia.csv")
write_csv(datos_analisis %>% rownames_to_column(var = "Pais"), "Base_2022_analisis.csv")

cat("Países:", nrow(datos_analisis), "\n")
cat("Variables:", ncol(datos_analisis), "\n\n")

# ACP -----------------------------------------------------------------------
acp_temp <- dudi.pca(datos_analisis, center = TRUE, scale = TRUE, 
                     scannf = FALSE, nf = ncol(datos_analisis))

varianza_acum <- cumsum(acp_temp$eig) / sum(acp_temp$eig) * 100
n_componentes <- which(varianza_acum >= 70)[1]  # <-- AHORA 6

cat("=== SELECCIÓN DE COMPONENTES ===\n")
cat("Componentes necesarios para ≥70% varianza:", n_componentes, "\n")
cat("Varianza explicada:", round(varianza_acum[n_componentes], 2), "%\n\n")

acp_resultado <- dudi.pca(df = datos_analisis,
                          center = TRUE, scale = TRUE,
                          scannf = FALSE, nf = n_componentes)  # <-- 6

factores <- acp_resultado$li

# GRÁFICO DE VARIANZA (70% y 6 DIMENSIONES) ----------------------------------
varianza_df <- data.frame(
  Componente = factor(1:length(acp_resultado$eig)),
  Varianza = acp_resultado$eig / sum(acp_resultado$eig) * 100,
  VarianzaAcum = varianza_acum
)

ggplot(varianza_df, aes(x = as.numeric(Componente), y = Varianza)) +
  geom_col(fill = "#2C5F8D", alpha = 0.9) +
  geom_line(aes(y = VarianzaAcum, group = 1), color = "#8E44AD", linewidth = 1.4) +
  geom_point(aes(y = VarianzaAcum), color = "#8E44AD", size = 3.5) +
  geom_hline(yintercept = 70, linetype = "dashed", color = "#27AE60", linewidth = 1.1) +
  geom_vline(xintercept = 6, linetype = "dotted", color = "#5DADE2", linewidth = 1.1) +
  annotate("text", x = 6.3, y = max(varianza_df$Varianza) * 0.9,
           label = "6 componentes", color = "#5DADE2", angle = 90, hjust = 0,
           fontface = "bold", size = 3.8) +
  annotate("text", x = 1.8, y = 73,
           label = "70% varianza acumulada", color = "#27AE60", hjust = 0,
           fontface = "bold", size = 3.8) +
  scale_x_continuous(breaks = 1:length(acp_resultado$eig)) +
  labs(title = "Gráfico de Sedimentación: Varianza Explicada por Componente Principal",
       subtitle = "Selección de componentes mediante criterio de varianza acumulada ≥70%",
       x = "Componentes Principales", y = "% de Varianza Explicada") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5, color = "#2C5F8D"),
        plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
        axis.title = element_text(face = "bold", size = 12, color = "#2C5F8D"),
        axis.text = element_text(size = 10, color = "gray30"),
        axis.text.x = element_text(angle = 45, vjust = 0.8),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "gray90", size = 0.3))

# (TODOS LOS DEMÁS GRÁFICOS QUEDAN IGUAL: silueta, codo, círculo, dispersión…)

# TABLA DE VARIANZA -----------------------------------------------------------
varianza_tabla <- varianza_df %>%
  mutate(Varianza = round(Varianza, 2), VarianzaAcum = round(VarianzaAcum, 2))

varianza_tabla %>%
  knitr::kable(caption = "Varianza explicada por componente principal",
               col.names = c("Componente", "% Varianza", "% Varianza Acumulada"),
               align = "c", format = "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = FALSE, position = "center", font_size = 14) %>%
  row_spec(0, bold = TRUE, color = "white", background = "#2E86AB") %>%
  column_spec(1, bold = TRUE, width = "7em") %>%
  column_spec(2:3, width = "10em")

# CÍRCULO DE CORRELACIONES (igual) -------------------------------------------
# (código idéntico al tuyo)

# DISPERSIÓN DE PAÍSES (igual) ------------------------------------------------
# (código idéntico al tuyo)

# TABLA DE DIMENSIONES → AHORA SÓLO 6 ----------------------------------------
dimensiones_tabla <- tribble(
  ~`Dimensión`, ~`Descripción`, ~`Variables`, ~`Ejemplo de Países`,
  "Dimensión 1", "Nivel de desarrollo humano, acceso a servicios básicos y tecnología, salud y conectividad digital",
  "Uso.internet, Esperanza.vida, Mortalidad.infantil",
  "Burundi (Poca, Poca, Mucha); Australia (Mucha, Mucha, Poca); Cambodia (Media, Media, Media)",
  
  "Dimensión 2", "Industrialización y crecimiento demográfico, desarrollo industrial, dependencia económica de remesas",
  "Industria, Crecimiento.poblacion, Remesas",
  "China (Mucha, Media, Poca); Comoros (Poca, Poca, Mucha); Algeria (Mucha, Mucha, Poca)",
  
  "Dimensión 3", "Apertura comercial, actividad comercial internacional, comercio exterior",
  "Importaciones, Exportaciones, Inversión.Extranjera",
  "Malta (Mucha, Mucha, Mucha); Djibouti (Mucha, Mucha, Media); San Marino (Mucha, Mucha, Media)",
  
  "Dimensión 4", "Presión demográfica y uso del suelo",
  "Área.boscosa, Tierra.cultivable, Población",
  "India (Poca, Mucha, Mucha); Timor Leste (Media, Poca, -); China (Poca, Poca, Mucha)",
  
  "Dimensión 5", "Uso de tierra agrícola y remesas, agricultura y dependencia externa",
  "Tierra.cultivable, Remesas, Crecimiento.PIB",
  "China (Mucha, Poca, Poca); Fiji (Poca, Mucha, Mucha); Ghana (Media, Media, Media)",
  
  "Dimensión 6", "Inversión en salud y economía, gasto en salud per cápita, atracción de inversión extranjera",
  "Gasto.salud, PIB_per, Inversión.Extranjera",
  "Australia (Mucha, Mucha, Mucha); Djibouti (Poca, Poca, Poca); Botswana (Media, Media, Media)"
)

dimensiones_tabla %>%
  knitr::kable(caption = "Interpretación de las 6 Dimensiones del ACP",
               col.names = c("Dimensión", "Descripción", "Variables Asociadas", "Ejemplo de Países"),
               align = c("c", "l", "l", "l"), format = "html", escape = FALSE) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = FALSE, position = "center", font_size = 13) %>%
  row_spec(0, bold = TRUE, color = "white", background = "#2E86AB") %>%
  column_spec(1, bold = TRUE, width = "8em")

# CLUSTERING ------------------------------------------------------------------
distancia <- dist(factores)
arbol <- hclust(distancia, method = "ward.D2")

# (codo, silueta, dendrograma, gráficos de clusters → todo igual)

# BASE FINAL CON 6 DIMENSIONES ------------------------------------------------
NuevaBase <- readr::read_csv("NuevaBase_clusters.csv", show_col_types = FALSE)

possible_names <- c("Pais", "pais", "PAIS", "Country", "country", "COUNTRY", "Row.names", "X1", "...1")
country_col <- intersect(possible_names, names(NuevaBase)) %>% first()
if (!is.null(country_col) && country_col != "Pais") NuevaBase <- rename(NuevaBase, Pais = all_of(country_col))

nombres_clusters <- c("1" = "Desarrollado", "2" = "Emergente", "3" = "Subdesarrollado")
NuevaBase <- NuevaBase %>%
  mutate(Cluster = as.character(Cluster),
         Cluster_nombre = recode(Cluster, !!!nombres_clusters, .default = Cluster)) %>%
  relocate(Cluster_nombre, .after = Cluster)

vars_num <- NuevaBase %>% select(-Pais, -Cluster, -Cluster_nombre) %>% select(where(is.numeric)) %>% names()
mat <- NuevaBase %>% select(Pais, all_of(vars_num)) %>% column_to_rownames("Pais")

res.pca <- prcomp(mat, center = TRUE, scale. = TRUE, rank. = 6)  # <-- SOLO 6

scores <- as.data.frame(res.pca$x[, 1:6]) %>% rownames_to_column("Pais")

nombre_dimensiones <- c("DesarrolloHumano", "Industrializacion", "ComercioApertura",
                        "PresionDemografica", "Agricultura_Remesas", "Salud_Inversion")

colnames(scores)[-1] <- nombre_dimensiones

Base_Final <- NuevaBase %>%
  select(Pais, Cluster_nombre) %>%
  left_join(scores, by = "Pais")

View(Base_Final)

# PREDICCIÓN (usa solo 6 dimensiones) -----------------------------------------
nuevo_pais <- data.frame(
  PIB_per = 18000, Poblacion = 12000000, `Esperanza vida` = 76,
  `Acceso electricidad` = 98, `Area boscosa` = 35, `Suscripciones movil` = 120,
  `Crecimiento PIB` = 3.2, `Mortalidad infantil` = 15, `Inversion extranjera` = 4,
  `Gasto salud` = 7, `Uso internet` = 85, Importaciones = 25, Exportaciones = 22,
  `Tierra cultivable` = 12, `Crecimiento poblacion` = 1.1, Industria = 28, Remesas = 2
)

nuevo_pais_pca <- predict(res.pca, newdata = nuevo_pais)[, 1:6]  # <-- 6 dims

centroides <- aggregate(res.pca$x[, 1:6], by = list(Cluster = res_hc$cluster), FUN = mean)
distancias <- apply(centroides[, -1], 1, function(c) dist(rbind(c, nuevo_pais_pca)))
cluster_predicho <- centroides$Cluster[which.min(distancias)]

cat("El nuevo país se clasifica en el Cluster:", cluster_predicho, "\n")
cat("Nombre del cluster:", nombres_clusters[as.character(cluster_predicho)], "\n")

# MAPA, MATRIZ DE CORRELACIONES, etc. → TODO IGUAL
# (el resto del código que ya tenías queda intacto)