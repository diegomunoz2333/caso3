library(tidyverse)
library(FactoClass)
library(ade4)
library(factoextra)
library(dendextend)
library(cluster)
library(ggrepel)
library(readr)
library(ggplot2)
library(ggrepel)
library(kableExtra)
library(dplyr)
library(ggrepel)
library(ggforce)
library(plotly)
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


cat("Países:", nrow(datos_analisis), "\n")
cat("Variables:", ncol(datos_analisis), "\n\n")

# ACP
acp_temp <- dudi.pca(datos_analisis, center = TRUE, scale = TRUE, scannf = FALSE, nf = ncol(datos_analisis))
varianza_acum <- cumsum(acp_temp$eig) / sum(acp_temp$eig) * 100
n_componentes <- which(varianza_acum >= 80)[1]

cat("=== SELECCIÓN DE COMPONENTES ===\n")
cat("Componentes necesarios para ≥80% varianza:", n_componentes, "\n")
cat("Varianza explicada:", round(varianza_acum[n_componentes], 2), "%\n\n")

acp_resultado <- dudi.pca(
  df = datos_analisis,
  center = TRUE,
  scale = TRUE,
  scannf = FALSE,
  nf = n_componentes
)

# Extraer los factores 
factores <- acp_resultado$li  # cada país con sus coordenadas 

# Gráficos ACP
varianza_df <- data.frame(
  Componente = factor(1:length(acp_resultado$eig)),
  Varianza = acp_resultado$eig / sum(acp_resultado$eig) * 100,
  VarianzaAcum = varianza_acum
)




#///////////////////////Gráfico de Codo/////////////////////////////////////////


ggplot(varianza_df, aes(x = as.numeric(Componente), y = Varianza)) +
  geom_col(fill = "#2C5F8D", alpha = 0.9) +
  geom_line(aes(y = VarianzaAcum, group = 1), color = "#8E44AD", linewidth = 1.4) +
  geom_point(aes(y = VarianzaAcum), color = "#8E44AD", size = 3.5) +
  geom_hline(yintercept = 80, linetype = "dashed", color = "#27AE60", linewidth = 1.1) +
  geom_vline(xintercept = 8, linetype = "dotted", color = "#5DADE2", linewidth = 1.1) +
  annotate("text", x = 8.3, y = max(varianza_df$Varianza) * 0.9,
           label = "8 componentes", color = "#5DADE2", angle = 90, hjust = 0, 
           fontface = "bold", size = 3.8) +
  annotate("text", x = 1.8, y = 83,
           label = "80% varianza acumulada", color = "#27AE60", hjust = 0,
           fontface = "bold", size = 3.8) +
  scale_x_continuous(breaks = 1:length(acp_resultado$eig)) +
  labs(
    title = "Gráfico de Sedimentación: Varianza Explicada por Componente Principal",
    subtitle = "Selección de componentes mediante criterio de varianza acumulada ≥80%",
    x = "Componentes Principales",
    y = "% de Varianza Explicada"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5, color = "#2C5F8D"),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    axis.title = element_text(face = "bold", size = 12, color = "#2C5F8D"),
    axis.text = element_text(size = 10, color = "gray30"),
    axis.text.x = element_text(angle = 45, vjust = 0.8),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90", size = 0.3),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white")
  )

library(kableExtra)
varianza_tabla <- varianza_df %>%
  mutate(
    Varianza = round(Varianza, 2),
    VarianzaAcum = round(VarianzaAcum, 2)
  )

varianza_tabla %>%
  knitr::kable(
    caption = "Varianza explicada por componente principal",
    col.names = c("Componente", "% Varianza", "% Varianza Acumulada"),
    align = c("c", "c", "c"),
    format = "html"
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width = FALSE,
    position = "center",
    font_size = 14
  ) %>%
  row_spec(0, bold = TRUE, color = "white", background = "#2E86AB") %>%
  column_spec(1, bold = TRUE, width = "7em") %>%
  column_spec(2:3, width = "10em")



#===============================================================================


#////////////////////// Círculo de correlaciones ///////////////////////////////

vars_df <- as.data.frame(acp_resultado$co)
vars_df$Variable <- rownames(vars_df)

vars_df <- as.data.frame(acp_resultado$co)
vars_df$Variable <- rownames(vars_df)

theta <- seq(0, 2*pi, length.out = 200)
circle_df <- data.frame(
  x = cos(theta),
  y = sin(theta)
)
max_coord <- max(1, max(abs(vars_df$Comp1), abs(vars_df$Comp2)))
lims <- c(-max_coord * 1.05, max_coord * 1.05) 


ggplot() +
  geom_path(data = circle_df, aes(x = x, y = y), 
            color = "gray60", linetype = "dashed", size = 0.6) +
  geom_hline(yintercept = 0, color = "gray85", size = 0.4) +
  geom_vline(xintercept = 0, color = "gray85", size = 0.4) +
  geom_segment(data = vars_df,
               aes(x = 0, y = 0, xend = Comp1, yend = Comp2),
               color = "#2C5F8D", alpha = 0.85,
               arrow = grid::arrow(length = unit(0.20, "cm"), type = "closed"),
               size = 0.9) +
  geom_point(data = vars_df, aes(x = Comp1, y = Comp2), 
             color = "#8E44AD", size = 3) +
  geom_text_repel(data = vars_df, aes(x = Comp1, y = Comp2, label = Variable),
                  size = 3.8, max.overlaps = 30, segment.alpha = 0.5,
                  fontface = "bold", color = "gray20") +
  coord_equal(xlim = lims, ylim = lims) +
  labs(
    title = "Círculo de Correlaciones de Variables en el Plano Factorial",
    x = "Componente Principal 1",
    y = "Componente Principal 2",
    subtitle = "Vectores indican la correlación de cada variable con los dos primeros componentes"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5, color = "#2C5F8D"),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    axis.title = element_text(face = "bold", size = 12, color = "#2C5F8D"),
    axis.text = element_text(size = 10, color = "gray30"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray92", size = 0.3),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white")
  )



#===============================================================================






#////////Gráfico de Dispersión de Individuos en el Espacio Factorial///////////

# Países en el espacio factorial
paises_df <- as.data.frame(acp_resultado$li)
paises_df$Pais <- rownames(paises_df)

# Calcular distancia al origen (para resaltar países más "extremos")
paises_df$Distancia <- sqrt(paises_df$Axis1^2 + paises_df$Axis2^2)

ggplot(paises_df, aes(x = Axis1, y = Axis2)) +
  geom_point(aes(color = Distancia), size = 3.5, alpha = 0.95) +
  geom_text_repel(
    aes(label = Pais),
    size = 3.4,
    color = "gray20",
    fontface = "bold",
    max.overlaps = 20,
    segment.color = "gray70",
    segment.size = 0.3
  ) +
  geom_hline(yintercept = 0, color = "gray75", linetype = "dashed", linewidth = 0.5) +
  geom_vline(xintercept = 0, color = "gray75", linetype = "dashed", linewidth = 0.5) +
  scale_color_gradient(low = "#A8E6CF", high = "#1A5490", name = "Distancia\nal origen") +
  labs(
    title = "Países en el Espacio Factorial del ACP",
    subtitle = "Distribución de países según los dos primeros componentes principales",
    x = "Componente Principal 1",
    y = "Componente Principal 2"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5, color = "#2C5F8D"),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    axis.title = element_text(face = "bold", size = 12, color = "#2C5F8D"),
    axis.text = element_text(size = 10, color = "gray30"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray92", size = 0.3),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 11, color = "#2C5F8D"),
    legend.text = element_text(size = 10),
    legend.background = element_rect(fill = "white", color = "gray80")
  )




#===============================================================================






#//////////////////////////Tabla de variaanza///////////////////////////////////


#===============================================================================






#///////////////// Nombramiento de las dimensiones /////////////////////////////

res.pca <- prcomp(NuevaBase, scale = TRUE)

res.pca
eig.val <- get_eigenvalue(res.pca)
eig.val

fviz_eig(res.pca)


# Eigenvalues
eig.val <- get_eigenvalue(res.pca)
eig.val


# Resultados para Variables
res.var <- get_pca_var(res.pca)
res.var$coord          # Coordinates
res.var$contrib        # Contributions to the PCs
res.var$cos2           # Quality of representation 

colSums( res.var$contrib[,1:2] )


# Results for individuals
res.ind <- get_pca_ind(res.pca)
res.ind$coord          # Coordinates
res.ind$contrib        # Contributions to the PCs
res.ind$cos2           # Quality of representation 
View(res.ind$contrib[,1:8]) # Miro los dos primeros factores
res.ind$contrib[,1:2]

#////////////////////// Tabla nuevas dimensiones ///////////////////////////////
# Crear la tabla manualmente con las dimensiones y variables
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
  
  "Dimensión 5", "Uso de tierra agrícola y remesas, agricultura y dependencia externa, crecimiento económico agrícola", 
  "Tierra.cultivable, Remesas, Crecimiento.PIB", 
  "China (Mucha, Poca, Poca); Fiji (Poca, Mucha, Mucha); Ghana (Media, Media, Media)",
  
  "Dimensión 6", "Inversión en salud y economía, gasto en salud per cápita, atracción de inversión extranjera", 
  "Gasto.salud, PIB_per, Inversión.Extranjera", 
  "Australia (Mucha, Mucha, Mucha); Djibouti (Poca, Poca, Poca); Botswana (Media, Media, Media)",
  
  "Dimensión 7", "Crecimiento económico intensivo", 
  "Área.boscosa, Población, Crecimiento.PIB", 
  "Malta (Poca, Poca, Media); India (Poca, Mucha, Mucha); China (Poca, Mucha, Media)",
  
  "Dimensión 8", "Conectividad y dinamismo económico", 
  "Suscripciones.móvil, Inversión.extranjera, Crecimiento.PIB", 
  "Malta (Mucha, Mucha, Media); Ireland (Mucha, Poca, Mucha); Sri Lanka (Mucha, Poca, Poca)"
)

# Crear la tabla con kableExtra
dimensiones_tabla %>%
  knitr::kable(
    caption = "Interpretación de las Dimensiones obtenidas del Análisis de Componentes Principales (ACP)",
    col.names = c("Dimensión", "Descripción", "Variables Asociadas", "Ejemplo de Países"),
    align = c("c", "l", "l", "l"),
    format = "html",
    escape = FALSE
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width = FALSE,
    position = "center",
    font_size = 13
  ) %>%
  row_spec(0, bold = TRUE, color = "white", background = "#2E86AB") %>%
  column_spec(1, bold = TRUE, width = "8em") %>%
  column_spec(2, width = "22em") %>%
  column_spec(3, width = "16em") %>%
  column_spec(4, width = "18em")


#===============================================================================





#////////////////////// CLUSTERING SOBRE LOS FACTORES //////////////////////////
distancia <- dist(factores)
arbol <- hclust(distancia, method = "ward.D2")

# Método del codo (WSS)
wss <- sapply(2:10, function(k) {
  grupos <- cutree(arbol, k = k)
  sum(tapply(1:nrow(factores), grupos, function(idx) {
    if (length(idx) > 1) {
      sum(dist(factores[idx, ])^2) / (2 * length(idx))
    } else 0
  }))
})

# Detectar número óptimo
diff_wss <- diff(wss)
diff_diff <- diff(diff_wss)
k_optimo <- which.max(diff_diff) + 2
if (k_optimo > 6) k_optimo <- 5
if (k_optimo < 2) k_optimo <- 2

cat("=== CLUSTERING ===\n")
cat("Número óptimo de clusters según el método del codo:", k_optimo, "\n\n")

# ASIGNAR CLUSTERS
clusters <- cutree(arbol, k = k_optimo)

# Nueva base con clusters
NuevaBase <- data.frame(Cluster = clusters, datos_analisis)
view(NuevaBase)
write.csv(NuevaBase, "NuevaBase_clusters.csv", row.names = TRUE)

# Promedios por cluster
carac_cont <- NuevaBase %>%
  group_by(Cluster) %>%
  summarise(across(everything(), mean, na.rm = TRUE), .groups = "drop") %>%
  as.data.frame()

# RESULTADOS
resultado_ACP <- list(
  dudi = acp_resultado,
  cluster = clusters,
  tree = arbol,
  carac.cont = carac_cont
)

cat("Clusters formados:", length(unique(clusters)), "\n\n")


# Círculo de correlaciones
s.corcircle(acp_resultado$co)

#//////////////////// Gráfico de países en espacio de factores//////////////////
s.class(factores, as.factor(clusters), sub = "Clusters en el espacio factorial", possub = "bottomright")

library(ggplot2)
library(ggrepel)
library(dplyr)

# Preparar datos
paises_clusters <- data.frame(
  Pais = rownames(factores),
  Comp1 = factores[, 1],
  Comp2 = factores[, 2],
  Cluster = as.factor(clusters)
)

# Calcular centroides para cada cluster
centroides <- paises_clusters %>%
  group_by(Cluster) %>%
  summarise(
    Centroid1 = mean(Comp1),
    Centroid2 = mean(Comp2)
  )

# Gráfico
ggplot(paises_clusters, aes(x = Comp1, y = Comp2, color = Cluster)) +
  geom_point(size = 3.5, alpha = 0.8) +
  stat_ellipse(aes(fill = Cluster), 
               type = "norm", 
               level = 0.68,
               geom = "polygon", 
               alpha = 0.15,
               size = 1.2) +
  geom_point(data = centroides, 
             aes(x = Centroid1, y = Centroid2, color = Cluster),
             size = 6, shape = 17, stroke = 1.5) +
  geom_text_repel(
    aes(label = Pais),
    size = 3.2,
    max.overlaps = 15,
    segment.alpha = 0.4,
    fontface = "bold",
    show.legend = FALSE
  ) +
  geom_hline(yintercept = 0, color = "gray75", linetype = "dashed", linewidth = 0.5) +
  geom_vline(xintercept = 0, color = "gray75", linetype = "dashed", linewidth = 0.5) +
  scale_color_manual(
    values = c("1" = "#2C5F8D", "2" = "#27AE60", "3" = "#8E44AD"),
    name = "Clúster",
    labels = c("1" = "Desarrollado", "2" = "Emergente", "3" = "Subdesarrollado")
  ) +
  scale_fill_manual(
    values = c("1" = "#2C5F8D", "2" = "#27AE60", "3" = "#8E44AD"),
    guide = "none"
  ) +
  labs(
    title = "Distribución de Clústeres en el Espacio Factorial",
    subtitle = "Agrupamiento de países según los dos primeros componentes principales",
    x = "Componente Principal 1",
    y = "Componente Principal 2"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5, color = "#2C5F8D"),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    axis.title = element_text(face = "bold", size = 12, color = "#2C5F8D"),
    axis.text = element_text(size = 10, color = "gray30"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray92", size = 0.3),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 11, color = "#2C5F8D"),
    legend.text = element_text(size = 10),
    legend.background = element_rect(fill = "white", color = "gray80")
  )

#===============================================================================




# Dendrograma
plot(arbol, labels = FALSE, main = "Dendrograma (método de Ward)", xlab = "", sub = "")
rect.hclust(arbol, k = k_optimo, border = 2:5)



#////////////////////////// Dendrograma Ward /////////////////////////////////


pal <- c("#2C5F8D", "#27AE60", "#8E44AD")

fviz_dend(arbol,
          k = k_optimo,
          k_colors = pal,           
          color_labels_by_k = TRUE, 
          rect = TRUE,               
          rect_fill = TRUE,          
          rect_border = "gray40",    
          cex = 0.85,                 
          lwd = 0.9,                 
          show_labels = TRUE,
          main = "Dendrograma de Agrupamiento Jerárquico",
          sub = "Método Ward.D2 aplicado sobre el espacio factorial del ACP",
          xlab = "",               
          ylab = "Altura de Fusión"
) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 17, hjust = 0.5, color = "#2C5F8D"),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    axis.title.y = element_text(face = "bold", size = 12, color = "#2C5F8D"),
    axis.text.y = element_text(size = 10, color = "gray30"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white")
  )




#===============================================================================






#////////////Gráfico de Dispersión de Clústeres en Espacio Factorial///////////


library(factoextra)
paleta_personalizada <- c("#2E86AB", "#047857", "#8B5CF6")

fviz_cluster(res_hc, 
             ellipse.type = "convex",
             show.clust.cent = TRUE,
             palette = paleta_personalizada,
             geom = c("point", "text"),
             repel = TRUE,
             ggtheme = theme_minimal(),
             xlab = "Dimensión 1 (Componente Principal 1)",
             ylab = "Dimensión 2 (Componente Principal 2)",
             legend.title = "Clúster") +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5, color = "#1B4965"),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    axis.title = element_text(face = "bold", size = 12, color = "#2E86AB"),
    axis.text = element_text(size = 10),
    legend.title = element_text(face = "bold", size = 11, color = "#2E86AB"),
    legend.text = element_text(size = 10),
    legend.position = "right",
    panel.grid.major = element_line(color = "gray90", size = 0.3),
    panel.grid.minor = element_line(color = "gray95", size = 0.2),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    legend.background = element_rect(fill = "white", color = "gray80")
  ) +
  labs(
    title = "Gráfico de Dispersión de Clústeres en Espacio Factorial",
    subtitle = "Clústeres formados mediante Ward.D2 en espacio factorial del ACP"
  )


#===============================================================================




#////////////////////////Gráfico de Silueta (Silhouette Plot)////////////////////

library(factoextra)

paleta_personalizada <- c("#2E86AB", "#047857", "#8B5CF6")

fviz_silhouette(res_hc, 
                palette = paleta_personalizada, 
                print.summary = TRUE) +
  labs(
    title = "Análisis de Calidad del Agrupamiento mediante Coeficiente de Silueta",
    subtitle = "Evaluación de la cohesión y separación de los clústeres identificados"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5, color = "#1B4965"),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    axis.title = element_text(face = "bold", size = 11, color = "#2E86AB"),
    axis.text = element_text(size = 10),
    legend.title = element_text(face = "bold", size = 11, color = "#2E86AB"),
    legend.text = element_text(size = 10),
    legend.position = "right",
    panel.grid.major = element_line(color = "gray90", size = 0.3),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    axis.text.y = element_text(size = 8)
  )


#===============================================================================





#/////////////Tabla de Distribución de Frecuencias de Clústeres/////////////////


tamaños <- as.data.frame(table(res_hc$cluster))
colnames(tamaños) <- c("Cluster", "N")
tamaños <- tamaños %>% arrange(as.integer(as.character(Cluster)))

tamaños %>%
  knitr::kable(
    caption = "Tamaño de cada clúster",
    digits = 0,
    align = "c",
    col.names = c("Clúster", "Número de Países")
  ) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width = FALSE,
    position = "center",
    font_size = 14
  ) %>%
  row_spec(0, bold = TRUE, color = "white", background = "#2E86AB") %>%
  column_spec(1, bold = TRUE, width = "10em", color = "#1B4965") %>%
  column_spec(2, width = "12em")



#===============================================================================






#///////////////////////////Tabla de medias por clúster////////////////////////

#    Usamos el data frame original de análisis con la columna Cluster
datos_con_cluster <- datos_analisis %>%
  mutate(Cluster = factor(res_hc$cluster))  # convertimos a factor para claridad

carac_cluster <- datos_con_cluster %>%
  group_by(Cluster) %>%
  summarise(across(everything(), mean, na.rm = TRUE))

kable(carac_cluster, 
      caption = "Medias de variables por clúster",
      digits = 2, align = "c") %>%
  kable_styling(bootstrap_options = c("striped","hover","condensed"),
                full_width = FALSE, position = "center") %>%
  row_spec(0, bold = TRUE, background = "#2E86AB", color = "white")


#===============================================================================

#///////////////////// Tabla interpretativa de los clusters/////////////////////

clusters_tabla <- tribble(
  ~`Cluster`, ~`Nombre propuesto`, ~`Características principales`, ~`Justificación del nombre`, ~`Ejemplos de Países`,
  
  "Cluster 1", "Países Desarrollados / Economías Avanzadas",
  "Alto PIB per cápita, larga esperanza de vida, baja mortalidad infantil, acceso casi total a electricidad e internet, gasto alto en salud.",
  "Agrupa países con indicadores sociales y económicos altos. Corresponde a economías de alto ingreso y alto desarrollo humano según ONU y Banco Mundial.",
  "Alemania, Estados Unidos, Japón, Australia",
  
  "Cluster 2", "Países en Desarrollo / Economías Emergentes",
  "PIB medio, crecimiento económico sostenido, buena industrialización, pero con desigualdades sociales y de acceso a servicios.",
  "Incluye países en transición hacia el desarrollo; presentan expansión industrial y tecnológica, pero aún brechas de ingreso.",
  "México, Turquía, Vietnam, Marruecos",
  
  "Cluster 3", "Países Subdesarrollados / Economías de Bajo Ingreso",
  "Bajo PIB, esperanza de vida reducida, alta mortalidad infantil, acceso limitado a electricidad, educación y salud.",
  "Refleja países con grandes desafíos estructurales y económicos. Clasificados como de bajo desarrollo humano o renta baja.",
  "Angola, Uganda, Haití, Zimbabue"
)

clusters_tabla %>%
  knitr::kable(
    caption = "Resumen interpretativo de los Clusters identificados mediante ACP y Análisis Jerárquico",
    col.names = c("Cluster", "Nombre propuesto", "Características principales", "Justificación del nombre", "Ejemplos de Países"),
    align = c("c", "l", "l", "l", "l"),
    format = "html",
    escape = FALSE
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width = FALSE,
    position = "center",
    font_size = 13
  ) %>%
  row_spec(0, bold = TRUE, color = "white", background = "#2E86AB") %>%
  column_spec(1, bold = TRUE, width = "6em") %>%
  column_spec(2, width = "20em") %>%
  column_spec(3, width = "22em") %>%
  column_spec(4, width = "22em") %>%
  column_spec(5, width = "16em")


#===============================================================================









#///////////////////Base de datos con cluster con nombre////////////////////////
NuevaBase <- read_csv("NuevaBase_clusters.csv")
nombres_clusters <- c(
  "1" = "Desarrollado",
  "2" = "Emergente",
  "3" = "Subdesarrollado"
)
NuevaBase <- NuevaBase %>%
  mutate(Cluster = recode(as.character(Cluster), !!!nombres_clusters))
view(NuevaBase)






#===============================================================================





#///////////////Base cluster x dimension con nombre////////////////////////////
NuevaBase <- readr::read_csv("NuevaBase_clusters.csv", show_col_types = FALSE)
possible_names <- c("Pais", "pais", "PAIS", "Country", "country", "COUNTRY", "Row.names", "Rowname", "X1", "...1")
country_col <- intersect(possible_names, names(NuevaBase)) %>% first()
if (is.null(country_col)) {
  if (!is.numeric(NuevaBase[[1]])) {
    country_col <- names(NuevaBase)[1]
  } else stop("No se pudo detectar la columna 'Pais'.")
}
if (country_col != "Pais") NuevaBase <- NuevaBase %>% rename(Pais = all_of(country_col))
if (!"Cluster" %in% names(NuevaBase)) stop("La tabla no tiene columna 'Cluster'.")
nombres_clusters <- c("1" = "Desarrollado", "2" = "Emergente", "3" = "Subdesarrollado")
NuevaBase <- NuevaBase %>%
  mutate(Cluster = as.character(Cluster),
         Cluster_nombre = recode(Cluster, !!!nombres_clusters, .default = Cluster)) %>%
  relocate(Cluster_nombre, .after = Cluster)
vars_num <- NuevaBase %>%
  select(-Pais, -Cluster, -Cluster_nombre) %>%
  select(where(is.numeric)) %>%
  names()
if (length(vars_num) < 2) stop("No hay suficientes variables numéricas para hacer PCA (mínimo 2).")
mat <- NuevaBase %>%
  select(Pais, all_of(vars_num)) %>%
  column_to_rownames("Pais")
n_comp_requested <- 8
n_comp_available <- min(n_comp_requested, ncol(mat))
res.pca <- prcomp(mat, center = TRUE, scale. = TRUE, rank. = n_comp_available)
scores <- as.data.frame(res.pca$x[, 1:n_comp_available, drop = FALSE]) %>%
  rownames_to_column("Pais")
nombre_dimensiones <- c(
  "DesarrolloHumano",
  "Industrializacion",
  "ComercioApertura",
  "PresionDemografica",
  "Agricultura_Remesas",
  "Salud_Inversion",
  "CrecimientoIntensivo",
  "Conectividad_Dinamismo"
)
nombre_dimensiones <- nombre_dimensiones[1:n_comp_available]
colnames(scores)[-1] <- nombre_dimensiones
Base_Final <- NuevaBase %>%
  select(Pais, Cluster_nombre) %>%
  left_join(scores, by = "Pais")
View(Base_Final)

#===============================================================================











#//////////////////////////////////Predicción////////////////////////////////////

nuevo_pais <- data.frame(
  PIB_per = 18000,
  Poblacion = 12000000,
  Esperanza.vida = 76,
  Acceso.electricidad = 98,
  Area.boscosa = 35,
  Suscripciones.movil = 120,
  Crecimiento.PIB = 3.2,
  Mortalidad.infantil = 15,
  Inversion.extranjera = 4,
  Gasto.salud = 7,
  Uso.internet = 85,
  Importaciones = 25,
  Exportaciones = 22,
  Tierra.cultivable = 12,
  Crecimiento.poblacion = 1.1,
  Industria = 28,
  Remesas = 2
)

nuevo_pais_pca <- predict(res.pca, newdata = nuevo_pais)

centroides <- aggregate(res.pca$x[, 1:8], 
                        by = list(Cluster = res_hc$cluster), 
                        FUN = mean)

distancias <- apply(centroides[, -1], 1, function(c) dist(rbind(c, nuevo_pais_pca[1, 1:8])))
cluster_predicho <- centroides$Cluster[which.min(distancias)]

cat("El nuevo país se clasifica en el Cluster:", cluster_predicho, "\n")
nombres_clusters <- c("1" = "Desarrollado", "2" = "Emergente", "3" = "Subdesarrollado")
cat("Nombre del cluster:", nombres_clusters[as.character(cluster_predicho)], "\n")
