library(tidyverse)     
library(FactoClass)    
library(ade4)          
library(factoextra)    
library(cluster)       
library(dendextend)    
library(ggrepel)      
library(kableExtra)    
library(ggforce)    
library(dendextend)
library(plotly)        
library(sf)            
library(leaflet)       
library(rnaturalearth)
library(RColorBrewer)  
library(ellipse)
library(tidyverse)
library(factoextra)
library(kableExtra)
library(ggrepel)
library(ggforce)
library(plotly)

# Cargar base
Base <- read_csv("f36a5086-3311-4b1a-9f0c-bda5cd4718df_Series - Metadata.csv",
                 show_col_types = FALSE)

# Selección y limpieza
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

# Preparar datos
datos_analisis <- Base_2022 %>%
  select(-Codigo) %>%
  column_to_rownames("Pais")

cat("Países:", nrow(datos_analisis), "\n")
cat("Variables:", ncol(datos_analisis), "\n\n")

# -----------------------------------------------------------
# Análisis de Componentes Principales (ACP) con prcomp()
# -----------------------------------------------------------

acp_prcomp <- prcomp(datos_analisis, center = TRUE, scale. = TRUE)

# Varianza explicada
varianza <- acp_prcomp$sdev^2
varianza_prop <- varianza / sum(varianza)
varianza_acum <- cumsum(varianza_prop) * 100

# Número de componentes que explican al menos el 70 %
n_componentes <- which(varianza_acum >= 70)[1]
cat("Número de componentes necesarios para explicar el 70% de la varianza:", n_componentes, "\n")

# Tabla resumen
varianza_df <- data.frame(
  Componente = 1:length(varianza_prop),
  Varianza = round(varianza_prop * 100, 2),
  VarianzaAcum = round(varianza_acum, 2)
)

# Visualizar tabla con kableExtra
varianza_df %>%
  kbl(caption = "Porcentaje de varianza explicada por componente") %>%
  kable_styling(full_width = FALSE)

##############################grafica varianza acumulada
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
  scale_x_continuous(breaks = 1:length(acp_prcomp$eig)) +
  labs(
    title = "Gráfico de Sedimentación: Varianza Explicada por Componente Principal",
    subtitle = "Selección de componentes mediante criterio de varianza acumulada ≥70%",
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


#/////////////Gráfico de varianza explicada////////////////////////////////////

fviz_eig(res.pca,
         addlabels = TRUE,
         ylim = c(0, max(get_eigenvalue(res.pca)[, 2]) + 5),
         choice = "variance") +
  labs(
    title = "Varianza Explicada por Componente Principal",
    subtitle = "Autovalores del análisis de componentes principales",
    x = "Componentes Principales",
    y = "Porcentaje de Varianza Explicada (%)"
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
  ) +
  scale_fill_manual(values = "#2C5F8D") +
  scale_color_manual(values = "#2C5F8D")
eig.val <- get_eigenvalue(res.pca)
eig.val # tabla ?
res.var <- get_pca_var(res.pca)
res.var$coord
res.var$contrib        
res.var$cos2           
View(res.var$contrib[,1:6]) ## clustrer ??


########################### hasta aqui van las dimensiones
#///////////////////////// Gráfico de silhouette /////////////////////////////

fviz_nbclust(
  datos_analisis, 
  FUN = kmeans, 
  method = "silhouette"
) +
  labs(
    title = "Número Óptimo de Clústeres según Silhouette",
    subtitle = "Método de la silueta aplicado sobre k-means",
    x = "Número de Clústeres (k)",
    y = "Ancho promedio de la silueta"
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


#////////////////////// CLUSTERING SOBRE LOS FACTORES //////////////////////////
distancia <- dist(factores)
arbol <- hclust(distancia, method = "ward.D2")

wss <- sapply(2:10, function(k) {
  grupos <- cutree(arbol, k = k)
  sum(tapply(1:nrow(factores), grupos, function(idx) {
    if (length(idx) > 1) {
      sum(dist(factores[idx, ])^2) / (2 * length(idx))
    } else 0
  }))
}) ## verificar 

# Gráfico del método del codo con estilo profesional
df_wss <- data.frame(
  k = 2:10,
  WSS = wss
) ## <correccion wss ??

# Detectar número óptimo
diff_wss <- diff(wss)
diff_diff <- diff(diff_wss)
k_optimo <- which.max(diff_diff) + 2 ## Por que mas dos??
if (k_optimo > 6) k_optimo <- 5
if (k_optimo < 2) k_optimo <- 2

cat("=== CLUSTERING ===\n")
cat("Número óptimo de clusters según el método del codo:", k_optimo, "\n\n")


ggplot(df_wss, aes(x = k, y = WSS)) +
  geom_line(color = "#2C5F8D", linewidth = 1.1) +
  geom_point(size = 3, color = "#2C5F8D") +
  geom_vline(xintercept = k_optimo, linetype = "dashed", color = "#E74C3C", linewidth = 1) +
  annotate("text",
           x = k_optimo + 0.4, y = df_wss$WSS[df_wss$k == k_optimo],
           label = paste("k óptimo =", k_optimo),
           color = "#E74C3C", hjust = 0, size = 4.2, fontface = "bold") +
  labs(
    title = "Número Óptimo de Clústeres según Método del Codo",
    subtitle = "Basado en la Suma de Cuadrados Intra-Cluster (WSS)",
    x = "Número de Clústeres (k)",
    y = "Suma de Cuadrados Intra-Cluster (WSS)"
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

# ASIGNAR CLUSTERS
clusters <- cutree(arbol, k = k_optimo)

# Nueva base con clusters
NuevaBase <- data.frame(Cluster = clusters, datos_analisis)
write.csv(NuevaBase, "NuevaBase_clusters.csv", row.names = TRUE)



##FVIZ-PCA-INDIVIUS

# Obtener datos de individuos
ind_data <- get_pca_ind(res.pca)

# Crear dataframe con todos los datos
ind_df <- data.frame(
  Pais = rownames(ind_data$coord),
  Dim1 = ind_data$coord[, 1],
  Dim2 = ind_data$coord[, 2],
  Dim3 = ind_data$coord[, 3],
  Dim4 = ind_data$coord[, 4],
  Dim5 = ind_data$coord[, 5],
  Dim6 = ind_data$coord[, 6],
  Contrib1 = ind_data$contrib[, 1],
  Contrib2 = ind_data$contrib[, 2],
  Contrib3 = ind_data$contrib[, 3],
  Contrib4 = ind_data$contrib[, 4],
  Contrib5 = ind_data$contrib[, 5],
  Contrib6 = ind_data$contrib[, 6],
  Cos2_1 = ind_data$cos2[, 1],
  Cos2_2 = ind_data$cos2[, 2],
  Cos2_3 = ind_data$cos2[, 3],
  Cos2_4 = ind_data$cos2[, 4],
  Cos2_5 = ind_data$cos2[, 5],
  Cos2_6 = ind_data$cos2[, 6]
)

# ============================================================================
# GRÁFICO 1: Individuos Dim 1 vs 2 (CON AMBAS CONTRIBUCIONES)
# ============================================================================

p1 <- plot_ly(ind_df,
              x = ~Dim1, 
              y = ~Dim2,
              color = ~Contrib1,
              colors = colorRampPalette(c("#00AFBB", "#E7B800", "#FC4E07"))(100),
              text = ~paste("<b>", Pais, "</b><br>",
                            "Dim 1:", round(Dim1, 3), "<br>",
                            "Dim 2:", round(Dim2, 3), "<br>",
                            "<b>Contribución Dim 1:</b> ", round(Contrib1, 2), "%<br>",
                            "<b>Contribución Dim 2:</b> ", round(Contrib2, 2), "%<br>",
                            "Cos2 Dim 1:", round(Cos2_1, 3), "<br>",
                            "Cos2 Dim 2:", round(Cos2_2, 3)),
              type = "scatter",
              mode = "markers",
              marker = list(size = 10, opacity = 0.8, line = list(width = 1, color = "white")),
              hovertemplate = '%{text}<extra></extra>') %>%
  
  layout(
    title = list(
      text = "Individuos: Dimensión 1 vs Dimensión 2<br><sub>Coloreado por Contribución Dim 1</sub>",
      font = list(size = 16, color = "#2C5F8D")
    ),
    xaxis = list(title = "Dimensión 1", zeroline = TRUE, zerolinecolor = "gray"),
    yaxis = list(title = "Dimensión 2", zeroline = TRUE, zerolinecolor = "gray"),
    hovermode = "closest",
    plot_bgcolor = "white",
    paper_bgcolor = "white"
  )

p1

# ============================================================================
# GRÁFICO 2: Individuos Dim 3 vs 4 (CON AMBAS CONTRIBUCIONES)
# ============================================================================

p2 <- plot_ly(ind_df,
              x = ~Dim3, 
              y = ~Dim4,
              color = ~Contrib3,
              colors = colorRampPalette(c("#00AFBB", "#E7B800", "#FC4E07"))(100),
              text = ~paste("<b>", Pais, "</b><br>",
                            "Dim 3:", round(Dim3, 3), "<br>",
                            "Dim 4:", round(Dim4, 3), "<br>",
                            "<b>Contribución Dim 3:</b> ", round(Contrib3, 2), "%<br>",
                            "<b>Contribución Dim 4:</b> ", round(Contrib4, 2), "%<br>",
                            "Cos2 Dim 3:", round(Cos2_3, 3), "<br>",
                            "Cos2 Dim 4:", round(Cos2_4, 3)),
              type = "scatter",
              mode = "markers",
              marker = list(size = 10, opacity = 0.8, line = list(width = 1, color = "white")),
              hovertemplate = '%{text}<extra></extra>') %>%
  
  layout(
    title = list(
      text = "Individuos: Dimensión 3 vs Dimensión 4<br><sub>Coloreado por Contribución Dim 3</sub>",
      font = list(size = 16, color = "#2C5F8D")
    ),
    xaxis = list(title = "Dimensión 3", zeroline = TRUE, zerolinecolor = "gray"),
    yaxis = list(title = "Dimensión 4", zeroline = TRUE, zerolinecolor = "gray"),
    hovermode = "closest",
    plot_bgcolor = "white",
    paper_bgcolor = "white"
  )

p2

# ============================================================================
# GRÁFICO 3: Individuos Dim 5 vs 6 (CON AMBAS CONTRIBUCIONES)
# ============================================================================

p3 <- plot_ly(ind_df,
              x = ~Dim5, 
              y = ~Dim6,
              color = ~Contrib5,
              colors = colorRampPalette(c("#00AFBB", "#E7B800", "#FC4E07"))(100),
              text = ~paste("<b>", Pais, "</b><br>",
                            "Dim 5:", round(Dim5, 3), "<br>",
                            "Dim 6:", round(Dim6, 3), "<br>",
                            "<b>Contribución Dim 5:</b> ", round(Contrib5, 2), "%<br>",
                            "<b>Contribución Dim 6:</b> ", round(Contrib6, 2), "%<br>",
                            "Cos2 Dim 5:", round(Cos2_5, 3), "<br>",
                            "Cos2 Dim 6:", round(Cos2_6, 3)),
              type = "scatter",
              mode = "markers",
              marker = list(size = 10, opacity = 0.8, line = list(width = 1, color = "white")),
              hovertemplate = '%{text}<extra></extra>') %>%
  
  layout(
    title = list(
      text = "Individuos: Dimensión 5 vs Dimensión 6<br><sub>Coloreado por Contribución Dim 5</sub>",
      font = list(size = 16, color = "#2C5F8D")
    ),
    xaxis = list(title = "Dimensión 5", zeroline = TRUE, zerolinecolor = "gray"),
    yaxis = list(title = "Dimensión 6", zeroline = TRUE, zerolinecolor = "gray"),
    hovermode = "closest",
    plot_bgcolor = "white",
    paper_bgcolor = "white"
  )

p3







# Obtener datos de variables
var_data <- get_pca_var(res.pca)

# Crear dataframe con todos los datos
var_df <- data.frame(
  Variable = rownames(var_data$coord),
  Dim1 = var_data$coord[, 1],
  Dim2 = var_data$coord[, 2],
  Dim3 = var_data$coord[, 3],
  Dim4 = var_data$coord[, 4],
  Dim5 = var_data$coord[, 5],
  Dim6 = var_data$coord[, 6],
  Contrib1 = var_data$contrib[, 1],
  Contrib2 = var_data$contrib[, 2],
  Contrib3 = var_data$contrib[, 3],
  Contrib4 = var_data$contrib[, 4],
  Contrib5 = var_data$contrib[, 5],
  Contrib6 = var_data$contrib[, 6],
  Cos2_1 = var_data$cos2[, 1],
  Cos2_2 = var_data$cos2[, 2],
  Cos2_3 = var_data$cos2[, 3],
  Cos2_4 = var_data$cos2[, 4],
  Cos2_5 = var_data$cos2[, 5],
  Cos2_6 = var_data$cos2[, 6]
)

# ============================================================================
# GRÁFICO 1: Variables Dim 1 vs 2 
# ============================================================================

p1 <- plot_ly(var_df,
              x = ~Dim1, 
              y = ~Dim2,
              color = ~Contrib1,
              colors = colorRampPalette(c("#00AFBB", "#E7B800", "#FC4E07"))(100),
              text = ~paste("<b>", Variable, "</b><br>",
                            "Dim 1:", round(Dim1, 3), "<br>",
                            "Dim 2:", round(Dim2, 3), "<br>",
                            "<b>Contribución Dim 1:</b> ", round(Contrib1, 2), "%<br>",
                            "<b>Contribución Dim 2:</b> ", round(Contrib2, 2), "%<br>",
                            "Cos2 Dim 1:", round(Cos2_1, 3), "<br>",
                            "Cos2 Dim 2:", round(Cos2_2, 3)),
              type = "scatter",
              mode = "markers",
              marker = list(size = 12, opacity = 0.8, line = list(width = 2, color = "white")),
              hovertemplate = '%{text}<extra></extra>') %>%
  
  add_segments(x = 0, xend = ~Dim1, y = 0, yend = ~Dim2,
               line = list(color = "rgba(100, 100, 100, 0.3)", width = 1),
               showlegend = FALSE, hoverinfo = "skip") %>%
  
  layout(
    title = list(
      text = "Variables: Dimensión 1 vs Dimensión 2<br><sub>Coloreado por Contribución Dim 1</sub>",
      font = list(size = 16, color = "#2C5F8D")
    ),
    xaxis = list(title = "Dimensión 1", zeroline = TRUE, zerolinecolor = "gray"),
    yaxis = list(title = "Dimensión 2", zeroline = TRUE, zerolinecolor = "gray"),
    hovermode = "closest",
    plot_bgcolor = "white",
    paper_bgcolor = "white"
  )

p1

# ============================================================================
# GRÁFICO 2: Variables Dim 3 vs 4 
# ============================================================================

p2 <- plot_ly(var_df,
              x = ~Dim3, 
              y = ~Dim4,
              color = ~Contrib3,
              colors = colorRampPalette(c("#00AFBB", "#E7B800", "#FC4E07"))(100),
              text = ~paste("<b>", Variable, "</b><br>",
                            "Dim 3:", round(Dim3, 3), "<br>",
                            "Dim 4:", round(Dim4, 3), "<br>",
                            "<b>Contribución Dim 3:</b> ", round(Contrib3, 2), "%<br>",
                            "<b>Contribución Dim 4:</b> ", round(Contrib4, 2), "%<br>",
                            "Cos2 Dim 3:", round(Cos2_3, 3), "<br>",
                            "Cos2 Dim 4:", round(Cos2_4, 3)),
              type = "scatter",
              mode = "markers",
              marker = list(size = 12, opacity = 0.8, line = list(width = 2, color = "white")),
              hovertemplate = '%{text}<extra></extra>') %>%
  
  add_segments(x = 0, xend = ~Dim3, y = 0, yend = ~Dim4,
               line = list(color = "rgba(100, 100, 100, 0.3)", width = 1),
               showlegend = FALSE, hoverinfo = "skip") %>%
  
  layout(
    title = list(
      text = "Variables: Dimensión 3 vs Dimensión 4<br><sub>Coloreado por Contribución Dim 3</sub>",
      font = list(size = 16, color = "#2C5F8D")
    ),
    xaxis = list(title = "Dimensión 3", zeroline = TRUE, zerolinecolor = "gray"),
    yaxis = list(title = "Dimensión 4", zeroline = TRUE, zerolinecolor = "gray"),
    hovermode = "closest",
    plot_bgcolor = "white",
    paper_bgcolor = "white"
  )

p2

# ============================================================================
# GRÁFICO 3: Variables Dim 5 vs 6 (CON AMBAS CONTRIBUCIONES)
# ============================================================================

p3 <- plot_ly(var_df,
              x = ~Dim5, 
              y = ~Dim6,
              color = ~Contrib5,
              colors = colorRampPalette(c("#00AFBB", "#E7B800", "#FC4E07"))(100),
              text = ~paste("<b>", Variable, "</b><br>",
                            "Dim 5:", round(Dim5, 3), "<br>",
                            "Dim 6:", round(Dim6, 3), "<br>",
                            "<b>Contribución Dim 5:</b> ", round(Contrib5, 2), "%<br>",
                            "<b>Contribución Dim 6:</b> ", round(Contrib6, 2), "%<br>",
                            "Cos2 Dim 5:", round(Cos2_5, 3), "<br>",
                            "Cos2 Dim 6:", round(Cos2_6, 3)),
              type = "scatter",
              mode = "markers",
              marker = list(size = 12, opacity = 0.8, line = list(width = 2, color = "white")),
              hovertemplate = '%{text}<extra></extra>') %>%
  
  add_segments(x = 0, xend = ~Dim5, y = 0, yend = ~Dim6,
               line = list(color = "rgba(100, 100, 100, 0.3)", width = 1),
               showlegend = FALSE, hoverinfo = "skip") %>%
  
  layout(
    title = list(
      text = "Variables: Dimensión 5 vs Dimensión 6<br><sub>Coloreado por Contribución Dim 5</sub>",
      font = list(size = 16, color = "#2C5F8D")
    ),
    xaxis = list(title = "Dimensión 5", zeroline = TRUE, zerolinecolor = "gray"),
    yaxis = list(title = "Dimensión 6", zeroline = TRUE, zerolinecolor = "gray"),
    hovermode = "closest",
    plot_bgcolor = "white",
    paper_bgcolor = "white"
  )

p3

##FVIZ-PCA-BIPLOTS

# Obtener datos
ind_data <- get_pca_ind(res.pca)
var_data <- get_pca_var(res.pca)

# Crear dataframes para individuos
ind_df <- data.frame(
  Pais = rownames(ind_data$coord),
  Dim1 = ind_data$coord[, 1],
  Dim2 = ind_data$coord[, 2],
  Dim3 = ind_data$coord[, 3],
  Dim4 = ind_data$coord[, 4],
  Dim5 = ind_data$coord[, 5],
  Dim6 = ind_data$coord[, 6],
  Contrib1 = ind_data$contrib[, 1],
  Contrib2 = ind_data$contrib[, 2],
  Contrib3 = ind_data$contrib[, 3],
  Contrib4 = ind_data$contrib[, 4],
  Contrib5 = ind_data$contrib[, 5],
  Contrib6 = ind_data$contrib[, 6]
)

# Crear dataframes para variables (amplificadas para visibilidad)
var_df <- data.frame(
  Variable = rownames(var_data$coord),
  Dim1 = var_data$coord[, 1] * 3,
  Dim2 = var_data$coord[, 2] * 3,
  Dim3 = var_data$coord[, 3] * 3,
  Dim4 = var_data$coord[, 4] * 3,
  Dim5 = var_data$coord[, 5] * 3,
  Dim6 = var_data$coord[, 6] * 3,
  Contrib1 = var_data$contrib[, 1],
  Contrib2 = var_data$contrib[, 2],
  Contrib3 = var_data$contrib[, 3],
  Contrib4 = var_data$contrib[, 4],
  Contrib5 = var_data$contrib[, 5],
  Contrib6 = var_data$contrib[, 6]
)

# ============================================================================
# BIPLOT 1: Dim 1 vs 2
# ============================================================================

p1 <- plot_ly() %>%
  
  # Líneas de variables (desde origen)
  add_segments(
    data = var_df,
    x = 0, xend = ~Dim1, y = 0, yend = ~Dim2,
    line = list(color = "#2E9FDF", width = 2),
    showlegend = FALSE, hoverinfo = "skip"
  ) %>%
  
  # Puntos de variables (azul)
  add_trace(
    data = var_df,
    x = ~Dim1, y = ~Dim2,
    text = ~paste("<b>VARIABLE: ", Variable, "</b><br>",
                  "Dim 1:", round(Dim1/3, 3), "<br>",
                  "Dim 2:", round(Dim2/3, 3), "<br>",
                  "<b>Contribución Dim 1:</b> ", round(Contrib1, 2), "%<br>",
                  "<b>Contribución Dim 2:</b> ", round(Contrib2, 2), "%"),
    type = "scatter",
    mode = "markers",
    marker = list(size = 10, color = "#2E9FDF", opacity = 0.8, line = list(width = 2, color = "white")),
    hovertemplate = '%{text}<extra></extra>',
    name = "Variables"
  ) %>%
  
  # Puntos de individuos (gris)
  add_trace(
    data = ind_df,
    x = ~Dim1, y = ~Dim2,
    text = ~paste("<b>PAÍS: ", Pais, "</b><br>",
                  "Dim 1:", round(Dim1, 3), "<br>",
                  "Dim 2:", round(Dim2, 3), "<br>",
                  "<b>Contribución Dim 1:</b> ", round(Contrib1, 2), "%<br>",
                  "<b>Contribución Dim 2:</b> ", round(Contrib2, 2), "%"),
    type = "scatter",
    mode = "markers",
    marker = list(size = 8, color = "#696969", opacity = 0.6, line = list(width = 1, color = "white")),
    hovertemplate = '%{text}<extra></extra>',
    name = "Países"
  ) %>%
  
  layout(
    title = list(
      text = "Biplot: Dimensión 1 vs Dimensión 2<br><sub>Azul = Variables | Gris = Países</sub>",
      font = list(size = 16, color = "#2C5F8D")
    ),
    xaxis = list(title = "Dimensión 1", zeroline = TRUE, zerolinecolor = "gray"),
    yaxis = list(title = "Dimensión 2", zeroline = TRUE, zerolinecolor = "gray"),
    hovermode = "closest",
    plot_bgcolor = "white",
    paper_bgcolor = "white",
    legend = list(x = 0.02, y = 0.98)
  )

p1

### los graficos relacionados a los mismo como el bitplot se pueden hacer en uno solo
# con faceta horizontal???'???'?'
# ============================================================================
# BIPLOT 2: Dim 3 vs 4
# ============================================================================

p2 <- plot_ly() %>%
  
  add_segments(
    data = var_df,
    x = 0, xend = ~Dim3, y = 0, yend = ~Dim4,
    line = list(color = "#2E9FDF", width = 2),
    showlegend = FALSE, hoverinfo = "skip"
  ) %>%
  
  add_trace(
    data = var_df,
    x = ~Dim3, y = ~Dim4,
    text = ~paste("<b>VARIABLE: ", Variable, "</b><br>",
                  "Dim 3:", round(Dim3/3, 3), "<br>",
                  "Dim 4:", round(Dim4/3, 3), "<br>",
                  "<b>Contribución Dim 3:</b> ", round(Contrib3, 2), "%<br>",
                  "<b>Contribución Dim 4:</b> ", round(Contrib4, 2), "%"),
    type = "scatter",
    mode = "markers",
    marker = list(size = 10, color = "#2E9FDF", opacity = 0.8, line = list(width = 2, color = "white")),
    hovertemplate = '%{text}<extra></extra>',
    name = "Variables"
  ) %>%
  
  add_trace(
    data = ind_df,
    x = ~Dim3, y = ~Dim4,
    text = ~paste("<b>PAÍS: ", Pais, "</b><br>",
                  "Dim 3:", round(Dim3, 3), "<br>",
                  "Dim 4:", round(Dim4, 3), "<br>",
                  "<b>Contribución Dim 3:</b> ", round(Contrib3, 2), "%<br>",
                  "<b>Contribución Dim 4:</b> ", round(Contrib4, 2), "%"),
    type = "scatter",
    mode = "markers",
    marker = list(size = 8, color = "#696969", opacity = 0.6, line = list(width = 1, color = "white")),
    hovertemplate = '%{text}<extra></extra>',
    name = "Países"
  ) %>%
  
  layout(
    title = list(
      text = "Biplot: Dimensión 3 vs Dimensión 4<br><sub>Azul = Variables | Gris = Países</sub>",
      font = list(size = 16, color = "#2C5F8D")
    ),
    xaxis = list(title = "Dimensión 3", zeroline = TRUE, zerolinecolor = "gray"),
    yaxis = list(title = "Dimensión 4", zeroline = TRUE, zerolinecolor = "gray"),
    hovermode = "closest",
    plot_bgcolor = "white",
    paper_bgcolor = "white",
    legend = list(x = 0.02, y = 0.98)
  )

p2

# ============================================================================
# BIPLOT 3: Dim 5 vs 6
# ============================================================================

p3 <- plot_ly() %>%
  
  add_segments(
    data = var_df,
    x = 0, xend = ~Dim5, y = 0, yend = ~Dim6,
    line = list(color = "#2E9FDF", width = 2),
    showlegend = FALSE, hoverinfo = "skip"
  ) %>%
  
  add_trace(
    data = var_df,
    x = ~Dim5, y = ~Dim6,
    text = ~paste("<b>VARIABLE: ", Variable, "</b><br>",
                  "Dim 5:", round(Dim5/3, 3), "<br>",
                  "Dim 6:", round(Dim6/3, 3), "<br>",
                  "<b>Contribución Dim 5:</b> ", round(Contrib5, 2), "%<br>",
                  "<b>Contribución Dim 6:</b> ", round(Contrib6, 2), "%"),
    type = "scatter",
    mode = "markers",
    marker = list(size = 10, color = "#2E9FDF", opacity = 0.8, line = list(width = 2, color = "white")),
    hovertemplate = '%{text}<extra></extra>',
    name = "Variables"
  ) %>%
  
  add_trace(
    data = ind_df,
    x = ~Dim5, y = ~Dim6,
    text = ~paste("<b>PAÍS: ", Pais, "</b><br>",
                  "Dim 5:", round(Dim5, 3), "<br>",
                  "Dim 6:", round(Dim6, 3), "<br>",
                  "<b>Contribución Dim 5:</b> ", round(Contrib5, 2), "%<br>",
                  "<b>Contribución Dim 6:</b> ", round(Contrib6, 2), "%"),
    type = "scatter",
    mode = "markers",
    marker = list(size = 8, color = "#696969", opacity = 0.6, line = list(width = 1, color = "white")),
    hovertemplate = '%{text}<extra></extra>',
    name = "Países"
  ) %>%
  
  layout(
    title = list(
      text = "Biplot: Dimensión 5 vs Dimensión 6<br><sub>Azul = Variables | Gris = Países</sub>",
      font = list(size = 16, color = "#2C5F8D")
    ),
    xaxis = list(title = "Dimensión 5", zeroline = TRUE, zerolinecolor = "gray"),
    yaxis = list(title = "Dimensión 6", zeroline = TRUE, zerolinecolor = "gray"),
    hovermode = "closest",
    plot_bgcolor = "white",
    paper_bgcolor = "white",
    legend = list(x = 0.02, y = 0.98)
  )

p3

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


## por pares de componenete 
#////////Gráfico de Dispersión de Individuos en el Espacio Factorial///////////
paises_df <- as.data.frame(acp_resultado$li)
paises_df$Pais <- rownames(paises_df)

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
## interactivo mejorar 
####base de datos dimensiones x pais
 


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

###dendograma
plot(arbol, labels = FALSE, main = "Dendrograma (método de Ward)", xlab = "", sub = "")
rect.hclust(arbol, k = k_optimo, border = 2:5)

# =====================================================
# DENDROGRAMA REAL + RAMAS QUE TERMINAN EN BOTONES
# =====================================================

library(jsonlite)
library(dendextend)
library(ggdendro)

# 1. Tus datos
k_optimo <- 3
clusters <- cutree(arbol, k = k_optimo)

# 2. Países por clúster
paises_cluster <- lapply(1:k_optimo, function(i) sort(rownames(factores)[clusters == i]))
n_paises <- sapply(paises_cluster, length)
json_clusters <- lapply(paises_cluster, toJSON, auto_unbox = TRUE)

# 3. Dendrograma con colores
dend <- as.dendrogram(arbol) %>%
  color_branches(k = k_optimo, col = c("#E74C3C", "#3498DB", "#27AE60")) %>%
  set("branches_lwd", 2.8) %>%
  set("labels_cex", 0)  # sin nombres de países

dend_data <- dendro_data(dend, type = "rectangle")
seg <- segment(dend_data)
lab <- label(dend_data)

# Posición X promedio de cada clúster (para la línea que baja)
cluster_center_x <- tapply(lab$x, clusters[match(lab$label, rownames(factores))], mean)

# 4. HTML definitivo
html <- paste0('
<!DOCTYPE html>
<html lang="es">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Dendrograma con Ramas a Botones</title>
  <script src="https://cdn.jsdelivr.net/npm/d3@7"></script>
  <style>
    body {font-family:Arial; background:#f0f4f8; margin:0; padding:30px;}
    h1 {text-align:center; color:#2c3e50;}
    .info {text-align:center; color:#666; margin-bottom:30px;}
    .container {max-width:1400px; margin:auto; background:white; border-radius:20px; padding:30px; box-shadow:0 15px 50px rgba(0,0,0,0.12);}
    .buttons {text-align:center; margin:50px 0 30px;}
    .btn {padding:18px 40px; margin:0 25px; border:none; border-radius:60px; font-size:20px; font-weight:bold; color:white; cursor:pointer; transition:0.3s;}
    .btn1 {background:#E74C3C;}
    .btn2 {background:#3498DB;}
    .btn3 {background:#27AE60;}
    .btn:hover {transform:scale(1.1); box-shadow:0 10px 30px rgba(0,0,0,0.3);}
    .table-container {padding:30px; background:#fafafa; border-radius:15px; box-shadow:0 8px 30px rgba(0,0,0,0.1); display:none;}
    .table-container.active {display:block; animation:fade 0.7s;}
    @keyframes fade {from{opacity:0; transform:translateY(20px);} to{opacity:1;}}
    table {width:100%; border-collapse:collapse;}
    th {background:#2c3e50; color:white; padding:18px; font-size:22px;}
    td {padding:14px; background:#fff; border-bottom:1px solid #eee;}
    tr:hover td {background:#e8f4fc;}
  </style>
</head>
<body>
  <h1>Dendrograma Jerárquico - 3 Clústeres</h1>
  <p class="info">Las ramas de cada clúster bajan directamente a su botón</p>
  <div class="container">
    <div id="dendrogram"></div>

    <div class="buttons">
      <button class="btn btn1" onclick="show(1)">Clúster 1 (', n_paises[1], ' países)</button>
      <button class="btn btn2" onclick="show(2)">Clúster 2 (', n_paises[2], ' países)</button>
      <button class="btn btn3" onclick="show(3)">Clúster 3 (', n_paises[3], ' países)</button>
    </div>

    <div id="tabla" class="table-container">
      <h2 id="titulo"></h2>
      <table><thead><tr><th>País</th></tr></thead><tbody id="cuerpo"></tbody></table>
    </div>
  </div>

  <script>
    const data = ', toJSON(paises_cluster, auto_unbox=TRUE), ';
    const centers = ', toJSON(cluster_center_x), ';
    const segs = ', toJSON(seg, auto_unbox=TRUE), ';

    const w = 1350, h = 800;
    const svg = d3.select("#dendrogram").append("svg").attr("width", w).attr("height", h);

    // Ramas del dendrograma
    svg.selectAll(".rama").data(segs).enter().append("path")
      .attr("d", d => `M${d$x*10},${d$y} L${d$xend*10},${d$yend}`)
      .attr("stroke", (d,i) => {
        if (d$yend > 250) return "#777";
        const c = segs[i].cluster || 1;
        return c===1?"#E74C3C":c===2?"#3498DB":"#27AE60";
      })
      .attr("stroke-width", 3).attr("fill","none");

    // Líneas gruesas que bajan al botón
    const buttonY = 680;
    const buttonX = [380, 680, 980];
    Object.keys(centers).forEach(c => {
      const x = centers[c] * 10;
      svg.append("line")
        .attr("x1", x).attr("y1", 400)
        .attr("x2", buttonX[c-1]).attr("y2", buttonY)
        .attr("stroke", c==1?"#E74C3C":c==2?"#3498DB":"#27AE60")
        .attr("stroke-width", 10)
        .attr("opacity", 0.9);
    });

    function show(n) {
      document.getElementById("titulo").innerText = `Clúster ${n} - ${data[n-1].length} países`;
      const tbody = document.getElementById("cuerpo");
      tbody.innerHTML = "";
      data[n-1].sort().forEach(p => {
        const tr = tbody.insertRow();
        tr.insertCell(0).textContent = p;
      });
      document.getElementById("tabla").classList.add("active");
    }
  </script>
</body>
</html>
')

# 5. Guardar y abrir
writeLines(html, "Dendrograma_PERFECTO.html")
browseURL("Dendrograma_PERFECTO.html")

cat("¡ÉXITO TOTAL!\n")
cat("Archivo creado: Dendrograma_PERFECTO.html\n")
cat("→ Ramas reales coloreadas\n")
cat("→ Cada clúster tiene su color\n")
cat("→ Líneas gruesas que bajan al botón correspondiente\n")
cat("→ Botones grandes y bonitos\n")
cat("→ Clic → tabla profesional\n")
cat("¡Este es EL DEFINITIVO para tu tesis!\n")






# paises en clusters


centroides <- paises_clusters %>%
  group_by(Cluster) %>%
  summarise(
    Centroid1 = mean(Comp1),
    Centroid2 = mean(Comp2)
  )

elipses_list <- list()

for (cluster_id in unique(paises_clusters$Cluster)) {
  cluster_data <- paises_clusters[paises_clusters$Cluster == cluster_id, ]
  
  if (nrow(cluster_data) > 2) {
    cov_matrix <- cov(cluster_data[, c("Comp1", "Comp2")])
    center <- c(mean(cluster_data$Comp1), mean(cluster_data$Comp2))
    
    elipse_points <- ellipse(cov_matrix, centre = center, level = 0.68, npoints = 100)
    
    elipses_list[[as.character(cluster_id)]] <- data.frame(
      x = elipse_points[, 1],
      y = elipse_points[, 2],
      Cluster = cluster_id
    )
  }
}

elipses_df <- do.call(rbind, elipses_list)
rownames(elipses_df) <- NULL

colores_clusters <- c("1" = "#2C5F8D", "2" = "#27AE60", "3" = "#8E44AD")
nombres_clusters <- c("1" = "Desarrollado", "2" = "Emergente", "3" = "Subdesarrollado")


p <- plot_ly() %>%
  
  add_trace(
    data = elipses_df[elipses_df$Cluster == "1", ],
    x = ~x, 
    y = ~y,
    type = "scatter",
    mode = "lines",
    line = list(color = "#2C5F8D", width = 2, dash = "dash"),
    fill = "toself",
    fillcolor = "rgba(44, 95, 141, 0.1)",
    showlegend = FALSE,
    hoverinfo = "skip"
  ) %>%
  
  add_trace(
    data = elipses_df[elipses_df$Cluster == "2", ],
    x = ~x,
    y = ~y,
    type = "scatter",
    mode = "lines",
    line = list(color = "#27AE60", width = 2, dash = "dash"),
    fill = "toself",
    fillcolor = "rgba(39, 174, 96, 0.1)",
    showlegend = FALSE,
    hoverinfo = "skip"
  ) %>%
  
  add_trace(
    data = elipses_df[elipses_df$Cluster == "3", ],
    x = ~x,
    y = ~y,
    type = "scatter",
    mode = "lines",
    line = list(color = "#8E44AD", width = 2, dash = "dash"),
    fill = "toself",
    fillcolor = "rgba(142, 68, 173, 0.1)",
    showlegend = FALSE,
    hoverinfo = "skip"
  ) %>%
  
  add_trace(
    data = paises_clusters,
    x = ~Comp1,
    y = ~Comp2,
    color = ~Cluster,
    colors = colores_clusters,
    text = ~paste("<b>", Pais, "</b><br>",
                  "Clúster:", nombres_clusters[as.character(Cluster)], "<br>",
                  "PC1:", round(Comp1, 2), "<br>",
                  "PC2:", round(Comp2, 2)),
    type = "scatter",
    mode = "markers",
    marker = list(size = 10, opacity = 0.8),
    hovertemplate = '%{text}<extra></extra>',
    name = "Países"
  ) %>%
  
  add_trace(
    data = centroides,
    x = ~Centroid1,
    y = ~Centroid2,
    type = "scatter",
    mode = "markers",
    marker = list(size = 15, symbol = "triangle-up",
                  line = list(width = 2, color = "white")),
    hoverinfo = "skip",
    showlegend = FALSE
  ) %>%
  
  layout(
    title = list(
      text = "Distribución de Clústeres en el Espacio Factorial<br><sub>Agrupamiento de países según los dos primeros componentes principales</sub>",
      font = list(size = 16, color = "#2C5F8D")
    ),
    xaxis = list(
      title = "Componente Principal 1",
      zeroline = TRUE,
      zerolinecolor = "gray",
      gridcolor = "lightgray"
    ),
    yaxis = list(
      title = "Componente Principal 2",
      zeroline = TRUE,
      zerolinecolor = "gray",
      gridcolor = "lightgray"
    ),
    legend = list(
      title = list(text = "<b>Clúster</b>", font = list(color = "#2C5F8D")),
      bgcolor = "rgba(255,255,255,0.9)",
      bordercolor = "gray",
      borderwidth = 1
    ),
    hovermode = "closest",
    plot_bgcolor = "white",
    paper_bgcolor = "white"
  )

p
#===============================================================================




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
  
)
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

#Tabla interpretativa de los clusters

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


#///////////////////Base de datos con cluster con nombre////////////////////////
NuevaBase <- read_csv("NuevaBase_clusters.csv")
nombres_clusters <- c(
  "1" = "Desarrollado",
  "2" = "Emergente",
  "3" = "Subdesarrollado"
)
NuevaBase <- NuevaBase %>%
  mutate(Cluster = recode(as.character(Cluster), !!!nombres_clusters))


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

#############mapa
world <- ne_countries(scale = "medium", returnclass = "sf")

Base_2022_con_cluster <- Base_2022 %>%
  mutate(Cluster = clusters) %>%  
  select(Codigo, Cluster, Pais)   
Base_con_mapa <- world %>%
  left_join(Base_2022_con_cluster, by = c("iso_a3" = "Codigo")) %>%
  filter(!is.na(Cluster))  

nombres_clusters <- c("1" = "Desarrollado", "2" = "Emergente", "3" = "Subdesarrollado")

ggplot(Base_con_mapa) +
  geom_sf(aes(fill = factor(Cluster))) +
  scale_fill_manual(values = c("1" = "#2C5F8D", "2" = "#27AE60", "3" = "#8E44AD"),
                    name = "Clúster", labels = nombres_clusters) +
  theme_minimal() +
  labs(title = "Mapa Mundial de Países por Clúster",
       subtitle = "Distribución geográfica de los grupos identificados") +
  theme(legend.position = "bottom")  

pal <- colorFactor(palette = c("#2C5F8D", "#27AE60", "#8E44AD"), domain = 1:3)
leaflet(Base_con_mapa) %>%
  addTiles() %>%  
  addPolygons(fillColor = ~pal(Cluster), weight = 1, opacity = 1,
              color = "white", fillOpacity = 0.7,
              popup = ~paste("País:", Pais, "<br>Clúster:", nombres_clusters[as.character(Cluster)]))  











#//////////////////////////////////Predicción////////////////////////////////////
##cuatro paises
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

# Obtener mapa mundial de rnaturalearth
world <- ne_countries(scale = "medium", returnclass = "sf")



