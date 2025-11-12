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
                      "Early-demographic dividend", "Late-demographic dividend")) %>% 
  filter(Pais != "Luxembourg")

datos_analisis <- Base_2022 %>%
  select(-Codigo) %>%
  column_to_rownames("Pais")

View(Base_2022)
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

# GUARDAR RESULTADOS

# Guardar objetos
saveRDS(resultado_ACP, "resultado_ACP.rds")
saveRDS(NuevaBase, "NuevaBase.rds")

# Guardar CSVs
write_csv(NuevaBase %>% rownames_to_column("Pais"), "Base_con_Clusters.csv")
write_csv(resultado_ACP$carac.cont, "Caracterizacion_Clusters.csv")

paises_clusters <- data.frame(
  Pais = rownames(datos_analisis),
  Cluster = clusters
) %>% arrange(Cluster, Pais)
write_csv(paises_clusters, "Paises_por_Cluster.csv")


remove.packages("xfun")
install.packages("xfun")



## tema 

theme_cyber <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.background = element_rect(fill = "#000000", color = NA),
      panel.background = element_rect(fill = "#0a0e1a", color = NA),
      panel.grid.major = element_line(color = "#12161f", size = 0.5),
      panel.grid.minor = element_line(color = "#12161f", size = 0.25),
      text = element_text(color = "#e0e6ed", family = "sans", size = 12),
      axis.text = element_text(color = "#8b92a0", size = 10),
      axis.title = element_text(color = "#e0e6ed", face = "bold", size = 13),
      axis.line = element_line(color = "#00e5ff", size = 0.5),
      plot.title = element_text(
        color = "#00e5ff", 
        face = "bold", 
        size = 18, 
        hjust = 0.5,
        margin = margin(b = 10)
      ),
      plot.subtitle = element_text(
        color = "#8b92a0", 
        size = 13, 
        hjust = 0.5,
        margin = margin(b = 15)
      ),
      legend.background = element_rect(fill = "#12161f", color = "#00e5ff", size = 0.5),
      legend.key = element_rect(fill = "#12161f", color = NA),
      legend.text = element_text(color = "#e0e6ed", size = 10),
      legend.title = element_text(color = "#00e5ff", face = "bold", size = 11),
      legend.position = "right",
      strip.background = element_rect(fill = "#12161f", color = "#00e5ff"),
      strip.text = element_text(color = "#00e5ff", face = "bold", size = 11),
      panel.border = element_rect(color = "#00e5ff", fill = NA, size = 0.5),
      plot.margin = margin(20, 20, 20, 20)
    )
}




```{r ambiental-scatter, fig.height=10, fig.width=20, out.width="100%"}

library(plotly)

# Asignar continentes
asignar_continentes <- function(data) {
  continentes_map <- c(
    "Argentina" = "Americas", "Bahamas, The" = "Americas", "Barbados" = "Americas",
    "Belize" = "Americas", "Bolivia" = "Americas", "Brazil" = "Americas",
    "Canada" = "Americas", "Chile" = "Americas", "Colombia" = "Americas",
    "Costa Rica" = "Americas", "Cuba" = "Americas", "Dominica" = "Americas",
    "Dominican Republic" = "Americas", "Ecuador" = "Americas", "El Salvador" = "Americas",
    "Grenada" = "Americas", "Guatemala" = "Americas", "Guyana" = "Americas",
    "Haiti" = "Americas", "Honduras" = "Americas", "Jamaica" = "Americas",
    "Mexico" = "Americas", "Nicaragua" = "Americas", "Panama" = "Americas",
    "Paraguay" = "Americas", "Peru" = "Americas", "St. Kitts and Nevis" = "Americas",
    "St. Lucia" = "Americas", "St. Vincent and the Grenadines" = "Americas",
    "Suriname" = "Americas", "Trinidad and Tobago" = "Americas", "United States" = "Americas",
    "Uruguay" = "Americas", "Venezuela, RB" = "Americas", "Antigua and Barbuda" = "Americas",
    
    "Albania" = "Europe", "Austria" = "Europe", "Belarus" = "Europe",
    "Belgium" = "Europe", "Bosnia and Herzegovina" = "Europe", "Bulgaria" = "Europe",
    "Croatia" = "Europe", "Cyprus" = "Europe", "Czech Republic" = "Europe",
    "Czechia" = "Europe", "Denmark" = "Europe", "Estonia" = "Europe",
    "Finland" = "Europe", "France" = "Europe", "Germany" = "Europe",
    "Greece" = "Europe", "Hungary" = "Europe", "Iceland" = "Europe",
    "Ireland" = "Europe", "Italy" = "Europe", "Kosovo" = "Europe",
    "Latvia" = "Europe", "Lithuania" = "Europe", "Luxembourg" = "Europe",
    "Malta" = "Europe", "Montenegro" = "Europe", "Netherlands" = "Europe",
    "North Macedonia" = "Europe", "Norway" = "Europe", "Poland" = "Europe",
    "Portugal" = "Europe", "Russian Federation" = "Europe", "Serbia" = "Europe",
    "Slovak Republic" = "Europe", "Slovakia" = "Europe", "Slovenia" = "Europe",
    "Spain" = "Europe", "Sweden" = "Europe", "Switzerland" = "Europe",
    "Turkey" = "Europe", "Turkiye" = "Europe", "Ukraine" = "Europe",
    "United Kingdom" = "Europe", "Armenia" = "Europe", "Azerbaijan" = "Europe",
    "Georgia" = "Europe", "Moldova" = "Europe", "Romania" = "Europe",
    "San Marino" = "Europe",
    
    "Afghanistan" = "Asia", "Bahrain" = "Asia", "Bangladesh" = "Asia",
    "Bhutan" = "Asia", "Brunei Darussalam" = "Asia", "Cambodia" = "Asia",
    "China" = "Asia", "Hong Kong SAR, China" = "Asia", "India" = "Asia",
    "Indonesia" = "Asia", "Iran, Islamic Rep." = "Asia", "Iraq" = "Asia",
    "Israel" = "Asia", "Japan" = "Asia", "Jordan" = "Asia",
    "Kazakhstan" = "Asia", "Korea, Dem. People's rep." = "Asia", "Korea, Rep." = "Asia",
    "Kuwait" = "Asia", "Kyrgyz Republic" = "Asia", "Lao PDR" = "Asia",
    "Lebanon" = "Asia", "Macao SAR, China" = "Asia", "Malaysia" = "Asia",
    "Maldives" = "Asia", "Mongolia" = "Asia", "Myanmar" = "Asia",
    "Nepal" = "Asia", "Oman" = "Asia", "Pakistan" = "Asia",
    "Philippines" = "Asia", "Qatar" = "Asia", "Saudi Arabia" = "Asia",
    "Singapore" = "Asia", "Sri Lanka" = "Asia", "Syrian Arab Republic" = "Asia",
    "Taiwan" = "Asia", "Tajikistan" = "Asia", "Thailand" = "Asia",
    "Timor-Leste" = "Asia", "Turkmenistan" = "Asia", "United Arab Emirates" = "Asia",
    "Uzbekistan" = "Asia", "Vietnam" = "Asia", "Viet Nam" = "Asia",
    "West Bank and Gaza" = "Asia", "Yemen, Rep." = "Asia",
    
    "Algeria" = "Africa", "Angola" = "Africa", "Benin" = "Africa",
    "Botswana" = "Africa", "Burkina Faso" = "Africa", "Burundi" = "Africa",
    "Cabo Verde" = "Africa", "Cameroon" = "Africa", "Central African Republic" = "Africa",
    "Chad" = "Africa", "Comoros" = "Africa", "Congo, Dem. Rep." = "Africa",
    "Congo, Rep." = "Africa", "Cote d'Ivoire" = "Africa", "Djibouti" = "Africa",
    "Egypt, Arab Rep." = "Africa", "Equatorial Guinea" = "Africa", "Eritrea" = "Africa",
    "Eswatini" = "Africa", "Ethiopia" = "Africa", "Gabon" = "Africa",
    "Gambia, The" = "Africa", "Ghana" = "Africa", "Guinea" = "Africa",
    "Guinea-Bissau" = "Africa", "Kenya" = "Africa", "Lesotho" = "Africa",
    "Liberia" = "Africa", "Libya" = "Africa", "Madagascar" = "Africa",
    "Malawi" = "Africa", "Mali" = "Africa", "Mauritania" = "Africa",
    "Mauritius" = "Africa", "Morocco" = "Africa", "Mozambique" = "Africa",
    "Namibia" = "Africa", "Niger" = "Africa", "Nigeria" = "Africa",
    "Rwanda" = "Africa", "Senegal" = "Africa", "Sierra Leone" = "Africa",
    "Somalia" = "Africa", "South Africa" = "Africa", "South Sudan" = "Africa",
    "Sudan" = "Africa", "Tanzania" = "Africa", "Togo" = "Africa",
    "Tunisia" = "Africa", "Uganda" = "Africa", "Zambia" = "Africa",
    "Zimbabwe" = "Africa",
    
    "Australia" = "Oceania", "Fiji" = "Oceania", "Kiribati" = "Oceania",
    "Marshall Islands" = "Oceania", "Micronesia, Fed. Sts." = "Oceania",
    "New Zealand" = "Oceania", "Palau" = "Oceania", "Samoa" = "Oceania",
    "Solomon Islands" = "Oceania", "Tonga" = "Oceania", "Vanuatu" = "Oceania"
  )
  
  data %>%
    mutate(Continente = ifelse(Pais %in% names(continentes_map),
                               continentes_map[Pais], NA))
}

# Preparar datos
datos_ambiental <- Base_2022 %>%
  asignar_continentes() %>%
  filter(
    !is.na(`Area boscosa`) & 
      !is.na(`Tierra cultivable`) & 
      !is.na(Poblacion) &
      !is.na(Continente)
  ) %>%
  select(
    Pais, Continente,
    `Area boscosa`,
    `Tierra cultivable`,
    Poblacion
  ) %>%
  mutate(
    Tamaño = log10(Poblacion),
    Hover = paste0(
      "<b style='font-size:13px; color:#00e5ff;'>", Pais, "</b><br>",
      "<span style='color:#a259ff; font-size:11px;'>", Continente, "</span><br><br>",
      "<b style='color:#e0e6ed;'>USO DE TIERRA:</b><br>",
      "├─ Área Boscosa: <b style='color:#00e676;'>", 
      round(`Area boscosa`, 1), "%</b><br>",
      "├─ Tierra Cultivable: <b style='color:#ffd600;'>", 
      round(`Tierra cultivable`, 1), "%</b><br>",
      "├─ Balance: <b style='color:#00e5ff;'>", 
      round(`Area boscosa` + `Tierra cultivable`, 1), "% (bosque + cultivo)</b><br>",
      "└─ Población: <b style='color:#ff1744;'>", 
      format(round(Poblacion/1e6, 1), trim = TRUE), " millones</b>"
    )
  )

# Colores por continente
color_map <- c(
  "Africa" = "#ff1744",
  "Americas" = "#00e676",
  "Asia" = "#ffd600",
  "Europe" = "#0084ff",
  "Oceania" = "#a259ff"
)

# Crear scatter plot
p <- plot_ly() %>%
  add_trace(
    data = datos_ambiental,
    x = ~`Area boscosa`,
    y = ~`Tierra cultivable`,
    size = ~Tamaño,
    color = ~Continente,
    colors = color_map,
    type = "scatter",
    mode = "markers",
    customdata = ~Hover,
    hovertemplate = "%{customdata}<extra></extra>",
    marker = list(
      sizeref = 2 * max(datos_ambiental$Tamaño) / 40^2,
      sizemin = 6,
      opacity = 0.75,
      line = list(color = "#00e5ff", width = 1.5)
    ),
    showlegend = TRUE
  ) %>%
  
  layout(
    title = list(
      text = "<b style='color:#00e5ff; font-size:20px;'>Sostenibilidad Ambiental: Uso de Tierra</b><br>
               <span style='color:#8b92a0; font-size:12px;'>Año 2022 | Tamaño de burbuja = Población | Balance entre conservación y agricultura</span>",
      x = 0.5,
      xanchor = 'center'
    ),
    
    xaxis = list(
      title = "<b>Área Boscosa (% de tierra)</b>",
      gridcolor = '#12161f',
      showgrid = TRUE,
      zeroline = FALSE,
      tickfont = list(color = '#8b92a0', size = 11),
      titlefont = list(color = '#e0e6ed', size = 12),
      range = c(-2, 100)
    ),
    
    yaxis = list(
      title = "<b>Tierra Cultivable (% de tierra)</b>",
      gridcolor = '#12161f',
      showgrid = TRUE,
      zeroline = FALSE,
      tickfont = list(color = '#8b92a0', size = 11),
      titlefont = list(color = '#e0e6ed', size = 12),
      range = c(-2, 70)
    ),
    
    plot_bgcolor = '#000000',
    paper_bgcolor = '#0a0e1a',
    font = list(family = "Arial", color = '#e0e6ed', size = 11),
    
    legend = list(
      title = list(text = "<b>Continente</b>", font = list(size = 12, color = "#00e5ff")),
      bgcolor = 'rgba(10,14,26,0.95)',
      bordercolor = '#00e5ff',
      borderwidth = 1,
      x = 0.02,
      y = 0.98,
      font = list(size = 11, color = "#e0e6ed")
    ),
    
    margin = list(l = 120, r = 80, b = 120, t = 140),
    hovermode = 'closest',
    
    shapes = list(
      # Zona CONSERVACIÓN (arriba-derecha: alto bosque, bajo cultivo)
      list(
        type = "rect",
        x0 = 60, x1 = 100,
        y0 = 0, y1 = 20,
        xref = "x", yref = "y",
        fillcolor = "rgba(0, 230, 118, 0.08)",
        line = list(color = "rgba(0, 230, 118, 0.3)", width = 2, dash = "dot"),
        layer = "below"
      ),
      
      # Zona AGRÍCOLA (arriba-izquierda: bajo bosque, alto cultivo)
      list(
        type = "rect",
        x0 = 0, x1 = 30,
        y0 = 40, y1 = 70,
        xref = "x", yref = "y",
        fillcolor = "rgba(255, 214, 0, 0.08)",
        line = list(color = "rgba(255, 214, 0, 0.3)", width = 2, dash = "dot"),
        layer = "below"
      ),
      
      # Zona BALANCE (centro: balance moderado)
      list(
        type = "rect",
        x0 = 30, x1 = 60,
        y0 = 20, y1 = 40,
        xref = "x", yref = "y",
        fillcolor = "rgba(0, 132, 255, 0.08)",
        line = list(color = "rgba(0, 132, 255, 0.3)", width = 2, dash = "dot"),
        layer = "below"
      ),
      
      # Zona CRÍTICA (abajo-izquierda: bajo bosque, bajo cultivo - desertificación)
      list(
        type = "rect",
        x0 = 0, x1 = 30,
        y0 = 0, y1 = 20,
        xref = "x", yref = "y",
        fillcolor = "rgba(255, 23, 68, 0.08)",
        line = list(color = "rgba(255, 23, 68, 0.3)", width = 2, dash = "dot"),
        layer = "below"
      )
    ),
    
    annotations = list(
      # Zona CONSERVACIÓN
      list(
        x = 80, y = 8,
        text = "<b style='color:#00e676; font-size:11px;'>🌲 CONSERVACIÓN</b><br><span style='color:#00e676; font-size:9px;'>Alto bosque<br>Bajo cultivo</span>",
        showarrow = FALSE,
        bgcolor = "rgba(0, 230, 118, 0.15)",
        bordercolor = "#00e676",
        borderwidth = 1,
        borderpad = 5,
        font = list(size = 9)
      ),
      
      # Zona AGRÍCOLA
      list(
        x = 15, y = 55,
        text = "<b style='color:#ffd600; font-size:11px;'>🌾 AGRÍCOLA</b><br><span style='color:#ffd600; font-size:9px;'>Bajo bosque<br>Alto cultivo</span>",
        showarrow = FALSE,
        bgcolor = "rgba(255, 214, 0, 0.15)",
        bordercolor = "#ffd600",
        borderwidth = 1,
        borderpad = 5,
        font = list(size = 9)
      ),
      
      # Zona BALANCE
      list(
        x = 45, y = 30,
        text = "<b style='color:#0084ff; font-size:11px;'>⚖️ BALANCE</b><br><span style='color:#0084ff; font-size:9px;'>Equilibrio<br>sostenible</span>",
        showarrow = FALSE,
        bgcolor = "rgba(0, 132, 255, 0.15)",
        bordercolor = "#0084ff",
        borderwidth = 1,
        borderpad = 5,
        font = list(size = 9)
      ),
      
      # Zona CRÍTICA
      list(
        x = 15, y = 8,
        text = "<b style='color:#ff1744; font-size:11px;'>⚠ CRÍTICA</b><br><span style='color:#ff1744; font-size:9px;'>Desertificación<br>Riesgo</span>",
        showarrow = FALSE,
        bgcolor = "rgba(255, 23, 68, 0.15)",
        bordercolor = "#ff1744",
        borderwidth = 1,
        borderpad = 5,
        font = list(size = 9)
      ),
      
      # Interpretación
      list(
        x = 0.5, y = -0.18,
        xref = "paper", yref = "paper",
        text = "<span style='color:#8b92a0; font-size:11px;'><b>🌍 INTERPRETACIÓN:</b> Cada país enfrenta trade-offs entre conservación forestal (capturas CO₂, biodiversidad) y agricultura (seguridad alimentaria). El balance sostenible respeta ambos. Burbujas grandes = millones de personas afectadas por políticas ambientales.</span>",
        showarrow = FALSE,
        xanchor = "center",
        yanchor = "top",
        font = list(size = 10, color = "#8b92a0")
      )
    )
  ) %>%
  
  config(
    responsive = TRUE,
    displayModeBar = TRUE,
    displaylogo = FALSE,
    modeBarButtonsToRemove = list('lasso2d', 'select2d')
  )

p

```




```{r scatter-3d-tecnologia, fig.height=10, fig.width=14, out.width="100%"}
datos_3d <- Base_2022 %>%
  asignar_continentes() %>%
  filter(
    !is.na(`Acceso electricidad`) & 
      !is.na(`Uso internet`) & 
      !is.na(`Suscripciones movil`) &
      !is.na(Poblacion) &
      !is.na(Continente) &
      Poblacion > 0
  ) %>%
  select(
    Pais, Continente,
    Electricidad = `Acceso electricidad`,
    Internet = `Uso internet`,
    Movil = `Suscripciones movil`,
    Poblacion,
    PIB_per
  ) %>%
  mutate(
    # Escalar población para tamaño de burbujas
    Tamano = 5 + (Poblacion - min(Poblacion)) / (max(Poblacion) - min(Poblacion)) * 50,
    
    # Tooltip detallado para hover
    Hover = paste0(
      "<b style='font-size:16px; color:#00e5ff;'>", Pais, "</b><br>",
      "<span style='color:#a259ff; font-size:12px;'>", Continente, "</span><br><br>",
      
      "<b style='color:#e0e6ed; font-size:12px;'>📡 INFRAESTRUCTURA DIGITAL:</b><br>",
      "├─ Electricidad: <b style='color:#ffd600;'>", round(Electricidad, 1), "%</b><br>",
      "├─ Internet: <b style='color:#00e676;'>", round(Internet, 1), "%</b><br>",
      "├─ Móvil: <b style='color:#00e5ff;'>", round(Movil, 1), "/100 hab.</b><br>",
      "├─ PIB per cápita: <b style='color:#ff1744;'>$", 
      format(round(PIB_per), big.mark=","), "</b><br>",
      "└─ Población: <b style='color:#76ff03;'>", 
      format(round(Poblacion/1e6, 2), big.mark=","), "M</b>"
    )
  )

# Mapa de colores por continente
color_map_continentes <- c(
  "Africa" = "#ff1744",
  "Americas" = "#00e676",
  "Asia" = "#ffd600",
  "Europe" = "#0084ff",
  "Oceania" = "#a259ff"
)

# Crear scatter 3D
scatter_3d <- plot_ly(
  data = datos_3d,
  x = ~Electricidad,
  y = ~Internet,
  z = ~Movil,
  color = ~Continente,
  colors = color_map_continentes,
  size = ~Tamano,
  sizes = c(10, 100),
  type = "scatter3d",
  mode = "markers",
  marker = list(
    opacity = 0.85,
    line = list(color = "#ffffff", width = 1.5)
  ),
  text = ~Hover,
  hovertemplate = "%{text}<extra></extra>",
  showlegend = TRUE
) %>%
  
  layout(
    # TÍTULO
    title = list(
      text = paste0(
        "<b style='color:#00e5ff; font-size:24px;'>Espacio 3D de Infraestructura Digital Global</b><br>",
        "<span style='color:#8b92a0; font-size:13px;'>",
        "Año 2022 | Acceso Electricidad × Uso Internet × Suscripciones Móviles",
        "</span>"
      ),
      x = 0.5,
      xanchor = "center",
      y = 0.98
    ),
    
    # CONFIGURACIÓN 3D
    scene = list(
      xaxis = list(
        title = "<b style='font-size:14px;'>ACCESO A ELECTRICIDAD (%)</b>",
        backgroundcolor = "rgba(10, 14, 26, 0.7)",
        gridcolor = "rgba(0, 229, 255, 0.1)",
        showbackground = TRUE,
        zeroline = FALSE,
        range = c(0, 105),
        tickfont = list(size = 11, color = "#8b92a0"),
        titlefont = list(size = 12, color = "#00e5ff")
      ),
      yaxis = list(
        title = "<b style='font-size:14px;'>USO DE INTERNET (%)</b>",
        backgroundcolor = "rgba(10, 14, 26, 0.7)",
        gridcolor = "rgba(0, 229, 255, 0.1)",
        showbackground = TRUE,
        zeroline = FALSE,
        range = c(0, 105),
        tickfont = list(size = 11, color = "#8b92a0"),
        titlefont = list(size = 12, color = "#00e5ff")
      ),
      zaxis = list(
        title = "<b style='font-size:14px;'>SUSCRIPCIONES MÓVILES (por 100 hab.)</b>",
        backgroundcolor = "rgba(10, 14, 26, 0.7)",
        gridcolor = "rgba(0, 229, 255, 0.1)",
        showbackground = TRUE,
        zeroline = FALSE,
        range = c(0, max(datos_3d$Movil) * 1.1),
        tickfont = list(size = 11, color = "#8b92a0"),
        titlefont = list(size = 12, color = "#00e5ff")
      ),
      camera = list(
        eye = list(x = 1.5, y = 1.5, z = 1.3),
        center = list(x = 0, y = 0, z = 0)
      )
    ),
    
    # LEYENDA
    legend = list(
      title = list(
        text = "<b style='font-size:13px; color:#00e5ff;'>REGIÓN</b>",
        font = list(size = 13, color = "#00e5ff")
      ),
      bgcolor = "rgba(10, 14, 26, 0.95)",
      bordercolor = "#00e5ff",
      borderwidth = 1.5,
      x = 1.02,
      y = 0.9,
      font = list(size = 12, color = "#e0e6ed")
    ),
    
    # ESTILOS
    plot_bgcolor = "#000000",
    paper_bgcolor = "#0a0e1a",
    font = list(family = "Arial, sans-serif", color = "#e0e6ed", size = 12),
    margin = list(l = 80, r = 150, b = 100, t = 120),
    
    # ANOTACIONES INFORMATIVAS
    annotations = list(
      # Instrucciones de uso
      list(
        x = 0.5,
        y = -0.12,
        xref = "paper",
        yref = "paper",
        text = paste0(
          "<span style='color:#8b92a0; font-size:12px;'>",
          "<b>🔄 ROTACIÓN:</b> Click + Arrastra | ",
          "<b>🔍 ZOOM:</b> Rueda del ratón | ",
          "<b>🎯 PAN:</b> Click derecho + Arrastra",
          "</span>"
        ),
        showarrow = FALSE,
        xanchor = "center"
      ),
      
      # Guía de interpretación
      list(
        x = 0.02,
        y = 0.98,
        xref = "paper",
        yref = "paper",
        text = paste0(
          "<span style='color:#00e5ff; font-size:13px; font-weight:bold;'>",
          "💡 INTERPRETACIÓN</span><br>",
          "<span style='color:#8b92a0; font-size:11px;'>",
          "• <b>Esquina superior derecha:</b> Alta infraestructura<br>",
          "• <b>Esquina inferior izquierda:</b> Brecha digital<br>",
          "• <b>Tamaño burbuja:</b> Población país<br>",
          "• <b>Clustering:</b> Patrones de desarrollo",
          "</span>"
        ),
        showarrow = FALSE,
        xanchor = "left",
        yanchor = "top",
        bgcolor = "rgba(10, 14, 26, 0.9)",
        bordercolor = "#00e5ff",
        borderwidth = 1.5,
        borderpad = 12
      )
    )
  ) %>%
  
  # CONFIGURACIÓN PLOTLY
  config(
    responsive = TRUE,
    displayModeBar = TRUE,
    displaylogo = FALSE,
    modeBarButtonsToRemove = list('lasso3d', 'select3d'),
    toImageButtonOptions = list(
      format = "png",
      filename = "infraestructura_digital_3d",
      width = 1600,
      height = 1000,
      scale = 2
    )
  )

scatter_3d

```



Base_2022 <- read_csv("f36a5086-3311-4b1a-9f0c-bda5cd4718df_Series - Metadata.csv",
                      show_col_types = FALSE) %>%
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
  )%>%
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
                      "Early-demographic dividend", "Late-demographic dividend", "Luxembourg")) %>% 
  mutate(across(3:19, as.numeric)) %>% drop_na()
View(Base_2022)
