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
datos_analisis <- Base_2022_filtrada %>%
  filter(Pais != "Luxembourg") %>% 
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

ggplot(varianza_df, aes(x = as.numeric(Componente), y = Varianza)) +
  geom_col(fill = "#2E86AB", alpha = 0.8) +
  geom_line(aes(y = VarianzaAcum, group = 1), color = "#E74C3C", linewidth = 1.2) +
  geom_point(aes(y = VarianzaAcum), color = "#E74C3C", size = 2.5) +
  geom_hline(yintercept = 80, linetype = "dashed", color = "darkgreen", linewidth = 1) +
  geom_vline(xintercept = 8, linetype = "dotted", color = "#F39C12", linewidth = 1.1) +
  annotate("text", x = 8.3, y = max(varianza_df$Varianza) * 0.9,
           label = "Componente 8", color = "#F39C12", angle = 90, hjust = 0) +
  annotate("text", x = 1.5, y = 82,
           label = "80% varianza acumulada", color = "darkgreen", hjust = 0) +
  scale_x_continuous(breaks = 1:length(acp_resultado$eig)) +
  labs(
    title = "Scree Plot — Varianza Explicada por Componente Principal",
    x = "Componentes Principales",
    y = "% de Varianza Explicada"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
    axis.text.x = element_text(angle = 45, vjust = 0.8),
    panel.grid.minor = element_blank()
  )

# Círculo de correlaciones

vars_df <- as.data.frame(acp_resultado$co)
vars_df$Variable <- rownames(vars_df)

# Data frame para el círculo
vars_df <- as.data.frame(acp_resultado$co)
vars_df$Variable <- rownames(vars_df)

# crear puntos para el círculo unitario (usando 100 puntos)
theta <- seq(0, 2*pi, length.out = 200)
circle_df <- data.frame(
  x = cos(theta),
  y = sin(theta)
)
max_coord <- max(1, max(abs(vars_df$Comp1), abs(vars_df$Comp2)))
lims <- c(-max_coord * 1.05, max_coord * 1.05) 
ggplot() +geom_path(data = circle_df, aes(x = x, y = y), color = "gray60", linetype = "dashed", size = 0.6) +
  geom_hline(yintercept = 0, color = "gray85", size = 0.4) +
  geom_vline(xintercept = 0, color = "gray85", size = 0.4) +
  geom_segment(data = vars_df,
               aes(x = 0, y = 0, xend = Comp1, yend = Comp2),
               color = "#2E86AB", alpha = 0.8,
               arrow = grid::arrow(length = unit(0.18, "cm"), type = "closed"),
               size = 0.8) + geom_point(data = vars_df, aes(x = Comp1, y = Comp2), color = "#E74C3C", size = 2.6) +
  geom_text_repel(data = vars_df, aes(x = Comp1, y = Comp2, label = Variable),
                  size = 3.8, max.overlaps = 30, segment.alpha = 0.5) +
  coord_equal(xlim = lims, ylim = lims) +
  labs(
    title = "Círculo de correlaciones — Variables (PC1 vs PC2)",
    x = "Componente Principal 1",
    y = "Componente Principal 2",
    subtitle = "Vectores muestran correlación de variables con los primeros dos ejes"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray95")
  )

#  Países en el espacio factorial
paises_df <- as.data.frame(acp_resultado$li)
paises_df$Pais <- rownames(paises_df)

# Calcular distancia al origen (para resaltar países más "extremos")
paises_df$Distancia <- sqrt(paises_df$Axis1^2 + paises_df$Axis2^2)

ggplot(paises_df, aes(x = Axis1, y = Axis2)) +
  geom_point(aes(color = Distancia), size = 3.2, alpha = 0.85) +
  geom_text_repel(
    aes(label = Pais),
    size = 3.3,
    color = "gray15",
    max.overlaps = 20,
    segment.color = "gray70",
    segment.size = 0.3
  ) +
  geom_hline(yintercept = 0, color = "gray75", linetype = "dashed", linewidth = 0.5) +
  geom_vline(xintercept = 0, color = "gray75", linetype = "dashed", linewidth = 0.5) +
  scale_color_gradient(low = "#7FB3D5", high = "#1B4F72", name = "Distancia\nal origen") +
  labs(
    title = "Países en el Espacio Factorial (ACP)",
    subtitle = "Representación de los países según los dos primeros componentes principales",
    x = "Componente Principal 1",
    y = "Componente Principal 2"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 15),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90"),
    legend.position = "right",
    legend.title = element_text(face = "bold")
  )


##tabla de variaanza
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

#  CLUSTERING SOBRE LOS FACTORES########################################3
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

# Gráfico de países en espacio de factores
s.class(factores, as.factor(clusters), sub = "Clusters en el espacio factorial", possub = "bottomright")

# Dendrograma
plot(arbol, labels = FALSE, main = "Dendrograma (método de Ward)", xlab = "", sub = "")
rect.hclust(arbol, k = k_optimo, border = 2:5)

