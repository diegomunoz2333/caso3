asignar_continentes <- function(data) {
  continentes_map <- c(
    # AMÉRICA
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
    
    # EUROPA
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
    
    # ASIA
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
    
    # ÁFRICA
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
    
    # OCEANÍA
    "Australia" = "Oceania", "Fiji" = "Oceania", "Kiribati" = "Oceania",
    "Marshall Islands" = "Oceania", "Micronesia, Fed. Sts." = "Oceania",
    "New Zealand" = "Oceania", "Palau" = "Oceania", "Samoa" = "Oceania",
    "Solomon Islands" = "Oceania", "Tonga" = "Oceania", "Vanuatu" = "Oceania"
  )
  
  data %>%
    mutate(Continente = ifelse(Pais %in% names(continentes_map),
                               continentes_map[Pais], NA))
}

# ============================================================================
# 2. PALETA DE COLORES CYBER
# ============================================================================
paleta_continentes <- c(
  "Americas" = "#00e676",
  "Europe" = "#0084ff",
  "Asia" = "#ffd600",
  "Africa" = "#ff1744",
  "Oceania" = "#a259ff"
)

paleta_cyber <- c(
  primary = "#00e5ff",
  secondary = "#0084ff",
  accent = "#a259ff",
  warning = "#ff1744",
  success = "#00e676",
  gold = "#ffd600"
)

# ============================================================================
# 3. TEMA CYBER PARA GGPLOT2
# ============================================================================
tema_cyber <- function() {
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

# ============================================================================
# 4. FUNCIÓN PARA TABLAS CON ESTILO CYBER
# ============================================================================
tabla_cyber <- function(data, caption = "", col_names = NULL) {
  if (is.null(col_names)) {
    col_names <- names(data)
  }
  
  data %>%
    kbl(
      caption = caption,
      col.names = col_names,
      align = "c"
    ) %>%
    kable_styling(
      bootstrap_options = c("striped", "hover", "condensed"),
      full_width = FALSE,
      font_size = 14,
      position = "center"
    ) %>%
    row_spec(0, background = "#0084ff", color = "black", bold = TRUE) %>%
    row_spec(1:nrow(data), background = "#0a0e1a", color = "#e0e6ed")
}
