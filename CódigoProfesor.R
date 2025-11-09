# Clusters



library(tidyverse)
jugadores <- tibble(x = c(-1, -2, 8, 7, -12, -15, -13, 15, 21, 12, -25, 26),
                    y = c(1, -3, 6, -8, 8, 0, -10, 16, 2, -15, 1, 0)
)
jugadores %>% 
  ggplot() +
  aes(x, y) +
  geom_point(size = 5)


modelo_kmeans <- kmeans(jugadores, centers = 2)


# Imprimimos las coordenadas de los centros
modelo_kmeans$centers
#>           x          y
#> 1 -11.33333 -0.5000000
#> 2  14.83333  0.1666667

equipo <- modelo_kmeans$cluster


# Agregamos la columna del cluster
jugadores_agrupados <- jugadores %>% 
  mutate(cluster = equipo)



# Visualizamos los jugadores de acuerdo a la agrupación
jugadores_agrupados %>% 
  ggplot() +
  aes(x, y, fill = factor(cluster)) +
  geom_point(size = 9, pch = 21) +
  scale_fill_manual(values=c("#EE220D", "green")) +
  theme(legend.position = "none")


# Agrupamiento k >= 3
modelo_kmeans <- kmeans(jugadores, centers = 5)
equipo <- modelo_kmeans$cluster
jugadores_agrupados <- jugadores %>% 
  mutate(cluster = equipo)
jugadores_agrupados %>% 
  ggplot() +
  aes(x, y, color = factor(cluster)) +
  geom_point(size = 5) +
  theme(legend.position = "none")


##########################################################################
library(factoextra)
fviz_nbclust(jugadores, FUN = kmeans, method = "wss")
fviz_nbclust(jugadores, FUN = kmeans, method = "silhouette")
##########
url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/00292/Wholesale%20customers%20data.csv"
clientes <- read_csv(url)
clientes

clientes_filtrado <- clientes %>% 
  select(Milk, Grocery, Frozen)


fviz_nbclust(clientes_filtrado, FUN = kmeans, method = "silhouette")

modelo <- kmeans(clientes_filtrado, centers = 2)

clientes_agrupados <- clientes_filtrado %>% 
  mutate(cluster = modelo$cluster)


clientes_agrupados


clientes_agrupados %>% 
  group_by(cluster) %>% 
  summarise(total = n(), 
            media_Milk = mean(Milk), 
            media_Grocery = mean(Grocery),
            media_Frozen = mean(Frozen))



library(reshape2)
data_long<- melt(clientes_agrupados, id = "cluster")
#Primer grafico
ggplot(data_long,aes(y= value, x= variable)) +
  geom_boxplot()
#Segundo Grafico
ggplot(data_long,aes(y= value, x= variable, color = as.factor(cluster))) +
  geom_boxplot()


##################################################################
#Metodo de Ward o Jerarquico
library(tidyverse)
jugadores <- tibble(x = c(-1, -2, 8, 7, -12, -15, -13, 15, 21, 12, -25, 26),
                    y = c(1, -3, 6, -8, 8, 0, -10, 16, 2, -15, 1, 0)
)
dist_entre_jugadores <- dist(jugadores)
modelo_jerarquico <- hclust(dist_entre_jugadores)


library(dendextend)
dend_modelo <- as.dendrogram(modelo_jerarquico)
plot(dend_modelo)
modelo_jerarquico
corte <- 28
dend_modelo %>% 
  color_branches(h = corte) %>% 
  color_labels(h = corte) %>% 
  plot() %>% 
  abline(h = corte, lty = 2)
clusters_deseados <- 6
dend_modelo %>% 
  color_branches(k = clusters_deseados) %>% 
  color_labels(k = clusters_deseados) %>% 
  plot()
library(factoextra)
fviz_nbclust(jugadores, FUN = hcut, method = "silhouette")







##################################################################
# Reducir la dimensionalidad
# ACP
##PCA:
##  library(stats)
##• prcomp() -> Forma rápida de implementar PCA sobre una matriz de datos.
##• princomp()
##library(FactoMineR)
##• PCA() -> PCA con resultados más detallados. Los valores ausentes se reemplazan por la media de cada columna. Pueden incluirse variables categóricas suplementarias. Estandariza automáticamente los datos.
##library(factoextra)
##• get_pca() -> Extrae la información sobre las observaciones y variables de un análisis PCA.
##• get_pca_var() -> Extrae la información sobre las variables.
##• get_pca_ind() -> Extrae la información sobre las observaciones.
##Visualizaciones:
##  library(FactoMineR)
##• fviz_pca_ind() -> Representación de observaciones sobre componentes principales.
##• fviz_pca_var() -> Representación de variables sobre componentes principales.
##• fviz_screeplot() -> Representación (gráfico barras) de eigenvalores.
##• fviz_contrib() -> Representa la contribución de filas/columnas de los resultados de un pca.

library(factoextra)
library(tidyverse)

data(decathlon2)

View(decathlon2)



decathlon2.train <- decathlon2[1:23, 1:10]


head(decathlon2.train[, 1:6])

res.pca <- prcomp(decathlon2.train, scale = TRUE)

res.pca


fviz_eig(res.pca)



fviz_pca_ind(res.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE
)


fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             axes = c(1,3)# Avoid text overlapping
)

fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969",
                axes=c(1,2)# Individuals color
)




## Acceder a los resultados
library(factoextra)


# Eigenvalues
eig.val <- get_eigenvalue(res.pca)
eig.val


# Resultados para Variables
res.var <- get_pca_var(res.pca)
res.var$coord          # Coordinates
res.var$contrib        # Contributions to the PCs
res.var$cos2           # Quality of representation 

View(res.var$contrib[,1:7]) # Miro los dos primeros factores
colSums( res.var$contrib[,1:2] )


# Results for individuals
res.ind <- get_pca_ind(res.pca)
res.ind$coord          # Coordinates
res.ind$contrib        # Contributions to the PCs
res.ind$cos2           # Quality of representation 
View(res.ind$contrib[,1:3]) # Miro los dos primeros factores
res.ind$contrib[,1:2]


# Utilizo un conjunto para predecir
ind.test <- decathlon2[24:27, 1:10]
head(ind.test)
ind.test.coord <- predict(res.pca, newdata = ind.test)
ind.test.coord[, 1:2] # Miro los primeros dos componentes
ind.test.coord


# Utilizo el gráfico anterior y le anexo los nuevos individuos
p <- fviz_pca_ind(res.pca, repel = TRUE)
fviz_add(p, ind.test.coord, color ="blue")



##############################################################################
##############################################################################
# Factoclass




library(FactoClass)

resultado_ACP<-FactoClass(decathlon2.train,dudi.pca)
3
3
4


resultado_ACP$cluster


NuevaBase<-data.frame(Cluster=resultado_ACP$cluster,decathlon2.train)  # unir base de datos con la nueva variable cluster

View(NuevaBase)



#Gráficos
plot(resultado_ACP$dudi) # Grafico del análisis

s.corcircle((resultado_ACP$dudi)$co)    

s.label((resultado_ACP$dudi)$li,label=row.names(decathlon2.train)) #Graf. Individuos

s.label((resultado_ACP$dudi)$co,xax=1,yax=2,sub="Componente 1 y 2",possub="bottomright") #Graf. Variables

scatter(resultado_ACP$dudi,xax=1,yax=2) # Graf. Conjuntos



Grupo<-NuevaBase$Cluster
s.class((resultado_ACP$dudi)$li,Grupo,sub="Componentes 1 y 2",possub="bottomright",xax=1,yax=3,col=c(1,2,3,4))



#Descripción de los grupos (Análisis de medias)
resultado_ACP$carac.cont
