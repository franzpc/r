Instalar librerías
#install.packages("viridis")
#install.packages("viridisLite")
#install.packages("tidyverse")
#install.packages("sf")
#install.packages("rayshader")
#install.packages("magick")
#install.packages("av")

Activar librerías
library(tidyverse)
library(sf)
library(viridis)
library(viridisLite)
library(rayshader)
library(magick)
library(av)

### Importar Shapefile
shp = st_read(dsn = "D:/R/rayshader_ggplot/ecu.shp")
print(shp)
shp = mutate(shp, Pop = COV)

### Dibujar
### scale_color_viridis()
### scale_fill_viridis()

ggShp = ggplot(data = shp) + 
  geom_sf(aes(fill = Pop)) + 
  scale_color_viridis() +
  ggtitle("Población del Ecuador") +
  theme_bw()

plot_gg(ggShp,multicore=TRUE,width=5,height=5,scale=300,windowsize=c(1280,720),
        zoom = 0.65, phi = 50)
render_snapshot()

### Crear un vídeo
Video <- file.path("D:/R/rayshader_ggplot/", "pop.mp4")
render_movie(filename = Video)
