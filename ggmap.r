### Instalar nuevas librerías
### install.packages("ggplot2")
### install.packages("ggmap")

library(tidyverse)
library(ggmap)
library(tibble)

register_google (key = "XXXXXXXXXX")

### Ejemplo

Londres <- geocode("London, UK", zoom = 13)
ggmap(get_map(Londres, maptype = "watercolor"))

### Ejemplo

Micasa <- get_map("ArcGeek", maptype = "roadmap", zoom = 15)
ggmap(Micasa)


lat = c(-4.008664, -4, -4.005)
lon = c(-79.210894, -79.22, -79.20)

Tabla = data.frame (cbind(lat,lon))

ggmap(Micasa) +
  geom_point(data = Tabla,
             aes(x = lon, y = lat),
             color = "red",
             size = 3,
             shape = 15) +
  ggtitle("Puntos de muestreo") +
  labs (x = "longitud", y = "latitud")
