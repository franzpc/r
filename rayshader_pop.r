#install.packages("viridis")
#install.packages("viridisLite")
#install.packages("tidyverse")
#install.packages("sf")
#install.packages("rayshader")
#install.packages("magick")
#install.packages("av")

library(tidyverse)
library(sf)
library(viridis)
library(viridisLite)
library(rayshader)
library(viridis)
library(magick)
library(av)

shp = st_read(dsn = "D:/R/Rshader/ecu_c.shp")
shp = mutate(shp, Pop = habitantes)

outputFile <- file.path("D:/R/Rshader/", "pop.mp4")

ggShp = ggplot(data = shp) + 
  geom_sf(aes(fill = Pop)) + 
  scale_fill_viridis("Pop") +
  ggtitle("Población del Ecuador (continental)") +
  theme_bw()

plot_gg(ggShp,multicore=TRUE,width=3,height=3,scale=250,windowsize=c(1280,720),
        zoom = 0.65, phi = 50)
render_snapshot()
render_movie(filename = outputFile)
