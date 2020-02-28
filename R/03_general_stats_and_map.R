# Script to analyze data of female representativity in science
# analyzing cited women and making map
# by Sara Mortara, first version 2020-02-28

## loading packages
library(ggplot2)
library(dplyr)
library(wesanderson)
library(maps)
library(rgdal)
library(raster)

## color pallete for graphics
cores <-  wes_palette("Darjeeling1", 2)[2:1]
ver <- rgb(255, 0, 0, maxColorValue = 255, alpha = 100)
cin <- rgb(106, 126, 133, maxColorValue = 255)

## reading file
fall <- read.csv("outputs/02_all_women.csv")
coord <- read.csv("data/coordinates.csv")

# saving world map into an object
map <- map_data("world")

# merging continent w/ count of citations per continent
cont <- as.data.frame(table(fall$continente))
names(cont) <- c("continente", "n")
cont.coord <- merge(cont, coord, by = "continente")
n <- 10
cont.coord$long2 <- cont.coord$long - n
cont.coord$lat2 <- cont.coord$lat - n

mapa <- ggplot() +
  geom_polygon(data = map, aes(x = long, y = lat, group = group),
               fill = "grey", alpha = 0.5) +
  geom_point(data = cont.coord,
             aes(x = long, y = lat, size = n*1000),
             color = "forestgreen", alpha = 0.5) +
  annotate("text", x = cont.coord$long2, y = cont.coord$lat2,
           label = cont.coord$n,
           size = 3) +
  geom_segment(data = cont.coord,
               aes(x = long, y = lat, xend = long2 + 3, yend = lat2 + 3),
               alpha = 0.5) +
  #scale_size_continuous(range = c(1, 12)) +
  #scale_color_viridis(trans = "log") +
  theme_void() +
  theme(legend.position = "none")

png("figs/mapa.png",
    res = 300,
    width = 1200,
    height = 600)
mapa
dev.off()

head(fall)

table(fall$cor)/nrow(fall)
