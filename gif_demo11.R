rm(list=ls())
setwd("C:/Users/firat/Desktop/maptrial")
library("foreign")
library("rgdal")
#install.packages("rgeos")
library("rgeos")
#install.packages("sp")
library("sp")
library("tmap")
library("leaflet")
library("RColorBrewer")
library(ggplot2)
install.packages('tidyverse')

#library(foreign)
#devtools::install_github('thomasp85/gganimate')
#installed.packages("gganimate")
#library(gapminder)


#gedata <-read.dbf("ge.dbf", as.is = FALSE)
gedata <-read.dbf("gepanel.dbf", as.is = FALSE)
head(gedata)
ge <- readOGR(".", "ge")
head(gedata)

library(dplyr)
gedata <- read.dbf("gepanel.dbf", as.is = FALSE)
ge_shp <- st_read("ge.shp")
ge_shp <- st_transform(ge_shp, proj4string = CRS("+init=EPSG:5637"))
joined <- st_as_sf(right_join(gedata, ge_shp, by = "PLAKA"))

tm_shape(ge) + tm_borders(alpha=.4) +
  tm_shape(ge) + tm_dots(col = "AKP16", palette = "Reds", style = "quantile")


akp07a <- tm_shape(ge) + 
  tm_fill("AKP07", n = 4, palette = "Reds", style = "equal", title = "%AKP07") + 
  tm_borders(alpha=.9)  + tm_shape(ge) + 
  tm_bubbles(size = "CONS07",n = 4, col = "CONS07", 
             palette = "Blues", style = "equal", 
             legend.size.show = FALSE, title.col = "%CONS07",
             scale = 2) +
  tm_layout(panel.show = TRUE, panel.labels=c("2007 Elections"), 
            legend.title.size =2,
            frame = FALSE, inner.margins = c(0.,.2,0.2,.02), asp = 2,
            legend.text.size = 1.7, 
            legend.outside=TRUE, legend.outside.position = 'bottom',
            legend.frame = FALSE,
            legend.outside.size = .4, 
            legend.position = c(0.0001, 0.81)) 

akp11a <- tm_shape(ge) + 
  tm_fill("AKP11", n = 4, palette = "Reds", style = "equal", title = "%AKP11") + 
  tm_borders(alpha=.9)  + tm_shape(ge) + 
  tm_bubbles(size = "CONS11",n = 4, col = "CONS11", 
             palette = "Blues", style = "equal", 
             legend.size.show = FALSE, title.col = "%CONS11",
             scale = 2) +
  tm_layout(panel.show = TRUE, panel.labels=c("2011 Elections"), 
            legend.title.size = 2,
            frame = FALSE, inner.margins = c(0.,.2,0.2,.02), asp = 2,
            legend.text.size = 1.7, 
            legend.outside=TRUE, legend.outside.position = 'bottom',
            legend.frame = FALSE,
            legend.outside.size = .4, 
            legend.position = c(0.0001, 0.81)) 

akp15a <- tm_shape(ge) + 
  tm_fill("AKP15", n = 4, palette = "Reds", style = "equal", title = "%AKP15J") + 
  tm_borders(alpha=.9)  + tm_shape(ge) + 
  tm_bubbles(size = "CONS15",n = 4, col = "CONS15", 
             palette = "Blues", style = "equal", 
             legend.size.show = FALSE, title.col = "%CONS15J",
             scale = 2) +
  tm_layout(panel.show = TRUE, panel.labels=c("2015 June Elections"), 
            legend.title.size = 2,
            frame = FALSE, inner.margins = c(0.,.2,0.2,.02), asp = 2,
            legend.text.size = 1.7, 
            legend.outside=TRUE, legend.outside.position = 'bottom',
            legend.frame = FALSE,
            legend.outside.size = .4, 
            legend.position = c(0.0001, 0.81)) 

akp16a <- tm_shape(ge) + 
  tm_fill("AKP16", n = 4, palette = "Reds", style = "equal", title = "%AKP15N") + 
  tm_borders(alpha=.9)  + tm_shape(ge) + 
  tm_bubbles(size = "CONS16",n = 4, col = "CONS16", 
             palette = "Blues", style = "equal", 
             legend.size.show = FALSE, title.col = "%CONS15N",
             scale = 2) +
  tm_facets(along = "year", free.coords = FALSE)+
  tm_layout(panel.show = TRUE, panel.labels=c("2015 November Elections"), 
            legend.title.size = 2,
            frame = FALSE, inner.margins = c(0.,.2,0.2,.02), asp = 2,
            legend.text.size = 1.7, 
            legend.outside=TRUE, legend.outside.position = 'bottom',
            legend.frame = FALSE,
            legend.outside.size = .4, 
            legend.position = c(0.0001, 0.81)) 


akp_anim = tm_polygons() + tm_shape(ge) +
  tm_shape(col = "AKP07")+tm_shape(col = "AKP11")+tm_shape(col = "AKP15J")+tm_shape(col ="AKP15N") + 
  tm_facets(along = "col", free.coords = FALSE)

tm_shape(ge) + tm_borders(alpha=.4) +
  tm_shape(ge) + tm_dots(col = "AKP16", palette = "Reds", style = "quantile")

akp_anim = tm_polygons() + tm_shape(ge) +
  tm_dots(size = "AKP",n = 4, col = "AKP", 
          palette = "Blues", style = "equal")+
  tm_dots(size = "CONS",n = 4, col = "CONS", 
          palette = "gray", style = "equal")+
  tm_facets(along = "TIME", free.coords = FALSE)

akp_anim = tm_polygons() + tm_shape(ge) +
  tm_shape(size = "AKP",n = 4, col = "AKP", 
           palette = "Blues", style = "equal")+
  tm_dots(size = "CONS",n = 4, col = "CONS", 
          palette = "gray", style = "equal")+
  tm_facets(along = "TIME", free.coords = FALSE)

tmap_animation(akp_anim, filename = "akp_anim.gif", loop = TRUE, delay = 125)

akp_anim = 
  tm_shape(joined) + tm_polygons() + 
  tm_shape(joined) + tm_dots(size = "AKP") +
  tm_facets(along = "TIME", free.coords = FALSE)

tmap_animation(akp_anim, filename = "akp_anim.gif", loop = TRUE, delay = 125)

akp_anim1 = 
  tm_shape(joined) + tm_polygons() + tm_borders(alpha=1.9) +
  tm_shape(joined) + tm_fill("AKP", n = 5, palette = "Reds", style = "equal")+
  tm_bubbles(size = "CONS",n = 5, palette = "Blues", style = "equal")+
  tm_facets(along = "TIME", free.coords = FALSE)+
  tm_layout(panel.show = TRUE, panel.labels=c("AKP vs CONSERVATISM"), 
            legend.outside=TRUE, legend.outside.position = 'bottom',
            legend.frame = FALSE)
#    legend.outside.size = .4, 
#   legend.position = c(0.0001, 0.81)) 

tmap_animation(akp_anim1, filename = "akp_anim1.gif", loop = TRUE, delay = 50)

hdp_anim1 = 
  tm_shape(joined) + tm_polygons() + tm_borders(alpha=1.9) +
  tm_shape(joined) + tm_fill("HDP", n = 5, palette = "Purples", style = "equal")+
  tm_bubbles(size = "KURD",n = 5, brewer.pal(3, "OrRd"), style = "equal")+
  tm_facets(along = "TIME", free.coords = FALSE)

tmap_animation(hdp_anim1, filename = "hdp_anim1.gif", loop = TRUE, delay = 80)
#################htryguhjikolp;



#ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, colour = country)) +
geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~continent) +
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  ease_aes('linear')  
