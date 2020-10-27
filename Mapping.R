# install.packages("sf")
# install.packages("raster")
# install.packages("spData")
# remotes::install_github("Nowosad/spDataLarge")


## install.packages("rgdal")
## install.packages(c("tmap", "tmaptools"))

library(tidyverse)
library(drat)
library(hurricaneexposuredata)
library(hurricaneexposure)
library(ggplot2)
addRepo("geanders")
data("hurr_tracks")
data("rain")
head(hurr_tracks)
head(rain)

# # tracking and rainfall but no data information
# p1 <- map_counties(storm = "Floyd-1999", metric = "rainfall") +
#   ggtitle("Floyd-1999") +
#   theme(plot.title = element_text(hjust = 0.5))

######################################################################
# usmap plot-11 Floyd-1999

library(dplyr)
rain_99 <- filter(rain,storm_id == "Floyd-1999")
rain_99 <- group_by(rain_99, fips)
rain_99 <- summarise(rain_99,sum_rain = sum(precip))
rain_99 <- as.data.frame(rain_99)
rain_99$rainfall <- NA
for (i in 1:dim(rain_99)[1]){
  rain_99$rainfall[i] <- rain_99$sum_rain[i]%/%25
}
rain_99$rainfall <- ordered(rain_99$rainfall,labels = c("[0.25]","(25,50]","(50,75]", "(75,100)","(100,125]","(125,150]","(150,175]","(175,200)","(200,222]"))

library(lubridate)
line_99 <- filter(hurr_tracks, storm_id == "Floyd-1999")
line_99 <-  separate(line_99, storm_id,c("id","year"),"-")
line_99$date <-  ymd_hm(line_99$date)
line_99 <- line_99[23:45,]

library(rgdal)
dt <- select(line_99,longitude, latitude)
data <- data.frame(
  lon = dt$longitude,
  lat = dt$latitude
)
dt <- usmap_transform(data)

library(maps) 


region <-  fips_info(rain_99$fips)
MainStates <- map_data("state",region = region$full)

MainStates <- data.frame(
  lon = MainStates$long,
  lat = MainStates$lat,
  group = MainStates$group,
  order = MainStates$order,
  region = MainStates$region
)
MainStates <- usmap_transform(MainStates)


library(usmap)
# plot_usmap("state") +
p11 <- plot_usmap(data = rain_99, values = "rainfall", color = "grey", include = rain_99$fips) +
  geom_polygon( data=MainStates, aes(x=lon.1, y=lat.1, group=group),
                color="black",  size = 0.05, alpha = 0) +
  scale_fill_brewer(palette = "Blues", name = "Rainfall(mm)") +
  labs(title = "Floyd-1999") +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5)) +
  theme(legend.position = "right")+
  geom_path(data = dt, aes(x = lon.1, y = lat.1),
            color = "red", size = 1)

#####################################################################################
# usmap plot-21 Allison-2001

# p2 <- map_rain_exposure(storm ="Allison-2001", 
#                   rain_limit = 175, 
#                   dist_limit = 500, 
#                   days_included =-5:3) +
#   ggtitle("Allison-2001") +
#   theme(plot.title = element_text(hjust = 0.5))

rain_01 <- filter(rain, storm_id == "Allison-2001")
rain_01 <- filter(rain_01, lag>-5 & lag <3)
rain_01 <- group_by(rain_01, fips)
rain_01 <- summarise(rain_01,sum_rain = sum(precip))
rain_01 <- as.data.frame(rain_01)
rain_01$rainfall <- NA

rain_limit <-  175

for (i in 1:dim(rain_01)[1]){
  if ( rain_01$sum_rain[i] < rain_limit) {
    rain_01$rainfall[i] <- 0
  }
  else rain_01$rainfall[i] <- 1
}

rain_01$rainfall <- ordered(rain_01$rainfall,labels = c("Unexposed","Exposed"))


line_01 <- filter(hurr_tracks, storm_id == "Allison-2001")
line_01 <-  separate(line_01, storm_id,c("id","year"),"-")
line_01$date <-  ymd_hm(line_01$date)
line_01 <- line_01[1:55,]

dt <- select(line_01,longitude, latitude)
data <- data.frame(
  lon = dt$longitude,
  lat = dt$latitude
)
dt <- usmap_transform(data)

region <-  fips_info(rain_01$fips)
MainStates <- map_data("state",region = region$full)

MainStates <- data.frame(
  lon = MainStates$long,
  lat = MainStates$lat,
  group = MainStates$group,
  order = MainStates$order,
  region = MainStates$region
)
MainStates <- usmap_transform(MainStates)

p21 <- plot_usmap(data = rain_01, values = "rainfall", color = "grey", include = rain_01$fips) +
  geom_polygon( data=MainStates, aes(x=lon.1, y=lat.1, group=group),
                color="black",  size = 0.05, alpha = 0) +
  scale_fill_brewer(palette = "Blues", name = "Rainfall > 175mm") +
  labs(title = "Allison-2001") +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5)) +
  theme(legend.position = "right")+
  geom_path(data = dt, aes(x = lon.1, y = lat.1),
            color = "red", size = 1)
p21

#################################################################################
# tmap plot-12 Floyd-1999

# library(tmap) 
# library(tmaptools)
# 
# library(sf)
# library(raster)
# 
# library(spData)

library(leaflet) 

dns <- "gadm36_USA_shp" 
fn <- list.files(dns, pattern=".shp", full.names=FALSE) 
fn <- gsub(".shp", "", fn) 
shape <- readOGR(dns, "gadm36_USA_2") 

# rain data
rain_99$cty <- region$county
rain_99 <- separate(rain_99,cty,c("county","ct"),sep = " County")
# merge data
shape <- merge(shape, rain_99, by.x="NAME_2", by.y="county",duplicateGeoms = TRUE) 

bins <- c(0, 25, 50, 75, 100, 125, 150, 175, 200, 250)
pal <- colorBin("YlGnBu", domain = shape$sum_rain, bins = bins) 

# mapping
m <- leaflet(shape) %>% addTiles() %>% setView(-89, 37.597042, zoom = 4) 

m %>% 
  addPolygons(fillColor = ~pal(shape$sum_rain), 
              weight = 2,
              opacity = 1,
              color = "grey",
              dashArray = "3",
              fillOpacity = 0.7) 
