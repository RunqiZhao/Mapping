# install.packages("sf")
# install.packages("raster")
# install.packages("spData")
# install.packages("rgdal")

library(tidyverse)
library(drat)
library(hurricaneexposuredata)
library(hurricaneexposure)
library(maps)
library(ggplot2)
library(dplyr)
library(lubridate)
library(rgdal)
library(geojsonio)


library(usmap)
library(leaflet)


addRepo("geanders")
data("hurr_tracks")
data("rain")
 head(hurr_tracks)
 head(rain)

# example plot for Floyd-1999 and Allison-2001
p1 <- map_counties(storm = "Floyd-1999", metric = "rainfall") +
  ggtitle("Floyd-1999") +
  theme(plot.title = element_text(hjust = 0.5))

p2 <- map_rain_exposure(storm ="Allison-2001",
                        rain_limit = 175,
                        dist_limit = 500,
                        days_included =-5:3) +
  ggtitle("Allison-2001") +
  theme(plot.title = element_text(hjust = 0.5))

######################################################################
# usmap plot-11 Floyd-1999

# rain data for Floyd-1999
rain_99 <- filter(rain,storm_id == "Floyd-1999")
rain_99 <- group_by(rain_99, fips)
rain_99 <- summarise(rain_99,sum_rain = sum(precip))
rain_99 <- as.data.frame(rain_99)
rain_99$rainfall <- NA
for (i in 1:dim(rain_99)[1]){
  rain_99$rainfall[i] <- rain_99$sum_rain[i]%/%25
}
rain_99$rainfall <- ordered(rain_99$rainfall,labels = c("[0,25]","(25,50]","(50,75]", "(75,100)","(100,125]","(125,150]","(150,175]","(175,200)","(200,222]"))

# line data for Floyd-1999
line_99 <- filter(hurr_tracks, storm_id == "Floyd-1999")
line_99 <-  separate(line_99, storm_id,c("id","year"),"-")
line_99$date <-  ymd_hm(line_99$date)
line_99 <- line_99[23:45,]

dt <- select(line_99,longitude, latitude)
data <- data.frame(
  lon = dt$longitude,
  lat = dt$latitude
)
dt <- usmap_transform(data)
dt99 <- dt

# State boundaries data
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
region99 <- region


p11 <- plot_usmap(data = rain_99, values = "rainfall", color = "grey", include = rain_99$fips) +
  geom_polygon( data=MainStates, aes(x=lon.1, y=lat.1, group=group),
                color="black",  size = 0.05, alpha = 0) +
  scale_fill_brewer(palette = "Blues", name = "Rainfall(mm)") +
  labs(title = "Floyd-1999") +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5)) +
  theme(legend.position = "right")+
  geom_path(data = dt, aes(x = lon.1, y = lat.1),
            color = "red", size = 1)
p11

#####################################################################################
# usmap plot-21 Allison-2001

# rain data for Allison-2001
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

# line data for Allison-2001
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
dt01 <- dt

# State boundaries data
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
region01 <- region

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
# leaflet plot-12 Floyd-1999

# rain data
rain_99$cty <- region99$county
rain_99 <- separate(rain_99,cty,c("county","ct"),sep = " County")
rain_99$state <- region99$full

# load shape data file
dns <- "gadm36_USA_shp" 
fn <- list.files(dns, pattern=".shp", full.names=FALSE) 
fn <- gsub(".shp", "", fn) 
shape <- readOGR(dns, "gadm36_USA_2") 

# merge rain data and shape data
rows <- which(shape@data$NAME_2 %in% rain_99$county)
data1 <- shape@data[rows,]
poly <- shape@polygons[rows]
po <- shape@plotOrder[rows]
shape@data <- data1
shape@polygons <- poly
shape@plotOrder <- po
data1$rainfall <- NA
for (i in 1:dim(data1)[1]){
  row <- which(rain_99$county == data1$NAME_2[i])
  if(rain_99$state[row] == data1$NAME_1[i]){
    data1$rainfall[i] <- rain_99$rainfall[row]
  }
  
}
shape@data <- data1

rows <- which(shape@data$rainfall != "NA")
data3 <- shape@data[rows,]
poly <- shape@polygons[rows]
po <- shape@plotOrder[rows]
shape@data <- data3
shape@polygons <- poly
shape@plotOrder <- po

data3$rainfall <- ordered(data3$rainfall,labels = c("[0,25]","(25,50]","(50,75]", "(75,100)","(100,125]","(125,150]","(150,175]","(175,200)","(200,222]"))
shape@data <- data3

# plot p12

factpal <- colorFactor("RdPu", shape$rainfall)
i_popup <- paste0("<strong>county: </strong>", shape$NAME_2, "<br>", "<strong>rainfall: </strong>", shape$rainfall) 
p12 <- leaflet(data = rain_99) %>% 
  addProviderTiles("CartoDB.Positron") %>%
  addProviderTiles(providers$Stamen.TonerLines,
                   options = providerTileOptions(opacity = 0.75)) %>%
  setView(-89.275673, 37.098, zoom = 4) %>%
  addPolygons(data = shape,
              color = ~factpal(shape$rainfall), 
              fillOpacity  = 0.5, 
              smoothFactor = 0.1,
              weight = 1,
              # stroke = FALSE,
              popup = i_popup) %>%
  addPolylines(data = dt99,~lon, ~lat, 
               color="darkturquoise",
               weight = 1.5)%>%
  addLegend(pal = factpal,
            values = shape$rainfall,
            position="bottomright",
            title = "Rainfall")
p12

#################################################################################

# leaflet plot-22 Allison-2001
# rain data
rain_01$cty <- region01$county
rain_01 <- separate(rain_01,cty,c("county","ct"),sep = " County")
rain_01$state <- region01$full

# load shape data file
shape2 <- readOGR(dns, "gadm36_USA_2") 

# merge rain data and shape data
rows <- which(shape2@data$NAME_2 %in% rain_01$county)
data1 <- shape2@data[rows,]
poly <- shape2@polygons[rows]
po <- shape2@plotOrder[rows]
shape2@data <- data1
shape2@polygons <- poly
shape2@plotOrder <- po
data1$rainfall <- NA
for (i in 1:dim(data1)[1]){
  row <- which(rain_01$county == data1$NAME_2[i])
  # if(rain_01$state[row] == data1$NAME_1[i]){
    data1$rainfall[i] <- rain_01$rainfall[row]
  # }
  
}
shape2@data <- data1

rows <- which(shape2@data$rainfall != "NA")
data3 <- shape2@data[rows,]
poly <- shape2@polygons[rows]
po <- shape2@plotOrder[rows]
shape2@data <- data3
shape2@polygons <- poly
shape2@plotOrder <- po

data3$rainfall <- ordered(data3$rainfall,labels = c("Unexposed","Exposed"))
shape2@data <- data3

pal <- colorFactor("RdPu", rain_01$rainfall)
i_popup2 <- paste0("<strong>county: </strong>", shape2$NAME_2, "<br>", "<strong>rainfall: </strong>", shape2$rainfall) 

# plot p22
# p22<- leaflet(data = rain_01) %>%
#       addProviderTiles("CartoDB.Positron") %>%
#       addProviderTiles(providers$Stamen.TonerLines,
#                         options = providerTileOptions(opacity = 0.75)) %>%
#       setView(-89.275673, 37.098, zoom = 4) %>%
#       addPolygons(data = shape2,
#                   color = ~pal(shape2$rainfall),
#                   fillOpacity  = 0.5,
#                   smoothFactor = 0.1,
#                   weight = 1,
#                   # stroke = FALSE
#                   popup = i_popup2) %>%
#       addPolylines(data = dt01, ~lon, ~lat,
#                     color="darkturquoise",
#                     weight = 1.5)%>%
#       addLegend(pal = pal,
#                 values = shape2$rainfall,
#                 position="bottomright",
#                 title = "Rainfall")
# 
# p22

pal <- colorFactor("RdPu", rain_01$rainfall)

mapCounty = map("county", region = region$full, fill = TRUE, plot = FALSE)

p22<- leaflet(data = rain_01) %>% 
  addProviderTiles("CartoDB.Positron") %>%
  addProviderTiles(providers$Stamen.TonerLines,
                   options = providerTileOptions(opacity = 0.75)) %>%
  setView(-89.275673, 37.098, zoom = 4) %>%
  addPolygons(data = mapCounty,
              color = ~pal(rain_01$rainfall), 
              fillOpacity  = 0.5, 
              smoothFactor = 0.1,
              weight = 1,
              stroke = FALSE) %>%
  addPolylines(data = dt01,~lon, ~lat, 
               color="darkturquoise",
               weight = 1.5)%>%
  addLegend(pal = pal,
            values = rain_01$rainfall,
            position="bottomright",
            title = "Rainfall")

p22
