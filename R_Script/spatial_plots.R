
library(ggthemes)
library(zipcode)
data("zipcode")
zipcode$region = substr(zipcode$zip, 1, 1)

ggplot(entire.data %>% filter(as.numeric(DRZIP) >= 210, as.numeric(DRZIP) <= 99950, !is.na(DRZIP)) %>% 
         mutate(DRZIP = clean.zipcodes(DRZIP)) %>% left_join(zipcode, by = c("DRZIP" = "zip")), 
       aes(x = longitude, y = latitude, color = state)) +
  geom_polygon(data = map_data("state"), aes(long, lat, group = group), color = "white", inherit.aes = FALSE) +
  geom_jitter(size = .1) +
  theme_map() +
  theme(legend.position = "none")

ggplot(map_data("world"), aes(long, lat, group = group)) +
  geom_polygon() +
  geom_point(aes(x = longitude, y = latitude, color = state), data = entire.data %>% filter(as.numeric(DRZIP) >= 210, as.numeric(DRZIP) <= 99950) %>% 
               mutate(DRZIP = clean.zipcodes(DRZIP), !is.na(DRZIP)) %>% left_join(zipcode, by = c("DRZIP" = "zip")), shape = '.', inherit.aes = FALSE) +
  theme_map()


# -----------------------------------------------------------------------------------------------

# 
# library(sp)
# library(rgeos)
# library(rgdal)
# 
# # The single argument to this function, pointsDF, is a data.frame in which:
# #   - column 1 contains the longitude in degrees (negative in the US)
# #   - column 2 contains the latitude in degrees
# 
# latlong2state <- function(pointsDF) {
#   states <-readOGR(dsn='cb_2015_us_state_20m',layer='cb_2015_us_state_20m')
#   states <- spTransform(states, CRS("+proj=longlat"))
#   
#   pointsSP <- SpatialPoints(pointsDF,proj4string=CRS("+proj=longlat"))
#   
#   # Use 'over' to get _indices_ of the Polygons object containing each point 
#   indices <- over(pointsSP, states)
#   indices$NAME
# }
