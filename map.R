require(ggplot2)
require(ggmap)
require(maps)
require(mapdata)

states <- map_data("state")

colnames(states) = c("long","lat","group","order","region","ref.num") 
states$ref.num=as.numeric(levels(states$ref.num))[states$ref.num]

for(x in 1:15537){
  if(states$region[x] %in% c("connecticut", "maine", "massachusetts", "new hampshire", "rhode island", "vermont")){
    states$ref.num[x] = 5.6
  } else if (states$region[x] %in% c("new jersey", "new york", "pennsylvania")){
    states$ref.num[x] = 15.4
  } else if (states$region[x] %in% c("illinois", "indiana", "michigan", "ohio",  "wisconsin")){
    states$ref.num[x] = 18.1
  } else if (states$region[x] %in% c("iowa", "kansas", "minnesota", "missouri", "nebraska", "north dakota",  "south dakota")){
    states$ref.num[x] = 8.3
  } else if (states$region[x] %in% c("delaware", "florida", "georgia", "maryland", "north carolina", "south carolina", "virginia", "district of columbia" , "west virginia")){
    states$ref.num[x] = 23.3
  } else if (states$region[x] %in% c("alabama", "kentucky", "mississippi", "tennessee")){
    states$ref.num[x] = 7.2
  } else if (states$region[x] %in% c("arkansas", "louisiana", "oklahoma","texas")){
    states$ref.num[x] = 13.5
  } else if (states$region[x] %in% c("arizona", "colorado", "idaho", "montana", "nevada", "new mexico", "utah","wyoming")){
    states$ref.num[x] = 8.5
  } else if (states$region[x] %in% c("alaska", "california", "hawaii", "oregon", "washington")){
    states$ref.num[x] = 17.9
  }
}



ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, fill = ref.num, group = group), color = "white") + 
  coord_fixed(1.3) +
  guides(fill=FALSE)

#########
state_base = ggplot(data = states, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(color = "black", fill = "gray")

state_bb2 = state_base +  geom_polygon(data = states, aes(fill = ref.num), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  theme_bw()+ scale_fill_gradientn(colours = rev(rainbow(7)), breaks = c(5, 10, 15, 20, 25, 30))
state_bb2
  