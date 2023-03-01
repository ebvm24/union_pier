## The following code is used to generate the data that was used in the
## 2023-02-27 blogpost.


library(tidyverse)
library(sp)
library(sf)
library(leaflet)

## -----------------------------------------------------------------------------
##
## Import GeoJson Files from City of Charleston Open Data GIS
##
## -----------------------------------------------------------------------------

census_blocks = st_read("https://services2.arcgis.com/tQaXW7Zb1Vphzvgd/arcgis/rest/services/Census_Blocks_2020_-_City_of_Charleston/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")
neighborhoods = st_read("https://services2.arcgis.com/tQaXW7Zb1Vphzvgd/arcgis/rest/services/Neighborhood_Councils/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")

## Keep only columns we need as this is a large file.
census_blocks = census_blocks[c("FID","STATEFP20","COUNTYFP20","TRACTCE20","BLOCKCE20","GEOID20","NAME20","P0010001","geometry")]

## -----------------------------------------------------------------------------
##
## Create intersection between Census Blocks and Neighborhood Association Boundaries
##
## -----------------------------------------------------------------------------

x <-  st_intersects(census_blocks,neighborhoods,sparse=F)
x <- sapply(1:nrow(x), function(u) which(x[u,])[1])

census_blocks$Neighborhood <-  neighborhoods$NAME[x]

x1 <- census_blocks %>%
  group_by(Neighborhood) %>%
  summarize(Population = sum(P0010001)) %>%
  ungroup() %>%
  as_tibble() %>%
  select(-geometry)

neighborhoods1 <- neighborhoods %>%
  left_join(x1,by = c("NAME"="Neighborhood")) %>%
  mutate(Acres = Shape__Area*0.000247105,
         Density = Population/Acres) %>%
  filter(Population > 0)

## -----------------------------------------------------------------------------
##
## Create Neighborhoods Table
##
## -----------------------------------------------------------------------------

neighborhoods1 %>%
  as_tibble() %>%
  mutate(SECTOR = as.integer(SECTOR)) %>%
  mutate(across(c(Acres, Density), round, digits = 1)) %>%
  select(NAME,STATUS,SECTOR,Population,Acres,Density)

## -----------------------------------------------------------------------------
##
## Create Overlay Map
##
## -----------------------------------------------------------------------------

width_pal <- colorBin("viridis", bins = c(0,5,10,20,40,80,160))

labels <- sprintf(
  "<b>Neighborhood:</b> %s<br/>
    <b>Population:</b> %g <br/>
    <b>Acres:</b> %g <br/>
    <b>Density (People/Acre):</b> %g",
  neighborhoods1$NAME,
  round(neighborhoods1$Population,1),
  round(neighborhoods1$Acres,1),
  round(neighborhoods1$Density,1)
) %>% lapply(htmltools::HTML)


m <- leaflet(neighborhoods1,width="100%") %>%
  setView(lat = 32.7765, lng = -79.9311, zoom=12) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(label = labels,
              labelOptions = labelOptions(
                textsize = "15px",
                direction = "auto"),
              weight=2,
              color="white",
              opacity =1,
              dashArray = "3",
              fillColor = ~width_pal(Density),
              fillOpacity=0.6) %>%
  addLegend(pal = width_pal,
            values = ~Density,
            opacity = 1.0,
            title = "People/Acre")