rm(list = ls())

detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# load necessary packages
lapply(c("ggplot2", "stargazer", "tidyverse", "stringr",
         "broom"),  pkgTest)
lapply(c("tigris", "sf", "cartography", "zipcodeR"),  pkgTest)
# littlemissdata
lapply(c("lubridate", "data.table", "ggrepel", "mapsf"),  pkgTest)


# function to save output to a file that you can read in later to your docs
output_stargazer <- function(outputFile, appendVal=TRUE, ...) {
  output <- capture.output(stargazer(...))
  cat(paste(output, collapse = "\n"), "\n", file=outputFile, append=appendVal)
}

# set working directory to current parent folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

getwd()
#library(sf) #Overall handling of sf objects
#library(cartography) #Plotting maps package


# Create some color variables for graphing later

col1 = "#011f4b"
col2 = "#6497b1"
col3 = "#b3cde0"
col4 = "#CC0000"
col5 = "#d8e1cf" 
col6 = "#438484"


##====

seawa <- search_city("Seattle", "WA")
wa <- search_state("WA")

View(seawa)

seawa_pop <- seawa %>% #filter(!is.na(population)) %>%
  select(zipcode, population) %>%
  mutate(population = ifelse(!is.na(population), population, 0))

View(seawa_pop)

options(tigris_use_cache = TRUE)
geo <- st_as_sf(zctas(cb = FALSE,   #TRUE
                      keep_zipped_shapefile = TRUE,
                      #                      refresh = FALSE,
                      starts_with = seawa_pop$zipcode))

places <- st_as_sf(places(state = "WA", cb=FALSE,
                          keep_zipped_shapefile = TRUE,
                          refresh = FALSE,))
places=st_transform(places,st_crs(geo))

seawa.sf <-  merge(geo, seawa)

mf_map(x = seawa.sf, var = "population", type = "choro")


#= = = = = = = = = = = = = = = = = = = = = = = = = = = 
price_data <- readRDS("../Data/train.rds")


testdf <- data.frame(price_data['ZipCode'],
                     price_data['AdjSalePrice'])

# 
testdf['ZipCode'] <- as.character(testdf$ZipCode)
testdf['AdjSalePrice'] <- as.double(testdf$AdjSalePrice)

unique(testdf$ZipCode)
price <- testdf %>% group_by(ZipCode) %>%
  mutate(AvgePrice = mean(AdjSalePrice)) %>%
  mutate(Centroid = geocode_zip(ZipCode)) %>%
  select(ZipCode, Centroid, AvgePrice ) %>%
  unique()



geo <- st_as_sf(zctas(cb = FALSE,   #TRUE
                      keep_zipped_shapefile = TRUE,
                      #                      refresh = FALSE,
                      starts_with = price$ZipCode))

places <- st_as_sf(places(state = "WA", cb=FALSE,
                          keep_zipped_shapefile = TRUE,
                          refresh = FALSE,))
counties <- st_as_sf(counties(state = "WA", cb=FALSE,
                          keep_zipped_shapefile = TRUE,
                          refresh = FALSE,))
counties=st_transform(counties,st_crs(geo))
places=st_transform(places,st_crs(geo))

price.sf <-  merge(geo, price, by.x = "ZCTA5CE20", 
                    by.y = "ZipCode")

##, expandBB = c(0, 0, 0 ,0.2)
#,theme = "nevermind"

mf_export(x = price.sf, width = 500, export = "png",
          filename = "../results/seattle_prices.png",
          theme = "nevermind", expandBB = c(0, 0, 0 ,0.2)
          )  
mf_map(x = price.sf, var = "AvgePrice", type = "choro",
       leg_pos = "right", leg_title = "Avge House Price")
mf_layout(
  title = "House Prices in Seattle, WA",
  credits = " x; Sources: me"
)
mf_arrow("topleft")
dev.off()

mf_map(x = price.sf)
# Plot proportional symbols
mf_map(x = price.sf, var = "AvgePrice", type = "prop",
       leg_pos = "right", leg_title = "Avge House Price")
# Plot a map layout
mf_layout(
  title = "House Prices in Seattle",
  credits = " x; Sources: me"
)
mf_arrow("topleft")


# Export a map with a theme and extra margins
mf_export(
  x = seawa.sf, filename = "mtq.png",
  width = 600, res = 120,
  theme = "green",
  expandBB = c(0, 0, 0, .3)
)
# Plot a shadow
mf_shadow(seawa.sf, col = "grey10", add = TRUE)
# Plot a choropleth map
mf_map(
  x = seawa.sf, var = "population", type = "choro",
  pal = "Dark Mint",
  breaks = "quantile",
  nbreaks = 6,
  leg_title = "population",
  leg_val_rnd = -2,
  add = TRUE
)
# Start an inset map
mf_inset_on(x = "worldmap", pos = "right")
# Plot mtq position on a worldmap
mf_worldmap(seawa.sf, col = "#0E3F5C")
# Close the inset
mf_inset_off()
# Plot a title
mf_title("population in seattle")
# Plot credits
mf_credits("..\nSources: ...")
# Plot a scale bar
mf_scale(size = 5)
# Plot a north arrow
mf_arrow("topleft")
dev.off()



#4. Plotting
par(mar=c(1,1,1,1))
ghostLayer(prices.sf)
#plot(st_geometry(states), add=TRUE)
plot(st_geometry(places), add=TRUE)
choroLayer(prices.sf,
           var="Cases",
           add=TRUE,
           border = NA,
           legend.pos = "right",
           legend.frame = TRUE)
layoutLayer(title = "Average Price by Zip Code",
            theme = "green.pal",
            scale = TRUE,
            sources = "Source; Seattle House Price Data",
            author = "by dieghernan, 2020"
)


#seawa_pop$population <- ifelse(!is.na(seawa$population), seawa_pop$population, 50)

options(tigris_use_cache = TRUE)
geo <- st_as_sf(zctas(cb = FALSE,   #TRUE
                      keep_zipped_shapefile = TRUE,
                      #                      refresh = FALSE,
                      starts_with = seawa_pop$zipcode))

plot(st_geometry(geo), col = NA, border = NA, 
     bg = "#aadaff")

choroLayer(
  x = geo, 
  var = "population",
  method = "geom",
  nclass=5,
  col = carto.pal(pal1 = "sand.pal", n1 = 5),
  border = "white", 
  lwd = 0.5,
  legend.pos = "topright", 
  legend.title.txt = "population",
  add = TRUE
) 
layoutLayer(title = "Sale Price in Seattle", 
            sources = "Sources: Martyn",
            author = paste0("cartography ", packageVersion("cartography")), 
            frame = FALSE, north = FALSE, tabtitle = TRUE, theme= "sand.pal") 
# north arrow
north(pos = "topleft")




#1. Create your data
price_data <- readRDS("../data/train.rds")
price_data <- readRDS("../data/test.rds")


testdf <- data.frame(price_data['ZipCode'],
                     price_data['AdjSalePrice'])

# 
testdf['ZipCode'] <- as.character(testdf$ZipCode)
testdf['AdjSalePrice'] <- as.double(testdf$AdjSalePrice)

unique(testdf$ZipCode)
yourdata <- testdf %>% group_by(ZipCode) %>%
  mutate(AvgePrice = mean(as.double(AdjSalePrice))) %>%
  mutate(Centroid = geocode_zip(ZipCode)) %>%
  select(ZipCode, Centroid, AvgePrice ) %>%
  unique()

View(yourdata)

testState  <- yourdata %>% reverse_zipcode(ZipCode)[1:7]

testState <- reverse_zipcode(yourdata$ZipCode)[1:7]

View(testState)



#2. Download a shapefile (shp,gpkg,geojson...)
#library(tigris) #For downloading the zipcode map
options(tigris_use_cache = TRUE)
geo <- st_as_sf(zctas(cb = FALSE,   #TRUE
                      keep_zipped_shapefile = TRUE,
#                      refresh = FALSE,
                      starts_with = yourdata$ZipCode))


#yourdata$priceCol <-  yourdata$AvgePrice #/ st_area(geo)
# plot municipalities (only the backgroung color is plotted)
plot(st_geometry(geo), col = NA, border = NA, bg = "#aadaff")

choroLayer(
  x = geo, 
  var = "AvgePrice",
  method = "geom",
  nclass=5,
  col = carto.pal(pal1 = "sand.pal", n1 = 5),
  border = "white", 
  lwd = 0.5,
  legend.pos = "topright", 
  legend.title.txt = "Avge Adj Sale Price",
  add = TRUE
) 
layoutLayer(title = "Sale Price in Seattle", 
            sources = "Sources: Martyn",
            author = paste0("cartography ", packageVersion("cartography")), 
            frame = FALSE, north = FALSE, tabtitle = TRUE, theme= "sand.pal") 
# north arrow
north(pos = "topleft")



mtq$POPDENS <- 1e6 * mtq$POP / st_area(mtq)
# plot municipalities (only the backgroung color is plotted)
plot(st_geometry(mtq), col = NA, border = NA, bg = "#aadaff")
# plot population density
choroLayer(
  x = mtq, 
  var = "POPDENS",
  method = "geom",
  nclass=5,
  col = carto.pal(pal1 = "sand.pal", n1 = 5),
  border = "white", 
  lwd = 0.5,
  legend.pos = "topright", 
  legend.title.txt = "Population Density\n(people per km2)",
  add = TRUE
) 








#Other general area functions: block_groups(), blocks(), counties(), county_subdivisions(), places(), pumas(), school_districts(), tracts(), zctas()

#Overall shape of USA states
places <- st_as_sf(places(state = "WA", cb=FALSE,
                          keep_zipped_shapefile = TRUE,
                          refresh = FALSE,))
counties <- st_as_sf(counties(state = "WA", cb=FALSE,
                          keep_zipped_shapefile = TRUE,
                          refresh = FALSE,))

#states <- st_as_sf(states(cb=FALSE,
#                          keep_zipped_shapefile = TRUE,
#                          refresh = FALSE,))


#For plotting, all the maps should have the same crs
places=st_transform(places,st_crs(geo))
counties=st_transform(counties,st_crs(geo))
#states=st_transform(states,st_crs(geo))

#3. Now Merge your data
yourdata.sf=merge(geo,yourdata)

#4. Plotting
par(mar=c(1,1,1,1))
ghostLayer(yourdata.sf)
#plot(st_geometry(states), add=TRUE)
plot(st_geometry(counties), add=TRUE)
choroLayer(yourdata.sf,
           var="Cases",
           add=TRUE,
           border = NA,
           legend.pos = "right",
           legend.frame = FALSE)
layoutLayer(title = "Cases by ZIPCODE",
            theme = "green.pal",
            scale = TRUE,
            sources = "Source; Seattle House Price Data",
            author = "by dieghernan, 2020"
)

#https://stackoverflow.com/questions/60419762/mapping-my-data-to-a-zip-code-area-map-in-r


