library(data.table)
library(sf)
library(leaflet)
library(dplyr)
library(councildown)
library(rmarkdown)
library(htmltools)
library(zoo)
library(Rcpp)
source("/home/brooke/PA/pa_lead/code/bin_geocode.R")
source("/home/brooke/PA/pa_lead/code/99_util.R")
library(councildown)
library(htmlwidgets)
source("/home/brooke/PA/pa_lead/code/utils.R")

# let's map the highest emitters of in the residential occupancy group 
ghgs <- read_excel("data/GHGI Model31.xlsx", sheet = "LL842017")
res <- ghg_31[`Threshold Group` %in% 10, ]
res_sub <- res[, .(BBL, `GHG Intensity (tCO2e/ft2)`)]
write_csv(res_sub, "res_ghg.csv")

setnames(res, "10 digit BBL", "BBL")
class(res$BBL)
res[, BBL := as.numeric(BBL)]

shapes <- st_read("/home/brooke/emissions/shapes")
crs_string <- '+proj=longlat +datum=WGS84'

all_bbls <- res %>%
  left_join(shapes, by = "BBL") %>%
  st_as_sf() 

pal <- colorNumeric(
  palette = colorRampPalette(c('#2F56A6','#D05D4E' ))(length(res$`GHG Intensity (tCO2e/ft2)`)), 
  domain = res$`GHG Intensity (tCO2e/ft2)`)

m <- leaflet() %>%
  # addProviderTiles("CartoDB.Positron") %>%
  addCouncilStyle() %>%
  addPolygons(data = all_bbls %>% st_transform(crs_string),
              weight = 1, 
              opacity = 1, 
              color = ~pal(res$`GHG Intensity (tCO2e/ft2)`), 
              # popup = ~paste(ghg_31$`Property Name`, scales::comma(ghg_31$`Penalty ($)`), sep = " "),
              labelOptions = labelOptions(noHide = T,
                                          direction = 'auto')) %>% 
  addLegend(pal = pal, values = res$`GHG Intensity (tCO2e/ft2)`) 


m <- m %>% setView(-73.88099670410158,40.72540497175607,  zoom = 10.5) %>%  registerPlugin(geocoder) %>% 
  onRender(geocode_js, data = list(key = Sys.getenv("GEOCODE_API_KEY")))
m


# let's look at total greenhouse gas emissions 


av


