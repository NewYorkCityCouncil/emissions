library(data.table)
library(sf)
library(leaflet)
library(dplyr)
library(councildown)
library(rmarkdown)
library(htmltools)
library(zoo)
library(Rcpp)
library(readxl)
source("/home/brooke/PA/pa_lead/code/bin_geocode.R")
source("/home/brooke/PA/pa_lead/code/99_util.R")
library(councildown)
library(htmlwidgets)
source("/home/brooke/PA/pa_lead/code/utils.R")

# let's map the highest emitters of in the residential occupancy group 
ghgs <- read_excel("data/GHGI Model31.xlsx", sheet = "LL842017")
setDT(ghgs)
res <- ghgs[`Threshold Group` %in% 10, ]
res[, `GHG Intensity (tCO2e/ft2)` := round(`GHG Intensity (tCO2e/ft2)`, 3)]
res[, "Percent Over" := round(`% of Total GHGI over target`, 3)]

# res_sub <- res[, .(BBL, `GHG Intensity (tCO2e/ft2)`)]
# write_csv(res_sub, "res_ghg.csv")
setnames(res, "10 digit BBL", "BBL")
class(res$BBL)
res[, BBL := as.numeric(BBL)]

res <- distinct(res) %>%
  mutate(ghgi = paste("Green House Gas Intensity (tCO2e/ft2):", `GHG Intensity (tCO2e/ft2)`),
         mean_ghg = (paste("Average Green House Gas Intensity (tCO2e/ft2) for Residential Buildings: 0.005")), 
         popup = map2_chr(ghgi, mean_ghg, ~caption_template(header_template(.x, .y), NULL)))

# res <- distinct(res) %>%
#   mutate(ghgi = paste("Green House Gas Intensity (tCO2e/ft2):", `GHG Intensity (tCO2e/ft2)`),
#          popup = map_chr(ghgi, ~caption_template(header_template(.), NULL)))

shapes <- st_read("/home/brooke/emissions/shapes")
crs_string <- '+proj=longlat +datum=WGS84'

all_bbls <- res %>%
  left_join(shapes, by = "BBL") %>%
  st_as_sf() 

# pal <- colorNumeric(
#   palette = colorRampPalette(c('dark green','#D05D4E' ))(length(res$`GHG Intensity (tCO2e/ft2)`)), 
#   domain = res$`GHG Intensity (tCO2e/ft2)`)

pal <- colorBin(c('#12B886','#2F56A6', '#D05D4E'), res$`GHG Intensity (tCO2e/ft2)`, 5, pretty = FALSE)

m <- leaflet() %>%
  # addProviderTiles("CartoDB.Positron") %>%
  addCouncilStyle() %>%
  addPolygons(data = all_bbls %>% st_transform(crs_string),
              weight = 1, 
              fillOpacity = .8, 
              color = ~pal(res$`GHG Intensity (tCO2e/ft2)`), 
              popup = ~popup,
              labelOptions = labelOptions(noHide = T,
                                          direction = 'auto')) %>% 
  addLegend(pal = pal, values = res$`GHG Intensity (tCO2e/ft2)`) 

m <- m %>% setView(-73.88099670410158,40.72540497175607,  zoom = 10.5) %>%  registerPlugin(geocoder) %>% 
  onRender(geocode_js, data = list(key = Sys.getenv("GEOCODE_API_KEY")))
m

saveWidget(m, "res_ghg_map.html")

# let's look at average greenhouse gas emissions intensities 
mean_ghgs <- ghgs[, mean(`GHG Intensity (tCO2e/ft2)`), by = "Threshold Group"]
mean_ghgs[`Threshold Group` %in% 10, Group := "Residential"]
mean_ghgs[`Threshold Group` %in% 13, Group:= NA]
mean_ghgs[`Threshold Group` %in% 11, Group:= "Storage"]
mean_ghgs[`Threshold Group` %in% 1, Group:= "Assembly"]
mean_ghgs[`Threshold Group` %in% 2, Group:= "Business"]
mean_ghgs[`Threshold Group` %in% 3, Group:= "Education"]
mean_ghgs[`Threshold Group` %in% 4, Group:= "Factory"]
mean_ghgs[`Threshold Group` %in% 8, Group:= "Institutional"]
mean_ghgs[`Threshold Group` %in% 9, Group:= "Mercantile"]
mean_ghgs[`Threshold Group` %in% 16, Group:= "House of Worship"]
mean_ghgs[`Threshold Group` %in% 15, Group:= "Senior Care Facility"]
mean_ghgs[`Threshold Group` %in% 12, Group:= "Hotel"]
mean_ghgs[`Threshold Group` %in% 7, Group:= "Hospital"]
mean_ghgs[`Threshold Group` %in% 14, Group:= "DCAS"]

setnames(mean_ghgs, "V1", "GHG Intensity (tCO2e/ft2)")
# write_csv(mean_ghgs, "mean_ghgs_grps.csv")




