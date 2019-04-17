# hospitals explore 
library(readxl)
library(data.table)

# using data from MOS that includes dervied ghg/sqft values 
ghg_31 <- read_excel("data/GHGI Model31.xlsx", sheet = "LL842017")
pluto <- read_excel("data/GHGI Model31.xlsx", sheet = "PLUTO BBLs w typologies")
setDT(ghg_31); setDT(pluto)

unique(ghg_31$`Primary Property Type - Self Selected`)
hosps <- ghg_31[`Threshold Group` %in% "7", ]
hosps[`GHG Intensity (tCO2e/ft2)` > 0.022282737,`GHG Intensity (tCO2e/ft2)`,by = "Property Name"][order(`GHG Intensity (tCO2e/ft2)`, decreasing = TRUE)]

hosps[`GHG Intensity (tCO2e/ft2)` > 0.022282737, unique(`Parent Property Id`)]
ghg_31[`Primary Property Type - Self Selected` %in% "Laboratory", ]
hosps <- ghg_31[ `Threshold Group` == 7, ]
ghg_31[`Largest Property Use Type` %in% "Laboratory", ]

unique(hosps$`List of All Property Use Types at Property`)
unique(hosps$`Target GHGI (tCO2e/ft2)`)
quantile(hosps$`GHG Intensity (tCO2e/ft2)`, .8) # threshold 
plot(ecdf(hosps$`GHG Intensity (tCO2e/ft2)`))

# 
unique(hosps$`Largest Property Use Type`)
unique(hosps$`Primary Property Type - Self Selected`)
labs <- hosps[`Primary Property Type - Self Selected` %in% "Laboratory", ]
labs <- hosps[!grep("lab", `List of All Property Use Types at Property`, ignore.case = TRUE), ]
hosps_nolabs <- hosps[!grep("lab", `List of All Property Use Types at Property`, ignore.case = TRUE), ]
quantile(hosps_nolabs$`GHG Intensity (tCO2e/ft2)`, .8) # threshold 

ghg_31[grep("lab",`Primary Property Type - Self Selected`, ignore.case = TRUE), `10 digit BBL`]
ghg_31[grep("lab",`Primary Property Type - Self Selected`, ignore.case = TRUE), mean(`GHG Intensity (tCO2e/ft2)`)]
hosps[, mean(`GHG Intensity (tCO2e/ft2)`)]


