# let's explore a few different models to predict the ghg output 

library(mgcv)
library(data.table)
library(ggplot2)
library(xgboost)

bdgs <- read_excel("data/02 - LL84 Raw Data.xlsx", sheet = "cleaned2016over50k")
setDT(bdgs)
bdgs[, total_ghg := `Total GHG Emissions (Metric Tons CO2e)`]
bdgs[, prop_use_type := `Largest Property Use Type`]
bdgs[, yr_blt := as.numeric(`Year Built`)]

# let's begin with just modelling based on basics - then work with pluto 
# what are the column names 
bdgs_sub <- bdgs[, .(total_ghg, Occupancy, prop_use_type, yr_blt)]
bdgs_sub <- na.omit(bdgs_sub)
bdgs_res <- bdgs_sub[prop_use_type %in% "Multifamily Housing", ]

mod <- gam(total_ghg ~ prop_use_type + s(Occupancy, k = 10) + s(yr_blt, k = 10), data = bdgs)

# let's try xgboost

