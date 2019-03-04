# let's explore a few different models to predict the ghg output 

library(mgcv)
library(data.table)
library(ggplot2)
library(xgboost)
library(readxl)

bdgs <- read_excel("data/02 - LL84 Raw Data.xlsx", sheet = "cleaned2016over50k")
setDT(bdgs)
bdgs[, total_ghg := `Total GHG Emissions (Metric Tons CO2e)`]
bdgs[, prop_use_type := `Largest Property Use Type`]
bdgs[, yr_blt := as.numeric(`Year Built`)]

# let's begin with just modelling based on basics - then work with pluto 
# what are the column names 
bdgs_sub <- bdgs[, .(total_ghg, Occupancy, prop_use_type, yr_blt)]
bdgs_sub <- na.omit(bdgs_sub)
apply(bdgs_sub, 2, function(x){sum(is.na(x))})

# let's try xgboost
labs <- bdgs_sub[, .(total_ghg)]
bdgs_sub[, total_ghg := NULL]
mod_mat <- model.matrix(~.+0, data = bdgs_sub,with=F) 
bdgs_sub[, prop_use_type := as.factor(prop_use_type)]
bdgs_sub[, prop_use_type := as.numeric(prop_use_type)]

xg <- xgboost(data =  as.matrix(bdgs_sub), 
        label = labels, 
        eta = 0.1,
        max_depth = 15, 
        nround=2500, 
        subsample = 0.5,
        colsample_bytree = 0.5,
        seed = 1,
        objective = "reg:linear",
        nthread = 3)

xgb.importance(model = xg)

cv <- xgb.cv(data = as.matrix(bdgs_sub), 
             nrounds = 30000, 
             nthread = 16, 
             nfold = 10, 
             label = labels, 
             metrics = list("rmse"),
             max_depth = 15, 
             eta = .1, 
             objective = "reg:linear")


# does a gam even make sense in this setting?
mod <- gam(total_ghg ~ prop_use_type + s(Occupancy, k = 10) + s(yr_blt, k = 10), data = bdgs)




