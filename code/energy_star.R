# UGC explore
# look at worst performers accd to energy star
# read in the energy score data 
# these are buildings that are over 50k
library(data.table)
library(readxl)
library(ggplot2)

bdgs_ov_50k <- read_excel("02 - LL84 Raw Data.xlsx", sheet = "cleaned2016over50k")
names(bdgs_ov_50k)
setDT(bdgs_ov_50k)
names(bdgs_ov_50k)

bdgs_ov_50k[, ghg_sq.ft := as.numeric(`Total GHG Emissions (Metric Tons CO2e)`)/`Largest Property Use Type - Gross Floor Area (ft²)`]
summary(bdgs_ov_50k$ghg_sq.ft)

ggplot(bdgs_ov_50k[ghg_sq.ft>.02, ], aes(y = as.numeric(`ENERGY STAR Score`), x  = ghg_sq.ft)) + geom_point() + 
  facet_wrap(~OccuGroup) + theme_bw() + xlab("GHG/sqft") + ylab("Energy Star Rating") 

# what buildings are producing the most carbon that are also scoring pretty high 
bdgs_ov_50k[ghg_sq.ft>.03 & OccuGroup %in% "Business" & `ENERGY STAR Score` == 100, ]

# let's kook at the relationship between total ghg and site/source eui 
ggplot(bdgs_ov_50k, aes(x = `Site EUI (kBtu/ft²)`, y  = `Total GHG Emissions (Metric Tons CO2e)`)) + geom_point() + facet_wrap(~OccuGroup)
ggplot(bdgs_ov_50k, aes(x = `Source EUI (kBtu/ft²)`, y  = `Total GHG Emissions (Metric Tons CO2e)`)) + geom_point() + facet_wrap(~OccuGroup)

# let's look at the relationship between source/site and energy star rating 
ggplot(bdgs_ov_50k, aes(x = `Source EUI (kBtu/ft²)`, y = as.numeric(`ENERGY STAR Score`))) + geom_point() + facet_wrap(~OccuGroup)

# get rid of weird occupancy group cats 
bdgs_sub <- bdgs_ov_50k[!OccuGroup %in% c("0", "N/A")]
ggplot(bdgs_ov_50k, aes(x = as.numeric(`ENERGY STAR Score`), y  = `Total GHG Emissions (Metric Tons CO2e)`)) + geom_point() + facet_wrap(~OccuGroup)
ggplot(bdgs_sub, aes(y = as.numeric(`ENERGY STAR Score`), x  = ghg_sq.ft)) + geom_point() + 
  facet_wrap(~OccuGroup) + theme_bw() + xlab("GHG/sqft") + ylab("Energy Star Rating") 

# let's look at the more granular classifications 
ggplot(bdgs_sub, aes(x=`Total GHG Emissions (Metric Tons CO2e)`, y = `Largest Property Use Type`)) + geom_line() + theme_bw()

# let's fit some lms for biz and res 
summary(lm(as.numeric(`ENERGY STAR Score`) ~ `Total GHG Emissions (Metric Tons CO2e)` + `Largest Property Use Type - Gross Floor Area (ft²)` + OccuGroup, data = bdgs_sub))
summary(lm(as.numeric(`ENERGY STAR Score`) ~ `Total GHG Emissions (Metric Tons CO2e)`, 
           data = bdgs_sub))

# no energy score 
no_score <- bdgs_ov_50k[`ENERGY STAR Score` %in% "Not Available", ]
no_score[, .N,  by = "OccuGroup"]
bdgs_ov_50k[OccuGroup %in% "Residential", ]

# lets look at buildings that will end up in the less than 61 
bdgs_ov_50k[`ENERGY STAR Score` %in% "Not Available", `ENERGY STAR Score` := NA ]
bdgs_ov_50k[, `ENERGY STAR Score`:= as.numeric(`ENERGY STAR Score`)]
bds_un_60 <- bdgs_ov_50k[`ENERGY STAR Score` < 60 & `Primary Property Type - Self Selected` %in% "Multifamily Housing", ]
bdgs_ov_50k[, norm_site := `Site EUI (kBtu/ft²)`/`DOF Gross Floor Area`]
bdgs_ov_50k[, norm_source := `Source EUI (kBtu/ft²)`/`DOF Gross Floor Area`]
bdgs_sub <- bdgs_ov_50k[!OccuGroup %in% c(NA, "N/A", 0), ]

ggplot(bdgs_sub, aes(x = norm_source, y = `ENERGY STAR Score`)) + geom_point() + theme_bw() + facet_wrap(~OccuGroup)
ggplot(bdgs_sub, aes(x = norm_site, y = `ENERGY STAR Score`)) + geom_point() + theme_bw() + facet_wrap(~OccuGroup)

# for residential buildings - what would reductions need to happen in order for 
# for residential buildings that score below 
bdgs <- bdgs_sub[OccuGroup %in% "Residential", ]
bdgs_sub_60 <- bdgs[`ENERGY STAR Score` < 60 , ]

# look at the dist of scores by occupancy groups
ggplot(bdgs_sub, aes(x = `ENERGY STAR Score`, group = OccuGroup, fill = OccuGroup)) + geom_histogram() 

ggplot(bdgs_sub, aes(x = OccuGroup, y = `ENERGY STAR Score`, fill = OccuGroup)) + geom_boxplot(show.legend = FALSE) + coord_flip() + theme_classic() + 
  ylab("Energy Star Score") + xlab("Occupancy Group") 

bdgs_sub[, .N, by = "OccuGroup"][order(N, decreasing = TRUE)]

# how many are there 
bdgs_sub_60

# plot energy star against ghg 
hist(bdgs_ov_50k$`ENERGY STAR Score`)
names(bdgs_ov_50k)
no_rat <- bdgs_ov_50k[`ENERGY STAR Score` %in% NA, `Site EUI (kBtu/ft²)`]
dt <- data.table(site_eui = as.numeric(bdgs_ov_50k$`Site EUI (kBtu/ft²)`), estar = bdgs_ov_50k$`ENERGY STAR Score`)

bdgs_ov_50k[`ENERGY STAR Score`  > 90, ]
bdgs_ov_50k[`Largest Property Use Type` %in% "Hospital (General Medical & Surgical)", ]

hosps <- bdgs_ov_50k[`Largest Property Use Type` %in% "Hospital (General Medical & Surgical)", ]
hist(hosps$`ENERGY STAR Score`)
hosps[`ENERGY STAR Score` > 90, .(`ENERGY STAR Score`, `Property Name`, `Parent Property Name`)]



ggplot(dt, aes(x = site_eui, y = estar)) + geom_point()
unique(bdgs_ov_50k$`Direct GHG Emissions (Metric Tons CO2e)`) 

# plot 

# look at hospitals 