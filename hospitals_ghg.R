# Speaker plots for Hospitals
ghg_31 <- read_excel("data/GHGI Model31.xlsx", sheet = "LL842017")
setDT(ghg_31)
hosps <- ghg_31[ `Threshold Group` == 7, ]
hosps_bbl <- hosps$`10 digit BBL`
hosps_bbl <- str_split(";", hosps_bbl)

# let's derive the intensities 
port_ghg <- fread("https://data.cityofnewyork.us/resource/8u86-bviy.csv?$limit=999999")
port_sub <- port_ghg[, .(property_gfa_self_reported_ft, total_ghg_emissions_metric_tons_co2e, bbl_10_digits)]
port_sub[, intens := total_ghg_emissions_metric_tons_co2e/property_gfa_self_reported_ft, by = "bbl_10_digits"]
hosp_port <- port_sub[bbl_10_digits %in% hosps_bbl, ]
hosps_sub <- hosps[, .(`GHG Intensity (tCO2e/ft2)`, Organization, `10 digit BBL`, `Total GHG Emissions (Metric Tons CO2e)`)]
hosps_ghg <- hosps_sub$`GHG Intensity (tCO2e/ft2)`
hosps_ghg2 <- hosps_ghg[-which.max(hosps_ghg)]
hosps_ghg2 <- sort(hosps_ghg2)
hosps_ghg3 <- hosps_ghg2[-c(96:101, 1:5)]

quantile(hosps_ghg3, .8)
# write_csv(hosps_sub, "hospitals_intensities.csv")

## 
thresh_1 <- 0.022282737 # first compliance data 
thresh_2 <- 0.011375724 # second compliance date 
thresh_40 = 0.03119583
thresh_30 = 0.02896756

hosps_sub[ghg>thresh_2, ]
hosps_sub[ghg > thresh_40, ]
hosps_sub[ghg < thresh_40, ][ghg >= thresh_30, ]

hosps_sub[ghg > thresh_1 + (thresh_1 * .3), ]
hosps_sub[ghg > thresh_1 + (thresh_1 * .4), ]
hosps_sub[ghg > thresh_2 + (thresh_2 * .4), ]

setorder(hosps_sub, -ghg)
hosps_sub[, pct_red := ghg - (.20*ghg)]
hosps_sub[, over_40 := ghg + (.20*ghg)]
hosps_sub[, id := 1:nrow(hosps_sub)]

ggplot(hosps_sub, aes(x=id, y=ghg)) + 
  geom_col(fill = "pink") + 
  geom_line(aes(x=id, y=pct_red)) +
  geom_hline(yintercept = 0.022282737) + 0
  geom_hline(yintercept = 0.011375724) + 
  theme_bw()


