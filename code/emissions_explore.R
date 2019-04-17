library(data.table)
library(ggplot2)
library(readxl)
library(stringr)
library(tidyr)

# this uses the old occupancy groups - the new model includes more granular groups 
# let's look at buildings that are > 50K sq.ft
bdgs <- read_excel("data/02 - LL84 Raw Data.xlsx", sheet = "cleaned2016over50k")
setDT(bdgs)

# can we look at ghg/sq ft 
bdgs[, ghgsqft := `Total GHG Emissions (Metric Tons CO2e)`/`Largest Property Use Type - Gross Floor Area (ft²)`]

# let's look at the residential buildings 
res <- bdgs[OccuGroup %in% "Residential", ]
ggplot(res, aes(x=ghgsqft)) + geom_histogram()
quantile(res$ghgsqft, .99, na.rm = TRUE)
res[ghgsqft > .015, ]
res[which.max(ghgsqft), ]

# how many NAs 
nafun <- function(x){
  sum(is.na(x))
}

apply(ghg, 2, nafun)
setnames(lg_bdg, "BBL - 10 digits", "bbl")
lg_bdg[, bbl := as.character(bbl)]

# bring in the rent controlled data; remove these builds - this is at the bbl level so we have to do some digging 
rentstabs <- fread("https://raw.githubusercontent.com/MODA-NYC/DHCR_RentStabData/master/dhcr.csv")

# remove 
inds <- lg_bdg[bbl %in% rentstabs$BBL & `Street Name` %in% rentstabs$STREET1 & `Street Number` %in% rentstabs$BLDGNO1, which = TRUE]
lgbd_sub <- lg_bdg[!inds, ]

# these are the large bdgs wo any rent controlled units - let's look at how many are under the threshold, by unit type 
# let's look at occupancy type 
names(lgbd_sub)
unique(lgbd_sub$`Primary Property Type - Self Selected`)
lgbd_sub[, Prop_type := `Primary Property Type - Self Selected`]

## add the property type codes 
lgbd_sub[Prop_type %in% c("Office", "Bank/Financial Institution", "Service (Vehicle Repair/Service, Postal Service)", "Bank Branch", 
                              "Medical Office" , "Health Care: Outpatient", "Clinic/Other Outpatient Health", "Financial Office",
                              "Senior Care Facility", "Automobile Dealership", "Urgent Care/Clinic/Other Outpatient", "Laboratory",
                              "Repair Services (Vehicle, Shoe, Locksmith, etc.)", 
                              "Outpatient Rehabilitation/Physical Therapy", "Mailing Center/Post Office", "Ambulatory Surgical Center"), 
                              Prop_type := "B"]
lgbd_sub[Prop_type %in% c("Supermarket/Grocery", "Retail", "Mall (Strip Mall and Enclosed)", "Retail Store",
                              "Food Sales", "Retail (Misc)", "Strip Mall", "Other - Mall"), Prop_type := "M"]
lgbd_sub[Prop_type %in% c("Library", "K-12 School", "Education", "College/University (Campus-Level)", "Other - Education", "College/University", "Pre-school/Daycare", 
                          "Supermarket/Grocery Store", "Adult Education"
                              ), Prop_type := "E"]
lgbd_sub[Prop_type %in% c("Hotel", "Multifamily Housing", "Residence Hall/Dormitory", "Lodging", "Senior Care Community", "Mixed Use Property", 
                          "Other - Lodging/Residential"
), Prop_type := "R"]
lgbd_sub[Prop_type %in% c("Restaurant/Cafeteria", "House of Worship",  "Entertainment/Culture", "Social/Meeting", "Recreation", "Public Assembly", "Food Service", 
                          "Social/Meeting Hall", "Worship Facility", "Movie Theater", "Enclosed Mall", "Other - Recreation", "Performing Arts", "Other - Entertainment/Public Assembly", 
                          "Restaurant", "Museum", "Courthouse",  "Wholesale Club/Supercenter", "Ice/Curling Rink", 
                          "Fitness Center/Health Club/Gym"
                          ), Prop_type := "A"]
lgbd_sub[Prop_type %in% c("Hospital (General Medical and Surgical)", "Health Care: Inpatient (Specialty Hospitals, Excluding Children's)", "Hospital (General Medical & Surgical)", 
                          "Residential Care Facility", "Other - Specialty Hospital"
), Prop_type := "I"]
lgbd_sub[Prop_type %in% c("Self-Storage", "Self-Storage Facility", "Data Center", "Storage/Shipping/Non-Refrigerated Warehouse", "Warehouse (Unrefrigerated)", "Warehouse (Refrigerated)", 
                          "Non-Refrigerated Warehouse", "Refrigerated Warehouse"
), Prop_type := "S"]
lgbd_sub[Prop_type %in% c("Other", "Other - Public Services", "Other - Technology/Science", "Other - Services") 
, Prop_type := "Other"]
lgbd_sub[Prop_type %in% "NA"
             , Prop_type := "NA"]

setnames(lgbd_sub, "DOF Gross Floor Area", "GFA")
setnames(lgbd_sub, "Total GHG Emissions (Metric Tons CO2e)", "GHG_tot")
lgbd_sub[, GFA := as.numeric(GFA)]
lgbd_sub[, GHG_tot := as.numeric(GHG_tot)]

# lgbd_sub_2 <- separate_rows(lgbd_sub, "Reported BINs", sep = ",", convert = FALSE)
# These are the current thresholds to try to meet 
# PFA will be over est sq ft - so these thresholds will be a bit higher for builds that dont have parking space 
lgbd_sub[Prop_type %in% c("B", "I", "M"), thresh := GFA*0.008552]
lgbd_sub[Prop_type %in% c("E", "H", "A"), thresh := GFA*0.008886]
lgbd_sub[Prop_type %in% c("R"), thresh := GFA*0.007010]
lgbd_sub[Prop_type %in% c("F", "S"), thresh := GFA*0.005170]

# which buildings are above their thresholds (above) for 2022 & 2023
lgbd_sub[GHG_tot > thresh, ]

# what is the multiplier that these builds are at?
lgbd_sub[GFA == 0, GFA := NA ]
lgbd_sub[GHG_tot != 0, current_intens := GHG_tot/GFA]

# let's use Trump builds as an example to figure out penalties 
trump <- lgbd_sub[grep("Trump", `Property Name`, ignore.case = TRUE), ]
trump[GHG_tot > thresh, .(Prop_type, GFA)]
trump[, .(Penalty = (268 * (GFA * (current_intens - 0.007010))), `Property Name`)] # trump - category "R"
lgbd_sub[grep("5th Ave", `Address 1 (self-reported)`, ignore.case=TRUE), unique(`Property Name`)]
lgbd_sub[grep("FIFTH Ave", `Address 1 (self-reported)`, ignore.case=TRUE), unique(`Property Name`)]
trumptow <- lgbd_sub[bbl %in% "1012927501", ]
trumptow[, (268 * (GFA * (current_intens - 0.007010)))]

# let's look at Empire State 
es <- lgbd_sub[grep("Empire State", `Property Name`, ignore.case = TRUE), ]
es[, .(Prop_type, GFA)]
# es[, 268 * (GFA * (current_intens - 0.007010))]


lgbd_sub[!is.na(current_intens), q99 := quantile(current_intens, .99), by = .(Prop_type)] # get the cdf for the GHG to get the worst ones 
lgbd_sub[!is.na(current_intens), q95 := quantile(current_intens, .95), by = .(Prop_type)]
lgbd_sub[!is.na(current_intens), q90 := quantile(current_intens, .90), by = .(Prop_type)]
lgbd_sub[!is.na(current_intens), q80 := quantile(current_intens, .80), by = .(Prop_type)]
lgbd_sub[!is.na(current_intens), q70 := quantile(current_intens, .70), by = .(Prop_type)]
lgbd_sub[!is.na(current_intens), q60 := quantile(current_intens, .60), by = .(Prop_type)]
lgbd_sub[!is.na(current_intens), q50 := quantile(current_intens, .50), by = .(Prop_type)]
lgbd_sub[!is.na(current_intens), q40 := quantile(current_intens, .40), by = .(Prop_type)]
lgbd_sub[!is.na(current_intens), q30 := quantile(current_intens, .30), by = .(Prop_type)]
lgbd_sub[!is.na(current_intens), q20 := quantile(current_intens, .20), by = .(Prop_type)]
lgbd_sub[!is.na(current_intens), q10 := quantile(current_intens, .10), by = .(Prop_type)]
lgbd_sub[!is.na(current_intens), q01 := quantile(current_intens, .01), by = .(Prop_type)]

quantdt <- melt(lgbd_sub, id.vars = c("bbl", "Prop_type"),
     measure.vars = c("q99", "q95", "q90", "q80", "q70", "q60", "q50", "q40", "q30", "q20", "q10", "q01"))
quantdt <- quantdt[!is.na(value), ]
quantdt[, bbl := NULL]
quantdt <- unique(quantdt)

lgbd_sub[Prop_type %in% c("E", "I")]

# ### ADD H LINES ######
# lgbd_sub_2[, thresh99 := round(quantile(thresh, .99, na.rm = TRUE), 2), by = .(Prop_type)]

# check out the groups not inc q99
ggplot(quantdt[variable != "q99", ], aes(x  = variable, y = value, fill = Prop_type)) + geom_col(position = "dodge") + 
    theme_bw()

ggplot(quantdt[variable != "q99", ], aes(x  = variable, y = value, fill = Prop_type)) + geom_col(position = "dodge") + 
  theme_bw() 

# check out q99
ggplot(quantdt[variable %in% "q99", ], aes(x  = variable, y = value, fill = Prop_type)) + geom_col(position = "dodge") + theme_bw()

## methods 
# let's look at the differences in reduction thresholds with site energy vs source energy 



