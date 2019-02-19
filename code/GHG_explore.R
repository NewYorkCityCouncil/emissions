library(data.table)
library(ggplot2)
library(readxl)

# read in the data
ghg_dat <- read_excel("GHGI Model for CC.xlsx", sheet = "Scrubbed Data & Calculations")

names(ghg) <- as.character(ghg[4, ])
ghg <- ghg[-c(1:5), ]
ghg [ ,BBL := `NYC Borough, Block and Lot (BBL)`]
ghg[ ,BBL := gsub("-", "", BBL)]
ghg[is.na(BBL), BBL := `10 Digit BBl`]

# get bbls of rent stab builds 
rentstabs <- fread("https://raw.githubusercontent.com/MODA-NYC/DHCR_RentStabData/master/dhcr.csv")

bbls <- strsplit(ghg$BBL, ";", fixed = TRUE)
bbls <- unlist(bbls)
ghg[, BBL := as.numeric(BBL)]
grep(";", ghg$BBL)

# which bbls that are in the rent stab'd are also in the ghg 
ghg[BBL %in% (rentstabs$BBL), ]

# how many rows have multiple bbls?
inds <- grep(";", ghg$BBL)
inds2 <- grep(",", ghg$BBL)
multbbls <- ghg[c(inds, inds2), ]
singbbls <- ghg[!c(inds, inds2), ]
multbbls <- unlist(strsplit(multbbls$BBL, ";", fixed = TRUE))
multbbls <- unlist(strsplit(multbbls, ",", fixed = TRUE))
multbbls <- trimws(multbbls)

# for the mult bbls - these are the ones that include at least 1 rent stab'd
rsbbl <- rentstabs[BBL %in% as.numeric(multbbls), BBL]

# for the single bbls, which are the ones that overlap with the rent stabilized data
singbbls[!as.numeric(BBL) %in% rentstabs$BBL, ]
nrow(singbbls)




