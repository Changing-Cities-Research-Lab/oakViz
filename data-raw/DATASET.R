## code to prepare `DATASET` dataset goes here

oak_ids <- readr::read_csv("../../oak-data-repo/oakland_geographies/trtid10_oak.csv")

relabel_gent_cat <- c("nongentrifiable" = "Nongentrifiable",
                      "gentrifying" = "Gentrifying",
                      "intense"  = "Intense",
                      "moderate" = "Moderate",
                      "earlygent" = "Early Gentrification",
                      "weak" = "Weak",
                      "peoplepricegent" = "People or Price")

gent_cat_plot_order <- c("Nongentrifiable", "Gentrifying",
                         "Intense", "Moderate",
                         "Early Gentrification", "Weak", "People or Price")

relabel_race_cat <- c("PredWhite" = "Predominantly White",
                      "PredBlack" = "Predominantly Black",
                      "PredOther"  = "Predominantly Other",
                      "WhiteOther" = "White-Other",
                      "BlackWhite" = "Black-White",
                      "BlackOther" = "Black-Other",
                      "Multiethnic" = "Multiethnic",
                      "Overall" = "Overall",
                      "WhiteMixed" = "White/White-Mixed",
                      "MixedOther" = "Multiethnic/Other")

race_cat_plot_order <- c("Predominantly White", "Predominantly Black",
                         "Predominantly Other","White-Other","Black-White","Black-Other","Multiethnic",
                         "White/White-Mixed", "Multiethnic/Other")

inc_cat_plot_order <- c("Bottom Quintile", "Second Quintile", "Middle Quintile",
                        "Fourth Quintile", "Top Quintile")

# gentrification data
gentcat <- read_csv("../../oak-data-repo/gentrification_categories/gentcat_006a_50_oak.csv") %>%
  select(tractid10 = trtid10, cat = gentcat_006a_50)
gentcat$cat <- plyr::revalue(gentcat$cat, relabel_gent_cat)
gentcat$cat <- factor(gentcat$cat, levels = gent_cat_plot_order)
gentcat$facet = "Gentrification"

# race data
racecat <- read_csv("../../oak-data-repo/ethnoracial_composition/racetypology_oak_tracts_00.csv") %>%
  select(tractid10 = trtid10, cat = race.shortcategory00)
racecat$cat <- plyr::revalue(racecat$cat, relabel_race_cat)
racecat$cat <- factor(racecat$cat, levels = race_cat_plot_order)
racecat$facet = "Ethnoracial"

# income data
inccat <- read_csv("../../oak-data-repo/income_categories/hinc09_categories.csv")
inccat$cat <- factor(inccat$cat, levels = inc_cat_plot_order)
inccat$facet = "Income"

# City shapefiles
cities <- read_csv("../../oak-data-repo/oakland_geographies/census_2010b_tracts_places_ca.csv")

# Oakland tracts data
oak_tracts <- oak_ids %>%
  select(tractid10 = trtid10)

# Captions
ses_caption = "\nSES Ranges by Equifax Risk Scores: Low = missing or <580, Moderate = 580-649, Middle = 650-749, High = 750+."
period_caption = "\nHousing Period Ranges: Boom = 2002-2006, Bust = 2007-2009, Recovery = 2010-2014, Post-Recovery = 2015-2017."
frb_caption = "\nSource: Federal Reserve Bank of New York Consumer Credit Panel/Equifax Data."

usethis::use_data(oak_ids,
                  gentcat,
                  racecat,
                  inccat,
                  cities,
                  oak_tracts,
                  ses_caption,
                  period_caption,
                  frb_caption,
                  overwrite = TRUE, internal = TRUE)
