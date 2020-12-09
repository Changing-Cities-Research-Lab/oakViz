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

usethis::use_data(oak_ids,
                  gentcat,
                  racecat,
                  inccat,
                  cities,
                  oak_tracts,
                  overwrite = TRUE, internal = TRUE)
