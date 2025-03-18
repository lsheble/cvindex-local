# Filter to selected indicators, detroit census tracts

# load-libraries

library(data.table)
library(stringr)
library(dplyr)
library(naniar)
library(readr)


# import-data-files

## 2010 FIPS to connect census tracts
## prepared with R project "CVI-detroit-prep-geographies"

Detroit_2010_FIPS <- read_csv("data-raw/detroit_2010_census_tracts.csv", 
                              col_types = cols(geoid_tract_10 = col_character()))



# import-indicator data

Wayne_MI_Indicator <- read_csv("data-raw/CVI_data_current_orig.csv",    # trying universal data file instead of Wayne co MI, Wayne-MI_Indicator_Native_Units_20240702.csv", 
                               col_types = cols(`GEOID.Tract` = col_character()))

# retrieve the full column specification for this data.
spec(Wayne_MI_Indicator)

# import-ref-info

Ref_info <- read_csv("data-raw/Reference_info_Census-tract-dat_20240702.csv")

# retrieve the full column specification for this data.
spec(Ref_info)


# filter-detroit-tracts

det_ind_tracts <- filter(Wayne_MI_Indicator, Wayne_MI_Indicator$`GEOID.Tract` %in% Detroit_2010_FIPS$geoid_tract_10)




## prep manually curated set of tract-level indicator data - we identified indicators that
## do not make sense, dropped them, and this is what is left.
# Items dropped include indicators not available at tract level + those that are not relevant for Detroit (e.g., hurricanes), 
## and those that showed no variation across Detroit despite availability at census tract level (based on previous versions of
## files in Diagnostics. That said, some indicators kept have minimal variation across Detroit. Could review indicator list, 
## diagnostics to further cull or identify items for replacement..

keep_ind_cols <- Ref_info %>% pull(`Indicator`)



## Also want to keep geo colnames:
keep_geo_cols <- colnames(Wayne_MI_Indicator[1:6])


## Combine the two lists of cols to keep:

keep_cols <- c(keep_geo_cols, keep_ind_cols)


# keep-curated-indicators

det_ind_tracts <- det_ind_tracts |> 
  select(any_of(keep_cols))


# write-det-indicators

write_csv(det_ind_tracts, "data-inter/det_indicator_data_tract_level_curated_geo.csv")


