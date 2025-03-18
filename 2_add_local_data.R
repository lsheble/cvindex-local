# add local data


# load-libraries
if (!require("dplyr")) {install.packages("dplyr"); require("dplyr")}
if (!require("readr")) {install.packages("readr"); require("readr")}


# read indicator data file derived from CVI_detroit_v5
## contains indicator data from TX A&M CVI data

det_indicator_data_cvi_v5 <- read_csv("data-inter/det_indicator_data_tract_level_curated_geo.csv", 
            col_types = cols(GEOID.Tract = col_character()))


# read data to be added

# Blight Violations
cvi_blight_measure <- read_csv("data-inter/cvi_blight_measure.csv", 
                               col_types = cols(CENSUS_TRACT_GEO_ID_2010 = col_character()))

# Improve Detroit Issues
cvi_improve_detroit_measure <- read_csv("data-inter/cvi_improve_detroit_measure.csv", 
                                        col_types = cols(CENSUS_TRACT_GEO_ID_2010 = col_character()))



# Join local measures to dataset

det_indicator_data_w_local <- det_indicator_data_cvi_v5 |>
  left_join(cvi_blight_measure, by = join_by(`GEOID.Tract` == CENSUS_TRACT_GEO_ID_2010)) |>
  left_join(cvi_improve_detroit_measure, by = join_by(`GEOID.Tract` == CENSUS_TRACT_GEO_ID_2010))


# relocate new columns based on where added to CVI_indicators_current_v4 in 00_CVI_Ref_file_w_local_data.xlsx

det_indicator_data_w_local <- det_indicator_data_w_local |> 
  relocate(`Blight Violations`, .before = `Flooding risk to roads`) |>
  relocate(`Improve Detroit Issues`, .before = `Total vehicle miles traveled per capita`)

# write data indicators with local data added
write_csv(det_indicator_data_w_local, "data-inter/det_indicator_data_tract_level_curated_geo_w_local.csv")
