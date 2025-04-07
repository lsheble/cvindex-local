# cvindex-local


```text

project_root
│   README.md
|   1_filter_data_v2.R  :  filter original data files to detroit
|   2_add_local_data.R  :  add data from ODP
|   3_get_cvi_toxpi.R   :  build index - modified tx script w orig extra outputs
│
└───data-raw
│   │   CVI_data_current_orig.csv  : curated version of original data
│   │   detroit_2010_census_tracts.csv  :  list of det census tracts from hub
|   |   Reference_info_Census-tract-dat_20240702.csv  :  curated metadata dataset
│   
└───data-inter
|   │   cvi_blight_measure.csv
|   │   cvi_improve_detroit_measure.csv
|   |   CVI_indicators_current_locals_v1.csv
|   |   det_indicator_data_tract_level_curated_geo_w_local.csv
|   |   det_indicator_data_tract_level_curated_geo.csv
└───Diagnostics
|   │   CVI-corr.pdf   : draft correlation graph: needs update
|   │   CheckDist.pdf  : distribution graphs for each indicator
└───CVI-pct
|   │   (Primary ToxPi Scores:) 
|   │   CVI-pct-cat-CC-<category name>.csv: ToxPi scores for each Climate Change category (slices are subcategories) 
|   │   CVI-pct-cat-<category name>.csv: ToxPi scores for each Baseline category (slices are subcategories)
|   │   CVI-pct-comb.csv: Overall ToxPi scores (slices are categories, using category-specific ToxPi scores, rescaled to 0-1) 
|   │   (“Community Baseline” and “Climate Impacts” ToxPi Scores:)
|   │   CVI-pct-comb-baseline.csv: ToxPi Scores for all baseline indicators together 
|   │   CVI-pct-comb-climate.csv: ToxPi Scores for all climate change indicators together 

```

CVI-pct directory: Directory tree only includes descriptions of files directly used in Detroit index.
