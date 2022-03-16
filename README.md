# Optimal Stream Gage Analysis

## Repository Structure

```
.
└── stream_gage_final  
    ├── scripts  
    │   ├── set_up  
    │   │   ├── 01_load_packages.R  
    │   │   ├── 02_set_options_parameters.R
    │   │   ├── 03_create_directories.R  
    │   │   ├── 04_build_functions.R
    │   │   ├── 05_build_base_maps.R 
    │   ├── clean_data  
    │   │   ├── 01_import_clean_nhd_wbd.R
    │   │   ├── 02_import_clean_ace.R
    │   ├── compile_analysis.Rmd
    ├── data  
    │   ├── metadata
    │   │   ├── ace_aquatic_biodiversity_factsheet.pdf 
    │   │   ├── nhdplus_v2_user_guide.pdf
    │   ├── raw_data # created via set-up script
    │   ├── processed_data # created via set-up script
    ├── output # created via set-up script
    │   ├── figures # created via set-up script
    ├── packages_unpublished  
    │   ├── StrmAnlyzeR_forked.zip  
    ├── .gitignore  
    ├── LICENSE  
    ├── README.md  
```