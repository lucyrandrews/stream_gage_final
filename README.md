# Optimal Stream Gage Analysis

## Repository Structure

```
.
└── stream_gage_final  
    ├── scripts  
        ├── 01_set_up  
            ├── 01_load_packages.R  
            ├── 02_set_options_parameters.R
            ├── 03_create_directories.R  
            ├── 04_build_functions.R
            ├── 05_build_base_maps.R
            ├── X_build_draft_functions.R
        ├── 02_clean_data  
            ├── 01_import_clean_nhd_wbd.R
            ├── 02_import_clean_ace.R
            ├── 03_import_clean_nccag.R
            ├── 04_import_clean_ref_streams.R
            ├── 05_import_clean_nid.R
            ├── 06_add_management_attributes.R
        ├── 03_analyze_networks  
            ├── 01_analyze_full_network.R
            ├── 02_reshape_network_analysis.R
            ├── 03_run_set_cover_expansion.R
            ├── 04_run_set_cover_all.R
            ├── 05_import_clean_nhd_wbd.R
            ├── 06_analyze_reconfigure_coverage.R
        ├── 04_create_output  
            ├── 01_create_text_output.R
            ├── 02_create_gaged_expansion_network_figures.R
            ├── 03_create_reconfig_network_figures.R
            ├── 04_create_comparison_figures.R
            ├── 05_create_manuscript_text_figures.R
            ├── 06_create_additional_manuscript_figures.R
            ├── 07_create_methods_example.R
        ├── compile_analysis.Rmd
    ├── data  
        ├── metadata
            ├── ace_aquatic_biodiversity_factsheet.pdf 
            ├── nhdplus_v2_user_guide.pdf
        ├── raw_data                                        # created via set-up script
        ├── processed_data                                  # created via set-up script
    ├── output                                              # created via set-up script
        ├── figures                                         # created via set-up script
    ├── packages_unpublished  
        ├── StrmAnlyzeR_forked.zip  
    ├── .gitignore  
    ├── LICENSE  
    ├── README.md  
```