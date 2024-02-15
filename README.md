# Efficient stream gauging networks

This repository serves two functions:

1. Provides a code workflow to illustrate how network coverage analysis was
performed for the State Water Resources Control Board's 2022 [Stream Gaging Plan](https://www.waterboards.ca.gov/waterrights/water_issues/programs/stream_gaging_plan/).
[Lucy Andrews](https://www.lucyrandrews.com/about) and [Dr. Theodore
Grantham](https://ourenvironment.berkeley.edu/people/theodore-grantham)
supported the computational work underlying the plan. The plan was created in response
to [California Senate Bill 19](https://leginfo.legislature.ca.gov/faces/billTextClient.xhtml?bill_id=201920200SB19).

2. Publish code used in a manuscript currently under review for possible
publication in _Nature Sustainability_.

**Abstract**

Designing efficient stream gauging networks for twenty-first century water management

_Stream gauging stations provide critical information to water managers, but biases and gaps in gauge placement, compounded by gauge deactivations, limit our ability to track river flows and address global water challenges. Here we introduce a novel approach for identifying gauging sites that efficiently fill gaps in coverage within river networks while also addressing water management priorities, including reservoir operations, biodiversity conservation, and hydroclimatic monitoring. Applying this approach to gauges in California, USA, we found significant gaps in the stream monitoring network. Hypothetically reconfiguring gauges to locations that maximize coverage and representation of management objectives highlights the current network’s biases. Through the strategic reactivation and placement of additional gauges, we demonstrate how stream gauging networks can be designed to support sustainable water management._ 

<br>
<br>

![flow chart of methods](https://github.com/lucyrandrews/stream_gage_final/blob/main/data/metadata/methods_flowchart.png "flow chart of methods")

<br>
<br>
## Repository Structure

The code workflow is structured for scripts to be run in sequence. A user should
run the file `compile_analysis.Rmd`, which does the following:

1. Sets up the environment
2. Downloads, imports, and cleans data that represent the stream network, stream
gages, and management objectives (gaging dams, critical biodiversity, reference-quality
streams, and natural communities commonly associated with groundwater)
3. Associates stream network segments with attributes relevant to management objectives
4. Analyzes network coverage provided by gages, both extant and hypothetical
5. Evaluates the coverage that the current gage network provides of management
objectives
6. Runs a greedy set cover algorithm on potential gage locations to identify
the most efficient set of gages that would cover the entire network for two scenarios
(network expansion and network reconfiguration)
7. Ranks efficient gages by the coverage of management objectives that gages
would provide.
8. Summarizes and visualizes results
9. Creates example figures that illustrate methods


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
        ├── compile_analysis.Rmd                            # key compilation script
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

## End Notes

Different entities spell "gauge" differently. Publishers tend to prefer "gauge",
while American water management agencies prefer "gage". We use "gage" in our
scripts (since they were originally conceived to support State Water Resources
Control Board activities) but "gauge" in our writing. Apologies for the slight
confusion this may create!