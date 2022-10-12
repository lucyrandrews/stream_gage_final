## IMPORT AND CLEAN NHD AND WBD DATA

# This scripts downloads, imports, and cleans USGS Watershed Boundary Dataset
# (WBD) and National Hydrography Dataset (NHD) spatial products that represent
# watershed realizations and stream channel realizations, respectively. It also
# reads in gage data from either the USGS GAGESII dataset embedded in the NHD
# or from The Nature Conservancy's 2019 Gage Gap analysis.

# Create directories ----

# create WBD directory
if(!dir.exists(here("data", "raw_data", "wbd"))) {
  dir.create(here("data", "raw_data", "wbd"))
}



# Download WBD ----

# download WBD as a zipped file
if(!file.exists(here("data", "raw_data", "wbd", "wbd_18.zip"))) {
  
  # HUC2 #18 is California (not state boundary - hydrologic boundary)
  wbd_18_url <- "https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/WBD/HU2/Shape/WBD_18_HU2_Shape.zip"
  
  download.file(url = wbd_18_url,
                destfile = here("data", "raw_data", "wbd", "wbd_18.zip"))
  
  rm(wbd_18_url)
  
}

# unzip WBD zipped file
unzip(zipfile = here("data", "raw_data", "wbd", "wbd_18.zip"),
      exdir = here("data", "raw_data", "wbd"))



# Import and clean WBD sf objects ----

# read in HUC2 as an sf object
huc2 <- st_read(dsn = here("data", "raw_data", "wbd", "Shape"),
                layer = "WBDHU2") %>%
  st_transform(crs = global_crs) %>%
  rename(huc2_id = huc2,
         huc2_name = name) %>%
  select(huc2_id, huc2_name) %>% 
  st_intersection(select(ca_boundary, geometry))

# read in HUC4s as an sf object
huc4s <- st_read(dsn = here("data", "raw_data", "wbd", "Shape"),
                 layer = "WBDHU4") %>%
  st_transform(crs = global_crs) %>%
  rename(huc4_id = huc4,
         huc4_name = name) %>%
  select(huc4_id, huc4_name) %>%
  st_intersection(select(ca_boundary, geometry))

# read in HUC12s as an sf object
huc12s <- st_read(dsn = here("data", "raw_data", "wbd", "Shape"),
                  layer = "WBDHU12") %>%
  st_transform(crs = global_crs) %>%
  rename(huc12_id = huc12,
         huc12_name = name) %>%
  mutate(huc4_group = str_sub(huc12_id, 1, 4)) %>%
  left_join(st_drop_geometry(huc4s), by = c("huc4_group" = "huc4_id")) %>%
  select(huc12_id, huc12_name, huc4_group, huc4_name) %>%
  st_intersection(select(ca_boundary, geometry))



# Download NHD products ----

# download flowlines
# note: Sept 7 - NHD API returns 403 error code (unauthorized) so adding `try`
# wrapper; if running code for first time, will hit issues if files aren't
# already locally available
try(nhd_plus_get(vpu = 18, component = "NHDSnapshot"), silent = FALSE)

# download flowline attributes
try(nhd_plus_get(vpu = 18, component = "NHDPlusAttributes"), silent = FALSE)

# download hydrologic and climatologic attributes
try(nhd_plus_get(vpu = 18, component = "EROMExtension"), silent = FALSE)

try(closeAllConnections())



# Read in and clean NHD products ----

# read in flowlines as sf object with geometry type LINESTRING
# then clip to California state boundary
flowlines <- nhd_plus_load(vpu = 18,
                           component = "NHDSnapshot",
                           dsn = "NHDFlowline") %>%
  st_transform(crs = global_crs) %>%
  st_intersection(select(huc2, geometry)) %>%
  mutate(COMID = as.character(COMID))

# read in value-added attributes as a dataframe
vaa <- nhd_plus_load(vpu = 18,
                     component = "NHDPlusAttributes",
                     dsn = "PlusFlowlineVAA") %>%
  mutate(ComID = as.character(ComID))

# extended unit runoff method average monthly flows as a dataframe
erom_extension <- nhd_plus_load (vpu = 18,
                                 component = "EROMExtension",
                                 dsn = "EROM_MA0001") %>%
  mutate(ComID = as.character(ComID))

# create a single flowlines sf object with all attributes and drop flowlines
# that do not have necessary extended attributes (e.g. total upstream
# drainage area)
flowlines <- flowlines %>%
  select(-FDATE, -LENGTHKM, -REACHCODE, -FCODE) %>%
  left_join(vaa, by = c("COMID" = "ComID")) %>%
  left_join(select(erom_extension, -AreaSqKm, -DivDASqKm),
            by = c("COMID" = "ComID")) %>%
  rename(est_discharge_cfs = Q0001E) %>%
  # 1 cfs = 723.968 af/year
  mutate(est_annual_discharge_af = est_discharge_cfs * 723.968) %>%
  rename_with(.fn = tolower, .cols = everything()) %>%
  filter(!is.na(totdasqkm),
         !ftype %in% drop_ftypes,
         totdasqkm > flowlines_min_dasqkm)

# specify columns to keep for analysis
keep_flowlines_colnames <- c("comid", "ftype", "streamorde", "gnis_name",
                             "fromnode", "tonode", "hydroseq", "levelpathi",
                             "pathlength", "terminalpa", "arbolatesu",
                             "divergence", "startflag", "terminalfl", "dnlevel", 
                             "uplevelpat", "uphydroseq", "dnlevelpat",
                             "dnminorhyd", "dnhydroseq", "frommeas", "tomeas",
                             "reachcode", "lengthkm", "totdasqkm",
                             "est_discharge_cfs", "est_annual_discharge_af")

# keep only specified columns
flowlines <- flowlines %>%
  select(all_of(keep_flowlines_colnames))

# import and clean dataframe of flowline comid to-from relationships
comids_to_from <- nhd_plus_load(vpu = 18,
                                component = "NHDPlusAttributes",
                                dsn = "PlusFlow") %>%
  select(FROMCOMID, TOCOMID) %>%
  mutate(FROMCOMID = as.character(FROMCOMID),
         TOCOMID = as.character(TOCOMID))

# clean up
rm(vaa, erom_extension, keep_flowlines_colnames, flowlines_min_dasqkm)



# Create flowlines midpoints and associate flowlines with HUC12s ----

# evalute midpoints and then bind comids to midpoint geometry
flowlines_midpoints <- st_line_midpoints(flowlines)

# associate comid midpoints with HUC12s via spatial intersection
flowlines_midpoints<- st_join(flowlines_midpoints,
                              huc12s) %>%
  cbind(flowlines$comid) %>%
  rename(comid = flowlines.comid)

# associate flowlines with HUC12s based on midpoint associations
flowlines <- flowlines %>%
  left_join(st_drop_geometry(flowlines_midpoints), by = "comid")



# Identify HUC12 outlet comids ----

# identify the comid at each HUC12's outlet using the minimum hydrosequence 
# value in each HUC12
huc12_outlet_comids <- flowlines %>%
  st_drop_geometry() %>%
  group_by(huc12_id) %>%
  summarize(min_hydroseq = min(hydroseq),
            outlet_comid = comid[which.min(hydroseq)])

# flag outlet comids in the flowlines dataset
flowlines <- flowlines %>%
  mutate(huc12_outlet = comid %in% huc12_outlet_comids$outlet_comid)

# clean up
rm(huc12_outlet_comids)



# Import gage data ----

# if not using TNC data, load GAGESII data from NHD
if(!tnc_data) {
  
  gages <- get_gagesII(AOI = ca_boundary) %>%
    rename(gage_id = id,
           gage_totdasqkm = drain_sqkm) %>%
    rowid_to_column(var = "gage_index")
  
} else { # otherwise, download and import TNC gage database
  
  # create TNC gage directory
  if(!dir.exists(here("data", "raw_data", "tnc_gages"))) {
    dir.create(here("data", "raw_data", "tnc_gages"))
  }
  
  # download TNC gage data as zipped file
  if(!file.exists(here("data", "raw_data", "tnc_gages", "GageGapII_DataDownload.zip"))) {
    
    tnc_url <- "https://www.dropbox.com/s/u6gabpthge8h22v/GageGapII_DataDownload.zip?dl=1"
    
    download.file(url = tnc_url,
                  destfile = here("data", "raw_data", "tnc_gages", "GageGapII_DataDownload.zip"))
    
    rm(tnc_url)
    
  }
  
  # unzip TNC gages zipped file
  unzip(zipfile = here("data", "raw_data", "tnc_gages", "GageGapII_DataDownload.zip"),
        exdir = here("data", "raw_data", "tnc_gages"))
  
  # import TNC gages as sf object
  gages <- st_read(here("data", "raw_data", "tnc_gages", "Gages_GGII.shp")) %>%
    st_transform(crs = global_crs) %>%
    filter(GageStyle == "Active") %>%
    rename(gage_totdasqkm = TotDASqKM) %>%
    rowid_to_column(var = "gage_index")
  
}


# join in hydrologic regions that match flowline hydrologic region coding
gages <- gages %>%
  mutate(HUC4 = substr(HUC8, 1, 4)) %>%
  left_join(st_drop_geometry(flowlines) %>% select(huc4_group, huc4_name) %>% distinct(),
            by = c("HUC4" = "huc4_group")) %>%
  mutate(huc4_group = replace_na(huc4_name, "North Lahontan"))