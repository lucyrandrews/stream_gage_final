## IMPORT AND CLEAN NHD AND WBD DATA

# This scripts downloads, imports, and cleans USGS Watershed Boundary Dataset
# (WBD) and National Hydrography Dataset (NHD) spatial products that represent
# watershed realizations and stream channel realizations, respectively

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
  select(huc12_id, huc12_name, huc4_group) %>%
  st_intersection(select(ca_boundary, geometry))



# Download NHD products ----

# download flowlines
nhd_plus_get(vpu = 18, component = "NHDSnapshot")

# download flowline attributes
nhd_plus_get(vpu = 18, component = "NHDPlusAttributes")

# download hydrologic and climatologic attributes
nhd_plus_get(vpu = 18, component = "EROMExtension")



# Read in and clean NHD products ----

# read in flowlines as sf object with geometry type LINESTRING
# then clip to California state boundary
flowlines <- nhd_plus_load(vpu = 18,
                           component = "NHDSnapshot",
                           dsn = "NHDFlowline") %>%
  st_transform(crs = global_crs) %>%
  st_intersection(select(huc2, geometry))

# read in value-added attributes as a dataframe
vaa <- nhd_plus_load(vpu = 18,
                     component = "NHDPlusAttributes",
                     dsn = "PlusFlowlineVAA")

# extended unit runoff method average monthly flows as a dataframe
erom_extension <- nhd_plus_load (vpu = 18,
                                 component = "EROMExtension",
                                 dsn = "EROM_MA0001")

# create a single flowlines sf object with all attributes and drop flowlines
# that do not have necessary extended attributes (e.g. total upstream
# drainage area)
flowlines <- flowlines %>%
  select(-FDATE, -LENGTHKM, -REACHCODE, -FCODE) %>%
  left_join(vaa, by = c("COMID" = "ComID")) %>%
  left_join(select(erom_extension, -AreaSqKm, -DivDASqKm),
            by = c("COMID" = "ComID")) %>%
  rename(est_discharge_cfs = Q0001E) %>%
  rename_with(.fn = tolower, .cols = everything()) %>%
  filter(!is.na(totdasqkm),
         !ftype %in% drop_ftypes,
         totdasqkm > drop_min_da_sqkm)

# specify columns to keep for analysis
keep_flowlines_colnames <- c("comid", "ftype", "streamorde", "fromnode",
                             "tonode", "hydroseq", "levelpathi", "pathlength",
                             "terminalpa", "arbolatesu", "divergence",
                             "startflag", "terminalfl", "dnlevel", "uplevelpat",
                             "uphydroseq", "dnlevelpat", "dnminorhyd",
                             "dnhydroseq", "frommeas", "tomeas", "reachcode",
                             "lengthkm", "totdasqkm", "est_discharge_cfs")

flowlines <- flowlines %>%
  select(all_of(keep_flowlines_colnames))

# clean up
rm(vaa, erom_extension, keep_flowlines_colnames)



# Associate flowlines with HUC12s ----

flowlines_midpoints <- st_line_midpoints(flowlines)

flowlines_midpoints_huc12s <- st_join(flowlines_midpoints,
                                      huc12s)

flowlines_midpoints <- flowlines_midpoints %>%
  cbind(st_drop_geometry(flowlines_midpoints_huc12s))

flowlines <- flowlines %>%
  cbind(st_drop_geometry(flowlines_midpoints_huc12s))

# clean up
rm(flowlines_midpoints_huc12s)



