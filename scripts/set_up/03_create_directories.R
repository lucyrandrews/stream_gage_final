## CREATE DIRECTORIES

# This scripts creates directories needed for analysis and output.

# Directories ----

if(!dir.exists(here("metadata"))) {
  dir.create("metadata")
}

if(!dir.exists(here("raw_data"))) {
  dir.create(here("raw_data"))
}

if(!dir.exists(here("output_data"))) {
  dir.create("output_data")
}

if(!dir.exists("output_figures")) {
  dir.create("output_figures")
}