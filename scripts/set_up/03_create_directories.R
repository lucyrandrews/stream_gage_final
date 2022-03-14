## CREATE DIRECTORIES

# This scripts creates directories needed for analysis and output.

# Directories ----

if(!dir.exists(here("metadata"))) {
  dir.create(here("metadata"))
}

if(!dir.exists(here("raw_data"))) {
  dir.create(here("raw_data"))
}

if(!dir.exists(here("output_data"))) {
  dir.create(here("output_data"))
}

if(!dir.exists(here("output_figures"))) {
  dir.create(here("output_figures"))
}