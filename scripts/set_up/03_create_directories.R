## CREATE DIRECTORIES

# This scripts creates directories needed for analysis and output.

# Directories ----

if(!dir.exists(here("data"))) {
  dir.create(here("data"))
}

if(!dir.exists(here("data", "metadata"))) {
  dir.create(here("data", "metadata"))
}

if(!dir.exists(here("data", "raw_data"))) {
  dir.create(here("data", "raw_data"))
}

if(!dir.exists(here("data", "processed_data"))) {
  dir.create(here("data", "processed_data"))
}

if(!dir.exists(here("output"))) {
  dir.create(here("output"))
}

if(!dir.exists(here("output", "figures"))) {
  dir.create(here("output", "figures"))
}