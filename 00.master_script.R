############################################################
# CovidHIV_NYC_ATL_data
#
# Sourcing this script runs the entire project
#
############################################################

# Weeks
# -----------------------------------------------
# Weeks begin on Sundays
#
#   Pre-COVID weeks:  1 =       2019-09-01 to 2019-09-07
#                    26 =       2020-02-23 to 2020-02-29
#   COVID weeks:     27 = C 1 = 2020-03-01 to 2020-03-07
#                    78 = C52 = 2021-02-21 to 2021-02-27

#   Pre=1:26, Q1=27:39, Q2=40:52, Q3=53:65, Q4=66:78

# Clean house
  rm(list=ls())

# Load packages
  library(tidyverse)
  library(magrittr)
  library(vioplot)
  library(readxl)

# Run project
  source("01_process_IQVIA_data.R")
  source("02_process_testing_data.R")
  source("03_process_covid_cases.R")
  source("04_process_AMIS_data.R")
  source("05_descriptive_plots.R")
  source("06_timeseries.R")

save.image()
