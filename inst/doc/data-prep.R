## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(tidyverse.quiet = TRUE)

## ----single-file, message=FALSE-----------------------------------------------
library(tidyverse)
# readr, dplyr and lubridate are part of tidyverse

raw_conc <- read_delim(
  "ex_data/26124054001.#00",
  delim = ",", # our file is comma separated
  skip = 25 # the first 25 rows are logger infos that we do not want to keep
)

## ----rawconc-str1, echo=FALSE-------------------------------------------------


str(raw_conc, width = 70, strict.width = "cut", give.attr = FALSE)

## ----cols-correction----------------------------------------------------------
raw_conc <- raw_conc |>
  rename(
    co2_conc = "CO2_calc (ppm)"
  ) |>
  mutate(
    datetime = paste0(Date, Time), # we paste date and time together
    datetime = dmy_hms(datetime) # datetime instead of character
  ) |>
  select(datetime, co2_conc)

## ----rawconc-str2, echo=FALSE-------------------------------------------------


str(raw_conc, width = 70, strict.width = "cut", give.attr = FALSE)

## ----multiple-files, message=FALSE--------------------------------------------
library(tidyverse)

raw_conc <- list.files( # list the files
  "ex_data", # at location "ex_data"
  full.names = TRUE,
  pattern = "*CO2*" # that contains "CO2" in their name
) |>
  map_dfr(
    read_csv, # we map read_csv on all the files
    na = c("#N/A", "Over") # "#N/A" and Over should be treated as NA
  ) |>
  rename(
    conc = "CO2 (ppm)"
  ) |>
  mutate(
    datetime = dmy_hms(`Date/Time`)
  ) |>
  select(datetime, conc)

## ----one-file-one-flux, message=FALSE-----------------------------------------

library(tidyverse)


raw_conc <- list.files( #listing all the files
  "ex_data/field_campaign", # at location "ex_data/field_campaign"
  full.names = TRUE
) |>
  map_dfr( # we map read_tsv on all the files
    # read_tsv is the version of read_delim for tab separated value files
    read_tsv,
    skip = 3,
    # creates a column with the filename, that we can use as flux ID
    id = "f_fluxid"
  ) |>
  rename( # a bit of renaming to make the columns more practical
    co2_conc = "CO2 (umol/mol)",
    h2o_conc = "H2O (mmol/mol)",
    air_temp = "Temperature (C)",
    pressure = "Pressure (kPa)"
  ) |>
  mutate(
    .by = f_fluxid,
    f_datetime = paste(Date, Time),
    # we get rid of the milliseconds
    f_datetime = as.POSIXct(f_datetime, format = "%Y-%m-%d %H:%M:%OS"),
    pressure = pressure / 101.325, # conversion from kPa to atm
    f_fluxid = basename(f_fluxid), # removing folder names
    f_start = min(f_datetime), # start is the smallest datetime of each flux
    f_end = max(f_datetime) # end is the greatest datetime of each flux
  ) |>
  select(
    f_datetime, co2_conc, h2o_conc, air_temp, pressure, f_fluxid, f_start, f_end
  )

## ----rawconc-str3, echo=FALSE-------------------------------------------------


str(raw_conc, width = 70, strict.width = "cut", give.attr = FALSE)

## ----tricky, message=FALSE, warning=FALSE-------------------------------------
library(tidyverse)

raw_conc <- read_csv( # read_csv is the same as read_delim(delim = ",")
  "ex_data/011023001.#01",
  col_types = "Tcdddddd",
  na = "#N/A" # we tell read_csv what NA look like in that file
)

## ----rawconc-str4, echo=FALSE-------------------------------------------------


str(raw_conc, width = 70, strict.width = "cut", give.attr = FALSE)

## ----tricky2, warning=FALSE, message=FALSE------------------------------------
raw_conc <- read_csv(
  "ex_data/011023001.#01",
  skip = 1, # this time we skip the row with the column names
  col_names = FALSE, # we tell read_csv that column names are not provided
  na = "#N/A" # we tell read_csv what NA looks like in that file
)

## ----rawconc-str5, echo=FALSE-------------------------------------------------


str(raw_conc, width = 70, strict.width = "cut", give.attr = FALSE)

## ----tricky3------------------------------------------------------------------

# we read each row of our file as an element of a list
lines <- readLines("ex_data/011023001.#01")
lines <- lines[-1] # removing the first element with the column names

# we first deal with the elements where we have those environmental data
# that were measured every 10 seconds
linesenv <- lines[seq(1, length(lines), 10)]
env_df <- read.csv(
  textConnection(linesenv), # we read the list into a csv
  header = FALSE, # there is no header
  colClasses = rep("character", 14)
  # specifying that those columns are character is important
  # if read as integer, 06 becomes 6, and when putting columns together,
  # 400.06 will be read as 400.6, which is wrong
)

env_df <- env_df |>
  mutate(
    datetime = dmy_hms(V1),
    temp_air = paste(
      V7, # V7 contains the left side of the decimal point
      V8, # V8 the right side
      sep = "." # this time we put it in american format
    ),
    temp_air = as.double(temp_air), # now we can make it a double
    temp_soil = as.double(paste(V9, V10, sep = ".")),
    co2_conc = as.double(paste(V11, V12, sep = ".")),
    PAR = as.double(paste(V13, V14, sep = "."))
  ) |>
  select(datetime, temp_air, temp_soil, co2_conc, PAR)

# now we do the same with the other elements of the list
lines_other <- lines[-seq(1, length(lines), 10)]
other_df <- read.csv(
  textConnection(lines_other),
  header = FALSE,
  colClasses = rep("character", 10)
)

other_df <- other_df  |>
  mutate(
    datetime = dmy_hms(V1),
    co2_conc = as.double(paste(V8, V9, sep = "."))
  ) |>
  select(datetime, co2_conc)

# and finally we do a full join with both
conc_df <- bind_rows(env_df, other_df) |>
  arrange(datetime) # I like my dataframes in chronological order

## ----rawconc-str6, echo=FALSE-------------------------------------------------


str(conc_df, width = 70, strict.width = "cut", give.attr = FALSE)

