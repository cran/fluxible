## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(tidyverse.quiet = TRUE)

## ----message = FALSE----------------------------------------------------------
library(tidyverse)
# readr is part of tidyverse, and since we will also use dplyr
# we might as well load tidyverse

raw_conc <- read_delim(
  "ex_data/26124054001.#00",
  delim = ",", # our file is comma separated
  skip = 25 # the first 25 rows are logger infos that we do not want to keep
)

# let's see
head(raw_conc)

## -----------------------------------------------------------------------------
library(lubridate) # lubridate is what you want to deal with datetime issues

raw_conc <- raw_conc |>
  rename(
    co2_conc = "CO2_calc (ppm)"
  ) |>
  mutate(
    Date = dmy(Date), # to transform the date as a yyyy-mm-dd format
    datetime = paste(Date, Time), # we paste date and time together
    datetime = as_datetime(datetime) # datetime instead of character
  ) |>
  select(datetime, co2_conc)

head(raw_conc) # Et voila!

## ----message = FALSE----------------------------------------------------------

raw_conc <- read_delim(
  "ex_data/26124054001.#00",
  delim = ",", # our file is comma separated
  skip = 26, # removing the first 25th row and the header
  col_select = c(1, 2, 6),
  col_names = c("date", "time", rep(NA, 3), "co2_conc", NA)
)
head(raw_conc)

## ----message = FALSE----------------------------------------------------------
library(fs)

raw_conc <- dir_ls( #listing all the files
  "ex_data", # at location "ex_data"
  regexp = "*CO2*" # that contains "CO2" in their name
) |>
  map_dfr(
    read_csv, # we map read_csv on all the files
    na = c("#N/A", "Over") # "#N/A" and Over should be treated as NA
  ) |>
  rename(
    conc = "CO2 (ppm)",
    datetime = "Date/Time"
  ) |>
  mutate(
    datetime = dmy_hms(datetime)
  ) |>
  select(datetime, conc)

head(raw_conc)

## -----------------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(fs)

raw_conc <- dir_ls( #listing all the files
  "ex_data/field_campaign" # at location "ex_data/field_campaign"
) |>
  map_dfr( # we map read_tsv on all the files
    read_tsv, # read_tsv is for tab separated value files
    skip = 3,
    id = "filename" # column with the filename, that we can use as flux ID
  ) |>
  rename( # a bit of renaming to make the columns more practical
    co2_conc = "CO2 (umol/mol)",
    h2o_conc = "H2O (mmol/mol)",
    air_temp = "Temperature (C)",
    pressure = "Pressure (kPa)"
  ) |>
  mutate(
    datetime = paste(Date, Time),
    datetime = as.POSIXct(
      datetime, format = "%Y-%m-%d %H:%M:%OS"
    ), # we get rid of the milliseconds
    pressure = pressure / 101.325, # conversion from kPa to atm
    filename = substr(filename, 24, 70) # removing folder names
  ) |>
  select(datetime, co2_conc, h2o_conc, air_temp, pressure, filename)

head(raw_conc)

## ----message = FALSE, warning = FALSE-----------------------------------------

raw_conc <- read_csv( # read_csv is the same as read_delim(delim = ",")
  "ex_data/011023001.#01",
  col_types = "Tcdddddd",
  na = "#N/A" # we tell read_csv what NA look like in that file
)

head(raw_conc)

## ----message = FALSE, warning = FALSE-----------------------------------------
raw_conc <- read_csv(
  "ex_data/011023001.#01",
  skip = 1, # this time we skip the row with the column names
  col_names = FALSE, # and we tell read_csv that we do not provide column names
  na = "#N/A" # we tell read_csv what NA look like in that file
)

head(raw_conc)

## -----------------------------------------------------------------------------
# we read each row of our file as an element of a list
list <- readLines("ex_data/011023001.#01")
list <- list[-1] # removing the first element with the column names

# we first deal with the elements where we have those environmental data
# that were measured every 10 seconds
listenv <- list[seq(1, length(list), 10)]
env_df <- read.csv(
  textConnection(listenv), # we read the list into a csv
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
list_other <- list[-seq(1, length(list), 10)]
other_df <- read.csv(
  textConnection(list_other),
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
conc_df <- full_join(env_df, other_df, by = c("datetime", "co2_conc")) |>
  arrange(datetime) # I like my dataframes in chronological order
head(conc_df)

