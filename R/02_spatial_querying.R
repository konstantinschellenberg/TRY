# Title     : Importing the TRY Database request 13377
# Created by: Konsti
# Created on: 23.01.2021

# Information on the request:

#' TRY Data Request 13377
#' Only public data were requested.
#' Title:
#' Modelling Plant Traits using Sythetic Aperture Radar (SAR) and optical remote sensing techniques.
#' Authors:
#' Konstantin Schellenberg (University of Jena) PI Marcel Urban (University of Jena) Trait List:
#' 2, 13, 14, 15, 21, 38, 42, 46, 47, 48, 56, 197, 410, 413, 773, 3106, 3107, 3117, 3120, 3122, 3395, 3396, 3397, 3398, 3399, 3441, 3445, 3446, Species List:
#'
#' Description:
#' The study shall explore the relationship of plant traits to remote sensing data. Mainly structural parameters are assessed due to an expected sensitivity of SAR sensors to plant morphology. The study areas are Thuringia, South Africa and the Amazon.

packages = c("ggplot2", "plotly", "gridExtra", "grid", "data.table", "dplyr", "readr", "purrr", "sf", "ggthemes")
lapply(packages, require, character.only = TRUE)

library(stringr)
library(tidyr)

source("./R/_wrapper_functions.R")
source("./R/_constants.R")

theme_set(theme_tufte())

# ------------------------------------------------------
# data.table options

# get a dummy subset, J. Cornelissen
(optbefore = getOption(x = "datatable.print.nrows"))
options("datatable.print.topn" = 10)
options("datatable.print.class" = TRUE)
options("datatable.print.trunc.cols" = FALSE)
options("digits" = 5)

# for tidyr tables
options(pillar.sigfig=6)



# ------------------------------------------------------
# read

tsv = "D:/Projects/TRY/data/develop/dt_spatial.RDS"
dt = readRDS(tsv)

# ------------------------------------------------------
# Metainformation about the entire dataset

# 10er log transformation
(dt.size = dt[, .(logN = log10(.N), .N, .GRP), by = DatasetID])
names(dt.size)

ggplot(dt.size, aes(as.factor(DatasetID), logN)) +
    geom_bar(stat = "identity")
ggplot(dt.size, aes(logN)) +
    geom_histogram(bins = 20) +
    geom_density(position = "identity")

# available traits
unique(dt[, TraitName])
names(dt)

# largest (interpretation):
10^6.753090


# -------------------------------------------------------
# WRAPPING DATA TO TIDY FORMAT
#dt.group.precision = dt[DataID == 59 | DataID == 60, .(precision = str_precision(StdValue), DatasetID, DataID, StdValue), by = ObservationID]

# dummy widen on subset
nr.ds = 170
ds1 = dt[DatasetID == nr.ds][order(ObservationID)]

# nr of observations in that subset dataset
ds1[, .N, by = ObservationID] %>% nrow()
names(ds1)

# how to discriminate DataID and TraitID?
(ds1.subset = ds1[, .(ObservationID, ObsDataID, TraitID, DataID, DataName, StdValue)])

# DataName is not a variable, making them names of variables
(ds1.wider = ds1.subset %>% pivot_wider(id_cols = "ObservationID", names_from = "DataID", values_from = "StdValue"))

widen_data = function(data.table){
    require(tidyr)
    wider = pivot_wider(data.table, id_cols = c("ObservationID", "Dataset"),  names_from = "DataID",
                        values_from = "StdValue", names_sort = TRUE)
    return(as.data.table(wider))
}

# widen whole dataset
dt.wide = widen_data(dt)
names(dt.wide)
ncol(dt.wide)
dt.wide[1:5, 1:5]

saveRDS(dt.wide, file = "./data/develop/dt_sp_wide.RDS")

# -------------------------------------------------------
# Query only these observations which are actual traits!
# only get columns which have actually been requested to TRY

dt.wide = readRDS(file = "./data/develop/dt_sp_wide.RDS")

# make a dictionary to find DataID needed to select widened columns
#dt.notNAtrait = dt[!is.na(TraitID), .(DataID, TraitID, TraitName, DataName)]
(dt.dict = dt[!is.na(TraitID), .(DataID, TraitID, TraitName, DataName)] %>% unique())

#dt.notNAtrait[fifelse(!is.na(TraitID), as.integer(DataID), NA_integer_, na = NA_integer_) ,.(DataID, TraitID)]

# check ordinal data
dt[DataID == 2330 | DataID == 2331, .(DataName, StdValue)] %>% unique()
dt[TraitID == 42, DataName] %>% unique()

# all widened cols
(dt.names = names(dt.wide))

# view our dictionary
dt.dict[, .(DataID, TraitID, TraitName)]
cols = dt.dict[,DataID]

# TODO: categorical/ordinal data to factor of one trait! (Many do this before widening)
# TODO: There are lat and lon estimated columns... how to deal with that?
# TODO: select growth forms shrub, trees, small tree etc.
# TODO: categorical/ordinal data to factor of one trait! (Many do this before widening)

# translate TraitID to DataID in order to query necessary information, 59, 60, 61 are geoms
selected_cols = cols[cols %in% dt.names] %>% as.character()
selected_cols = selected_cols %>% c("ObservationID", "Dataset", "59", "60", "61", "6601", .)

# select
dt.selected = dt.wide[, ..selected_cols]
dt.selected[, 1:20]

# make pretty colnames
# wrapper for nice long variable names -> stackoverflow
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
      sep="", collapse=" ")
}
traitnames = dt.dict[, TraitName]
datanames = dt.dict[, DataName]
traitIDs = dt.dict[, TraitID]
dataIDs = dt.dict[, DataID]

make_clean_vars = function(charvec){
    require(janitor)
    cleaned = janitor::make_clean_names(charvec, sep_out = " ")
    capitalised = map_chr(cleaned, ~ simpleCap(.x))
    abbrev = abbreviate(capitalised)
    return(abbrev)
}
tn = make_clean_vars(traitnames)
dn = make_clean_vars(datanames)
length(dn)
nrow(dt.dict)

new_colnames = c("ObservationID", "Dataset", "Lat", "Lon", "Alt", "Date", dn)
new_colnames = c("ObservationID", "Dataset", "Lat", "Lon", "Alt", "Date", dataIDs)

setnames(dt.selected, new = new_colnames)
names(dt.selected)
nrow(dt.selected)

# Quick-Dict
names(dataIDs) = datanames
print(dataIDs)

# LOOKUPTABLES
dt.dict[, .(DataID, DataName)] %>% cbind(dn)
dt.dict[, .(TraitID, TraitName)]
dt.dict[, .(DataID, TraitName)]
dt[, .N, by = .(DataID, DataName, TraitID, TraitName)][order(-N)]

# get specific leaf area
dt.selected[, "6584", "2261"]

#DataID = 6584 -> Specific Area
#DataID = 2261 -> Leaf Water Content
#DataID = 448 -> Plant height vegetative


saveRDS(dt.selected, file = "./data/develop/dt_sp_wide_dataName.RDS")

# -------------------------------------------------------
# read in widened data with data names as headers

dt.selected = readRDS(file = "./data/develop/dt_sp_wide_dataName.RDS")
#dt.wide = readRDS("./data/develop/dt_sp_wide.RDS")

# nr of NA in columns
nas = dt.selected[, lapply(.SD, function(x) sum(!is.na(x)))]
nas.tibble = nas %>% t() %>% as_tibble() %>% arrange(desc(V1)) %>% print(n = 200)

# trim dataset to assess geometry
dt.trim = dt.selected[, .(ObservationID, Dataset, Lon, Lat, Alt, Date, SLA = `6584`, WC = `2261`, CH =  `448`)]
names(dt.trim)

# check NA (as fraction)
dt.trim[, lapply(.SD, function(x) sum(is.na(x)) / .N)]

# -------------------------------------------------------
# GETTING BOUNDING BOXES

# in lat lon
sf1 = st_read("D:/Projects/TRY/data/ancillary/Boundaries.gpkg", layer = "Middle_Germany")
sf2 = st_read("D:/Projects/TRY/data/ancillary/Boundaries.gpkg", layer = "Amazon")
sf3 = st_read("D:/Projects/TRY/data/ancillary/Boundaries.gpkg", layer = "South_Africa")

st_bbox_to_matrix = function(sf){
    #' extracts a matrix of the sf bbox
    mat = sf %>%
        st_bbox() %>%
        as.matrix()
    mat = rbind(mat[1:2,1], mat[3:4,1]) %>% t()
    rownames(mat) = c("x", "y")
    colnames(mat) = c("min", "max")
    return(mat)
}

st_bbox_to_matrix(sf1)

# -------------------------------------------------------
# QUERYING BY SPATIAL INDEX USING GEOGRAPHIC AREAS

#' DataID 59 = Latitude, DataID 60 = Longitude
#' DataID 61 = Altitude
#' DataID 6601 = measurement date

# TODO: Write wrapper function with query by the spatial indizes

# remove geographic dublicates
dt.unique = unique(dt.trim, by = c("Lon", "Lat"))

# convert to sf
sf = st_as_sf(dt.trim, coords = c("Lon", "Lat")) %>% st_set_crs(4326)
st_write(sf, dsn = "./data/develop/sf_observations.gpkg", layer = "allObservations", append = FALSE)

ggplot(sf) +
    geom_sf(aes(size = SUIPIOE))

ggplot(dt.trim, aes(Lat, SLA)) + geom_point()
ggplot(dt.trim, aes(Lat, WC)) + geom_point()
ggplot(dt.trim, aes(Lat, CH)) + geom_point()

ggplot(dt.trim, aes(SLA)) + geom_histogram(bins = 300) + xlim(c(0, 100))
ggplot(dt.trim, aes(WC)) + geom_histogram(bins = 300) + xlim(c(0, 25))
ggplot(dt.trim, aes(CH)) + geom_histogram(bins = 300) + xlim(c(0, 30))

# scatter
ggplot(dt.trim, aes(SLA, WC)) +
    geom_point(aes(color = Lat))

# (end)