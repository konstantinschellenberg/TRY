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

packages = c("ggplot2", "plotly", "gridExtra", "grid", "data.table", "dplyr", "readr", "purrr", "sf")
lapply(packages, require, character.only = TRUE)

library(stringr)
library(tidyr)


# ------------------------------------------------------
# read

tsv = "D:/Projects/TRY_exploration/data/request/13377.txt"
readin = read_tsv(tsv)

dt = setDT(readin)

# ------------------------------------------------------
# Metainformation about the entire dataset

dt.size = dt[, .N, by = DatasetID]
dt.headers = names(dt)
names(dt)

#' DataID 59 = Latitude, DataID 60 = Longitude
#' DataID 61 = Altitude
#' DataID 6601 = measurement date

dt[DataID == 59 | DataID == 60]

# ------------------------------------------------------
# Querying a single dataset to check accessablilty

# get a dummy subset, J. Cornelissen
(optbefore = getOption(x = "datatable.print.nrows"))
options("datatable.print.topn" = 10)
options("datatable.print.class" = TRUE)
options("datatable.print.trunc.cols" = FALSE)

# get dataset
nr.ds = 1
ds1 = dt[DatasetID == nr.ds][order(ObservationID)]

# column headers
names(ds1)
# metainformation
print(head(ds1[, .(Dataset, FirstName, LastName, DatasetID)], 1))
# total observations
ds1[, .N, by = ObservationID] %>% nrow()

# trunc less important columns
ds1[, -c("Reference", "Comment", "X28", "LastName", "FirstName", "Dataset", "UncertaintyName", "Replicates")]
# get most important
ds1.subset = ds1[, .(ObservationID, ObsDataID, TraitID, DataID, DataName, StdValue)]

# how many data points for each observation?
ds1.subset[, .N, by = ObservationID]

# observations containing spatial ID (4th digits precise -> canopy can be trusted)
ds1.subset[DataID == 59 | DataID == 60, by = ObservationID, StdValue]
dp = ds1.subset[DataID == 59 | DataID == 60, by = ObservationID, StdValue][1,2]
char = as.character(abs(dp))

# get numeric precision (base)
nchar(sub(".", "", char, fixed=TRUE))
vec = ds1.subset[DataID == 59 | DataID == 60, StdValue]
vec[1] = NA_real_

# TODO: Make precision function detecting periods (recurring real number endings)

# get numeric precision (stringr)
str_precision = function(numvec){
    #' vectorise with purrr
    vec = map_dbl(numvec, function(numeric){
        #cat(numeric)
        char = as.character(abs(numeric))
        split = str_split_fixed(char, pattern = "\\.", n = 2)[2]
        #cat(split)
        return(str_length(split))
    })
    return(vec)
}

# test if working
str_precision(151.11333)
str_precision(-33.76972784)
vec[1]
str_precision(vec[1])
vec[20]
str_precision(vec[20])
str_precision(vec)
str_precision(NA)

# implement in data.table structure
ds1.subset[DataID == 59 | DataID == 60, .(str_precision(StdValue), DataID, StdValue), by = ObservationID]

# ----------------------------------------------------
#' 1. Querying if spatial index is existing, than only choose those more precise than 3 digits lat & lon with feature of spatial precision
#' 2. Querying sptially (Amazon, Thuringia, South Africa)
# ----------------------------------------------------
# QUERYING THE SPATIAL INDEX

# id = 19375
# grouping by Observation (creating index at last)
#' takes a second to run!
dt.group.precision = dt[DataID == 59 | DataID == 60, .(precision = str_precision(StdValue), DatasetID, DataID, StdValue), by = ObservationID]

# subsets of precison
cat("1. all datasets:", sep = "\n")
(nrow.all = dt.group.precision[precision >= 0 , .N, by = ObservationID] %>% nrow())
cat("2. Non-spatial datasets (or with precision 0):", sep = "\n")
(nrow.nonspatial = dt.group.precision[precision == 0 , .N, by = ObservationID] %>% nrow())
cat("3. datasets of with any spatial reference:", sep = "\n")
(nrow.anyspatial = dt.group.precision[precision > 0 , .N, by = ObservationID] %>% nrow())
cat("4. datasets more precise than 3 digits (yet not cleared if lat and lon have a similar precision:", sep = "\n")
(nrow.spatialmore3unprecise = dt.group.precision[precision > 3 , .N, by = ObservationID] %>% nrow())

# get unique observations with geographic information more precise than 3 digits for both measurements, Lat and Lon
dt.group.precision[precision > 3, .N, by = ObservationID][N > 1]
index = dt.group.precision[precision > 3, .N, by = ObservationID][N > 1, ObservationID]

# lengths
cat("Both spatial indizes precise:", sep = "\n")
(nrow.spatialmore3precise = length(index))
cat("Only one spatial indix precise (buggy):", sep = "\n")
nrow.spatialmore3unprecise - nrow.spatialmore3precise
# example for observation where one is more precise than the other (numerically nonsense thus) ->
# Measure to clean the data from spatially unprecise observations
cat("Example for unprecise measurement")
dt.group.precision[ObservationID == 19375]
dt.group.precision[ObservationID == 19382]

# Indexing with the precision filter
dt.spatial = dt[ObservationID %in% index]
nrow(dt)
nrow(dt.spatial)
nrow(dt) - nrow(dt.spatial)

# 23 -> 8 mio. data rows

# -------------------------------------------------------
# QUERYING BY SPATIAL INDEX USING GEOGRAPHIC AREAS

# in lat lon
library(osmdata)
bb1 = getbb("Thuringia")
bb2 = getbb("Amazon")
bb3.in = st_read("D:/Projects/TRY_exploration/data/ancillary/Boundaries.gpkg") %>% st_bbox()

bb3.early = as.matrix(bb3.in)
bb3 = rbind(bb3.early[1:2,1], bb3.early[3:4,1]) %>% t()
rownames(bb3) = c("x", "y")
colnames(bb3) = c("min", "max")

bboxes = list(bb1, bb2, bb3)
names(bboxes) = c("Thuringia", "Amazon", "SouthAfrica")

#TODO: Write wrapper function with query by the spatial indizes

# (end)