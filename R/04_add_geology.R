### -------------------- ###
### --- Add Geology  --- ### 
### -------------------- ###

# --------------- #
# date:  
#               07.05.21
# files in 
#               -> 03_w_lgm.rds
# files out
#               <- 04_w_geo.rds 
# Project: 
#               New typology for Europe 
# Purpose:  
#               Add Geology (IHME v11) from Cornu et al (2013). 
# --------------- #


# setup -----------------------------------------------------------------------------
library(sf)


# load data -------------------------------------------------------------------------
## -- vegetation at last glacial maximum 
geo    <- st_read("D://Arbeit/Data/IHME1500_v11/ihme_1500_litho4changed.shp")
rivers <- readRDS(here::here("data/03_w_lgm.rds"))

# prepare ---------------------------------------------------------------------------
geo = st_transform(geo, crs = st_crs(rivers[[1]]))
geo = geo[, which(names(geo) %in% c("acid_basic", "geometry"))]
names(geo)[1] = "geo"

# intersect -------------------------------------------------------------------------
rivers = lapply(rivers, st_intersection, geo)

# check for NAs ---------------------------------------------------------------------
lapply(rivers, function(x)any(is.na(x$geo)))

# save to file  ---------------------------------------------------------------------
saveRDS(rivers, "data/04_w_geo.rds")
