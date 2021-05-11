### -------------------- ###
### --- Add Ice Age  --- ### 
### -------------------- ###

# --------------- #
# date:  
#               06.05.21
# files in 
#               -> 02_w_wc.rds
# files out
#               <- 03_w_lgm.rds 
# Project: 
#               New typology for Europe 
# Purpose:  
#               Add ice age vegetation from Ray and Adams (2001). Internet Archaeology
# --------------- #


# setup -----------------------------------------------------------------------------
library(sf)
library(mapview)

# load data -------------------------------------------------------------------------
## -- vegetation at last glacial maximum 
lgm <- st_read("D://Arbeit/Data/Ray_and_Adams2001/world_cont.shp")
rivers <- readRDS(here::here("data/02_w_wc.rds"))

# prepare ---------------------------------------------------------------------------
lgm = st_set_crs(lgm, 4326)
lgm = lgm[, which(names(lgm) %in% c("VEG_ID", "geometry"))]
names(lgm)[1] = "lgm"
# intersect -------------------------------------------------------------------------
rivers = lapply(rivers, st_intersection, lgm)

# save to file  ---------------------------------------------------------------------
saveRDS(rivers, "data/03_w_lgm.rds")





