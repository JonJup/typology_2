### -------------------------------- ###
### --- Combine and crop rivers  --- ### 
### -------------------------------- ###

# --------------- #
# date:  
#               22.02.21
# files in 
#               -> original Lin et al. 21 river network 
# files out
#               <- 01_rivers_cropped.rds 
# Project: 
#               New typology for Europe 
# Purpose:  
#               Combine single river river zones. 
#               Crop to study area. 
# --------------- #


# setup -----------------------------------------------
pacman::p_load(dplyr,
               magrittr,
               purrr,
               sf)

# load data -------------------------------------------

data = 
        "D://Arbeit/Data/Lin_et_al_21_Scientific_data/river_network_variable_Dd/" %>% 
        dir(full.names = TRUE) %>% 
        subset(grepl(x = ., pattern = "pfaf")) %>%  
        subset(grepl(x = ., pattern =  "_21.shp|_22.shp|_23.shp|_24.shp|_25.shp")) %>%
        map(.f = st_read) 

europe <- 
        st_read("D://Arbeit/Data/natural_earth/2020_06_29_europe.gpkg")

# prepare data ----------------------------------------
# same CRS 
st_crs(europe) == st_crs(data[[1]])

# crop to Europe
data2 <- 
        data %>% 
        map(.f = ~ st_crop(x = .x, 
                           y = europe))

# save to file ----------------------------------------------------------------------
saveRDS(data2, "data/01_rivers_cropped.rds")


