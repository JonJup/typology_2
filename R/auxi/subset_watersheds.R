### -------------------------- ###
### --- subset watersheds  --- ### 
### -------------------------- ###

# --------------- #
# date:  24.02.21
# files in 
        #-> watersheds.gdb 
# files out
        #<- GIVE NAME 
# new typology for Europe 
        # combine single river river zones. Crop to study area. 
# --------------- #


# setup -----------------------------------------------
pacman::p_load(sf, dplyr, purrr, magrittr, tmap)

# load data -------------------------------------------

## -- load watersheds 
data <- 
        "D://Arbeit/Data/Lin_et_al_21_Scientific_data/watersheds.gdb" %>% 
        st_read()

rivrs <- 
        "data/rivers_cropped.rds" %>% 
        readRDS

# prepare data ----------------------------------------
# not the same CRS 
st_crs(data) == st_crs(rivrs[[1]])
data2 = data
st_crs(data2) = st_crs(rivrs[[1]])
data %<>% st_transform(crs = st_crs(rivrs[[1]]))

riv_sub <- rivrs[[1]] %>% 
        filter(LINKNO == "5072990") 
riv_buf <- 
        riv_sub %>% 
        st_buffer(dist = 5)

tm_shape(cat_crop) + tm_polygons() + 
tm_shape(riv_sub) + tm_lines () 
 
        data %>% 
        filter(basid  == 10066801) %>% 
                tm_shape() + tm_polygons()



# crop to Europe
data2 <- 
        data %>% 
        map(.f = ~ st_crop(x = .x, 
                           y = europe))
# save to file 
saveRDS(data2, "data/rivers_cropped.rds")
