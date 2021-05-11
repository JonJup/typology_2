### ------------------------------- ###
### --- Add Worldclim variables --- ### 
### ------------------------------- ###

# --------------- #
# date:  
#               22.02.21
# files in 
#               -> 01_rivers_cropped.rds
# files out
#               <- ???? 
# Project: 
#               New typology for Europe 
# Purpose:  
#               Combine single river river zones. 
#               Crop to study area. 
# --------------- #

# setup -------------------------------------------------------------------
source("R/auxi/setup_worldclim.R")

# load data ---------------------------------------------------------------
data     <- "data/01_rivers_cropped.rds" %>% readRDS()

## -- Rasters to load
rasters = c("wc2.1_30s_elev/wc2.1_30s_elev.tif" ,
            "wc2.1_30s_bio/wc2.1_30s_bio_5.tif" ,
            "wc2.1_30s_bio/wc2.1_30s_bio_6.tif" ,
            "wc2.1_30s_bio/wc2.1_30s_bio_13.tif",
            "wc2.1_30s_bio/wc2.1_30s_bio_14.tif",
            "wc2.1_30s_bio/wc2.1_30s_bio_15.tif")

## -- lr is a function from setup
## -- it reads a the terra::rast() function to read raster layers
## -- "raster_list" is a list of all the rasters in the character vector "raster"
raster_list = lapply(rasters, lr, dir$wc)

## -- rename rasters 
names(raster_list) = c(
        "elev",
        "max_temp",
        "min_temp",
        "max_prec",
        "min_prec",
        "cv_prec" 
)

# prepare data  -----------------------------------------------------------
## -- convert to SpatVector (terra vector format)
data2 = map(data,
            .f = ~ .x %>%
                    as_Spatial %>%
                    vect
)

## -- saving and loading the resulting list does not work properly. 
# saveRDS(data2, "data/temp/data2.rds")
# data2 = "data/temp/data2.rds" %>% readRDS()

# crop  -------------------------------------------------------------------
## - crop worldclim data to river data 

rasters_cropped = lapply(raster_list,
                         function(rl)
                                 map(.x = data2,
                                     .f = ~ terra::crop(x = rl,
                                                        y = .x)))

# assign values to streams -------------------------------------------------

## -- open connection to log file 
log_con <- file("data/temp/assign_loop_log.txt", open = "a")

## -- loop over different rasters 
#for (i in 5:length(rasters_cropped)) {
for (i in 6){
        ## -- loop over 
        #for (j in 1:length(rasters_cropped[[i]])){
         for (j in 5){       
                        fn.dat = data2[[j]]
                        fn.tem = rasters_cropped[[i]][[j]]
                        
                        out =  map_dbl(.x = 1:nrow(fn.dat),
                                       .f = ~ round(
                                               terra::extract(
                                                       x = fn.tem,
                                                       y = fn.dat[.x, ],
                                                       fun = mean,
                                                       na.rm = T
                                               )[, 2],
                                               1
                                       ))
                        saveRDS(out, paste0("data/temp/", i,"_",j,".rds"))
                        cat(paste(i,j, "done @", Sys.time()), file = log_con, append = TRUE, sep = "\n") # creates file and writes to it
                }
                
}
        
## -- load results of loop 
for (i in 1:5){
        data[[i]]$elev     = readRDS(paste0("data/temp/1_",i, ".rds"))
        data[[i]]$max_temp = readRDS(paste0("data/temp/2_",i, ".rds"))
        data[[i]]$min_temp = readRDS(paste0("data/temp/3_",i, ".rds"))
        data[[i]]$max_prec = readRDS(paste0("data/temp/4_",i, ".rds"))
        data[[i]]$min_prec = readRDS(paste0("data/temp/5_",i, ".rds"))
        data[[i]]$cv_prec  = readRDS(paste0("data/temp/6_",i, ".rds"))
}

## -- fix NAs 
## -- for NA or NaN values we insert the values of the adjacent streams
## -- Loop over elements temperature and precipitation rasters 
counter = 1 
while(counter < 100){
for (i in 1:5){
        ## -- elevation
        if (any(is.na(data[[i]]$elev))) {
                id = which(is.na(data[[i]]$elev))
                for (j in 1:length(id)) {
                        focal_from = data[[i]][id[j], "fromnode"]
                        focal_to   = data[[i]][id[j], "tonode"]
                        neighbour_to = filter(data[[i]],
                                              tonode == focal_from$fromnode)
                        neighbour_from = filter(data[[i]],
                                                fromnode == focal_to$tonode)
                        assign_value = c(neighbour_to$elev,
                                         neighbour_from$elev)
                        assign_value = mean(assign_value,
                                            na.rm = T)
                        data[[i]]$elev[id[j]] = assign_value
                        rm(
                                assign_value,
                                focal_from,
                                focal_to,
                                neighbour_to,
                                neighbour_from,
                                j
                        )
                }
        }
        print(paste(sum(is.na(
                data[[i]]$elev
        )), "left"))
        
        ## -------------- ## 
        ## -- max temp -- ## 
        ## -------------- ## 
        
        if (any(is.na(data[[i]]$max_temp))){
                id = which(is.na(data[[i]]$max_temp))
                for (j in 1:length(id)){
                        focal_from = data[[i]][id[j], "fromnode"]
                        focal_to   = data[[i]][id[j], "tonode"]
                        neighbour_to = filter(data[[i]], tonode == focal_from$fromnode)
                        neighbour_from = filter(data[[i]], fromnode == focal_to$tonode)
                        assign_value = c(neighbour_to$max_temp,  neighbour_from$max_temp)
                        assign_value = mean(assign_value,
                                            na.rm = T)
                        data[[i]]$max_temp[id[j]] = assign_value
                        rm(assign_value, focal_from, focal_to, neighbour_to, neighbour_from, j )
                }
        }
        print(paste(sum(is.na(data[[i]]$max_temp)), "left"))
        ## -------------- ##
        ## -- min temp -- ## 
        ## -------------- ## 
        if (any(is.na(data[[i]]$min_temp))){
                id = which(is.na(data[[i]]$min_temp))
                for (j in 1:length(id)){
                        focal_from = data[[i]][id[j], "fromnode"]
                        focal_to   = data[[i]][id[j], "tonode"]
                        neighbour_to = filter(data[[i]], tonode == focal_from$fromnode)
                        neighbour_from = filter(data[[i]], fromnode == focal_to$tonode)
                        assign_value = c(neighbour_to$min_temp,  neighbour_from$min_temp)
                        assign_value = mean(assign_value,
                                            na.rm = T)
                        data[[i]]$min_temp[id[j]] = assign_value
                        rm(assign_value, focal_from, focal_to, neighbour_to, neighbour_from, j )
                }
        }    
        print(paste(sum(is.na(data[[i]]$min_temp)), "left"))
        ## -- max precipitation
        if (any(is.na(data[[i]]$max_prec))){
                id = which(is.na(data[[i]]$max_prec))
                for (j in 1:length(id)){
                        focal_from = data[[i]][id[j], "fromnode"]
                        focal_to   = data[[i]][id[j], "tonode"]
                        neighbour_to = filter(data[[i]], tonode == focal_from$fromnode)
                        neighbour_from = filter(data[[i]], fromnode == focal_to$tonode)
                        assign_value = c(neighbour_to$max_prec,  neighbour_from$max_prec)
                        assign_value = mean(assign_value,
                                            na.rm = T)
                        data[[i]]$max_prec[id[j]] = assign_value
                        rm(assign_value, focal_from, focal_to, neighbour_to, neighbour_from, j )
                }
        }
        print(paste(sum(is.na(data[[i]]$max_prec)), "left"))
        ## -- min precipitation        
        if (any(is.na(data[[i]]$min_prec))){
                id = which(is.na(data[[i]]$min_prec))
                for (j in 1:length(id)){
                        focal_from = data[[i]][id[j], "fromnode"]
                        focal_to   = data[[i]][id[j], "tonode"]
                        neighbour_to = filter(data[[i]], tonode == focal_from$fromnode)
                        neighbour_from = filter(data[[i]], fromnode == focal_to$tonode)
                        assign_value = c(neighbour_to$min_prec,  neighbour_from$min_prec)
                        assign_value = mean(assign_value,
                                            na.rm = T)
                        data[[i]]$min_prec[id[j]] = assign_value
                        rm(assign_value, focal_from, focal_to, neighbour_to, neighbour_from, j )
                }
        }
        print(paste(sum(is.na(data[[i]]$min_prec)), "left")  )   
        if (any(is.na(data[[i]]$cv_prec))){
                id = which(is.na(data[[i]]$cv_prec))
                for (j in 1:length(id)){
                        focal_from = data[[i]][id[j], "fromnode"]
                        focal_to   = data[[i]][id[j], "tonode"]
                        neighbour_to = filter(data[[i]], tonode == focal_from$fromnode)
                        neighbour_from = filter(data[[i]], fromnode == focal_to$tonode)
                        assign_value = c(neighbour_to$cv_prec,  neighbour_from$cv_prec)
                        assign_value = mean(assign_value,
                                            na.rm = T)
                        data[[i]]$cv_prec[id[j]] = assign_value
                        rm(assign_value, focal_from, focal_to, neighbour_to, neighbour_from, j )
                }
        }
        print(paste(sum(is.na(data[[i]]$cv_prec)), "left"))
        print(paste0(i, Sys.time()))
        counter = counter +1 
} # END LOOP i 
}
saveRDS(data, "data/temp/temp_save_210425.rds")

# save to file  ---------------------------------------------------------------------
saveRDS(data, "data/02_w_wc.rds")

