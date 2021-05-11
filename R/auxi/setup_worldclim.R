### -------------------------------- ###
### --- setup typology worldclim --- ### 
### -------------------------------- ###
# libraries ---------------------------------------------------------------
pacman::p_load(terra, purrr, sf, dplyr, magrittr, data.table)


# dir ---------------------------------------------------------------------
dir = list(wc = "D://Arbeit/Data/worldclim/")

# functions ---------------------------------------------------------------
assign_raster = function(x, rast) {
        fn.dat = data2[[x]]
        fn.tem = rast[[x]]
        
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
        return(out)
}

lr = function(x,y){
        rast(file.path(y,x))   
}

        


