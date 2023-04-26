################
# add further information to the river network file: height differences during flow
# author: david mennekes, PhD Student at Empa ST. Galen / ETH Zürich, Switzerland, david.mennekes@empa.ch, david.mennekes@posteo.de
# march 2021
###############

	###########
	# packages and main path
	################
	
	library(sf)
	library(dplyr)
	library(qgisprocess)
	
	#path to subfolder
	
	main.path <- "PhD/mennekes/"
	

	
	
	
	################
	# load data
	###############
	
	load(paste0(main.path, "data_modified/river_network/rivers_all2.Rdata"))
	load(paste0(main.path, "data_modified/river_network/rivers_all01.Rdata")) #get first and last points
	rm(rivers.all)
	
	#load a digital elevation model data
	dem <- raster("PhD/data/karten/swisstopo/200901_mennekes/200901_mennekes/swissALTI3D_LV95_2019/swissALTI3D_LV95_2019_2m.tif")
	st_crs(dem)
	
	

	rivers.all3 <- rivers.all2
	rm(rivers.all2)
	
	### functions
	qgis_to_sf <- function(input){
		return(sf::st_as_sf(sf::read_sf(qgis_output(input, "OUTPUT"))))
	}
	
	
	
	
	#################
	# calculation
	#################
	
	#sample raster data
	#first
	dem_first <- qgis_run_algorithm(
		"native:rastersampling",
		INPUT = first,
		RASTERCOPY = dem
	)
	
	dem_first_sf <- qgis_to_sf(dem_first)
	
	
	#last points
	dem_last <- qgis_run_algorithm(
		"native:rastersampling",
		INPUT = last,
		RASTERCOPY = dem
	)
	
	dem_last_sf <- qgis_to_sf(dem_last)
	
	# 1) calculate slope based on  dem_first and dem_last
	###################################
	
	# calculate diff of height
	dem_diff <- dem_first_sf$SAMPLE_1-dem_last_sf$SAMPLE_1

	#make new col
	rivers.all3$dem_diff <- NA
	rivers.all3$dem_diff[1: length(dem_diff)] <- dem_diff #last 11 rows are containers for outflow out of the country..
	rivers.all3$dem_diff[rivers.all3$OBJEKTART == 6] <- NA # for lakes the diff = NA (calculating a slope is useless)
	
	rivers.all3$Shape_Length <- st_length(rivers.all3)	# recalculate length
	
	#calculate slope in percent ->  (dem_diff / length) * 100
	
	rivers.all3$slope_percent <- (rivers.all3$dem_diff / as.numeric(rivers.all3$Shape_Length)) * 100
	
	#heigth
	rivers.all3$height_last <- NA
	rivers.all3$height_last[1: nrow(dem_last_sf)] <- dem_last_sf$SAMPLE_1
	
	rivers.all3$height_first <- NA
	rivers.all3$height_first[1: nrow(dem_first_sf)] <- dem_first_sf$SAMPLE_1
	
	
	# st_write(rivers.all3, paste0(main.path, "temp_data/rivers_all3.gpkg"), append = F)
	
	save(rivers.all3, file = paste0(main.path, "temp_data/rivers_all3.Rdata"))
	
	

	
	
	rm(list = ls())
	