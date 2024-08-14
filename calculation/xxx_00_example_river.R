	###########################
	## Manuscript to prepare example river
	# July 2022
	# contact: david.mennekes@posteo.de; david.mennekes@empa.ch
	#################################
	
	#main path
	setwd("~/")
	main.path <- "PhD/mennekes/"
	
	
	
	#load data
	library(readr)
	example_river <- read_delim(paste0(main.path, "temp_data/example_river/example_river.txt"), 
															delim = "\t", escape_double = FALSE, 
															col_types = cols(isLake = col_logical()), 
															trim_ws = TRUE)
	#polymers:
	polymers <- c("LDPE", "HDPE", "PP", "PS", "EPS", "PVC", "PET")
	
	
	#prepare example_river for modelling (add further information)
	
	#1.) assume a flow velocity of 1m/s everywhere
	example_river$flow_velocity <- 1
	
	#2.) segment lengths in seconds -> length_seconds
	
	example_river$length_seconds <- example_river$length_m/example_river$flow_velocity
	
	#3.) change the pollution to each river segment and for each polymer
	for (mat in polymers) {
		example_river <- cbind(example_river, example_river$input) #add the input
		names(example_river)[ncol(example_river)] <- paste0("sum_", mat, "_WaterMP_concMSV")
	}
	example_river$id_all <- example_river$ID

	example_river
	save(example_river, file = paste0(main.path, "temp_data/example_river/rivers.all5.Rdata"))
	
	rm(list = ls())
	