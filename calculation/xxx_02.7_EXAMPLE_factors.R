#######
# add accumulation and removal factors to the data.
# scenario used: with all factors....
# author: david mennekes, david.mennekes@posteo.de, david.mennekes@empa.ch
# march 2022
# last edit:
#################





###############################
#packages and path
###############################

	library(tidyverse)
	library(sf)
	
	#path to sub-folders
	setwd("~/")
	main.path <- "PhD/mennekes/"
	# 
	# 
	#polymers
	polymers <- c("LDPE", "HDPE", "PP", "PS", "EPS", "PVC", "PET")
	
	#scenarios
	extra_name01 <- "withallfactors"

	

###############################
# load data
###############################

	load(paste0(main.path, "temp_data/example_river/rivers.all5.Rdata"))
	rivers.all6 <- example_river
	rm(example_river)
	
	#load factors for river
	load(paste0(main.path, "temp_data/factor_rivers.Rdata")) #only sedimentation factor Microplastic
	
	
	
	#############
	# define factors
	#############
	
	
	# add all data to rivers.calc
	
	for (i in polymers) {
		a <- as.data.frame(matrix(0, nrow = nrow(rivers.all6), ncol = 6))
		names(a) <- c(paste0("sed.fac_", i, "_MiP"), paste0("removal.fac_", i, "_MiP"), #create rows for each polymer
									paste0("sed.fac_", i, "_MaP"), paste0("removal.fac_", i, "_MaP"),
									paste0("acc.fac_", i, "_MiP"), paste0("acc.fac_", i, "_MaP"))
		
		rivers.all6 <- cbind(rivers.all6, a)
		rm(a)
	}
	
	
	##### add futher information to derive the factors.
	#all extra columns have the name "xxx_" to delete them later
	
	
	xxx_MiP_EPS <- fac_rivers["EPS"]
	xxx_MiP_PP <- fac_rivers["PP"]
	xxx_MiP_LDPE <- fac_rivers["LDPE"]
	xxx_MiP_HDPE <- fac_rivers["HDPE"]
	xxx_MiP_PS <- fac_rivers["PS"]
	xxx_MiP_PVC <- fac_rivers["PVC"]
	xxx_MiP_PET <- fac_rivers["PET"]
	
	
	
	#collect the column names for all factors or specific factors to modify multiple columns
	c.names <- names(rivers.all6)
	
	c.fac <- grep(".fac_", c.names)
	c.fac.names <- c.names[c.fac]
	c.acc <- grep("acc.fac_", c.fac.names)
	c.ac.names <- c.fac.names[c.acc]
	c.removal <- grep("removal.fac_", c.fac.names)
	c.sed <- grep("sed.fac_", c.fac.names)
	c.sed.names <- c.fac.names[c.sed]
	c.sed.MiP.names <- c.sed.names[grepl("_MiP", c.sed.names)]
	
	c.acc_MiP <- c.fac.names[c.acc[grepl("_MiP", c.fac.names[c.acc])]] #colnames with MiP and burial
	r.rivers <- which(rivers.all6$isLake==F)
	r.lakes <- which(rivers.all6$isLake == T)
	
	
	# add factors:
	#accordingly to Mennekes et al. 2022
	
	##### Microplastics #####
	#### rivers ####
	##################
	#no MiP removal in lakes and rivers.
	c.removal_MiP <- c.fac.names[c.removal[grepl("_MiP", c.fac.names[c.removal])]]
	rivers.all6[, c.removal_MiP] <- 0 #no MiP removal in lakes and rivers
	
	#### sedimentation rates for MiP in Rivers based on Bessling2017, EPS is to light to sediment
	#generally, rates are a result of removal per second. The rate can be multiplied with the ms contamination value because the rates take into account the length of each section
	rivers.all6[r.rivers, "sed.fac_EPS_MiP"] <- 1-(1-xxx_MiP_EPS)^rivers.all6$length_seconds[r.rivers]
	rivers.all6[r.rivers, "sed.fac_PP_MiP"] <- 1-(1-xxx_MiP_PP)^rivers.all6$length_seconds[r.rivers] # use compound interest equation to show rate over entire section: xn = x0(1-r)^n; where x0=1, r = factors per second, n = length in seconds
	rivers.all6[r.rivers, "sed.fac_HDPE_MiP"] <- 1-(1-xxx_MiP_HDPE)^rivers.all6$length_seconds[r.rivers]
	rivers.all6[r.rivers, "sed.fac_LDPE_MiP"] <- 1-(1-xxx_MiP_LDPE)^rivers.all6$length_seconds[r.rivers]
	rivers.all6[r.rivers, "sed.fac_PS_MiP"] <- 1-(1-xxx_MiP_PS)^rivers.all6$length_seconds[r.rivers]
	rivers.all6[r.rivers, "sed.fac_PVC_MiP"] <- 1-(1-xxx_MiP_PVC)^rivers.all6$length_seconds[r.rivers]
	rivers.all6[r.rivers, "sed.fac_PET_MiP"] <- 1-(1-xxx_MiP_PET)^rivers.all6$length_seconds[r.rivers]
	
	
	##### burial
	#burial (accumulation) rate MiP in all rivers from sediments into accumulation: 
	burial_rate_rivers <- 0.1 # standard we assume 10 % for all MiP. However, the max rate of burial is set by lit. data:
	# here we assume 1e-8 per second. Important steady constant, consequently multiply with length seconds to show the value over the entire distance
	rivers.all6[r.rivers, c.acc_MiP] <- 1e-8 * rivers.all6$length_seconds[r.rivers] #per second. Important steady constant directly calculated for the entire length
	# the run model is taking care of taking the right rate!
	
	
	#resuspension
	#same for all sections and polymers.
	rivers.all6$resus.fac_MiP <- 0.03 #3%
	
	
	
	
	
	
	#### lakes ####
	##############
	# removal == 0 (see above)
	
	#### sedimentation
	# we follow the equation given in the paper mennekes et al 2022 in the section lakes
	# = 0.95*(1- exp(1)^(-0.004*AREA)), c*(1 - exp(1)^(-k*x))
	rivers.all6[r.lakes, c.sed.MiP.names] <- 0.95*(1- exp(1)^(-0.004*rivers.all6$area[r.lakes]*0.000001)) #area is in m2 -> change to km2
	
	#so far all polymers have the same factor. For EPS we apply a factor 0.5 afterwards to reduce the sedimentation rate by half compared to the other polymers.
	rivers.all6[r.lakes, "sed.fac_EPS_MiP"] <- 0.5*rivers.all6$sed.fac_EPS_MiP[r.lakes]
	
	# based on Yang2022 PP is more dominant in the water than in the sediment when compared to the other polymers. Therefore we apply a factor 0.75 to assure implement this finding
	rivers.all6[r.lakes, "sed.fac_PP_MiP"] <- 0.75*rivers.all6$sed.fac_PP_MiP[r.lakes]
	
	### the heavy polymers are more likely to sediment therefore we adjust their sedimentation behavior
	rivers.all6[r.lakes, "sed.fac_PET_MiP"] <- 0.95*(1- exp(1)^(-0.012*rivers.all6$area[r.lakes]*0.000001))
	rivers.all6[r.lakes, "sed.fac_PVC_MiP"] <- 0.95*(1- exp(1)^(-0.012*rivers.all6$area[r.lakes]*0.000001))
	
	#### burial
	#burial rate MiP in lakes is 100%
	rivers.all6[r.lakes, c.acc_MiP] <- 1 #100%
	
	
	
	#### resuspension
	rivers.all6$resus.fac_MiP[r.lakes] <- 0 #0 for lakes
	
	
	
	
	
	#safe files for the different scenarios. therefore factors are adjust accordingly
	##############################	
	
	####
	# change all factors to 0 for rows which are not part of the network but for collecting data
	rivers.all6[rivers.all6$outflow!=0, c.fac.names] <- 0
	extra_name <- extra_name01
	save(rivers.all6,extra_name, burial_rate_rivers, file = paste0(main.path, "temp_data/example_river/rivers_all6_", extra_name, ".Rdata"))
	
	
	
	
	rm(list = ls())	
	
	