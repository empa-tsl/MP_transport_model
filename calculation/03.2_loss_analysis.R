######################
# add data of river / lake contamination and run model
# unit: g/ s
# author: david mennekes, PhD Student at Empa St. Gallen / ETH Zürich, Switzerland, david.mennekes@empa.ch, david.mennekes@posteo.de
# march 2022, last edit:
######################


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
polymers <- c("EPS", "PS", "PET")
# polymers <- polymers[2]

NRsample = 60

#important non polymer specific columns that should be saved:
save_c <- c("id_all", "flow_to", "outflow", "height_last", "name_river", "slope_percent", "length_m", "length_seconds", "flow_velocity", "isLake", "country", "area", "name_2", "FID_poly_s", "OBJEKTART", "NAME", "VERLAUF", "GEWISS_NR", "LAUF_NR", "verweilzeiten")

xxx <-  "_withallfactors"
N <- 600 # number of runs. should be high enough to establish a constant, constant with about 600 for Rhine
print_round <- c(600) #rounds which will be saved
# print_round <- c(-9999)

###############################
# functions
###############################


#loop for extra_names

	
	
	###############################
	# load data
	###############################
	
	# # load river data
	load(paste0(main.path, "temp_data/rivers_all6", xxx, ".Rdata"))
	rivers.calc <- st_drop_geometry(rivers.all6)

	
	ids <- rivers.calc$id_all[rivers.calc$outflow == 0] #all possible ids
	ID_select <- sample(ids, NRsample)#a selection of IDs that will be rund
	
	rm(rivers.all6)
	results<- as_tibble(matrix(NA, nrow = nrow(rivers.calc), ncol = length(ID_select)*length(polymers)))
	names(results) <- paste0(polymers, "_", rep(ID_select,1,each=3))
	
	
	abc <- 1
	for (j in 1:length(ID_select)) {
		
	
	#for each ID_select
	
	#change all input to 0
	concMSV <- grep("_concMSV", names(rivers.calc))
	sss <- grep("sum_", names(rivers.calc))
	
	rivers.calc[ ,concMSV] <- 0
	rivers.calc[ ID_select[j], sss] <- 1 #here inflow 1
	
	
	
	
	##############
	#add data for actual contamination: sum data is yearly flow in to the environment calculated by Delphine in contamination per s and river m for rivers and total for lakes. Mass in g
	##############
	##############
	
	for(mat in polymers){
		rivers.calc[ , paste0("actualcon_", mat, "_WaterMiP_concMSV")] <- 0
		# rivers.calc[ , paste0("actualcon_", mat, "_WaterMaP_concMSV")] <- 0
	}
	
	for(mat in polymers){
		rivers.calc[ , paste0("actualinflow_", mat, "_WaterMiP_concMSV")] <- 0
		# rivers.calc[ , paste0("actualinflow_", mat, "_WaterMaP_concMSV")] <- 0
	}
	
	for(mat in polymers){
		rivers.calc[ , paste0("actualinflow_sed_", mat, "_WaterMiP_concMSV")] <- 0
		# rivers.calc[ , paste0("actualinflow_sed_", mat, "_WaterMaP_concMSV")] <- 0
	}
	
	
	# for actual accumulation
	for(mat in polymers){
		rivers.calc[ , paste0("actualacc_", mat, "_WaterMiP_concMSV")] <- 0
		# rivers.calc[ , paste0("actualacc_", mat, "_WaterMaP_concMSV")] <- 0
	}
	
	
	#for actual clean up
	
	for(mat in polymers){
		rivers.calc[ , paste0("actualremoval_", mat, "_WaterMiP_concMSV")] <- 0
		# rivers.calc[ , paste0("actualremoval_", mat, "_WaterMaP_concMSV")] <- 0
	}
	
	#actual concentration in sedimentation
	for(mat in polymers){
		rivers.calc[ , paste0("actualsed_", mat, "_WaterMiP_concMSV")] <- 0
		# rivers.calc[ , paste0("actualsed_", mat, "_WaterMaP_concMSV")] <- 0
	}
	
	
	
	
	###############
	######versuch der 2.
	r.rivers <- which(rivers.calc$isLake == F & rivers.calc$outflow == 0)
	r.rivers.l <- rivers.calc$isLake == F & rivers.calc$outflow == 0
	
	r.lakes <- which(rivers.calc$isLake == T)
	
	
	
	
	#find duplicated numbers, like this the calculation will be faster
	dup <- unique(rivers.calc$flow_to[duplicated(rivers.calc$flow_to)])
	multi_flow_to <- rivers.calc$flow_to %in% dup
	dup_id <- rivers.calc$id_al[multi_flow_to]
	single_flow_to <- !(multi_flow_to)
	
	
	
	
	z <- 0 #for polymers
	for (mat in polymers) {
		
		#create names sum (yearly outflowin MSV)
		MiPw.sum <- paste0("sum_", mat, "_WaterMP_concMSV")
		# MaPw.sum <- paste0("sum_", mat, "_Water_concMSV")
		
		#inflow from previous section
		MiPw.in <- paste0("actualinflow_", mat, "_WaterMiP_concMSV")
		# MaPw.in <- paste0("actualinflow_", mat, "_WaterMaP_concMSV")
		
		
		# sediment inflow from previous section
		MiPw.in_sed <- paste0("actualinflow_sed_", mat, "_WaterMiP_concMSV")
		# MaPw.in_sed <- paste0("actualinflow_sed_", mat, "_WaterMaP_concMSV")
		
		
		
		#create names accumulation
		MiPw.acc <- paste0("actualacc_", mat, "_WaterMiP_concMSV")
		# MaPw.acc <- paste0("actualacc_", mat, "_WaterMaP_concMSV")
		
		#create names removal
		MiPw.clean <- paste0("actualremoval_", mat, "_WaterMiP_concMSV")
		# MaPw.clean <- paste0("actualremoval_", mat, "_WaterMaP_concMSV")
		
		
		#create names sedimentation
		MiPw.sed <- paste0("actualsed_", mat, "_WaterMiP_concMSV")
		# MaPw.sed <- paste0("actualsed_", mat, "_WaterMaP_concMSV")
		
		
		#select only columns that are needed
		
		
		for (i in 1:N) {
			# create empty container for data
			temp_MiP <- rep(0, nrow(rivers.calc))
			# temp_MaP <- rep(0, nrow(rivers.calc))
			
			temp_MiP_sed <- rep(0, nrow(rivers.calc))
			# temp_MaP_sed <- rep(0, nrow(rivers.calc))
			
			
			#calculate accumulation = total*acc factor; acc. factor in [0; 1]
			# reducing factors are stored as negative numbers. thus in the next step a sum function will substrect this amount
			#sum input data is for the entire segment.
			MiP <- tibble(sed = (rivers.calc[ , MiPw.sum] * rivers.calc[ , paste0("sed.fac_", mat, "_MiP")] + rivers.calc[ , MiPw.in] * rivers.calc[ , paste0("sed.fac_", mat, "_MiP")])*-1, #for entire segment!
										removal = ((rivers.calc[ , MiPw.sum])  * (rivers.calc[ , paste0("removal.fac_", mat, "_MiP")]) + (rivers.calc[ , MiPw.in]) * (rivers.calc[ , paste0("removal.fac_", mat, "_MiP")]))*-1,
										sum = (rivers.calc[ , MiPw.sum]),
										inflow = rivers.calc[ , MiPw.in],
										inflow_sed = rivers.calc[ ,MiPw.in_sed]*-1,
										burial = 0)
			
			#burial is for entire segment
			MiP$burial[r.lakes] <- MiP$sed[r.lakes]+ rivers.calc[ r.lakes, MiPw.in_sed]*-1 #burial == sed for lakes plus inlfow of sed
			MiP$burial[r.rivers] <- burial_rate_rivers*(MiP$sed[r.rivers] + rivers.calc[r.rivers , MiPw.in_sed]*-1) # burial rate is a portion of MiP in sediments but it has a maximum constant given as well.
			c <- which(MiP$burial*-1 > as.vector(rivers.calc[ , paste0("acc.fac_", mat,"_MiP")]) & r.rivers.l) #is per lengths in second. see factors
			if(length(c)>0){
				MiP$burial[c] <- as.vector(rivers.calc[ c, paste0("acc.fac_", mat,"_MiP")])*-1#if the burial is higher than the burial rate, replace it with the max value
			}
			# make burial a positive number
			MiP$burial <- MiP$burial*-1
			
			
			#resuspension from sediments
			
			MiP$resuspension <- I(MiP$sed*-1+MiP$inflow_sed*-1)*rivers.calc[ , "resus.fac_MiP"] #for entire segment, sed and inflow_sed are negative
			#check
			c <- which(I((MiP$sed*-1+MiP$inflow_sed*-1) - MiP$burial-MiP$resuspension)<=0) #show errors. here resuspension is to high
			
			MiP$resuspension[c] <- I((MiP$sed[c]*-1+MiP$inflow_sed[c]*-1) - MiP$burial[c]) #when the resuspension is smaller than what is left in sediments. then take all the rest
			
			##
			
			
			
			#neu machen mit sed inflow
			# MaP <- tibble(sed = (rivers.calc[ , MaPw.sum] * rivers.calc[ , paste0("sed.fac_", mat, "_MaP")] + rivers.calc[ , MaPw.in] * rivers.calc[ , paste0("sed.fac_", mat, "_MaP")])*-1,
			# 							removal = ((rivers.calc[ , MaPw.sum])  * (rivers.calc[ , paste0("removal.fac_", mat, "_MaP")]) + (rivers.calc[ , MaPw.in]) * (rivers.calc[ , paste0("removal.fac_", mat, "_MaP")]))*-1,
			# 							sum = (rivers.calc[ , MaPw.sum]),
			# 							inflow = rivers.calc[ , MaPw.in],
			# 							inflow_sed = rivers.calc[ ,MaPw.in_sed],
			# 							burial = 0)
			
			# MaP$burial <- MaP$sed #burial == sed tbd
			## resuspension rate?
			
			##
			
			
			
			
			
			
			########## ################
			#sum the MiP dataframes. assumptions: inflow is per second and all other data is for the entire segment. therefore all "changes" along the segment are included. Thus the result is per second at the end of the segment
			
			
			#if only one section flows to another section:
			# just sum the rows of removal, burial, sum and act. conc. -> because of the negative numbers of burial and removal this is a easy solution to find the new act.conc.
			temp_MiP[rivers.calc$flow_to[single_flow_to]] <- rowSums(MiP[single_flow_to, c("inflow", "sum", "removal", "sed", "resuspension")], na.rm = T) # because sed and clean up are stored as neg. numbers this amount will be substrected.resuspension, sum (data by Kawecki) and inflow are positive numbers
			
			
			# temp_MaP[rivers.calc$flow_to[single_flow_to]] <- rowSums(MaP[single_flow_to, c("inflow", "sum", "removal", "sed", "resuspension")], na.rm = T)
			
			
			# for summing in groups / meaning multiple flows go into one, rowsum takes a grouping argument
			temp_MiP[sort(dup)] <- rowSums(rowsum(MiP[multi_flow_to, c("inflow", "sum", "removal", "sed", "resuspension")], rivers.calc$flow_to[multi_flow_to])) #sorts always according to second argument
			
			# temp_MaP[sort(dup)] <- rowSums(rowsum(MaP[multi_flow_to, c("inflow", "sum", "removal", "sed", "resuspension")], rivers.calc$flow_to[multi_flow_to]))
			
			
			
			#pass on sedimentation.. in steady state all will be passed on.  until here sediments, burial and resuspension are considered as a compartment across the entire river segment. temp_MiP_sed will be the sed input
			temp_MiP_sed[rivers.calc$flow_to[single_flow_to]] <- rowSums(MiP[single_flow_to, c("burial","inflow_sed", "sed", "resuspension")], na.rm = T)*-1 # because sed is stored as neg. and burial and resuspension are stored as positive numbers; to make final number positive *-1
			
			# temp_MaP_sed
			
			
			# for summing in groups / meaning multiple flows go into one, rowsum takes a grouping argument
			temp_MiP_sed[sort(dup)] <- rowSums(rowsum(MiP[multi_flow_to, c("burial","inflow_sed", "sed", "resuspension")], rivers.calc$flow_to[multi_flow_to]))*-1 #sorts always according to second argument
			
			
			
			
			
			
			# use the temp_data as new inflow concentration data.
			rivers.calc[ ,MiPw.in] <- temp_MiP 
			# rivers.calc[ , MaPw.in] <- temp_MaP
			
			
			#use the temp_data of sed. for sed inflow
			rivers.calc[ , MiPw.in_sed] <- temp_MiP_sed
			
			# rivers.calc[ , MaPw.in_sed] <- temp_MaP_sed
			
			#controll that no negative accumulation. possible when cleaning + accumulation is higher than "inflow"
			if(length(which((rivers.calc[ ,MiPw.in]) < 0)) >=1 ){
				warning("negative contamination (MiP)!")
				break
			}
			# if(length(which((rivers.calc[ ,MaPw.in]) < 0)) >=1 ){
			# 	warning("negative contamination! (MaP)")
			# 	break
			# }
			
			rivers.calc[ , MiPw.acc] <- MiP[ , "burial"] #
			
			# rivers.calc[ , MaPw.acc] <- MaP[ , "burial"]*-1
			
			
			rivers.calc[ , MiPw.clean] <- MiP[ , "removal"]*-1 #neg numbers were needed before
			
			# rivers.calc[ , MaPw.clean] <- MaP[ , "removal"]*-1
			
			
			#for sed: what you will find / is present in the entire river segment!!! in one second
			rivers.calc[ , MiPw.sed] <- MiP[ , "sed"]*-1 +MiP[ , "inflow_sed"]*-1 - MiP[ , "burial"] - MiP[ , "resuspension"] #sed an inflow_sed are negative, while  burial and resuspension are is positive numbers. 
			
			
			
		}
		ccc <- abc+z
		results[, ccc] <- temp_MiP
		
		
		z<- z+1
	}
	
	abc <- abc+3
	print(paste0(Sys.time()," ", ID_select[j], " done. Round ", j))
	}
	
	
	x <- Sys.time()
	t <- paste0(substr(x,1,4),substr(x,6,7),substr(x,9,10), "_", substr(x,12,13),substr(x,15,16))
save(ID_select,results, file = paste0(main.path, "temp_data/loss/output",t, ".Rdata"))
rm(list = ls())

