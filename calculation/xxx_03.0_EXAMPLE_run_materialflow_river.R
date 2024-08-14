######################
# Example river -> this file uses the example river only!
# add data of river / lake contamination and run model
# unit: g/ s
# author: david mennekes, PhD Student at Empa St. Gallen / ETH Zürich, Switzerland, david.mennekes@empa.ch, david.mennekes@posteo.de
# august 2022, last edit:
######################


###############################
#packages and path
###############################

library(tidyverse)
library(sf)

#path to sub-folders
# change this path to the directory of the model
setwd("~/")
main.path <- "PhD/mennekes/" #change!
#
#
#polymers of interest
polymers <- c("LDPE", "HDPE", "PP", "PS", "EPS", "PVC", "PET")

#important non polymer specific columns that should be saved:
#adjust according to your desires
save_c <- c("id_all", "flow_to", "outflow", "length_m", "length_seconds", "flow_velocity", "isLake", "area")

#scenarios defined in 2.7_factors. Each factor represents a different parameter set-up
extra_names <- c( "_withallfactors") 


N <- 25 # number of runs. should be high enough to establish a constant, constant with about 600 for Rhine
print_round <- c(25) #rounds which will be saved, should be at least the last round

###############################
# functions
###############################


#loop for extra_names
# run the model for each scenario / parameter set-up
for (xxx in extra_names) {
	
	
	###############################
	# load data
	###############################
	
	# # load river data saved in 2.7_factors
	load(paste0(main.path, "temp_data/example_river/rivers_all6", xxx, ".Rdata"))
	
	rivers.calc <- rivers.all6 #drop geometry for faster calculation
	
	#save geometry in a different file
	#does not apply for example river....	
	
	
	
	##############
	#add data for actual contamination: sum data is yearly flow in to the environment calculated by Delphine in contamination per s and river m for rivers and total for lakes. Mass in g
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
	
	
	# find rows which are no outflow and lakes or rivers
	
	r.rivers <- which(rivers.calc$isLake == F & rivers.calc$outflow == 0)
	r.rivers.l <- rivers.calc$isLake == F & rivers.calc$outflow == 0 #for lakes
	
	r.lakes <- which(rivers.calc$isLake == T)
	
	
	
	
	#find duplicated numbers, like this the calculation will be faster
	dup <- unique(rivers.calc$flow_to[duplicated(rivers.calc$flow_to)])
	multi_flow_to <- rivers.calc$flow_to %in% dup
	dup_id <- rivers.calc$id_al[multi_flow_to]
	single_flow_to <- !(multi_flow_to)
	
	
	rivers.calc.all <- rivers.calc #transfer data to new data with selected columns
	
	for (mat in polymers) { #loop for each polymer. 
		
		#create name vectors for each polymer
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
		c.mat <- grep(mat, names(rivers.calc.all))
		c.resus <- grep("resus.", names(rivers.calc.all))
		rivers.calc <- rivers.calc.all[, c(save_c, names(rivers.calc.all)[c.mat], names(rivers.calc.all)[c.resus])]
		
		
		for (i in 1:N) { #loop for each calculation step
			# create empty container for data
			temp_MiP <- rep(0, nrow(rivers.calc)) #MiP in suspension
			
			temp_MiP_sed <- rep(0, nrow(rivers.calc)) #MiP in sediment
			
			
			# factors are stored as negative numbers. thus in the next step a sum function will subtract this amount
			#MiP: create a data frame for microplastic with input emission, emission in sediment, inflow from upper segment, and burial. Data represents transport per second or entire segment.
			MiP <- tibble(sed = (rivers.calc[ , MiPw.sum] * rivers.calc[ , paste0("sed.fac_", mat, "_MiP")] + rivers.calc[ , MiPw.in] * rivers.calc[ , paste0("sed.fac_", mat, "_MiP")])*-1, #for entire segment!
										removal = ((rivers.calc[ , MiPw.sum])  * (rivers.calc[ , paste0("removal.fac_", mat, "_MiP")]) + (rivers.calc[ , MiPw.in]) * (rivers.calc[ , paste0("removal.fac_", mat, "_MiP")]))*-1,
										sum = (rivers.calc[ , MiPw.sum]),
										inflow = rivers.calc[ , MiPw.in],
										inflow_sed = rivers.calc[ ,MiPw.in_sed]*-1,
										burial = 0)
			
			#add burial is for entire segment
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
			
			
			
			
			
			
			
			########## ################
			#sum the MiP dataframes per row ->negative numbers are subtracted (sedimentation, burial) while positive number are added (input emission, resuspension, inflow from upstream)
			#assumptions: inflow is per second and all other data is for the entire segment. therefore all "changes" along the segment are included. Thus the result is per second at the end of the segment
			
			# to reduce computation time we separate multiflow vs. single flow
			#if only one section flows to another section:
			# just sum the rows of removal, burial, sum and act. conc. -> because of the negative numbers of burial and removal this is a easy solution to find the new act.conc.
			temp_MiP[rivers.calc$flow_to[single_flow_to]] <- rowSums(MiP[single_flow_to, c("inflow", "sum", "removal", "sed", "resuspension")], na.rm = T) # because sed and clean up are stored as neg. numbers this amount will be substrected.resuspension, sum (data by Kawecki) and inflow are positive numbers
			
			
			
			# for summing in groups / meaning multiple flows go into one, rowsum takes a grouping argument
			temp_MiP[sort(dup)] <- rowSums(rowsum(MiP[multi_flow_to, c("inflow", "sum", "removal", "sed", "resuspension")], rivers.calc$flow_to[multi_flow_to])) #sorts always according to second argument
			
			
			#samoe for sediments
			#pass on sedimentation.. in steady state all will be passed on.  until here sediments, burial and resuspension are considered as a compartment across the entire river segment. temp_MiP_sed will be the sed input
			temp_MiP_sed[rivers.calc$flow_to[single_flow_to]] <- rowSums(MiP[single_flow_to, c("burial","inflow_sed", "sed", "resuspension")], na.rm = T)*-1 # because sed is stored as neg. and burial and resuspension are stored as positive numbers; to make final number positive *-1
			
			
			# for summing in groups / meaning multiple flows go into one, rowsum takes a grouping argument
			temp_MiP_sed[sort(dup)] <- rowSums(rowsum(MiP[multi_flow_to, c("burial","inflow_sed", "sed", "resuspension")], rivers.calc$flow_to[multi_flow_to]))*-1 #sorts always according to second argument
			
			
			
			
			
			
			# use the temp_data as new inflow concentration data.
			rivers.calc[ ,MiPw.in] <- temp_MiP 
			
			#use the temp_data of sed. for sed inflow
			rivers.calc[ , MiPw.in_sed] <- temp_MiP_sed
			
			
			#controll that no negative accumulation. possible when cleaning + accumulation is higher than "inflow"
			if(length(which((rivers.calc[ ,MiPw.in]) < 0)) >=1 ){
				warning("negative contamination (MiP)!")
				break
			}
			
			
			#transfer data to rivers.calc dataframe
			rivers.calc[ , MiPw.acc] <- MiP[ , "burial"] #
			
			
			
			rivers.calc[ , MiPw.clean] <- MiP[ , "removal"]*-1 #neg numbers were needed before
			
			
			
			#for sed: what you will find / is present in the entire river segment!!! in one second
			rivers.calc[ , MiPw.sed] <- MiP[ , "sed"]*-1 +MiP[ , "inflow_sed"]*-1 - MiP[ , "burial"] - MiP[ , "resuspension"] #sed an inflow_sed are negative, while  burial and resuspension are is positive numbers. 
			
			
			
			# actual concentration of plastic in the water
			rivers.calc[ , paste0("actualcon_", mat, "_WaterMiP_concMSV")] <- rowSums(MiP[ , c("inflow", "sum", "removal", "sed", "resuspension")], na.rm = T)
			
			if(i %in% print_round){
				Sys.sleep(10)
				save(rivers.calc, file = paste0(main.path, "temp_data/example_river/output/round_", i, "_", mat,"_",  extra_name, ".Rdata"))
				print(paste(Sys.time(), ":", mat, extra_name, "done!"))
				Sys.sleep(5)
			}
		}
	}
	
	
	
} #end loop extra_names

save(print_round,extra_names, file = paste0(main.path, "temp_data/example_river/output/print_round.Rdata"))
rm(list = ls())

