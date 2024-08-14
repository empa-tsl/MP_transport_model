############################
# script to read saved Rdata file and produce gpkg file which can be viewed in GIS software
# make graphs
# unit: general in g / s
# rivers per m in g/ m (river section length)
# lakes per m in g / m2 
# author: David Mennekes, PhD Student at Empa, Switzerland, david.mennekes@posteo.de, david.mennekes@empa.ch
# May 2021, last edited: May 2021
############################

	
	# packages
	library(sf)
	library(dplyr)

	# main path
	
	setwd("~/")
	main.path <- "PhD/mennekes/"
	
	#subfolder (change if needed)
	sub.path <- "temp_data/example_river/output/"

	
	# define output folder
	
	#load geo
	
	# polymers
	polymers <- c("LDPE", "HDPE", "PP", "PS", "EPS", "PVC", "PET")
	# polymers <- polymers[2] #choose your polymer
	

	# important: runs you want to save, default you will save all rounds you printed in the script 3.0
	load(paste0(main.path, "temp_data/example_river/output/print_round.Rdata"))
	rounds <- print_round
	
	extra_names <- extra_names
	# extra_name
	# rounds <- c(801)
	
	#columns that should be included in final report (no contamination columns)
	nselection <- c("isLake", "id_all", "flow_velocity", "flow_to", "outflow", "length_m", "length_seconds")
	
	
	
	
	
	

	#files directionary
	rfiles_short <- list.files(path = paste0(main.path, sub.path))
	rfiles_long <- list.files(path = paste0(main.path, sub.path), full.names = T)



	

	
	
	#read.files
	for (extra_name in extra_names) {
		for(mat in polymers){
			rfiles_short_mat <- rfiles_short[grepl(paste0("_", mat, extra_name, ".Rdata"), rfiles_short)]
			rfiles_long_mat <- rfiles_long[grepl(paste0("_", mat, extra_name, ".Rdata"), rfiles_long)]
			for (NR in rounds) {
				r <- grep(paste0("_", NR, "_"), rfiles_short_mat) #check for Number in file name
				load(rfiles_long_mat[r])
				r.rivers <- which(rivers.calc$isLake == F & rivers.calc$outflow == 0)
				r.lakes <- which(rivers.calc$isLake == T)
				r.outflow <- which(rivers.calc$outflow != 2)
				lake_id <- id_lakes <- unique(rivers.calc$FID_poly_s)[-is.na(unique(rivers.calc$FID_poly_s))]
				
				if(length(r)!= 1){next} #if number does not exist, next round
				n_old <- names(rivers.calc)
				#calculate total loads per section and load per river meter
				rivers.calc[ , paste0("total_tot_", mat, "_MiP")] <- 0 #all summed together over the entire distance per segments
				rivers.calc[ , paste0("total_perM_", mat, "_MiP")] <- 0 #same as total_tot but per /m segment
				rivers.calc[ , paste0("water_tot_", mat, "_MiP")] <- 0
				rivers.calc[ , paste0("water_perS_", mat, "_MiP")] <- 0
				rivers.calc[ , paste0("water_perM_", mat, "_MiP")] <- 0
				rivers.calc[ , paste0("sediment_tot_", mat, "_MiP")] <- 0
				rivers.calc[ , paste0("sediment_perS_", mat, "_MiP")] <- 0
				rivers.calc[ , paste0("sediment_perM_", mat, "_MiP")] <- 0
				rivers.calc[ , paste0("accumulation_tot_", mat, "_MiP")] <- 0
				rivers.calc[ , paste0("accumulation_perM_", mat, "_MiP")] <- 0
				rivers.calc[ , paste0("input_tot_", mat, "_MiP")] <- rivers.calc[ , paste0("sum_",mat, "_WaterMP_concMSV")]
				rivers.calc[ , paste0("input_perM_", mat, "_MiP")] <- NA
				rivers.calc[ , paste0("acutalinflow_water_", mat, "_MiP")] <- rivers.calc[ , paste0("actualinflow_", mat, "_WaterMiP_concMSV")]
				rivers.calc[ , paste0("acutalinflow_sediment_", mat, "_MiP")] <- rivers.calc[ , paste0("actualinflow_sed_", mat, "_WaterMiP_concMSV")]
				n_new <- names(rivers.calc)
				n1 <- n_new[!(n_new %in% n_old)]
				
				#weight in gramms
				#for rivers (perM in per m of the river section length)
				# lake section all have the length 1m therefore the numbers will not be changed and water_tot, accumulation.... etc are per second!!!
				rivers.calc[r.rivers , paste0("input_perM_", mat, "_MiP")] <- rivers.calc[r.rivers, paste0("sum_", mat, "_WaterMP_concMSV")] / rivers.calc$length_m[r.rivers]
				
				rivers.calc[ , paste0("water_tot_", mat, "_MiP")] <- rivers.calc[ , paste0("actualcon_", mat, "_WaterMiP_concMSV")] *rivers.calc$length_seconds
				
				rivers.calc[ , paste0("water_perS_", mat, "_MiP")] <- rivers.calc[ , paste0("actualcon_", mat, "_WaterMiP_concMSV")] 
				
				rivers.calc[ , paste0("water_perM_", mat, "_MiP")] <- rivers.calc[ , paste0("water_tot_", mat, "_MiP")] / rivers.calc$length_m
				
				#for sediments
					
				rivers.calc[ r.rivers, paste0("sediment_tot_", mat, "_MiP")] <- rivers.calc[ r.rivers, paste0("actualsed_", mat, "_WaterMiP_concMSV")] * rivers.calc$length_seconds[r.rivers]
				
				rivers.calc[ , paste0("sediment_perS_", mat, "_MiP")] <- rivers.calc[ , paste0("actualsed_", mat, "_WaterMiP_concMSV")] 
				
				
				rivers.calc[ r.rivers, paste0("sediment_perM_", mat, "_MiP")] <- rivers.calc[ r.rivers, paste0("sediment_tot_", mat, "_MiP")] / rivers.calc$length_m[r.rivers]
				
				#accumulation
				rivers.calc[ , paste0("accumulation_tot_", mat, "_MiP")] <- rivers.calc[ , paste0("actualacc_", mat, "_WaterMiP_concMSV")]
				
				rivers.calc[ , paste0("accumulation_perM_", mat, "_MiP")] <- rivers.calc[ , paste0("accumulation_tot_", mat, "_MiP")] / rivers.calc$length_m
				
				#total
				rivers.calc[ , paste0("total_tot_", mat, "_MiP")] <- rivers.calc[ , paste0("accumulation_tot_", mat, "_MiP")] + rivers.calc[ , paste0("sediment_tot_", mat, "_MiP")] + rivers.calc[ , paste0("water_tot_", mat, "_MiP")] #evtl. removal hinzufügen
				
				rivers.calc[ , paste0("total_perM_", mat, "_MiP")] <- rivers.calc[ , paste0("total_tot_", mat, "_MiP")]  / rivers.calc$length_m
				
				
				rivers.calc[r.lakes , paste0("input_perM_", mat, "_MiP")] <- rivers.calc[r.lakes, paste0("sum_", mat, "_WaterMP_concMSV")] / rivers.calc$area[r.lakes]
				
				
				## for all lakes... per lake ID if needed
				# all sections per lake will have the same value. this is better for interpretation
				
				
				lakes_single <- rivers.calc #jeder abschnitt bleibt unabhängig voneinander...
				for (id in lake_id) {
					x = which(rivers.calc$FID_poly_s == id)
					rivers.calc[x , paste0("water_tot_", mat, "_MiP")] <- sum(rivers.calc[x, paste0("actualcon_", mat, "_WaterMiP_concMSV")], na.rm = T) # nothing in sediments, because sediments = accumulation
	
					rivers.calc[x , paste0("water_perS_", mat, "_MiP")] <- sum(rivers.calc[x, paste0("actualcon_", mat, "_WaterMiP_concMSV")], na.rm = T)
					
					rivers.calc[x , paste0("water_perM_", mat, "_MiP")] <- rivers.calc[x , paste0("water_tot_", mat, "_MiP")] / rivers.calc$area[x]
	
					rivers.calc[x , paste0("sediment_tot_", mat, "_MiP")] <- 0
					rivers.calc[x , paste0("sediment_perM_", mat, "_MiP")] <- 0
	
					rivers.calc[x , paste0("accumulation_tot_", mat, "_MiP")] <- sum(rivers.calc[x , paste0("actualacc_", mat, "_WaterMiP_concMSV")])
					rivers.calc[x , paste0("accumulation_perM_", mat, "_MiP")] <- rivers.calc[x , paste0("accumulation_tot_", mat, "_MiP")]/rivers.calc$area[x]
	
					rivers.calc[x , paste0("total_tot_", mat, "_MiP")] <- rivers.calc[x , paste0("water_tot_", mat, "_MiP")] + rivers.calc[x , paste0("accumulation_tot_", mat, "_MiP")] #accumulation = sedimentation therefore only one time considered
	
					rivers.calc[x , paste0("total_perM_", mat, "_MiP")] <- rivers.calc[x , paste0("accumulation_perM_", mat, "_MiP")] + rivers.calc[x , paste0("water_perM_", mat, "_MiP")]
					}
	
				
				
				n2 <- c(nselection, n1) #make vector with all col names which should be selected
				s_r <- rivers.calc[ , n2]
				s_r2 <- lakes_single[ , n2]
				Sys.sleep(5)
				# st_write(geo_x, paste0(output.gpkg, "cont_", mat, "_round_", NR, extra_name, ".gpkg" ), append = F)
				save(s_r, file = paste0(main.path, "temp_data/example_river/output/cont_",mat,"_", NR, extra_name, ".Rdata"))
			}
		}
	}
	

	rm(list = ls())
	