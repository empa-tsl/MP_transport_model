#plot along ther rivers.

# first all rivers that belong to the system

library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggrepel)
library(patchwork) #for making all images the same dimensions.
library(cowplot)
library(sf)
library(reshape2)

#path:
main.path <- "PhD/mennekes/"
sub.path <- "temp_data/example_river/output/"

polymers <- c("LDPE", "HDPE", "PP", "PS", "EPS", "PVC", "PET")

load(paste0(main.path, "temp_data/example_river/output/cont_PET_25_withallfactors.Rdata"))
d01 <- data.frame(id_all = s_r$id_all,
									lengths_s = s_r$length_seconds,
									lengths_m = s_r$length_m,
									isLake = s_r$isLake,
									input = s_r$input_tot_PET_MiP)


rfiles_short <- list.files(path = paste0(main.path, sub.path))
rfiles_long <- list.files(path = paste0(main.path, sub.path), full.names = T)

for (mat in polymers) {
	print(mat)
	rfiles <- rfiles_long[grepl(paste0("cont_", mat, "_"), rfiles_short)]
	load(rfiles)
	d01[ , paste0("water_perS_", mat)] <- s_r[ , paste0("water_perS_", mat, "_MiP")]
	d01[ , paste0("sediment_perS_", mat)] <- s_r[ , paste0("sediment_perS_", mat, "_MiP")]
	d01[ , paste0("accumulation_tot_", mat)] <- s_r[ , paste0("accumulation_tot_", mat, "_MiP")]
}



s <- c(1,3,4,5,6,8,14) #select only one river branch -> see figure in SI for the IDs
d02 <- d01[s, ]
d02$cumsum_m <- cumsum(d02$lengths_m)/1000


#polymer in the water. input is 1 for the ID 1 and 12
ggplot(d02, aes(x = cumsum_m))+
	geom_line(aes(y = water_perS_EPS, color = "EPS"))+
	geom_line(aes(y = water_perS_PS, color = "PS"))+
	geom_line(aes(y = water_perS_PET, color = "PET"))+
	theme_bw()+
	labs(x = "river lengths in [km]",
			 y = "contamination transport in suspension\n[per second]")+
	geom_label(aes(y = -0.1, label = id_all))+
	scale_color_manual("polymer", values = c('dodgerblue','#555555',"#CC5700"))
	





rm(list = ls())
