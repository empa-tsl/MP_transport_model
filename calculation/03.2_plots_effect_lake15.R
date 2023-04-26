#### plot 02

#total masses outflows with lake15

####
# plotting figures of the model
# author: david mennekes, david.mennekes@empa.ch, david.mennekes@posteo.de
# march 2022
##################


# library packages

library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggrepel)
library(patchwork) #for making all images the same dimensions.
library(cowplot)



#load data

polymers <- c("LDPE", "HDPE", "PP", "PS", "EPS", "PVC", "PET")

main.path <- "PhD/mennekes/"
without <- "_withoutfactors"
withf <- "_withallfactors"
lakesonly <- "_lakeonlyfactors"
lake15 <- "_lake15"
rounds <- "801"

load(paste0(main.path, "output_files/rdata/cont_HDPE_", rounds, without, ".Rdata"))
rm(s_r)
compartments <- c("outflow", "sediment_tot", "accumulation_tot")
scenarios <- c( without, lakesonly, lake15, withf)

# load for each variable a one data frame

# overall contamination switzerland, figure with overall burial, accumulation, sedimentation and plastics in water
#create dataframe


df01 <- as.data.frame(matrix(NA, nrow = length(polymers)*length(scenarios)*2, ncol = 4)) #*2 for scenarios
names(df01) <- c("scenario", "polymer",  "compartment", "value")


#load data
counter <- 1
for (mat in polymers) {
	for (j in scenarios) {
		load(paste0(main.path, "output_files/rdata/lakes_single/cont_", mat,"_", rounds, j, ".Rdata"))
		df01[counter, 1:3] <- c(j,mat, "outflow") # Namen in die Spalten 1 bis 3, compartment outflow aus dem Land aber auch in NA
		df01[counter, 4] <- sum(s_r2[s_r2$outflow %in% c(1,3), paste0("total_perM_",mat,"_MiP")]) #4.spalte kommt der entsprechende ausfluss in einem der großen flüsse
		df01[I(counter+1), 1:3] <- c(j,mat, "accumulation") # accumulating amount around everywhere
		df01[I(counter+1), 4] <- sum(s_r2[s_r2$outflow == 0, paste0("accumulation_tot_",mat,"_MiP")])
		df01[I(counter+2), 1:3] <- c(j,mat, "sedimentation") # accumulating amount around everywhere
		df01[I(counter+2), 4] <- sum(s_r2[s_r2$outflow == 0, paste0("sediment_tot_",mat,"_MiP")])
		
		counter <- counter +3
	}
}
rm(s_r2)
df01 #Tabelle mit Reduktionen zum Vergleichen auch mit lake 15 Scenario

rm(list = ls())
