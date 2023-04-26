#### plot 02

#total masses outflows:

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
rounds <- "801"




load(paste0(main.path, "output_files/rdata/cont_HDPE_", rounds, without, ".Rdata"))
rm(s_r)
compartments <- c("outflow", "sediment_tot", "accumulation_tot")
scenarios <- c( without, lakesonly, withf)

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
		df01[counter, 4] <- sum(s_r2[s_r2$outflow %in% c(1,3), paste0("water_perS_",mat,"_MiP")]) #4.spalte kommt der entsprechende ausfluss in einem der großen flüsse
		df01[I(counter+1), 1:3] <- c(j,mat, "accumulation") # accumulating amount around everywhere
		df01[I(counter+1), 4] <- sum(s_r2[s_r2$outflow == 0, paste0("accumulation_tot_",mat,"_MiP")])
		df01[I(counter+2), 1:3] <- c(j,mat, "sedimentation") # accumulating amount around everywhere
		df01[I(counter+2), 4] <- sum(s_r2[s_r2$outflow == 0, paste0("sediment_tot_",mat,"_MiP")])
		
		counter <- counter +3
	}
}
rm(s_r2)
df01


####check unterschiede zwischen Seen und flüssen

df01 <- as.data.frame(matrix(NA, nrow = length(polymers)*length(scenarios)*2, ncol = 4)) #*2 for scenarios
names(df01) <- c("scenario", "polymer",  "compartment", "value")


#load data
counter <- 1
for (mat in polymers) {
	for (j in scenarios) {
		load(paste0(main.path, "output_files/rdata/lakes_single/cont_", mat,"_", rounds, j, ".Rdata"))
		df01[counter, 1:3] <- c(j,mat, "outflow") # Namen in die Spalten 1 bis 3, compartment outflow aus dem Land aber auch in NA
		df01[counter, 4] <- sum(s_r2[s_r2$outflow %in% c(1,3), paste0("water_perS_",mat,"_MiP")]) #4.spalte kommt der entsprechende ausfluss in einem der großen flüsse
		df01[I(counter+1), 1:3] <- c(j,mat, "accumulation_lake") # accumulating amount around everywhere
		df01[I(counter+1), 4] <- sum(s_r2[s_r2$outflow == 0 & s_r2$isLake == T, paste0("accumulation_tot_",mat,"_MiP")])
		df01[I(counter+2), 1:3] <- c(j,mat, "accumulation_river") # accumulating amount around everywhere
		df01[I(counter+2), 4] <- sum(s_r2[s_r2$outflow == 0 & s_r2$isLake == F, paste0("accumulation_tot_",mat,"_MiP")])
		
		counter <- counter +3
	}
}
rm(s_r2)
all <- df01 %>% filter(scenario == "_withallfactors", compartment != "outflow")
lake <- df01 %>% filter(scenario == "_lakeonlyfactors", compartment != "outflow")

all$value_lake <- lake$value
all$value_diff <- all$value-lake$value
all
#Erkenntniss: Wenn man die Flüsse mit einberechnet, dann gibt es in der Tat eine etwas größere Reduction von MiP in Suspension. Allerdings liegt das vor allem daran, dass MP im Sediment vorrübergehend gelagert ist...Vergleichend zwischen den verschiedenen Polymeren, kann sich die akkumulation in gleichen Größenordnungen bewegen für PET/PVC, wobei es ein bis größenordnungen kleiner ausfällt für LDPE/HDPE/PS und PP respective. Die acc in Seen wird leicht vergeringert, wenn die acc in Flüssen berücksichtigt wird.











#overall change, including lake 15
#outflow 1 und 3 für outflow in NA und durch große flüsse
scenarios <- c("_withoutfactors", "_lakeonlyfactors", "_lake15", "_withallfactors")

df01 <- as.data.frame(matrix(NA, nrow = length(polymers)*length(scenarios)*2, ncol = 4)) #*2 for scenarios
names(df01) <- c("scenario", "polymer",  "compartment", "value")


#load data
counter <- 1
for (mat in polymers) {
	for (j in scenarios) {
		load(paste0(main.path, "output_files/rdata/lakes_single/cont_", mat,"_", rounds, j, ".Rdata"))
		df01[counter, 1:3] <- c(j,mat, "outflow") # Namen in die Spalten 1 bis 3, compartment outflow aus dem Land aber auch in NA
		df01[counter, 4] <- sum(s_r2[s_r2$outflow %in% c(1,3), paste0("water_perS_",mat,"_MiP")]) #4.spalte kommt der entsprechende ausfluss in einem der großen flüsse, 
		df01[I(counter+1), 1:3] <- c(j,mat, "accumulation_lake") # accumulating amount around everywhere
		df01[I(counter+1), 4] <- sum(s_r2[s_r2$outflow == 0 & s_r2$isLake == T, paste0("accumulation_tot_",mat,"_MiP")])
		df01[I(counter+2), 1:3] <- c(j,mat, "accumulation_river") # accumulating amount around everywhere
		df01[I(counter+2), 4] <- sum(s_r2[s_r2$outflow == 0 & s_r2$isLake == F, paste0("accumulation_tot_",mat,"_MiP")])
		
		counter <- counter +3
	}
}
rm(s_r2)

df01
df02 <- df01 %>% filter(compartment == "outflow") %>% group_by(scenario) %>% summarise(MiP = sum(value))
df02$anteil <- 1-(df02$MiP/df02$MiP[df02$scenario == "_withoutfactors"])
df02 #Anteil shows how much is retained in percent

#how important are lake15
df02$MiP[2]/df02$MiP[1] #99% in big lakes

#contribution rivers
(df02$MiP[2]- df02$MiP[3])/df02$MiP[4]


#differences per polymer
(df01$value[df01$scenario=="_lakeonlyfactors" & df01$polymer == "PET" & df01$compartment == "outflow"] - df01$value[df01$scenario=="_withallfactors" & df01$polymer == "PET" & df01$compartment == "outflow"]) / df01$value[df01$scenario=="_withoutfactors" & df01$polymer == "PET" & df01$compartment == "outflow"]






###########
#check for masses leaving Switzerland via the catchments of focus. Analyse transport in suspension as well as transport in sediments

#load data

polymers <- c("EPS","PP", "LDPE", "HDPE", "PS", "PVC", "PET")

main.path <- "PhD/mennekes/"
withf <- "_withallfactors"
rounds <- "801"

load(paste0(main.path, "output_files/rdata/cont_HDPE_", rounds, withf, ".Rdata"))
# s_r[s_r$outflow==1, ]
ids <- c(441464,  110437, 441470, 441468) #ids for outflow (Rhine, Rhône (vor und nach Genf), Doubs)
s_r[ids, ] #check
rm(s_r)
scenarios <- c( withf)
# scenarios <- c( withf)

# load for each variable a one data frame

# overall contamination switzerland, figure with overall burial, accumulation, sedimentation and plastics in water
#create dataframe


df01 <- as.data.frame(matrix(NA, nrow = length(polymers)*length(scenarios)*length(ids), ncol = 5)) #*2 for scenarios
names(df01) <- c("scenario", "polymer",  "river", "value_water", "value_sed")


#load data
counter <- 1
for (mat in polymers) {
	for (j in scenarios) {
		load(paste0(main.path, "output_files/rdata/lakes_single/cont_", mat,"_", rounds, j, ".Rdata"))
		#rhine
		df01[counter, 1:3] <- c(j,mat, "Rhine") # Namen in die Spalten 1 bis 3, river Rhine aus dem Land aber auch in NA
		df01[counter, 4] <- sum(s_r2[ids[1], paste0("water_perS_",mat,"_MiP")]) #4.spalte kommt der entsprechende ausfluss
		df01[counter, 5] <- sum(s_r2[ids[1], paste0("acutalinflow_sediment_",mat,"_MiP")]) #5. spalte kommt sediment
		#rhone before geneva
		df01[I(counter+1), 1:3] <- c(j,mat, "Rhône i") # Namen in die Spalten 1 bis 3, river rhone aus dem Land aber auch in NA
		df01[I(counter+1), 4] <- sum(s_r2[ids[2], paste0("water_perS_",mat,"_MiP")])
		df01[I(counter+1), 5] <- sum(s_r2[ids[2], paste0("acutalinflow_sediment_",mat,"_MiP")]) #5. spalte kommt sediment
		df01[I(counter+2), 1:3] <- c(j,mat, "Rhône ii") # Namen in die Spalten 1 bis 3, river rhone aus dem Land aber auch in NA
		df01[I(counter+2), 4] <- sum(s_r2[ids[3], paste0("water_perS_",mat,"_MiP")])
		df01[I(counter+2), 5] <- sum(s_r2[ids[3], paste0("acutalinflow_sediment_",mat,"_MiP")])
		#doubs
		df01[I(counter+3), 1:3] <- c(j,mat, "Doubs") # Namen in die Spalten 1 bis 3, river Rhine aus dem Land aber auch in NA
		df01[I(counter+3), 4] <- sum(s_r2[ids[4], paste0("water_perS_",mat,"_MiP")])
		df01[I(counter+3), 5] <- sum(s_r2[ids[4], paste0("acutalinflow_sediment_",mat,"_MiP")]) #5. spalte kommt sediment
		counter <- counter +4
	}
}

df01$total <- df01$value_sed+df01$value_water
df02 <- df01 %>% group_by(river) %>% summarise(tot = sum(total,na.rm = T)*365*24*60*60, sed = sum(value_sed,na.rm = T)*365*24*60*60,wat = sum(value_water,na.rm = T)*365*24*60*60)
df02

df02$sed/df02$tot*100 #percent of sediment outflow from total outflow



####
#share among total and other rivers

load(paste0(main.path, "output_files/rdata/cont_HDPE_", rounds, without, ".Rdata"))
#check for all outflows == 1 and outflows == 3
of1 <- which(s_r$outflow == 1)
of3 <- which(s_r$outflow == 3)

df01 <- as.data.frame(matrix(NA, nrow = (length(of1)+1)*length(polymers), ncol = 5)) #*2 for scenarios
names(df01) <- c( "polymer",  "river", "value_water", "id_all", "name")


#load data
counter <- 1
for (mat in polymers) {
		load(paste0(main.path, "output_files/rdata/cont_", mat,"_", rounds,  "_withallfactors.Rdata"))
		#rhine
	df01[counter:I(counter+length(of1)), 1] <- mat
		df01[counter:I(counter+length(of1)-1), 2:5] <- s_r[of1, c("name_river", paste0("water_perS_", mat, "_MiP"), "id_all", "NAME")]
		#for all the flow to unknonw (outflow == 3)
		df01[I(counter+length(of1)), 3]<- sum(s_r[of3, paste0("water_perS_", mat, "_MiP")], na.rm = T)
		df01[I(counter+length(of1)), c(2,4,5)] <- c("unknown", 0, "unknown")
		counter <- counter + length(of1)+1
}
tail(df01)

#total outflow
total_of <- sum(df01$value_water)
total_of

total_catchment_focus <- df01 %>% filter(river%in% c("Rhein", "Le Doubs", "Le Rhone")) %>% summarise(sum(value_water))
total_catchment_focus

total_catchment_focus/total_of #percent of outflow was covered by catchments of focus


sums_catchments <- df01 %>% group_by(river) %>% summarise(MiP = sum(value_water))

sums_catchments$percent_total <- sums_catchments$MiP/total_of
sums_catchments$percent_missing <- sums_catchments$MiP/as.numeric(total_of-total_catchment_focus)
sums_catchments

sums_catchments$MiP*365*24*60*60/1000







##### compare with measurement values##################
polymers <- c("EPS","PP", "LDPE", "HDPE", "PS", "PVC", "PET")

main.path <- "PhD/mennekes/"
withf <- "_withallfactors"
rounds <- "801"

load(paste0(main.path, "output_files/rdata/cont_HDPE_", rounds, withf, ".Rdata"))
# s_r[s_r$outflow==1, ]
ids <- c(441464,  110437, 441470, 441468) #ids for outflow (Rhine, Rhône (vor und nach Genf), Doubs)
s_r[ids, ] #check
rm(s_r)
compartments <- c("outflow")
scenarios <- c( withf)
# scenarios <- c( withf)

# load for each variable a one data frame

# overall contamination switzerland, figure with overall burial, accumulation, sedimentation and plastics in water
#create dataframe


df01 <- as.data.frame(matrix(NA, nrow = length(polymers)*length(scenarios)*length(ids), ncol = 4)) #*2 for scenarios
names(df01) <- c("scenario", "polymer",  "river", "value")


#load data
counter <- 1
for (mat in polymers) {
	for (j in scenarios) {
		load(paste0(main.path, "output_files/rdata/cont_", mat,"_", rounds, j, ".Rdata"))
		#rhine
		df01[counter, 1:3] <- c(j,mat, "Rhine") # Namen in die Spalten 1 bis 3, river Rhine aus dem Land aber auch in NA
		df01[counter, 4] <- sum(s_r[ids[1], paste0("water_perS_",mat,"_MiP")]) #4.spalte kommt der entsprechende ausfluss in einem der großen flüsse
		#rhone before geneva
		df01[I(counter+1), 1:3] <- c(j,mat, "Rhône i") # Namen in die Spalten 1 bis 3, river rhone aus dem Land aber auch in NA
		df01[I(counter+1), 4] <- sum(s_r[ids[2], paste0("water_perS_",mat,"_MiP")])
		
		df01[I(counter+2), 1:3] <- c(j,mat, "Rhône ii") # Namen in die Spalten 1 bis 3, river rhone aus dem Land aber auch in NA
		df01[I(counter+2), 4] <- sum(s_r[ids[3], paste0("water_perS_",mat,"_MiP")])
		#doubs
		df01[I(counter+3), 1:3] <- c(j,mat, "Doubs") # Namen in die Spalten 1 bis 3, river Rhine aus dem Land aber auch in NA
		df01[I(counter+3), 4] <- sum(s_r[ids[4], paste0("water_perS_",mat,"_MiP")])
		counter <- counter +4
	}
}
rm(s_r)
df01


summen_sec <- df01 %>% group_by(scenario, river) %>% summarise(value = sum(value))
summen <- summen_sec
summen
summen$value*365*24*60*60/1000 #kg pro Jahr
df02 <- df01 %>% filter(scenario == "_withallfactors") %>% dplyr::select(-scenario)
df02$value <- df02$value*365*24*60*60/1000
df02$polymer <- factor(df02$polymer,
											 levels = c(polymers))

