	#### plot 04
	
	#total masses polymer specific for selected ids
	
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
	
	polymers <- c("EPS","PP", "LDPE", "HDPE", "PS", "PVC", "PET")
	
	main.path <- "PhD/mennekes/"
	without <- "_withoutfactors"
	withf <- "_withallfactors"
	lakesonly <- "_lakeonlyfactors"
	rounds <- "801"
	
	load(paste0(main.path, "output_files/rdata/cont_HDPE_", rounds, without, ".Rdata"))
	# s_r[s_r$outflow==1, ]
	ids <- c(110436) #ids for outflow (Rhine, Rhône, Doubs)
	s_r[ids, ] #check
	rm(s_r)
	compartments <- c("outflow", "sediment_tot", "accumulation_tot")
	scenarios <- c( withf)
	
	# load for each variable a one data frame
	
	# overall contamination switzerland, figure with overall burial, accumulation, sedimentation and plastics in water
	#create dataframe
	
	
	df01 <- as.data.frame(matrix(NA, nrow = length(polymers)*length(scenarios)*length(ids), ncol = 5)) #*2 for scenarios
	names(df01) <- c("scenario", "polymer",  "river", "id", "value_g_per_s") #value per s
	
	
	#load data
	counter <- 1
	for (mat in polymers) {
		for (j in scenarios) {
			load(paste0(main.path, "output_files/rdata/lakes_single/cont_", mat,"_", rounds, j, ".Rdata"))
			#rhine
			for (id in ids) {
				df01[counter, 1:2] <- c(j,mat) # Namen in die Spalten 1 bis 2
				df01[counter, 3] <- s_r2[id, "NAME"] #name of river
				df01[counter, 4] <- id #id
				df01[counter, 5] <- s_r2[id, paste0("total_perM_",mat,"_MiP")]*s_r2$flow_velocity[id] #value
				
				counter <- counter +1
			}
		}
	}
	rm(s_r2)
	df01
	
	
	summen_sec <- df01 %>% group_by(scenario, id) %>% summarise(value_g_per_s = sum(value_g_per_s))
	summen <- summen_sec
	summen$value <- summen$value_g_per_s*365*24*60*60/1000 #kg pro Jahr
	df02 <- df01 %>% filter(scenario == "_withallfactors") %>% dplyr::select(-scenario)
	df02$value <- df02$value_g_per_s*365*24*60*60/1000
	df02$polymer <- factor(df02$polymer,
												 levels = c(polymers))
	
	#color
	farben <- c('#222222', '#bdbdbd','#222222', '#bdbdbd','#222222', '#bdbdbd','#222222')
	#ggtheme
	nice <- theme_classic()+
		theme(legend.position = "none",
					legend.background = element_rect(fill = "transparent", color = NA),
					plot.background = element_rect(fill = "transparent",colour = NA),
					text = element_text(color = "black"),
					axis.text.x = element_text(color = "black", face = "plain", size = 10),
					axis.text.y = element_text(color = "black", face = "plain", size = 11),
					axis.title.x = element_blank(), 
					axis.ticks.x = element_blank(),
					panel.background = element_rect(fill = "transparent")) #trbl
	
	
	# barplots per river
	#rhone
	ggplot(data=df02 %>% filter(id == ids), aes(x=river, y=value, fill=polymer)) +
		geom_bar(stat="identity")+
		scale_y_continuous(expand = c(0,0,0, summen$value[summen$scenario == "_withallfactors"&summen$id == ids]*0.1))+
		scale_x_discrete( expand = c(0.4,0.1))+
		geom_text(aes(y = summen$value[summen$scenario == "_withallfactors"&summen$id == ids], label= round(summen$value[summen$scenario == "_withallfactors"&summen$id == ids],0)), vjust=-0.8, color="black", size=3.5)+
		labs(y = "MiP\nin [kg / year]")+
		scale_fill_manual("",
											values = farben)+
		nice +
		theme(plot.margin=unit(c(0.1,0.6,0.1,0.2),"cm"),
					axis.title.y = element_blank()) #trbl
