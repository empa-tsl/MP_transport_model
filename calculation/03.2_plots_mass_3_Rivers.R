	#### plot 03

	#total masses polymer specific rhine rhone doubs

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
	summen$value <- summen$value*365*24*60*60/1000 #kg pro Jahr
	df02 <- df01 %>% filter(scenario == "_withallfactors") %>% dplyr::select(-scenario)
	df02$value <- df02$value*365*24*60*60/1000
	df02$polymer <- factor(df02$polymer,
														levels = c(polymers))

	
	#color
	# farben <- c('#222222', '#bdbdbd','#222222', '#bdbdbd','#222222', '#bdbdbd','#222222')
	farben <- rev(c("#cc5700", "#c97544", "#bf9077", "#aaaaaa", "#92a1c7", "#6f98e3", "#1e90ff"))
	
	#ggtheme
	nice <- theme_classic()+
		theme(legend.position = "none",
					legend.background = element_rect(fill = "transparent", color = NA),
					plot.background = element_rect(fill = "transparent",colour = NA),
					text = element_text(color = "black", size = 7),
					axis.text.x = element_text(color = "black", face = "plain", size = 7),
					axis.text.y = element_text(color = "black", face = "plain", size = 7),
					axis.title.y = element_text(color = "black", face = "plain", size = 7),
					axis.ticks.x = element_blank(),
					axis.title.x = element_blank(),
					panel.background = element_rect(fill = "transparent")) #trbl
	
	# barplots per river
	#rhone
	rhone01 <- ggplot(data=df02 %>% filter(river == "Rhône i"), aes(x=river, y=value, fill=polymer)) +
		geom_bar(stat="identity", show.legend = F)+
		scale_y_continuous(expand = c(0,0,0, summen$value[summen$scenario == "_withallfactors"&summen$river == "Rhône i"]*0.1), breaks = c(20,40,60,80))+
		scale_x_discrete( expand = c(0.4,0.1), label = "Rhône\noutflow Lake Geneva\n(before Geneva)")+
		geom_text(aes(y = summen$value[summen$scenario == "_withallfactors"&summen$river == "Rhône i"], label= round(summen$value[summen$scenario == "_withallfactors"&summen$river == "Rhône i"],0)), vjust=-0.8, color="black", size=2)+
		labs(y = "mass microplastic\nin [kg / year]")+
		scale_fill_manual("",
											values = farben)+
		nice +
		theme(plot.margin=unit(c(0.1,0.2,0.1,0.7),"cm"),
					axis.title.y = element_blank()) #trbl
	
	
	rhone02 <- ggplot(data=df02 %>% filter(river == "Rhône ii"), aes(x=river, y=value, fill=polymer)) +
		geom_bar(stat="identity")+
		scale_y_continuous(expand = c(0,0,0, summen$value[summen$scenario == "_withallfactors"&summen$river == "Rhône ii"]*0.1))+
		scale_x_discrete( expand = c(0.4,0.1), label = "Rhône\n \n(after Geneva)")+
		geom_text(aes(y = summen$value[summen$scenario == "_withallfactors"&summen$river == "Rhône ii"], label= round(summen$value[summen$scenario == "_withallfactors"&summen$river == "Rhône ii"],0)), vjust=-0.8, color="black", size=2)+
		labs(y = "MiP\nin [kg / year]")+
		scale_fill_manual("",
											values = farben)+
		nice +
		theme(plot.margin=unit(c(0.1,0.9,0.1,0.),"cm"),
					axis.title.y = element_blank()) #trbl
	
	
	#rhine
	rhine <- ggplot(data=df02 %>% filter(river == "Rhine"), aes(x=river, y=value, fill=polymer)) +
		geom_bar(stat="identity")+
		scale_y_continuous(expand = c(0,0,0, summen$value[summen$scenario == "_withallfactors"&summen$river == "Rhine"]*0.1))+
		scale_x_discrete( expand = c(0.4,0.1), label = "Rhine\n ")+
		geom_text(aes(y = summen$value[summen$scenario == "_withallfactors"&summen$river == "Rhine"], label= round(summen$value[summen$scenario == "_withallfactors"&summen$river == "Rhine"],0)), vjust=-0.8, color="black", size=2)+
		labs(y = "microplastic outflow\nin suspension [kg / year]")+
		scale_fill_manual("",
											values = farben)+
		nice +
		theme(plot.margin=unit(c(0.1,0.6,0.1,0.2),"cm"),
					axis.title = element_text(size = 7))
	
	
	#doubs
	doubs <- ggplot(data=df02 %>% filter(river == "Doubs"), aes(x=river, y=value, fill=polymer)) +
		geom_bar(stat="identity")+
		scale_y_continuous(expand = c(0,0,0, summen$value[summen$scenario == "_withallfactors"&summen$river == "Doubs"]*0.1))+
		scale_x_discrete( expand = c(0.4,0.1))+
		geom_text(aes(y = summen$value[summen$scenario == "_withallfactors"&summen$river == "Doubs"], label= round(summen$value[summen$scenario == "_withallfactors"&summen$river == "Doubs"],0)), vjust=-0.8, color="black", size=2)+
		labs(y = "MiP\nin [kg / year]")+
		scale_fill_manual("",
											values = farben)+
		nice +
		theme(plot.margin=unit(c(0.1,0.6,0.1,0.4),"cm"),
					axis.title.y = element_blank())
	
	#legend
	l <- data.frame(value = rev(c(1.5,2.5,1,7,1,4,1)),
									river = "Legend",
									polymer = polymers,
									pos = c(0.75,2.75,4.5,8.5,12.5,15,17.5))
	l$polymer <- factor(l$polymer,
											levels = c(polymers))
	
	legende <- ggplot(data=l, aes(x=river, y=value, fill=polymer)) +
		geom_bar(stat="identity")+
		scale_y_continuous(expand = c(0,0.1,0, 17.5*0.1))+
		scale_x_discrete( expand = c(0.4,0.1))+
		geom_text(aes(y = pos, label = rev(polymer)), color = c("black"), size = 2)+
		labs(y = "microplastic outflow\nin suspension [kg / year]")+
		scale_fill_manual("",
											values = farben)+
		theme_void()+
		theme(plot.margin=unit(c(0.1,0,0.1,0.2),"cm"),
				legend.position = "none",
				axis.text.x = element_text(color = "black", size = 7))

	legende
	#plots zusammenbauen	
	
	pall <- ggdraw(plot_grid(rhine, rhone01, rhone02, doubs, legende, nrow = 1, rel_widths = c(1.25,1, 1,1,0.5), align = "h"))
	
	pall
	ggsave(paste0(main.path, "output_files/plots/outflow_rivers.png"), pall, height = 6, width = 17, units = "cm", dpi = 500, bg = "transparent")
	ggsave(paste0(main.path, "output_files/plots/outflow_rivers.pdf"), pall, height = 6, width = 17, units = "cm", dpi = 500, bg = "transparent")
	
	
	
	rm(list = ls())
	