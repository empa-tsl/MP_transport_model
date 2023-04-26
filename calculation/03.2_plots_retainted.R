####
# plotting figures of the model
# author: david mennekes, david.mennekes@empa.ch, david.mennekes@posteo.de
# march 2022
##################

setwd("~/")
# library packages

	library(ggplot2)
	library(dplyr)
	library(tidyverse)
	library(ggrepel)
library(patchwork) #for making all images the same dimensions.
library(cowplot)
library(ggpattern)

	
	
	#load data
	
	polymers <- c("LDPE", "HDPE", "PP", "PS", "EPS", "PVC", "PET")

	main.path <- "PhD/mennekes/"
	without <- "_withoutfactors"
	withf <- "_withallfactors"
	lakesonly <- "_lakeonlyfactors"
	rounds <- "801"	

	load(paste0(main.path, "output_files/rdata/cont_HDPE_", rounds, without, ".Rdata"))
	rm(s_r)
	scenarios <- c( without,  lakesonly, withf)
	
	###df 02 for water
	df02 <- data.frame(river = rep(NA, 3*length(polymers)),
										 none = NA,
										 lakes = NA,
										 all = NA,
										 polymer = NA)
	counter <- 1
	for (mat in polymers) {
		c2 <- 2
		for (j in scenarios) {
			load(paste0(main.path, "output_files/rdata/cont_", mat,"_", rounds, j, ".Rdata"))
			df02[counter:I(counter +2), "river"] <- c("rhine", "rhone", "Le doubs") 
			df02[counter:I(counter +2), "polymer"] <- mat
			df02[counter:I(counter +2), c2] <- s_r[c(441464, 441470, 441468), paste0("water_tot_", mat, "_MiP")]
			c2 <- c2 +1
		}
			counter <- counter +3
	}
	
	
	#df03 for sediment
	df03 <- data.frame(river = rep(NA, 3*length(polymers)),
										 none = NA,
										 lakes = NA,
										 all = NA,
										 polymer = NA)
	counter <- 1
	for (mat in polymers) {
		c2 <- 2
		for (j in scenarios) {
			load(paste0(main.path, "output_files/rdata/cont_", mat,"_", rounds, j, ".Rdata"))
			df03[counter:I(counter +2), "river"] <- c("rhine", "rhone", "Le doubs") 
			df03[counter:I(counter +2), "polymer"] <- mat
			df03[counter:I(counter +2), c2] <- s_r[c(441464, 441470, 441468), paste0("acutalinflow_sediment_", mat, "_MiP")]
			c2 <- c2 +1
		}
		counter <- counter +3
	}
	df03
	
	
	df02
	df02$rest <- df02$all
	df02$River <- df02$lakes-df02$all#reduction through rivers
	df02$River2 <- df02$lakes-(df02$all+df03$all) # add outflow through sediments to the river, and see how much is reduce when we consider outflow through sediments and suspension. The number is lower than Rivers, because the reduction is lower.
	df02$River1 <- df02$River-df02$River2 # gives the diff which will outflow in the sediment
	df02$Lake <- df02$none-df02$lakes
	df02 <- as_tibble(df02)
	df02
	#colors
	blue <- c('#bdd7e7','#6baed6','#2171b5')
	black <- c('#cccccc','#969696','#525252')
	red <- c('#fcae91','#fb6a4a','#cb181d')
	nice <- theme(legend.position = "none",
								legend.background = element_rect(fill = "transparent", color = NA),
								plot.background = element_rect(fill = "transparent",colour = NA),
								text = element_text(color = "black", size = 12),
								legend.text = element_text(size = 7),
								plot.margin=unit(c(0.0, -0.7,0.1,-0.6),"cm")) #trbl
	
	
	###########################################################################
	#kreis Diagramme
	#EPS
	#rhine
	ddd <- df02 %>% filter(polymer == "EPS"&river == "rhine") %>% dplyr::select(rest, River, Lake)  #!
	ddd2 <- data.frame(value = as.numeric(ddd),
										 gr = names(ddd))
	ddd2$percent <- round(ddd2$value/sum(ddd2$value)*100,1)
	ddd2$percent[1] <- 100 - sum(ddd2$percent[2:3])
	df2 <- ddd2 %>% 
		mutate(csum = rev(cumsum(rev(value))), 
					 pos = value/2 + lead(csum, 1),
					 pos = if_else(is.na(pos), value/2, pos))
	
	p11 <- ggplot(ddd2, aes(x = "" , y = value, fill = fct_inorder(gr))) +
		geom_col() +
		coord_polar(theta = "y") +
		scale_fill_manual("",
											values = blue,
											labels = c("remaining", "rivers", "lakes"),
											breaks = c("rest", "River", "Lake"))+
		geom_label_repel(data = df2,
										 aes(y = pos, label = paste0(percent, "%")),
										 size = 4.5, nudge_x = 1, show.legend = FALSE,
										 segment.colour = NA) +
		guides(fill = guide_legend(title = element_blank())) +
		theme_void()+
		nice
	p11
	#rhone
	ddd <- df02 %>% filter(polymer == "EPS"&river == "rhone") %>% dplyr::select(rest, River, Lake) 
	ddd2 <- data.frame(value = as.numeric(ddd),
										 gr = names(ddd))
	ddd2$percent <- round(ddd2$value/sum(ddd2$value)*100,1)
	ddd2$percent[1] <- 100 - sum(ddd2$percent[2:3])
	df2 <- ddd2 %>% 
		mutate(csum = rev(cumsum(rev(value))), 
					 pos = value/2 + lead(csum, 1),
					 pos = if_else(is.na(pos), value/2, pos))
	
	p12 <- ggplot(ddd2, aes(x = "" , y = value, fill = fct_inorder(gr))) +
		geom_col() +
		coord_polar(theta = "y") +
		scale_fill_manual("",
											values = blue,
											labels = c("remaining", "rivers", "lakes"),
										breaks = c("rest", "River", "Lake"))+
		geom_label_repel(data = df2,
										 aes(y = pos, label = paste0(percent, "%")),
										 size = 4.5, nudge_x = 1, show.legend = FALSE, segment.colour = NA) +
		guides(fill = guide_legend(title = element_blank())) +
		theme_void()+
		nice
	
	# le doubs
	ddd <- df02 %>% filter(polymer == "EPS"&river == "Le doubs") %>% dplyr::select(rest, River, Lake) 
	ddd2 <- data.frame(value = as.numeric(ddd),
										 gr = names(ddd))
	ddd2$percent <- round(ddd2$value/sum(ddd2$value)*100,1)
	ddd2$percent[1] <- 100 - sum(ddd2$percent[2:3])
	df2 <- ddd2 %>% 
		mutate(csum = rev(cumsum(rev(value))), 
					 pos = value/2 + lead(csum, 1),
					 pos = if_else(is.na(pos), value/2, pos))
	
	p13 <- ggplot(ddd2, aes(x = "" , y = value, fill = fct_inorder(gr))) +
		geom_col() +
		coord_polar(theta = "y") +
		scale_fill_manual("",
											values = blue,
											labels = c(" ", " ", " "),
										breaks = c("rest", "River", "Lake"))+
		geom_label_repel(data = df2,
										 aes(y = pos, label = paste0(percent, "%")),
										 size = 4.5, nudge_x = 1, force = 5, max.time = 2, show.legend = FALSE, segment.colour = NA) +
		guides(fill = guide_legend(title = element_blank())) +
		theme_void()+
		nice+
		theme(legend.position = "right")
	
	legend1 <- get_legend(p13)
	p13 <- p13+theme(legend.position = "none")
		
	
	#PS#######
	#rhine
	ddd <- df02 %>% filter(polymer == "PS"&river == "rhine") %>% dplyr::select(rest, River, Lake) 
	ddd2 <- data.frame(value = as.numeric(ddd),
										 gr = names(ddd))
	ddd2$percent <- round(ddd2$value/sum(ddd2$value)*100,1)
	ddd2$percent[1] <- 100 - sum(ddd2$percent[2:3])
	df2 <- ddd2 %>% 
		mutate(csum = rev(cumsum(rev(value))), 
					 pos = value/2 + lead(csum, 1),
					 pos = if_else(is.na(pos), value/2, pos))
	
	ddd3 <- df02 %>% filter(polymer == "PS"&river == "rhine") %>% dplyr::select(rest, River2, River1, Lake) 
	ddd32 <- data.frame(value = as.numeric(ddd3),
										 gr = names(ddd3))
	ddd32$group <- c("1","2","2","3")
	ddd32$subgroup <- c("1","1","2","1")
	ddd32$pos <- NA
	ddd32$percent <- NA
	ddd32$csum <- NA
	ddd32$pos[c(1,2,4)] <- df2$pos
	ddd32$csum[c(1,2,4)] <- df2$csum
	ddd32$percent[c(1,2,4)] <- df2$percent
	
	
	p21 <- ggplot(ddd32, aes(x = "" , y = value,fill = group, group = group)) +
		coord_polar(theta = "y") +
		geom_col_pattern(aes(fill = group,
												 pattern = subgroup,
												 pattern_fill = after_scale(darken(fill, 0.5))),
										 pattern_colour = NA) +
		geom_col(colour = NA, fill = NA) + # black trim
		scale_pattern_discrete(
			choices = c("none", "stripe")
		)+
		scale_fill_manual("",
											values = black,
											labels = c("remaining", "rivers", "lakes"),
											breaks = c("1","2","3"))+
		geom_label_repel(data = ddd32,
										 aes(y = pos, label = paste0(percent, "%")),
										 size = 4.5, nudge_x = 1, force = 5, max.time = 2, show.legend = FALSE, segment.colour = NA) +
		guides(fill = guide_legend(title = element_blank())) +
		theme_void()+
		nice
	
	
	#ohne sediment
	# p21 <- ggplot(ddd2, aes(x = "" , y = value, fill = fct_inorder(gr))) +
	# 	geom_col() +
	# 	coord_polar(theta = "y") +
	# 	scale_fill_manual("",
	# 										values = black,
	# 										labels = c("remaining", "rivers", "lakes"),
	# 									breaks = c("rest", "River", "Lake"))+
	# 	geom_label_repel(data = df2,
	# 									 aes(y = pos, label = paste0(percent, "%")),
	# 									 size = 4.5, nudge_x = 1, force = 5, max.time = 2,, show.legend = FALSE, segment.colour = NA) +
	# 	guides(fill = guide_legend(title = element_blank())) +
	# 	theme_void()+
	# 	nice
	
	#rhone
	ddd <- df02 %>% filter(polymer == "PS"&river == "rhone") %>% dplyr::select(rest, River, Lake) 
	ddd2 <- data.frame(value = as.numeric(ddd),
										 gr = names(ddd))
	ddd2$percent <- round(ddd2$value/sum(ddd2$value)*100,1)
	ddd2$percent[1] <- 100 - sum(ddd2$percent[2:3])
	df2 <- ddd2 %>% 
		mutate(csum = rev(cumsum(rev(value))), 
					 pos = value/2 + lead(csum, 1),
					 pos = if_else(is.na(pos), value/2, pos))
	
	ddd3 <- df02 %>% filter(polymer == "PS"&river == "rhone") %>% dplyr::select(rest, River2, River1, Lake) 
	ddd32 <- data.frame(value = as.numeric(ddd3),
											gr = names(ddd3))
	ddd32$group <- c("1","2","2","3")
	ddd32$subgroup <- c("1","1","2","1")
	ddd32$pos <- NA
	ddd32$percent <- NA
	ddd32$csum <- NA
	ddd32$pos[c(1,2,4)] <- df2$pos
	ddd32$csum[c(1,2,4)] <- df2$csum
	ddd32$percent[c(1,2,4)] <- df2$percent
	
	
	p22 <- ggplot(ddd32, aes(x = "" , y = value,fill = group, group = group)) +
		coord_polar(theta = "y") +
		geom_col_pattern(aes(fill = group,
												 pattern = subgroup,
												 pattern_fill = after_scale(darken(fill, 0.5))),
										 pattern_colour = NA) +
		geom_col(colour = NA, fill = NA) + # black trim
		scale_pattern_discrete(
			choices = c("none", "stripe")
		)+
		scale_fill_manual("",
											values = black,
											labels = c("remaining", "rivers", "lakes"),
											breaks = c("1","2","3"))+
		geom_label_repel(data = ddd32,
										 aes(y = pos, label = paste0(percent, "%")),
										 size = 4.5, nudge_x = 1, force = 5, max.time = 2, show.legend = FALSE, segment.colour = NA) +
		guides(fill = guide_legend(title = element_blank())) +
		theme_void()+
		nice
	
	# p22 <- ggplot(ddd2, aes(x = "" , y = value, fill = fct_inorder(gr))) +
	# 	geom_col() +
	# 	coord_polar(theta = "y") +
	# 	scale_fill_manual("",
	# 										values = black,
	# 										labels = c("remaining", "rivers", "lakes"),
	# 									breaks = c("rest", "River", "Lake"))+
	# 	geom_label_repel(data = df2,
	# 									 aes(y = pos, label = paste0(percent, "%")),
	# 									 size = 4.5, nudge_x = 1, force = 5, max.time = 2,, show.legend = FALSE, segment.colour = NA) +
	# 	guides(fill = guide_legend(title = element_blank())) +
	# 	theme_void()+
	# 	nice
	
	# le doubs
	ddd <- df02 %>% filter(polymer == "PS"&river == "Le doubs") %>% dplyr::select(rest, River, Lake) 
	ddd2 <- data.frame(value = as.numeric(ddd),
										 gr = names(ddd))
	ddd2$percent <- round(ddd2$value/sum(ddd2$value)*100,1)
	ddd2$percent[1] <- 100 - sum(ddd2$percent[2:3])
	ddd2$percent <- round(ddd2$percent, 1)
	df2 <- ddd2 %>% 
		mutate(csum = rev(cumsum(rev(value))), 
					 pos = value/2 + lead(csum, 1),
					 pos = if_else(is.na(pos), value/2, pos))
	ddd3 <- df02 %>% filter(polymer == "PS"&river == "Le doubs") %>% dplyr::select(rest, River2, River1, Lake) 
	ddd32 <- data.frame(value = as.numeric(ddd3),
											gr = names(ddd3))
	ddd32$group <- c("1","2","2","3")
	ddd32$subgroup <- c("1","1","2","1")
	ddd32$pos <- NA
	ddd32$percent <- NA
	ddd32$csum <- NA
	ddd32$pos[c(1,2,4)] <- df2$pos
	ddd32$csum[c(1,2,4)] <- df2$csum
	ddd32$percent[c(1,2,4)] <- df2$percent
	
	
	p23 <- ggplot(ddd32, aes(x = "" , y = value,fill = group, group = group)) +
		coord_polar(theta = "y") +
		geom_col_pattern(aes(fill = group,
												 pattern = subgroup,
												 pattern_fill = after_scale(darken(fill, 0.5))),
										 pattern_colour = NA) +
		geom_col(colour = NA, fill = NA) + # black trim
		scale_pattern_discrete(
			choices = c("none", "stripe")
		)+
		scale_fill_manual("",
											values = black,
											labels = c("remaining", "rivers", "lakes"),
											breaks = c("1","2","3"))+
		geom_label_repel(data = ddd32,
										 aes(y = pos, label = paste0(percent, "%")),
										 size = 4.5, nudge_x = 1, force = 5, max.time = 2, show.legend = FALSE, segment.colour = NA) +
		guides(fill = guide_legend(title = element_blank())) +
		theme_void()+
		nice
	
	p232 <- ggplot(ddd2, aes(x = "" , y = value, fill = fct_inorder(gr))) +
		geom_col() +
		coord_polar(theta = "y") +
		scale_fill_manual("",
											values = black,
											labels = c(" ", " ", " "),
											breaks = c("rest", "River", "Lake"))+
		geom_label_repel(data = df2,
										 aes(y = pos, label = paste0(percent, "%")),
										 size = 4.5, nudge_x = 1, force = 5, max.time = 2, show.legend = FALSE, segment.colour = NA) +
		guides(fill = guide_legend(title = element_blank())) +
		theme_void()+
		nice+
		theme(legend.position = "right")
	
	legend2 <- get_legend(p232)
	# p23 <- p23+theme(legend.position = "none")
	
	
	
	#PET#######
	#rhine
	ddd <- df02 %>% filter(polymer == "PET"&river == "rhine") %>% dplyr::select(rest, River, Lake) 
	ddd2 <- data.frame(value = as.numeric(ddd),
										 gr = names(ddd))
	ddd2$percent <- round(ddd2$value/sum(ddd2$value)*100,1)
	ddd2$percent[1] <- 100 - sum(ddd2$percent[2:3])
	ddd2$percent <- round(ddd2$percent, 1)
	df2 <- ddd2 %>% 
		mutate(csum = rev(cumsum(rev(value))), 
					 pos = value/2 + lead(csum, 1),
					 pos = if_else(is.na(pos), value/2, pos))
	
	ddd3 <- df02 %>% filter(polymer == "PET"&river == "rhine") %>% dplyr::select(rest, River2, River1, Lake) 
	ddd32 <- data.frame(value = as.numeric(ddd3),
											gr = names(ddd3))
	ddd32$group <- c("1","2","2","3")
	ddd32$subgroup <- c("1","1","2","1")
	ddd32$pos <- NA
	ddd32$percent <- NA
	ddd32$csum <- NA
	ddd32$pos[c(1,2,4)] <- df2$pos
	ddd32$csum[c(1,2,4)] <- df2$csum
	ddd32$percent[c(1,2,4)] <- df2$percent
	
	
	p31 <- ggplot(ddd32, aes(x = "" , y = value,fill = group, group = group)) +
		coord_polar(theta = "y") +
		geom_col_pattern(aes(fill = group,
												 pattern = subgroup,
												 pattern_fill = after_scale(darken(fill, 0.5))),
										 pattern_colour = NA) +
		geom_col(colour = NA, fill = NA) + # black trim
		scale_pattern_discrete(
			choices = c("none", "stripe")
		)+
		scale_fill_manual("",
											values = red,
											labels = c("remaining", "rivers", "lakes"),
											breaks = c("1","2","3"))+
		geom_label_repel(data = ddd32,
										 aes(y = pos, label = paste0(percent, "%")),
										 size = 4.5, nudge_x = 1, force = 5, max.time = 2, show.legend = FALSE, segment.colour = NA) +
		guides(fill = guide_legend(title = element_blank())) +
		theme_void()+
		nice
	# 
	# p31 <- ggplot(ddd2, aes(x = "" , y = value, fill = fct_inorder(gr))) +
	# 	geom_col() +
	# 	coord_polar(theta = "y") +
	# 	scale_fill_manual("",
	# 										values = red,
	# 										labels = c("remaining", "rivers", "lakes"),
	# 									breaks = c("rest", "River", "Lake"))+
	# 	geom_label_repel(data = df2,
	# 									 aes(y = pos, label = paste0(percent, "%")),
	# 									 size = 4.5, nudge_x = 1, force = 5, max.time = 2,, show.legend = FALSE, segment.colour = NA) +
	# 	guides(fill = guide_legend(title = element_blank())) +
	# 	theme_void()+
	# 	nice
	
	
	#rhone
	ddd <- df02 %>% filter(polymer == "PET"&river == "rhone") %>% dplyr::select(rest, River, Lake) 
	ddd2 <- data.frame(value = as.numeric(ddd),
										 gr = names(ddd))
	ddd2$percent <- round(ddd2$value/sum(ddd2$value)*100,1)
	ddd2$percent[1] <- 100 - sum(ddd2$percent[2:3])
	ddd2$percent <- round(ddd2$percent, 1)
	df2 <- ddd2 %>% 
		mutate(csum = rev(cumsum(rev(value))), 
					 pos = value/2 + lead(csum, 1),
					 pos = if_else(is.na(pos), value/2, pos))
	
	ddd3 <- df02 %>% filter(polymer == "PET"&river == "rhone") %>% dplyr::select(rest, River2, River1, Lake) 
	ddd32 <- data.frame(value = as.numeric(ddd3),
											gr = names(ddd3))
	ddd32$group <- c("1","2","2","3")
	ddd32$subgroup <- c("1","1","2","1")
	ddd32$pos <- NA
	ddd32$percent <- NA
	ddd32$csum <- NA
	ddd32$pos[c(1,2,4)] <- df2$pos
	ddd32$csum[c(1,2,4)] <- df2$csum
	ddd32$percent[c(1,2,4)] <- df2$percent
	
	
	p32 <- ggplot(ddd32, aes(x = "" , y = value,fill = group, group = group)) +
		coord_polar(theta = "y") +
		geom_col_pattern(aes(fill = group,
												 pattern = subgroup,
												 pattern_fill = after_scale(darken(fill, 0.5))),
										 pattern_colour = NA) +
		geom_col(colour = NA, fill = NA) + # black trim
		scale_pattern_discrete(
			choices = c("none", "stripe")
		)+
		scale_fill_manual("",
											values = red,
											labels = c("remaining", "rivers", "lakes"),
											breaks = c("1","2","3"))+
		geom_label_repel(data = ddd32,
										 aes(y = pos, label = paste0(percent, "%")),
										 size = 4.5, nudge_x = 1, force = 5, max.time = 2, show.legend = FALSE, segment.colour = NA) +
		guides(fill = guide_legend(title = element_blank())) +
		theme_void()+
		nice
	
	
	# p32 <- ggplot(ddd2, aes(x = "" , y = value, fill = fct_inorder(gr))) +
	# 	geom_col() +
	# 	coord_polar(theta = "y") +
	# 	scale_fill_manual("",
	# 										values = red,
	# 										labels = c("remaining", "rivers", "lakes"),
	# 									breaks = c("rest", "River", "Lake"))+
	# 	geom_label_repel(data = df2,
	# 									 aes(y = pos, label = paste0(percent, "%")),
	# 									 size = 4.5, nudge_x = 1, force = 5, max.time = 2,, show.legend = FALSE, segment.colour = NA) +
	# 	guides(fill = guide_legend(title = element_blank())) +
	# 	theme_void()+
	# 	nice
	
	# le doubs
	ddd <- df02 %>% filter(polymer == "PET"&river == "Le doubs") %>% dplyr::select(rest, River, Lake) 
	ddd2 <- data.frame(value = as.numeric(ddd),
										 gr = names(ddd))
	ddd2$percent <- round(ddd2$value/sum(ddd2$value)*100,1)
	ddd2$percent[1] <- 100 - sum(ddd2$percent[2:3])
	ddd2$percent <- round(ddd2$percent, 1)
	df2 <- ddd2 %>% 
		mutate(csum = rev(cumsum(rev(value))), 
					 pos = value/2 + lead(csum, 1),
					 pos = if_else(is.na(pos), value/2, pos))
	
	ddd3 <- df02 %>% filter(polymer == "PET"&river == "Le doubs") %>% dplyr::select(rest, River2, River1, Lake) 
	ddd32 <- data.frame(value = as.numeric(ddd3),
											gr = names(ddd3))
	ddd32$group <- c("1","2","2","3")
	ddd32$subgroup <- c("1","1","2","1")
	ddd32$pos <- NA
	ddd32$percent <- NA
	ddd32$csum <- NA
	ddd32$pos[c(1,2,4)] <- df2$pos
	ddd32$csum[c(1,2,4)] <- df2$csum
	ddd32$percent[c(1,2,4)] <- df2$percent
	
	
	p33 <- ggplot(ddd32, aes(x = "" , y = value,fill = group, group = group)) +
		coord_polar(theta = "y") +
		geom_col_pattern(aes(fill = group,
												 pattern = subgroup,
												 pattern_fill = after_scale(darken(fill, 0.5))),
										 pattern_colour = NA) +
		geom_col(colour = NA, fill = NA) + # black trim
		scale_pattern_discrete(
			choices = c("none", "stripe")
		)+
		scale_fill_manual("",
											values = red,
											labels = c("remaining", "rivers", "lakes"),
											breaks = c("1","2","3"))+
		geom_label_repel(data = ddd32,
										 aes(y = pos, label = paste0(percent, "%")),
										 size = 4.5, nudge_x = 1, force = 5, max.time = 2, show.legend = FALSE, segment.colour = NA) +
		guides(fill = guide_legend(title = element_blank())) +
		theme_void()+
		nice
	
	
	p332 <- ggplot(ddd2, aes(x = "" , y = value, fill = fct_inorder(gr))) +
		geom_col() +
		coord_polar(theta = "y") +
		scale_fill_manual("",
											values = red,
											labels = c("   in suspension (outflow in water)", "   retained in rivers", "   retained in other lakes"),
										breaks = c("rest", "River", "Lake"))+
		geom_label_repel(data = df2,
										 aes(y = pos, label = paste0(percent, "%")),
										 size = 4.5, nudge_x = 1, show.legend = FALSE, segment.colour = NA) +
		guides(fill = guide_legend(title = element_blank())) +
		theme_void()+
		nice+
		theme(legend.position = "right")
	
	legend3 <- get_legend(p332)
	# p33 <- p33+theme(legend.position = "none")
	
	
	#legend stripes
	dfL <- data.frame(level = c("a"), outcome = c(2.3))
	
	pL <- ggplot(dfL) +
		geom_col_pattern(
			aes(x = level, y = outcome, pattern = level), 
			# pattern = c("none", 'stripe'),
			fill    = 'white',
			colour  = 'black',
			pattern_fill = "black"
		) +
		scale_pattern_manual("", labels = "outflow through sediments", values="stripe")+
		nice+
	theme(legend.position = 'right')
	
	
	legend4 <- get_legend(pL)
	# p33 <- p33+theme(legend.position = "none")
	
		#text
		text_size = 6
		t1 <- ggplot() +                     
			annotate("text",
							 x = 1,
							 y = 1,
							 size = text_size,
							 label = "Rhine") + 
			theme_void()+
			nice
		
		t2 <- ggplot() +                     
			annotate("text",
							 x = 1,
							 y = 1,
							 size = text_size,
							 label = "Rhône") + 
			theme_void()+
			nice
		
		t3 <- ggplot() +                     
			annotate("text",
							 x = 1,
							 y = 1,
							 size = text_size,
							 label = "Doubs") + 
			theme_void()+
			nice
		
		ta <- ggplot() +                     
			annotate("text",
							 x = 1,
							 y = 1,
							 size = text_size,
							 label = "EPS") + 
			theme_void()+
			nice
		
		tb <- ggplot() +                     
			annotate("text",
							 x = 1,
							 y = 1,
							 size = text_size,
							 label = "PS") + 
			theme_void()+
			nice
		tc <- ggplot() +                     
			annotate("text",
							 x = 1,
							 y = 1,
							 size = text_size,
							 label = "PET") + 
			theme_void()+
			nice
		

		t0 <- ggplot() +                     
			annotate("text",
							 x = 1,
							 y = 1,
							 size = text_size,
							 label = "     ") + 
			theme_void()+
			nice
		
		
		#put all together
		
		pall <- ggdraw(plot_grid(plot_grid(plot_grid(t0, ta, tb, tc, rel_heights = c(0.1,1,1,1), ncol = 1), plot_grid(t1, t2,t3,p11,p12,p13,p21,p22,p23,p31,p32,p33, rel_heights = c(0.2,1,1,1), ncol = 3), ncol = 2, rel_widths = c(0.08, 1)), plot_grid(t0, legend1, legend2, legend3, legend4, t0, nrow = 1, rel_widths = c(0.5,0.2,0.2,1.6,1.5, 0.5)), nrow = 2, rel_heights = c(1,0.3)))
		Sys.sleep(5)
		
		pall
	ggsave(paste0(main.path, "output_files/plots/retain.png"), pall, height = 19, width = 20, units = "cm", dpi = 500)
	
	
	rm(list = ls())
	