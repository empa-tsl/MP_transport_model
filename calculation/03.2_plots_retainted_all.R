# plotting figures of the model
####
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
farben <- c('dodgerblue',  "#CC5700", '#555555','#cccccc')
nice <- theme(legend.position = "none",
							legend.background = element_rect(fill = "transparent", color = NA),
							plot.background = element_rect(fill = "transparent",colour = NA),
							text = element_text(color = "black", size = 7),
							legend.text = element_text(size = 7),
							plot.margin=unit(c(0.2, 0.2,0.2,0.2),"cm")) #trbl


###########################################################################
#kreis Diagramme
#EPS

ddd3 <- df02 %>% filter(polymer == "EPS"&river == "rhine") %>% dplyr::select(rest, River1, River2, Lake) 
ddd32 <- data.frame(value = as.numeric(ddd3),
										gr = names(ddd3))
ddd32$percent <- round(ddd32$value/sum(ddd32$value)*100,1)
ddd32$percent[1] <- 100 - sum(ddd32$percent[2:4])
df2 <- ddd32 %>% 
	mutate(csum = rev(cumsum(rev(value))), 
				 pos = value/2 + lead(csum, 1),
				 pos = if_else(is.na(pos), value/2, pos))





epsRhine <- ggplot(ddd32, aes(x = "" , y = value, fill = fct_inorder(gr))) +
	geom_col() +
	coord_polar(theta = "y") +
	scale_fill_manual("",
										values = farben,
										labels = c("remaining", "rivers", "river2", "lakes"),
										breaks = c("rest", "River1", "River2", "Lake"))+
	geom_label_repel(data = df2,
									 aes(y = pos, label = paste0(percent, "%")),
									 size = 4.5, nudge_x = 1, force = 5, max.time = 2, show.legend = FALSE, segment.colour = NA) +
	guides(fill = guide_legend(title = element_blank())) +
	theme_void()+
	nice

ddd3 <- df02 %>% filter(polymer == "EPS"&river == "rhone") %>% dplyr::select(rest, River1, River2, Lake) 
ddd32 <- data.frame(value = as.numeric(ddd3),
										gr = names(ddd3))
ddd32$percent <- round(ddd32$value/sum(ddd32$value)*100,1)
ddd32$percent[1] <- 100 - sum(ddd32$percent[2:4])
df2 <- ddd32 %>% 
	mutate(csum = rev(cumsum(rev(value))), 
				 pos = value/2 + lead(csum, 1),
				 pos = if_else(is.na(pos), value/2, pos))





epsRhone <- ggplot(ddd32, aes(x = "" , y = value, fill = fct_inorder(gr))) +
	geom_col() +
	coord_polar(theta = "y") +
	scale_fill_manual("",
										values = farben,
										labels = c("remaining", "rivers", "river2", "lakes"),
										breaks = c("rest", "River1", "River2", "Lake"))+
	geom_label_repel(data = df2,
									 aes(y = pos, label = paste0(percent, "%")),
									 size = 4.5, nudge_x = 1, force = 5, max.time = 2, show.legend = FALSE, segment.colour = NA) +
	guides(fill = guide_legend(title = element_blank())) +
	theme_void()+
	nice


ddd3 <- df02 %>% filter(polymer == "EPS"&river == "Le doubs") %>% dplyr::select(rest, River1, River2, Lake) 
ddd32 <- data.frame(value = as.numeric(ddd3),
										gr = names(ddd3))
ddd32$percent <- round(ddd32$value/sum(ddd32$value)*100,1)
ddd32$percent[1] <- 100 - sum(ddd32$percent[2:4])
df2 <- ddd32 %>% 
	mutate(csum = rev(cumsum(rev(value))), 
				 pos = value/2 + lead(csum, 1),
				 pos = if_else(is.na(pos), value/2, pos))





epsDoubs <- ggplot(ddd32, aes(x = "" , y = value, fill = fct_inorder(gr))) +
	geom_col() +
	coord_polar(theta = "y") +
	scale_fill_manual("",
										values = farben,
										labels = c("remaining", "rivers", "river2", "lakes"),
										breaks = c("rest", "River1", "River2", "Lake"))+
	geom_label_repel(data = df2,
									 aes(y = pos, label = paste0(percent, "%")),
									 size = 4.5, nudge_x = 1, force = 5, max.time = 2, show.legend = FALSE, segment.colour = NA) +
	guides(fill = guide_legend(title = element_blank())) +
	theme_void()+
	nice



ddd3 <- df02 %>% filter(polymer == "PP"&river == "rhine") %>% dplyr::select(rest, River1, River2, Lake) 
ddd32 <- data.frame(value = as.numeric(ddd3),
										gr = names(ddd3))
ddd32$percent <- round(ddd32$value/sum(ddd32$value)*100,1)
ddd32$percent[1] <- 100 - sum(ddd32$percent[2:4])
df2 <- ddd32 %>% 
	mutate(csum = rev(cumsum(rev(value))), 
				 pos = value/2 + lead(csum, 1),
				 pos = if_else(is.na(pos), value/2, pos))





ppRhine <- ggplot(ddd32, aes(x = "" , y = value, fill = fct_inorder(gr))) +
	geom_col() +
	coord_polar(theta = "y") +
	scale_fill_manual("",
										values = farben,
										labels = c("remaining", "rivers", "river2", "lakes"),
										breaks = c("rest", "River1", "River2", "Lake"))+
	geom_label_repel(data = df2,
									 aes(y = pos, label = paste0(percent, "%")),
									 size = 4.5, nudge_x = 1, force = 5, max.time = 2, show.legend = FALSE, segment.colour = NA) +
	guides(fill = guide_legend(title = element_blank())) +
	theme_void()+
	nice

ddd3 <- df02 %>% filter(polymer == "PP"&river == "rhone") %>% dplyr::select(rest, River1, River2, Lake) 
ddd32 <- data.frame(value = as.numeric(ddd3),
										gr = names(ddd3))
ddd32$percent <- round(ddd32$value/sum(ddd32$value)*100,1)
ddd32$percent[1] <- 100 - sum(ddd32$percent[2:4])
df2 <- ddd32 %>% 
	mutate(csum = rev(cumsum(rev(value))), 
				 pos = value/2 + lead(csum, 1),
				 pos = if_else(is.na(pos), value/2, pos))





ppRhone <- ggplot(ddd32, aes(x = "" , y = value, fill = fct_inorder(gr))) +
	geom_col() +
	coord_polar(theta = "y") +
	scale_fill_manual("",
										values = farben,
										labels = c("remaining", "rivers", "river2", "lakes"),
										breaks = c("rest", "River1", "River2", "Lake"))+
	geom_label_repel(data = df2,
									 aes(y = pos, label = paste0(percent, "%")),
									 size = 4.5, nudge_x = 1, force = 5, max.time = 2, show.legend = FALSE, segment.colour = NA) +
	guides(fill = guide_legend(title = element_blank())) +
	theme_void()+
	nice



ddd3 <- df02 %>% filter(polymer == "PP"&river == "Le doubs") %>% dplyr::select(rest, River1, River2, Lake) 
ddd32 <- data.frame(value = as.numeric(ddd3),
										gr = names(ddd3))
ddd32$percent <- round(ddd32$value/sum(ddd32$value)*100,1)
ddd32$percent[1] <- 100 - sum(ddd32$percent[2:4])
df2 <- ddd32 %>% 
	mutate(csum = rev(cumsum(rev(value))), 
				 pos = value/2 + lead(csum, 1),
				 pos = if_else(is.na(pos), value/2, pos))





ppDoubs <- ggplot(ddd32, aes(x = "" , y = value, fill = fct_inorder(gr))) +
	geom_col() +
	coord_polar(theta = "y") +
	scale_fill_manual("",
										values = farben,
										labels = c("remaining", "rivers", "river2", "lakes"),
										breaks = c("rest", "River1", "River2", "Lake"))+
	geom_label_repel(data = df2,
									 aes(y = pos, label = paste0(percent, "%")),
									 size = 4.5, nudge_x = 1, force = 5, max.time = 2, show.legend = FALSE, segment.colour = NA) +
	guides(fill = guide_legend(title = element_blank())) +
	theme_void()+
	nice

ddd3 <- df02 %>% filter(polymer == "LDPE"&river == "rhine") %>% dplyr::select(rest, River1, River2, Lake) 
ddd32 <- data.frame(value = as.numeric(ddd3),
										gr = names(ddd3))
ddd32$percent <- round(ddd32$value/sum(ddd32$value)*100,1)
ddd32$percent[1] <- 100 - sum(ddd32$percent[2:4])
df2 <- ddd32 %>% 
	mutate(csum = rev(cumsum(rev(value))), 
				 pos = value/2 + lead(csum, 1),
				 pos = if_else(is.na(pos), value/2, pos))





ldpeRhine <- ggplot(ddd32, aes(x = "" , y = value, fill = fct_inorder(gr))) +
	geom_col() +
	coord_polar(theta = "y") +
	scale_fill_manual("",
										values = farben,
										labels = c("remaining", "rivers", "river2", "lakes"),
										breaks = c("rest", "River1", "River2", "Lake"))+
	geom_label_repel(data = df2,
									 aes(y = pos, label = paste0(percent, "%")),
									 size = 4.5, nudge_x = 1, force = 5, max.time = 2, show.legend = FALSE, segment.colour = NA) +
	guides(fill = guide_legend(title = element_blank())) +
	theme_void()+
	nice


ddd3 <- df02 %>% filter(polymer == "LDPE"&river == "rhone") %>% dplyr::select(rest, River1, River2, Lake) 
ddd32 <- data.frame(value = as.numeric(ddd3),
										gr = names(ddd3))
ddd32$percent <- round(ddd32$value/sum(ddd32$value)*100,1)
ddd32$percent[1] <- 100 - sum(ddd32$percent[2:4])
df2 <- ddd32 %>% 
	mutate(csum = rev(cumsum(rev(value))), 
				 pos = value/2 + lead(csum, 1),
				 pos = if_else(is.na(pos), value/2, pos))





ldpeRhone <- ggplot(ddd32, aes(x = "" , y = value, fill = fct_inorder(gr))) +
	geom_col() +
	coord_polar(theta = "y") +
	scale_fill_manual("",
										values = farben,
										labels = c("remaining", "rivers", "river2", "lakes"),
										breaks = c("rest", "River1", "River2", "Lake"))+
	geom_label_repel(data = df2,
									 aes(y = pos, label = paste0(percent, "%")),
									 size = 4.5, nudge_x = 1, force = 5, max.time = 2, show.legend = FALSE, segment.colour = NA) +
	guides(fill = guide_legend(title = element_blank())) +
	theme_void()+
	nice



ddd3 <- df02 %>% filter(polymer == "LDPE"&river == "Le doubs") %>% dplyr::select(rest, River1, River2, Lake) 
ddd32 <- data.frame(value = as.numeric(ddd3),
										gr = names(ddd3))
ddd32$percent <- round(ddd32$value/sum(ddd32$value)*100,1)
ddd32$percent[1] <- 100 - sum(ddd32$percent[2:4])
df2 <- ddd32 %>% 
	mutate(csum = rev(cumsum(rev(value))), 
				 pos = value/2 + lead(csum, 1),
				 pos = if_else(is.na(pos), value/2, pos))





ldpeDoubs <- ggplot(ddd32, aes(x = "" , y = value, fill = fct_inorder(gr))) +
	geom_col() +
	coord_polar(theta = "y") +
	scale_fill_manual("",
										values = farben,
										labels = c("remaining", "rivers", "river2", "lakes"),
										breaks = c("rest", "River1", "River2", "Lake"))+
	geom_label_repel(data = df2,
									 aes(y = pos, label = paste0(percent, "%")),
									 size = 4.5, nudge_x = 1, force = 5, max.time = 2, show.legend = FALSE, segment.colour = NA) +
	guides(fill = guide_legend(title = element_blank())) +
	theme_void()+
	nice



ddd3 <- df02 %>% filter(polymer == "HDPE"&river == "rhine") %>% dplyr::select(rest, River1, River2, Lake) 
ddd32 <- data.frame(value = as.numeric(ddd3),
										gr = names(ddd3))
ddd32$percent <- round(ddd32$value/sum(ddd32$value)*100,1)
ddd32$percent[1] <- 100 - sum(ddd32$percent[2:4])
df2 <- ddd32 %>% 
	mutate(csum = rev(cumsum(rev(value))), 
				 pos = value/2 + lead(csum, 1),
				 pos = if_else(is.na(pos), value/2, pos))





hdpeRhine <- ggplot(ddd32, aes(x = "" , y = value, fill = fct_inorder(gr))) +
	geom_col() +
	coord_polar(theta = "y") +
	scale_fill_manual("",
										values = farben,
										labels = c("remaining", "rivers", "river2", "lakes"),
										breaks = c("rest", "River1", "River2", "Lake"))+
	geom_label_repel(data = df2,
									 aes(y = pos, label = paste0(percent, "%")),
									 size = 4.5, nudge_x = 1, force = 5, max.time = 2, show.legend = FALSE, segment.colour = NA) +
	guides(fill = guide_legend(title = element_blank())) +
	theme_void()+
	nice


ddd3 <- df02 %>% filter(polymer == "HDPE"&river == "rhone") %>% dplyr::select(rest, River1, River2, Lake) 
ddd32 <- data.frame(value = as.numeric(ddd3),
										gr = names(ddd3))
ddd32$percent <- round(ddd32$value/sum(ddd32$value)*100,1)
ddd32$percent[1] <- 100 - sum(ddd32$percent[2:4])
df2 <- ddd32 %>% 
	mutate(csum = rev(cumsum(rev(value))), 
				 pos = value/2 + lead(csum, 1),
				 pos = if_else(is.na(pos), value/2, pos))





hdpeRhone <- ggplot(ddd32, aes(x = "" , y = value, fill = fct_inorder(gr))) +
	geom_col() +
	coord_polar(theta = "y") +
	scale_fill_manual("",
										values = farben,
										labels = c("remaining", "rivers", "river2", "lakes"),
										breaks = c("rest", "River1", "River2", "Lake"))+
	geom_label_repel(data = df2,
									 aes(y = pos, label = paste0(percent, "%")),
									 size = 4.5, nudge_x = 1, force = 5, max.time = 2, show.legend = FALSE, segment.colour = NA) +
	guides(fill = guide_legend(title = element_blank())) +
	theme_void()+
	nice


ddd3 <- df02 %>% filter(polymer == "HDPE"&river == "Le doubs") %>% dplyr::select(rest, River1, River2, Lake) 
ddd32 <- data.frame(value = as.numeric(ddd3),
										gr = names(ddd3))
ddd32$percent <- round(ddd32$value/sum(ddd32$value)*100,1)
ddd32$percent[1] <- 100 - sum(ddd32$percent[2:4])
df2 <- ddd32 %>% 
	mutate(csum = rev(cumsum(rev(value))), 
				 pos = value/2 + lead(csum, 1),
				 pos = if_else(is.na(pos), value/2, pos))





hdpeDoubs <- ggplot(ddd32, aes(x = "" , y = value, fill = fct_inorder(gr))) +
	geom_col() +
	coord_polar(theta = "y") +
	scale_fill_manual("",
										values = farben,
										labels = c("remaining", "rivers", "river2", "lakes"),
										breaks = c("rest", "River1", "River2", "Lake"))+
	geom_label_repel(data = df2,
									 aes(y = pos, label = paste0(percent, "%")),
									 size = 4.5, nudge_x = 1, force = 5, max.time = 2, show.legend = FALSE, segment.colour = NA) +
	guides(fill = guide_legend(title = element_blank())) +
	theme_void()+
	nice


ddd3 <- df02 %>% filter(polymer == "PS"&river == "rhine") %>% dplyr::select(rest, River1, River2, Lake) 
ddd32 <- data.frame(value = as.numeric(ddd3),
										gr = names(ddd3))
ddd32$percent <- round(ddd32$value/sum(ddd32$value)*100,1)
ddd32$percent[1] <- 100 - sum(ddd32$percent[2:4])
df2 <- ddd32 %>% 
	mutate(csum = rev(cumsum(rev(value))), 
				 pos = value/2 + lead(csum, 1),
				 pos = if_else(is.na(pos), value/2, pos))





psRhine <- ggplot(ddd32, aes(x = "" , y = value, fill = fct_inorder(gr))) +
	geom_col() +
	coord_polar(theta = "y") +
	scale_fill_manual("",
										values = farben,
										labels = c("remaining", "rivers", "river2", "lakes"),
										breaks = c("rest", "River1", "River2", "Lake"))+
	geom_label_repel(data = df2,
									 aes(y = pos, label = paste0(percent, "%")),
									 size = 4.5, nudge_x = 1, force = 5, max.time = 2, show.legend = FALSE, segment.colour = NA) +
	guides(fill = guide_legend(title = element_blank())) +
	theme_void()+
	nice

ddd3 <- df02 %>% filter(polymer == "PS"&river == "rhone") %>% dplyr::select(rest, River1, River2, Lake) 
ddd32 <- data.frame(value = as.numeric(ddd3),
										gr = names(ddd3))
ddd32$percent <- round(ddd32$value/sum(ddd32$value)*100,1)
ddd32$percent[1] <- 100 - sum(ddd32$percent[2:4])
df2 <- ddd32 %>% 
	mutate(csum = rev(cumsum(rev(value))), 
				 pos = value/2 + lead(csum, 1),
				 pos = if_else(is.na(pos), value/2, pos))





psRhone <- ggplot(ddd32, aes(x = "" , y = value, fill = fct_inorder(gr))) +
	geom_col() +
	coord_polar(theta = "y") +
	scale_fill_manual("",
										values = farben,
										labels = c("remaining", "rivers", "river2", "lakes"),
										breaks = c("rest", "River1", "River2", "Lake"))+
	geom_label_repel(data = df2,
									 aes(y = pos, label = paste0(percent, "%")),
									 size = 4.5, nudge_x = 1, force = 5, max.time = 2, show.legend = FALSE, segment.colour = NA) +
	guides(fill = guide_legend(title = element_blank())) +
	theme_void()+
	nice

ddd3 <- df02 %>% filter(polymer == "PS"&river == "Le doubs") %>% dplyr::select(rest, River1, River2, Lake) 
ddd32 <- data.frame(value = as.numeric(ddd3),
										gr = names(ddd3))
ddd32$percent <- round(ddd32$value/sum(ddd32$value)*100,1)
ddd32$percent[1] <- 100 - sum(ddd32$percent[2:4])
df2 <- ddd32 %>% 
	mutate(csum = rev(cumsum(rev(value))), 
				 pos = value/2 + lead(csum, 1),
				 pos = if_else(is.na(pos), value/2, pos))





psDoubs <- ggplot(ddd32, aes(x = "" , y = value, fill = fct_inorder(gr))) +
	geom_col() +
	coord_polar(theta = "y") +
	scale_fill_manual("",
										values = farben,
										labels = c("remaining", "rivers", "river2", "lakes"),
										breaks = c("rest", "River1", "River2", "Lake"))+
	geom_label_repel(data = df2,
									 aes(y = pos, label = paste0(percent, "%")),
									 size = 4.5, nudge_x = 1, force = 5, max.time = 2, show.legend = FALSE, segment.colour = NA) +
	guides(fill = guide_legend(title = element_blank())) +
	theme_void()+
	nice



ddd3 <- df02 %>% filter(polymer == "PVC"&river == "rhine") %>% dplyr::select(rest, River1, River2, Lake) 
ddd32 <- data.frame(value = as.numeric(ddd3),
										gr = names(ddd3))
ddd32$percent <- round(ddd32$value/sum(ddd32$value)*100,1)
ddd32$percent[1] <- 100 - sum(ddd32$percent[2:4])
df2 <- ddd32 %>% 
	mutate(csum = rev(cumsum(rev(value))), 
				 pos = value/2 + lead(csum, 1),
				 pos = if_else(is.na(pos), value/2, pos))





pvcRhine <- ggplot(ddd32, aes(x = "" , y = value, fill = fct_inorder(gr))) +
	geom_col() +
	coord_polar(theta = "y") +
	scale_fill_manual("",
										values = farben,
										labels = c("remaining", "rivers", "river2", "lakes"),
										breaks = c("rest", "River1", "River2", "Lake"))+
	geom_label_repel(data = df2,
									 aes(y = pos, label = paste0(percent, "%")),
									 size = 4.5, nudge_x = 1, force = 5, max.time = 2, show.legend = FALSE, segment.colour = NA) +
	guides(fill = guide_legend(title = element_blank())) +
	theme_void()+
	nice

ddd3 <- df02 %>% filter(polymer == "PVC"&river == "rhone") %>% dplyr::select(rest, River1, River2, Lake) 
ddd32 <- data.frame(value = as.numeric(ddd3),
										gr = names(ddd3))
ddd32$percent <- round(ddd32$value/sum(ddd32$value)*100,1)
ddd32$percent[1] <- 100 - sum(ddd32$percent[2:4])
df2 <- ddd32 %>% 
	mutate(csum = rev(cumsum(rev(value))), 
				 pos = value/2 + lead(csum, 1),
				 pos = if_else(is.na(pos), value/2, pos))





pvcRhone <- ggplot(ddd32, aes(x = "" , y = value, fill = fct_inorder(gr))) +
	geom_col() +
	coord_polar(theta = "y") +
	scale_fill_manual("",
										values = farben,
										labels = c("remaining", "rivers", "river2", "lakes"),
										breaks = c("rest", "River1", "River2", "Lake"))+
	geom_label_repel(data = df2,
									 aes(y = pos, label = paste0(percent, "%")),
									 size = 4.5, nudge_x = 1, force = 5, max.time = 2, show.legend = FALSE, segment.colour = NA) +
	guides(fill = guide_legend(title = element_blank())) +
	theme_void()+
	nice

ddd3 <- df02 %>% filter(polymer == "PVC"&river == "Le doubs") %>% dplyr::select(rest, River1, River2, Lake) 
ddd32 <- data.frame(value = as.numeric(ddd3),
										gr = names(ddd3))
ddd32$percent <- round(ddd32$value/sum(ddd32$value)*100,1)
ddd32$percent[1] <- 100 - sum(ddd32$percent[2:4])
df2 <- ddd32 %>% 
	mutate(csum = rev(cumsum(rev(value))), 
				 pos = value/2 + lead(csum, 1),
				 pos = if_else(is.na(pos), value/2, pos))





pvcDoubs <- ggplot(ddd32, aes(x = "" , y = value, fill = fct_inorder(gr))) +
	geom_col() +
	coord_polar(theta = "y") +
	scale_fill_manual("",
										values = farben,
										labels = c("remaining", "rivers", "river2", "lakes"),
										breaks = c("rest", "River1", "River2", "Lake"))+
	geom_label_repel(data = df2,
									 aes(y = pos, label = paste0(percent, "%")),
									 size = 4.5, nudge_x = 1, force = 5, max.time = 2, show.legend = FALSE, segment.colour = NA) +
	guides(fill = guide_legend(title = element_blank())) +
	theme_void()+
	nice


ddd3 <- df02 %>% filter(polymer == "PET"&river == "rhine") %>% dplyr::select(rest, River1, River2, Lake) 
ddd32 <- data.frame(value = as.numeric(ddd3),
										gr = names(ddd3))
ddd32$percent <- round(ddd32$value/sum(ddd32$value)*100,1)
ddd32$percent[1] <- 100 - sum(ddd32$percent[2:4])
df2 <- ddd32 %>% 
	mutate(csum = rev(cumsum(rev(value))), 
				 pos = value/2 + lead(csum, 1),
				 pos = if_else(is.na(pos), value/2, pos))





petRhine <- ggplot(ddd32, aes(x = "" , y = value, fill = fct_inorder(gr))) +
	geom_col() +
	coord_polar(theta = "y") +
	scale_fill_manual("",
										values = farben,
										labels = c("remaining", "rivers", "river2", "lakes"),
										breaks = c("rest", "River1", "River2", "Lake"))+
	geom_label_repel(data = df2,
									 aes(y = pos, label = paste0(percent, "%")),
									 size = 4.5, nudge_x = 1, force = 5, max.time = 2, show.legend = FALSE, segment.colour = NA) +
	guides(fill = guide_legend(title = element_blank())) +
	theme_void()+
	nice


ddd3 <- df02 %>% filter(polymer == "PET"&river == "rhone") %>% dplyr::select(rest, River1, River2, Lake) 
ddd32 <- data.frame(value = as.numeric(ddd3),
										gr = names(ddd3))
ddd32$percent <- round(ddd32$value/sum(ddd32$value)*100,1)
ddd32$percent[1] <- 100 - sum(ddd32$percent[2:4])
df2 <- ddd32 %>% 
	mutate(csum = rev(cumsum(rev(value))), 
				 pos = value/2 + lead(csum, 1),
				 pos = if_else(is.na(pos), value/2, pos))





petRhone <- ggplot(ddd32, aes(x = "" , y = value, fill = fct_inorder(gr))) +
	geom_col() +
	coord_polar(theta = "y") +
	scale_fill_manual("",
										values = farben,
										labels = c("remaining", "rivers", "river2", "lakes"),
										breaks = c("rest", "River1", "River2", "Lake"))+
	geom_label_repel(data = df2,
									 aes(y = pos, label = paste0(percent, "%")),
									 size = 4.5, nudge_x = 1, force = 5, max.time = 2, show.legend = FALSE, segment.colour = NA) +
	guides(fill = guide_legend(title = element_blank())) +
	theme_void()+
	nice


ddd3 <- df02 %>% filter(polymer == "PET"&river == "Le doubs") %>% dplyr::select(rest, River1, River2, Lake) 
ddd32 <- data.frame(value = as.numeric(ddd3),
										gr = names(ddd3))
ddd32$percent <- round(ddd32$value/sum(ddd32$value)*100,1)
ddd32$percent[1] <- 100 - sum(ddd32$percent[2:4])
df2 <- ddd32 %>% 
	mutate(csum = rev(cumsum(rev(value))), 
				 pos = value/2 + lead(csum, 1),
				 pos = if_else(is.na(pos), value/2, pos))





petDoubs <- ggplot(ddd32, aes(x = "" , y = value, fill = fct_inorder(gr))) +
	geom_col() +
	coord_polar(theta = "y") +
	scale_fill_manual("",
										values = farben,
										labels = c("remaining", "rivers", "river2", "lakes"),
										breaks = c("rest", "River1", "River2", "Lake"))+
	geom_label_repel(data = df2,
									 aes(y = pos, label = paste0(percent, "%")),
									 size = 4.5, nudge_x = 1, force = 5, max.time = 2, show.legend = FALSE, segment.colour = NA) +
	guides(fill = guide_legend(title = element_blank())) +
	theme_void()+
	nice







#text
text_size = 4
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
					 label = "PP") + 
	theme_void()+
	nice

tc <- ggplot() +                     
	annotate("text",
					 x = 1,
					 y = 1,
					 size = text_size,
					 label = "LDPE") + 
	theme_void()+
	nice
td <- ggplot() +                     
	annotate("text",
					 x = 1,
					 y = 1,
					 size = text_size,
					 label = "HDPE") + 
	theme_void()+
	nice

te <- ggplot() +                     
	annotate("text",
					 x = 1,
					 y = 1,
					 size = text_size,
					 label = "PS") + 
	theme_void()+
	nice

tf <- ggplot() +                     
	annotate("text",
					 x = 1,
					 y = 1,
					 size = text_size,
					 label = "PVC") + 
	theme_void()+
	nice

tg <- ggplot() +                     
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

palla <- ggdraw(plot_grid(plot_grid(t0, ta, tb, tc, td, rel_heights = c(0.1,1,1,1,1), ncol = 1), plot_grid(t1, t2,t3,epsRhine,epsRhone,epsDoubs,ppRhine,ppRhone,ppDoubs,ldpeRhine,ldpeRhone,ldpeDoubs, hdpeRhine, hdpeRhone, hdpeDoubs, rel_heights = c(0.2,1,1,1,1), ncol = 3), ncol = 2, rel_widths = c(0.08, 1)))
Sys.sleep(5)

ggsave(paste0(main.path, "output_files/plots/retain_all.png"), palla, height = 20, width = 20, units = "cm", dpi = 500)


pallb <- ggdraw(plot_grid(plot_grid(t0, te, tf, tg,  rel_heights = c(0.1,1,1,1), ncol = 1), plot_grid(t1, t2,t3,psRhine, psRhone, psDoubs, pvcRhine,pvcRhone,pvcDoubs,petRhine,petRhone,petDoubs, rel_heights = c(0.2,1,1,1), ncol = 3), ncol = 2, rel_widths = c(0.08, 1)))
Sys.sleep(5)

ggsave(paste0(main.path, "output_files/plots/retain_all2.png"), pallb, height = 15, width = 20, units = "cm", dpi = 500)


rm(list = ls())
