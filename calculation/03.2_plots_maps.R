#Maps für Schweiz

# first all rivers that belong to the system

library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggrepel)
library(patchwork) #for making all images the same dimensions.
library(cowplot)
library(sf)
library(reshape2)
library(ggspatial)

#function

#round numbers and find groups:
col.NR <- function(max.NR){
	if(is.infinite(max.NR)){
		return(c(0,0,0,0,0,0))
	}else{
	l <- log10(max.NR)
	if(l == 0){start.NR <- 1} #wenn max wert genau 1 ist
	if(l > 0){start.NR <- ceiling(max.NR/10^floor(l))*10^floor(l)}
	if(l < 0){start.NR <- ceiling(max.NR*10^ceiling(l*-1))/10^ceiling(l*-1)}
	return(c(start.NR,  start.NR*0.5, start.NR*0.25, start.NR*0.125, start.NR*0.0625, 0))
	}
}

# col.NR(2)

#load dataset
main.path <- "PhD/mennekes/"
rounds <- "801"
polymers <- c("EPS", "PP", "LDPE", "HDPE", "PS", "PVC", "PET")
load(paste0("PhD/mennekes/output_files/rdata/cont_EPS_801_lake15.Rdata"))

lake <- s_r$isLake
no_outflow <- s_r$outflow == 0
rivers <- lake == F & no_outflow == T

dfwater <- as.data.frame(matrix(NA, nrow = nrow(s_r), ncol = I(length(polymers)+ 1))) #*2 for scenarios
names(dfwater) <- c(polymers, "all") #value per s

for (mat in polymers) {
	load(paste0(main.path, "output_files/rdata/cont_", mat,"_", rounds, "_withallfactors", ".Rdata"))
	dfwater[ , mat] <-  s_r[ , paste0("water_perM_",mat,"_MiP")]
}

dfwater$all <- rowSums(dfwater[ , 1:length(polymers)])



dfsed <- as.data.frame(matrix(NA, nrow = nrow(s_r), ncol = I(length(polymers)+ 1))) #*2 for scenatgrios
names(dfsed) <- c(polymers, "all") #value per s

for (mat in polymers) {
	load(paste0(main.path, "output_files/rdata/cont_", mat,"_", rounds, "_withallfactors", ".Rdata"))
	dfsed[ , mat] <-  s_r[ , paste0("accumulation_perM_",mat,"_MiP")] #für Flüsse per m
}

dfsed$all <- rowSums(dfsed[ , 1:length(polymers)])


#figure##############################

n <- names(dfwater)
n
load("PhD/mennekes/temp_data/flow_files/0000_geo.Rdata")
load("PhD/mennekes/data_raw/maps/lakes/lakes_poly.Rdata")

farben <- c('#d7191c','#fdae61','#ffd700','#91bfdb','#0571b0', "grey70")

nice <- theme_bw()+
	theme(legend.background = element_rect(fill = "transparent", color = NA),
				plot.background = element_rect(fill = "transparent",colour = NA),
				text = element_text(color = "black"),
				axis.text.x = element_text(color = "black", face = "plain", size = 7),
				axis.text.y = element_text(color = "black", face = "plain", size = 7),
				legend.text = element_text(size = 7),
				legend.title = element_text(size = 7),
				panel.background = element_rect(fill = "transparent"),
				plot.title = element_text(size = 7),
				plot.subtitle = element_text(size = 7)) #trbl
klein = 0.1
gross = 0.5



for (i in n) {
	i_print <- i
	if(i == "all"){
		i_print <- "all polymers"
	}
	
	
	#for MiP in river water
	conc <- dfwater[rivers, i]
	d2 <- geo[rivers, ]
	d2 <- cbind(d2, conc)
	d2$conc <- d2$conc*1000 #change from m to km
	d2$size <- 1
	d2$size[d2$conc >0 ] <- 2
	d2$conc[d2$conc == 0] <- NA
	d2$conc_print <- "x"
	# m <- mean(d2$conc, na.rm = T)
	m <- col.NR(max(d2$conc, na.rm = T)) #find 5 groups of colors based on the function
	#make factors of conc. for printing
	d2$conc_print[d2$conc >= m[2]] <- "a"
	d2$conc_print[d2$conc <= m[2]] <- "b"
	d2$conc_print[d2$conc <= m[3]] <- "c"
	d2$conc_print[d2$conc <= m[4]] <- "d"
	d2$conc_print[d2$conc <= m[5]] <- "e"
	d2$conc_print <- as.factor(d2$conc_print)
	pr <- ggplot()+
		geom_sf(data = lakes_poly[lakes_poly$area > 100000, ], color = "black", fill = "white", size = 0.1)+
		geom_sf(data = d2[d2$size == 1, ],  aes(color = conc_print), size = klein)+
		geom_sf(data = d2[d2$size == 2, ],  aes(color = conc_print), size = gross)+
		scale_colour_manual(paste0("mass in water\n", i_print, "\nin g/km"),
												values = farben,
												breaks = c("a", "b", "c", "d", "e", "x"),
												labels = c(paste0(m[2]," - ",m[1]), paste0(m[3]," - ",m[2]),paste0(m[4]," - ",m[3]),paste0(m[5]," - ",m[4]),paste0("> 0 - ",m[5]), "0"), drop = F)+
				nice+
		labs(title = paste0(i_print, " in rivers"))+
		ggspatial::annotation_scale(location = "br", height = unit(0.2, "cm"), text_pad = unit(7, "pt"), line_width = 1)+
		ggspatial::annotation_north_arrow(width = unit(0.4, "cm"), height = unit(0.8, "cm"),style = north_arrow_orienteering(text_size = 7))
	ggsave(paste0("PhD/mennekes/output_files/plots/map_river_", i, ".png"), plot = pr, width = 17, height = 8, units = "cm", dpi = 1000, bg = "transparent")

	
	
	
	#####################
	#for MiP accumulation
	
	
	conc <- dfsed[rivers, i]
	d2 <- geo[rivers, ]
	d2 <- cbind(d2, conc)
	d2$conc <- d2$conc*365*24*60*60*1000 #change to g/km and year
	d2$size <- 1
	d2$size[d2$conc >0 ] <- 2
	d2$conc[d2$conc == 0] <- NA
	d2$conc_print <- "x"
	# m <- mean(d2$conc, na.rm = T)
	m <- col.NR(max(d2$conc, na.rm = T)) #find 5 groups of colors based on the function
	#make factors of conc. for printing
	d2$conc_print[d2$conc >= m[2]] <- "a"
	d2$conc_print[d2$conc <= m[2]] <- "b"
	d2$conc_print[d2$conc <= m[3]] <- "c"
	d2$conc_print[d2$conc <= m[4]] <- "d"
	d2$conc_print[d2$conc <= m[5]] <- "e"
	d2$conc_print <- as.factor(d2$conc_print)
	ps <- ggplot()+
		geom_sf(data = lakes_poly[lakes_poly$area > 100000, ], color = "black", fill = "white", size = 0.1)+
		geom_sf(data = d2[d2$size == 1, ],  aes(color = conc_print), size = klein)+
		geom_sf(data = d2[d2$size == 2, ],  aes(color = conc_print), size = gross)+
		scale_colour_manual(paste0("yearly accumulation\n", i_print, "\nin g/km*year"),
												values = farben,
												breaks = c("a", "b", "c", "d", "e", "x"),
												labels = c(paste0(m[2]," - ",m[1]), paste0(m[3]," - ",m[2]),paste0(m[4]," - ",m[3]),paste0(m[5]," - ",m[4]),paste0("> 0 - ",m[5]), "0"), drop = F)+
		nice+
		labs(title = paste0(i_print, " accumulation in rivers"))+
		ggspatial::annotation_scale(location = "br", height = unit(0.2, "cm"), text_pad = unit(7, "pt"), line_width = 1)+
		ggspatial::annotation_north_arrow(width = unit(0.4, "cm"), height = unit(0.8, "cm"),style = north_arrow_orienteering(text_size = 7))
	
	ggsave(paste0("PhD/mennekes/output_files/plots/map_river_acc_", i, ".png"), plot = ps, width = 17, height = 8, units = "cm", dpi = 1000, bg = "transparent")
	
	
	#########
	#
	conc <- dfsed[lake, i]
	d2 <- geo[lake, ]
	d2 <- cbind(d2, conc)
	d2$conc <- d2$conc*365*24*60*60*10000 #change to g/ha and year
	d2$size <- 1
	d2$size[d2$conc >0 ] <- 2
	d2$conc[d2$conc == 0] <- NA
	d2$conc_print <- "x"
	# m <- mean(d2$conc, na.rm = T)
	m <- col.NR(max(d2$conc, na.rm = T)) #find 5 groups of colors based on the function
	#make factors of conc. for printing
	d2$conc_print[d2$conc >= m[2]] <- "a"
	d2$conc_print[d2$conc <= m[2]] <- "b"
	d2$conc_print[d2$conc <= m[3]] <- "c"
	d2$conc_print[d2$conc <= m[4]] <- "d"
	d2$conc_print[d2$conc <= m[5]] <- "e"
	d2$conc_print <- as.factor(d2$conc_print)
	dlake <- st_join(lakes_poly, d2) %>% filter(!(is.na(id_all_geo)))

	psl <- ggplot()+
		geom_sf(data = dlake, aes(fill = conc_print, color = conc_print), size = 0.1)+
		scale_fill_manual(paste0("yearly accumulation\n", i_print, "\nin g/ha*year"),
												values = farben,
												breaks = c("a", "b", "c", "d", "e", "x"),
												labels = c(paste0(m[2]," - ",m[1]), paste0(m[3]," - ",m[2]),paste0(m[4]," - ",m[3]),paste0(m[5]," - ",m[4]),paste0("> 0 - ",m[5]), "0"), drop = F)+
		scale_color_manual(paste0("yearly ", i_print, " accumulation\ng/ha*yr"),
											values = farben,
											breaks = c("a", "b", "c", "d", "e", "x"),
											labels = c(paste0(m[2]," - ",m[1]), paste0(m[3]," - ",m[2]),paste0(m[4]," - ",m[3]),paste0(m[5]," - ",m[4]),paste0("> 0 - ",m[5]), "0"))+
		guides(col = "none") +
		nice+
		labs(title = paste0(i_print, " accumulation in lakes"))+
		ggspatial::annotation_scale(location = "br", height = unit(0.2, "cm"), text_pad = unit(7, "pt"), line_width = 1)+
		ggspatial::annotation_north_arrow(width = unit(0.4, "cm"), height = unit(0.8, "cm"),style = north_arrow_orienteering(text_size = 7))
	
	ggsave(paste0("PhD/mennekes/output_files/plots/map_lake_acc_", i, ".png"), plot = psl, width = 17, height = 8, units = "cm", dpi = 1000, bg = "transparent")
	
	#3 karten in einer
	
	pall <- ggdraw(plot_grid(pr, ps, psl, rel_heights = c(1,1,1), nrow = 3, align = "v", labels = c("a)", "b)", "c)"), label_size = 7))
	ggsave(paste0("PhD/mennekes/output_files/plots/map_all", i, ".png"), plot = pall, width = 17, height = 17, units = "cm", dpi = 1000, bg = "transparent")
	
	# ggsave(paste0("PhD/mennekes/output_files/plots/map_all", i, ".pdf"), plot = pall, width = 17, height = 17, units = "cm", dpi = 1000, bg = "transparent")
	
	print(paste0(i_print, " done!"))
	
}





rm(list = ls())
