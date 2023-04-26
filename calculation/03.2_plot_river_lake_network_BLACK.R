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


load("PhD/mennekes/temp_data/flow_files/0000_geo.Rdata")
load("PhD/mennekes/data_raw/maps/lakes/lakes_poly.Rdata")
load(paste0("PhD/mennekes/output_files/rdata/cont_EPS_801_lake15.Rdata"))# for futher informations e. g. lakes / outflow

farben <- c('#d7191c','#fdae61','#ffd700','#91bfdb','#0571b0', "grey70")


nice <- theme_minimal()+
	theme(legend.background = element_rect(fill = "transparent", color = NA),
				plot.background = element_rect(fill = "transparent",colour = NA),
				text = element_text(color = "black"),
				axis.text = element_blank(),
				legend.text = element_text(size = 7),
				legend.title = element_text(size = 7),
				panel.background = element_rect(fill = "transparent"),
				plot.title = element_text(size = 7),
				plot.subtitle = element_text(size = 7),
				axis.ticks = element_blank(),
				rect = element_blank(),
				panel.grid = element_blank(), panel.border = element_blank()) #trbl


plake <- ggplot(lakes_poly)+
		geom_sf(color = "blue", fill = "blue", alpha = 0.5, size = 0.1)+
	ggspatial::annotation_scale(location = "br", height = unit(0.2, "cm"), bar_cols = c("white", "black"), text_pad = unit(7, "pt"), line_width = 0.7, line_col = "white", text_col = "white")+
	nice
		
	ggsave(paste0("PhD/mennekes/output_files/plots/lakes.png"), plot = plake, width = 22, height = 14, units = "cm", dpi = 1000, bg = "transparent")
	
	priver <- ggplot(geo[s_r$outflow == 0 & s_r$isLake == F, ])+
		geom_sf(color = "white", size = 0.1)+
		ggspatial::annotation_scale(location = "br", height = unit(0.2, "cm"), bar_cols = c("white", "black"), text_pad = unit(7, "pt"), line_width = 0.7, line_col = "white", text_col = "white")+
		nice
	
	ggsave(paste0("PhD/mennekes/output_files/plots/rivers.png"), plot = priver, width = 22, height = 14, units = "cm", dpi = 1000, bg = "transparent")
	
	


rm(list = ls())
