
####
#load data

library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggrepel)
library(patchwork) #for making all images the same dimensions.
library(cowplot)
library(sf)
library(reshape2)
library(ggpattern)

#path to sub-folders
setwd("~/")
main.path <- "PhD/mennekes/"

#################################################################
# load data here!
#################################################################
load("PhD/mennekes/temp_data/data_scatterplot.Rdata")
df02[which(df02$ID == 378891), ]


# colors <- c("#b10026", "#998ec3", "#eeee55", "#9c9c9c")# according to maps
colors <- c('dodgerblue',  "#CC5700", '#555555','#aaaaaa') #colors of paper

nice <- theme_bw()+
	theme(legend.position = "right",
				legend.title = element_text(size = 7),
				legend.text = element_text(size = 16, color = "white"),
				legend.background = element_rect(fill = "transparent", color = NA),
				plot.background = element_rect(fill = "transparent",colour = NA),
				text = element_text(color = "white", size = 16),
				panel.grid = element_blank(),
				axis.text.x = element_text(color = "white", face = "plain", size = 16),
				axis.text.y = element_text(color = "white", face = "plain", size = 16),
				panel.background = element_rect(fill = "transparent"),
				plot.subtitle = element_text(size = 10),
				plot.margin = unit(c(0.2,0.2,0.2,0.2), units = "cm"),
				axis.ticks = element_line(color = "white"),
				axis.line = element_line(colour = "white"),
				panel.border = element_rect(color = "white"))

#Rhine

#make figure
df03 <- df02 %>% filter(catchment == "Rhine", polymer == "PET")
df03$len_km <- df03$len_m/1000

df04 <- df02 %>% filter(catchment == "Rhine", polymer == "PS")
df04$len_km <- df04$len_m/1000

x <- which(df03$len_km<50 & df03$len_km>20)
df03[x, ]

#chull
a <- chull(data.frame(x = df03$len_km, y = df03$value))
length(a)
a <- c(a[1:14], 348, a[15], a[1])

b <- chull(data.frame(x = df04$len_km, y = df04$value))

# library(devtools)
# devtools::install_github("cmartin/ggConvexHull")
library(ggConvexHull)

petrhine <- ggplot(df03 , aes(x = len_km, y = (1-value)*100))+
	geom_polygon(data = df03[a, ], aes(x = len_km, y = (1-value)*100), color = "#FF9900", fill = "#FF9900", alpha = 0.5)+
	geom_point(size = 0.8, color = "white")+
	labs(x = "distance to outflow [km]",
			 y = "retention in catchment [%]")+
	scale_x_continuous(limits = c(0,350), expand = c(0,0))+
	scale_y_continuous(limits = c(0,101),expand = c(0,0))+
	nice

ggsave(paste0("PhD/mennekes/output_files/plots/black/scatterPET_rhine.png"), plot = petrhine, width = 18, height = 10, units = "cm", dpi = 1000, bg = "transparent")

	

psrhine <- ggplot(df04 , aes(x = len_km, y = (1-value)*100))+
	geom_polygon(data = df04[b, ], aes(x = len_km, y = (1-value)*100), color = "#FF9900", fill = "#FF9900", alpha = 0.5)+
	geom_point(size = 0.8, color = "white")+
	labs(x = "distance to outflow [km]",
			 y = "retention in catchment [%]")+
	scale_x_continuous(limits = c(0,350), expand = c(0,0))+
	scale_y_continuous(limits = c(0,101),expand = c(0,0))+
	nice

ggsave(paste0("PhD/mennekes/output_files/plots/black/scatterPS_rhine.png"), plot = psrhine, width = 18, height = 10, units = "cm", dpi = 1000, bg = "transparent")


#rhone
df05 <- df02 %>% filter(catchment == "Rhône", polymer == "PS")
df05$len_km <- df05$len_m/1000

d <- chull(data.frame(x = df05$len_km, y = df05$value))
d <- c(d, 18)

psrhone <- ggplot(df05 , aes(x = len_km, y = (1-value)*100))+
	geom_polygon(data = df05[d, ], aes(x = len_km, y = (1-value)*100), color = "#FF9900", fill = "#FF9900", alpha = 0.5)+
	geom_point(size = 0.8, color = "white")+
	labs(x = "distance to outflow [km]",
			 y = "retention in catchment [%]")+
	scale_x_continuous(limits = c(0,350), expand = c(0,0))+
	scale_y_continuous(limits = c(0,101),expand = c(0,0))+
	nice

ggsave(paste0("PhD/mennekes/output_files/plots/black/scatterPS_rhone.png"), plot = psrhone, width = 18, height = 10, units = "cm", dpi = 1000, bg = "transparent")

#rhine and rhone in one image
psrhone2 <- ggplot(df05 , aes(x = len_km, y = (1-value)*100))+
	geom_polygon(data = df04[b, ], aes(x = len_km, y = (1-value)*100), color = "#FF9900", fill = "#FF9900", alpha = 0.5)+
	geom_polygon(data = df05[d, ], aes(x = len_km, y = (1-value)*100), color = "#FF9900", fill = "#FF9900", alpha = 0.5)+
	labs(x = "distance to outflow [km]",
			 y = "retention in catchment [%]")+
	scale_x_continuous(limits = c(0,350), expand = c(0,0))+
	scale_y_continuous(limits = c(0,101),expand = c(0,0))+
	nice

ggsave(paste0("PhD/mennekes/output_files/plots/black/scatterPS_rhone_rhine.png"), plot = psrhone2, width = 18, height = 10, units = "cm", dpi = 1000, bg = "transparent")

#empty plot
leer <- ggplot(df05 , aes(x = len_km, y = (1-value)*100))+
	labs(x = "distance to outflow [km]",
			 y = "retention in catchment [%]")+
	scale_x_continuous(limits = c(0,350), expand = c(0,0))+
	scale_y_continuous(limits = c(0,101),expand = c(0,0))+
	nice

ggsave(paste0("PhD/mennekes/output_files/plots/black/scatter_leer.png"), plot = leer, width = 18, height = 10, units = "cm", dpi = 1000, bg = "transparent")

