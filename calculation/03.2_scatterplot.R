####################################
## scatter plot
# plot the data from plots_loss

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
#


fl_short <- list.files(paste0(main.path, "temp_data/loss"))
fl_long <- list.files(paste0(main.path, "temp_data/loss"), full.names = T)

load(paste0(main.path, "temp_data/loss/", fl_short[1]))

#load and create extra data
load("PhD/mennekes/temp_data/rivers_all6_withallfactors.Rdata")
geo <- rivers.all6 %>% select(id_all)
outlet <- rivers.all6$id_all[rivers.all6$outflow %in% c(1,3)]
flow_to_outlet <- which(rivers.all6$flow_to %in% outlet)

d <- rivers.all6 %>% dplyr::select(id_all, flow_to) %>% st_drop_geometry()
len <- rivers.all6$length_m
lakes <- rivers.all6$FID_poly_s
lake_areas <- data.frame(ids = rivers.all6$FID_poly_s,
												 area = rivers.all6$area)
lake_areas <- lake_areas[!(is.na(lake_areas$ids)), ]
lake_areas <- lake_areas[!(duplicated(lake_areas$ids)), ]

#load data with catchment information
ezg <- st_read(paste0(main.path, "data_raw/maps/rivers/ezgsCH.shp")) %>% select(name)

#dataframe
df01 <- data.frame(ID=NA, polymer = NA, value = NA, len_m = NA, NR_lakes = NA, outflow_id = NA, lake_area_tot = NA, lake_area_avg = NA, catchment = NA)
df <- data.frame(ID=NA, polymer = NA, value = NA, len_m = NA, NR_lakes = rep(NA,3), outflow_id = NA, lake_area_tot = NA, lake_area_avg = NA, catchment = NA)

for (k in 1:length(fl_short)) {
	load(paste0(main.path, "temp_data/loss/", fl_short[k]))
	counter <- 1
	for (j in 1:I(ncol(results)/3)) {
		print(counter)
		f <- which(results[flow_to_outlet, counter] >0)
		if(length(f)!=1){
			next
		}
		f_id <- flow_to_outlet[f] #final ID for the river
		id_start <- as.numeric(substr(names(results[counter]),5,15))
		df$ID <- id_start
		df$outflow_id <- f_id
		df$polymer <- c("EPS", "PS", "PET")
		
		d$x <- 0
		d$x[id_start] <- 1
		ft <- d$flow_to[which(d$x == 1)]
		ft_long <- id_start
		for (i in 1:600) {
			d$x[ft] <- 1
			ft_long <- c(ft_long, ft)
			ft <- d$flow_to[ft]
		}
		ft_long <- ft_long[1:which(ft_long == f_id)]
		df$len_m <- sum(len[ft_long])
		
		#for lakes
		l <- lakes[ft_long]#get FID_poly_s for lakes, can be used in lake_areas to get the area
		l <- unique(l[!(is.na(l))])
		if(length(l)>0){ #only if lakes exists
			df$NR_lakes <- length(l)
			df$lake_area_avg <- mean(lake_areas$area[lake_areas$ids %in% l])
			df$lake_area_tot <- sum(lake_areas$area[lake_areas$ids %in% l])
		}else{
			df$NR_lakes <- 0
			df$lake_area_avg <- 0
			df$lake_area_tot <- 0
		}
		
		#final values
		df$value <- as.numeric(results[f_id, counter : I(counter+2)])
		df01 <- rbind(df01, df)
		counter <- counter+3
		
	}
		
}

#final data
df02 <- df01[2:nrow(df01), ]
length(unique(df02$ID)) #number of runs

#get catchment
df_poly <- geo[df02$ID, ]
df_poly02 <- df_poly %>% mutate(withins = as.integer(st_intersects(geometry, ezg)),
									 area = if_else(is.na(withins), " ", ezg$name[withins]))




df02$sub_catchment <- df_poly02$area
df02$sub_catchment[!(df02$sub_catchment %in% c("Rhine", "Aare", "Reuss", "Limmat", "Rhône", "Doubs"))] <- "other"
df02$catchment <- df02$sub_catchment
df02$catchment[df02$catchment %in% c("Limmat", "Aare", "Reuss")] <- "Rhine"
df02$sub_catchment[!(df02$sub_catchment %in% c( "Aare", "Reuss", "Limmat"))] <- "X"

save(df02, file = "PhD/mennekes/temp_data/data_scatterplot.Rdata")


#################################################################
# load data here!
#################################################################
load("PhD/mennekes/temp_data/data_scatterplot.Rdata")

# colors <- c("#b10026", "#998ec3", "#eeee55", "#9c9c9c")# according to maps
colors <- c('dodgerblue',  "#CC5700", '#555555','#aaaaaa') #colors of paper

nice <- theme_bw()+
	theme(legend.position = "right",
				legend.title = element_text(size = 7),
				legend.text = element_text(size = 7),
				legend.background = element_rect(fill = "transparent", color = NA),
				plot.background = element_rect(fill = "transparent",colour = NA),
				text = element_text(color = "black", size = 7),
				panel.grid = element_blank(),
				axis.text.x = element_text(color = "black", face = "plain", size = 7),
				axis.text.y = element_text(color = "black", face = "plain", size = 7),
				panel.background = element_rect(fill = "transparent"),
				plot.subtitle = element_text(size = 10),
				plot.margin = unit(c(0.2,0.2,0.2,0.2), units = "cm"))

#s
df02 %>%filter(polymer == "PET") %>%  group_by(catchment) %>% summarise(n = n())
#make figure
pet1 <- ggplot(df02 %>% filter(polymer == "PET"), aes(x = len_m/1000, y = (1-value)*100, shape = sub_catchment, color = catchment))+
	geom_point(size = 0.8)+
	scale_color_manual(" catchment",
										 values = c("Rhine" = colors[1], "Rhône" = colors[2], "Doubs" = colors[3], "other" = colors[4]), 
										 breaks = c("Rhine", "Rhône", "Doubs", "other"),
										 labels = c("Rhine", "Rhône", "Doubs", "other catchments"))+
	labs(x = "distance to outflow [km]",
			 y = "retention in catchment [%]")+
	scale_shape_manual("sub-catchment",
										 values = c("X" = 16, "Aare" = 3, "Reuss" = 4, "Limmat" = 8),
										 breaks = c( "Aare", "Reuss", "Limmat"),
										 labels = c( "Aare", "Reuss", "Limmat"))+
	scale_x_continuous(limits = c(0,350), expand = c(0,0))+
	scale_y_continuous(limits = c(0,101),expand = c(0,0))+
	annotate("text", x = 300, y = 10, size = 2, label = "PET")+
	nice+
	theme(legend.position = "none")

df04 <- df02
df04$catchment <- factor(df04$catchment, levels = c("Rhine", "Rhône", "Doubs", "other"))
df04$polymer <- factor(df04$polymer, levels = c("EPS", "PS", "PET"))
all1 <- ggplot(df04, aes(x = polymer, y = (1-value)*100, fill = catchment))+
	geom_boxplot(width = 0.5, position = position_dodge(0.6), outlier.colour = NA, alpha = 0.2, size = 0.3)+
	geom_jitter(position = position_dodge(0.6), shape = 1, aes(color = catchment), size = 0.5 )+
	labs(x = "polymers",
			 y = "retention in catchment [%]")+
	scale_color_manual(" ",
										 values = c("Rhine" = colors[1], "Rhône" = colors[2], "Doubs" = colors[3], "other" = colors[4]), 
										 breaks = c("Rhine", "Rhône", "Doubs", "other"),
										 labels = c("Rhine", "Rhône", "Doubs", "other catchments"))+
	scale_fill_manual(" ",
										 values = c("Rhine" = colors[1], "Rhône" = colors[2], "Doubs" = colors[3], "other" = colors[4]), 
										 breaks = c("Rhine", "Rhône", "Doubs", "other"),
										 labels = c("Rhine", "Rhône", "Doubs", "other catchments"))+
	scale_x_discrete(expand = c(0,0))+
	scale_y_continuous(expand = c(0.01,0), limits = c(0,100))+
	nice+
	theme(plot.margin = unit(c(0.5,0.5,0.1,0.8), units = "cm"),
				axis.ticks.x = element_blank(), legend.position = "none")
all1

ps1 <- ggplot(df02 %>% filter(polymer == "PS"), aes(x = len_m/1000, y = (1-value)*100, shape = sub_catchment, color = catchment))+
	geom_point(size = 0.8)+
	scale_color_manual(" catchment",
										 values = c("Rhine" = colors[1], "Rhône" = colors[2], "Doubs" = colors[3], "other" = colors[4]), 
										 breaks = c("Rhine", "Rhône", "Doubs", "other"),
										 labels = c("Rhine", "Rhône", "Doubs", "other catchments"))+
	labs(x = "distance to outflow [km]",
			 y = "retention in catchment [%]")+
	scale_shape_manual("sub-catchments Rhine",
										 values = c("X" = 16, "Aare" = 3, "Reuss" = 4, "Limmat" = 8),
										 breaks = c( "Aare", "Reuss", "Limmat"),
										 labels = c( "Aare", "Reuss", "Limmat"))+
	scale_x_continuous(limits = c(0,350), expand = c(0,0))+
	scale_y_continuous(limits = c(0,101),expand = c(0,0))+
	annotate("text", x = 300, y = 10, size = 2, label = "PS")+
	nice+
	theme(legend.position = "none")

eps1 <- ggplot(df02 %>% filter(polymer == "EPS"), aes(x = len_m/1000, y = (1-value)*100, shape = sub_catchment, color = catchment))+
	geom_point(size = 0.8)+
	scale_color_manual(" catchment",
										 values = c("Rhine" = colors[1], "Rhône" = colors[2], "Doubs" = colors[3], "other" = colors[4]), 
										 breaks = c("Rhine", "Rhône", "Doubs", "other"),
										 labels = c("Rhine", "Rhône", "Doubs", "other catchments"))+
	labs(x = "distance to outflow [km]",
			 y = "retention in catchment [%]")+
	scale_shape_manual("sub-catchment",
										 values = c("X" = 16, "Aare" = 3, "Reuss" = 4, "Limmat" = 8),
										 breaks = c( "Aare", "Reuss", "Limmat"),
										 labels = c( "Aare", "Reuss", "Limmat"))+
	scale_x_continuous(limits = c(0,350), expand = c(0,0))+
	scale_y_continuous(limits = c(0,101),expand = c(0,0))+
	annotate("text", x = 300, y = 90, size = 2, label = "EPS")+
	nice+
	theme(legend.position = "none")

ggplot(df02 %>% filter(polymer == "PS", catchment == "Rhine"), aes(x = len_m/1000, y = (1-value)*100, color = sub_catchment))+
	geom_point(size = 2)+
	scale_color_manual(" catchment",
										 values = c("Aare" = colors[1], "Reuss" = colors[2], "Limmat" = colors[3], "X" = colors[4]), 
										 breaks = c("Aare", "Reuss", "Limmat", "X"),
										 labels = c("Aare", "Reuss", "Limmat", "Rhine"))+
	labs(x = "distance to outflow [km]",
			 y = "retention in catchment [%]")+
	scale_x_continuous(limits = c(0,350), expand = c(0,0))+
	scale_y_continuous(limits = c(0,101),expand = c(0,0))+
	nice


legend1 <- ggplot(df02 %>% filter(polymer == "EPS"), aes(x = len_m/1000, y = (1-value)*100, shape = sub_catchment, color = catchment))+
	geom_point(size = 1)+
	scale_color_manual(" catchment",
										 values = c("Rhine" = colors[1], "Rhône" = colors[2], "Doubs" = colors[3], "other" = colors[4]), 
										 breaks = c("Rhine", "Rhône", "Doubs", "other"),
										 labels = c("Rhine", "Rhône", "Doubs", "other catchments"))+
	labs(x = "distance to outflow [km]",
			 y = "retention in catchment [%]")+
	scale_shape_manual("\nsub-catchment Rhine",
										 values = c("X" = 16, "Aare" = 3, "Reuss" = 4, "Limmat" = 8),
										 breaks = c( "Aare", "Reuss", "Limmat"),
										 labels = c( "Aare", "Reuss", "Limmat"))+
	scale_x_continuous(limits = c(0,350), expand = c(0,0))+
	scale_y_continuous(limits = c(0,101),expand = c(0,0))+
	nice

legend1 <- get_legend(legend1)




#ratio

df05 <- df02
df05$ratio <- ((1-df05$value)*100)/(df05$len_m/1000)
df05$catchment <- factor(df05$catchment, levels = c("Rhine", "Rhône", "Doubs", "other"))
df05$polymer <- factor(df05$polymer, levels = c("EPS", "PS", "PET"))

all2 <- ggplot(df05 %>% filter(ratio != 0), aes(x = polymer, y = log10(ratio), fill = catchment))+
	geom_boxplot(width = 0.5, position = position_dodge(0.6), outlier.colour = NA, alpha = 0.2, size = 0.3)+
	geom_jitter(position = position_dodge(0.6), shape = 1, aes(color = catchment), size = 0.5 )+
	labs(x = "polymers",
			 y = "retention rate [% / km]")+
	scale_color_manual(" ",
										 values = c("Rhine" = colors[1], "Rhône" = colors[2], "Doubs" = colors[3], "other" = colors[4]), 
										 breaks = c("Rhine", "Rhône", "Doubs", "other"),
										 labels = c("Rhine", "Rhône", "Doubs", "other catchments"))+
	scale_fill_manual(" ",
										values = c("Rhine" = colors[1], "Rhône" = colors[2], "Doubs" = colors[3], "other" = colors[4]), 
										breaks = c("Rhine", "Rhône", "Doubs", "other"),
										labels = c("Rhine", "Rhône", "Doubs", "other catchments"))+
	scale_x_discrete(expand = c(0.1,0.1))+
	scale_y_continuous(expand = c(0.05,0.05), breaks = c(1,-1,-3, -5), labels = c(10,  0.1, 0.001, "0.00001"))+
	nice+
	theme(plot.margin = unit(c(0.5,0.5,0.1,0.8), units = "cm"),
				axis.ticks.x = element_blank(), legend.position = "none")
all2



# math
df05 %>% group_by(polymer, catchment) %>% summarise(sd(ratio), median(ratio), )
df05 %>% summarise(sd(ratio), mean(ratio))
df05 %>%  group_by(catchment)%>% summarise(sd(ratio), mean(ratio))
df05 %>%  group_by(polymer)%>% summarise(sd(ratio), means = mean(ratio)) 

pp1 <- ggdraw(plot_grid(plot_grid(eps1, ps1, pet1, all2, nrow = 2, rel_widths = c(1,1,1,1), labels = c("a)", "b)", "c)", "d)"), label_size = 7, label_x = c(0,0,0,0.08), label_y = c(1,1,1,0.95)), legend1, nrow = 1, rel_widths = c(1,0.2)))

ggsave("PhD/mennekes/output_files/plots/diff_ezg.png", plot = pp1, dpi = 500, width = 17, height = 10, units = "cm", bg = "transparent")
ggsave("PhD/mennekes/output_files/plots/diff_ezg.pdf", plot = pp1, dpi = 500, width = 17, height = 12, units = "cm", bg = "transparent")




