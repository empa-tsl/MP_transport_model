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


#colors
blue <- c("#deebf7", "#9ecae1", "#3182bd")
black <- c('#f0f0f0','#bdbdbd','#636363')
red <- c('#fee6ce','#fdae6b','#e6550d')
farben <- c( 'dodgerblue','#555555',"#CC5700")
nice <- theme_classic()+
				theme(legend.position = "none",
							legend.background = element_rect(fill = "transparent", color = NA),
							plot.background = element_rect(fill = "transparent",colour = NA),
							text = element_text(color = "black"),
							axis.text.x = element_text(color = "black", face = "plain", size = 7),
							axis.text.y = element_text(color = "black", face = "plain", size = 7),
							axis.title.x = element_blank(), 
							axis.title.y = element_text(size = 7),
							axis.ticks.x = element_blank(),
							panel.background = element_rect(fill = "transparent")) #trbl


###########################################################################
#Balkendiagramme
# für alle Polymere
df01_total <- df01 %>% group_by(scenario, compartment) %>% summarise(value = sum(value))
df01_total$value <- (df01_total$value*365*24*60*60)/1000000 #change to yearly values and tonnes!
df01_total$scenario <- factor(df01_total$scenario,                                    # Change ordering manually
									levels = c(scenarios))
df21 <- df01 %>% filter(compartment == "outflow")
df21$scenario <- factor(df21$scenario,
												levels = scenarios)

df21$polymer <- factor(df21$polymer,
											 levels = c("EPS", "PP", "LDPE", "HDPE", "PS", "PVC", "PET"))
df21$value <- (df21$value*365*24*60*60)/1000000


#### mit farben!!!!!!!!!
#for all mip in CH!
# outflow
size_label = 2
p1 <- ggplot(data=df01_total[df01_total$compartment == "outflow", ], aes(x=scenario, y=value, fill=scenario)) +
	geom_bar(stat="identity")+
	scale_y_continuous(expand = c(0,0,0,1),limits = c(0,I(max(df01_total$value[df01_total$compartment == "outflow"]) +1.7 )), breaks = c(0,5,10))+
	scale_x_discrete(labels = c(bquote("S"[0]),bquote("S"[lake]), bquote("S"[all]) ), expand = c(0.2,0.1))+
	geom_text(aes(label= round(value,1)), vjust=-0.8, color="black",
						position = position_dodge(0.9), size=2)+
	labs(y = "total microplastic outflow\nin suspension [tonnes / year]")+
	scale_fill_manual("",
										values = farben,
										labels = c("S0", "Slake", "Sall"),
										breaks = c("_withoutfactors", "_lakeonlyfactors", "_withallfactors"))+
	nice +
	theme(plot.margin=unit(c(0.1,0.6,0.1,0.2),"cm"))+ #trbl
	geom_curve(
		aes(x = as.factor("_lakeonlyfactors"), y = 8.3, xend = as.factor("_withallfactors"), yend = 6.3),
		arrow = arrow(length = unit(0.04, "npc") ),
		ncp = 1000,
		curvature = -0.57, size = 1, angle = 100,
		color = farben[3]
	)+
	geom_curve(
		aes(x = as.factor("_withoutfactors"), y = 11.4, xend = as.factor("_lakeonlyfactors"), yend = 8.3),
		arrow = arrow(length = unit(0.04, "npc") ),
		ncp = 1000,
		curvature = -0.5, size = 1, angle = 100,
		color = farben[2]
	)+
	geom_label(aes(x = as.factor("_lakeonlyfactors"), y = 11.9, label = "reduction through\nsedimentation in lakes"), fill = farben[2],hjust = 0.6, color = "white", size = size_label)+
	geom_label(aes(x = as.factor("_withallfactors"), y = 9.8, label = "additional reduction through\nsedimentation in rivers"), hjust = 0.75, fill = farben[3], color = "white", size = size_label)



#fancy
farben <- rev(c("#cc5700", "#c97544", "#bf9077", "#aaaaaa", "#92a1c7", "#6f98e3", "#1e90ff"))

p21 <- ggplot(data=df21, aes(x=scenario, y=value, fill=polymer)) +
	geom_bar(stat="identity")+
	scale_y_continuous(expand = c(0,0,0,1),limits = c(0,I(max(df01_total$value[df01_total$compartment == "outflow"]) +1.7 )), breaks = c(0,5,10))+
	scale_x_discrete(labels = c(bquote("S"[0]),bquote("S"[lake]), bquote("S"[all]) ), expand = c(0.2,0.1))+
	annotate("text", label = round(df01_total$value[df01_total$scenario == "_withoutfactors"& df01_total$compartment == "outflow"], 1), x = as.factor("_withoutfactors"), y = I(df01_total$value[df01_total$scenario == "_withoutfactors"& df01_total$compartment == "outflow"]+ df01_total$value[df01_total$scenario == "_withoutfactors"& df01_total$compartment == "outflow"]*0.08), size = 2)+ #text of total numbers
	annotate("text", label = round(df01_total$value[df01_total$scenario == "_lakeonlyfactors"& df01_total$compartment == "outflow"], 1), x = as.factor("_lakeonlyfactors"), y = I(df01_total$value[df01_total$scenario == "_lakeonlyfactors"& df01_total$compartment == "outflow"]+ df01_total$value[df01_total$scenario == "_lakeonlyfactors"& df01_total$compartment == "outflow"]*0.08), size = 2)+
	annotate("text", label = round(df01_total$value[df01_total$scenario == "_withallfactors"& df01_total$compartment == "outflow"], 1), x = as.factor("_withallfactors"), y = I(df01_total$value[df01_total$scenario == "_withallfactors"& df01_total$compartment == "outflow"]+ df01_total$value[df01_total$scenario == "_withallfactors"& df01_total$compartment == "outflow"]*0.08), size = 2)+
	labs(y = "total microplastic outflow\nin suspension [tonnes / year]")+
	scale_fill_manual("",
										values = farben)+
	nice +
	theme(plot.margin=unit(c(0.1,0.6,0.1,0.2),"cm"))+ #trbl
	geom_curve(
		aes(x = as.factor("_lakeonlyfactors"), y = 8.1, xend = as.factor("_withallfactors"), yend = 6.3),
		arrow = arrow(length = unit(0.04, "npc") ),
		ncp = 1000,
		curvature = -0.57, size = 0.5, angle = 100, 
		color = "black"
	)+
	geom_curve(
		aes(x = as.factor("_withoutfactors"), y = 11.5, xend = as.factor("_lakeonlyfactors"), yend = 8.1), 
		arrow = arrow(length = unit(0.04, "npc") ),
		ncp = 1000,
		curvature = -0.5, size = 0.5, angle = 100,
		color = "black"
	)+
	geom_label(aes(x = as.factor("_lakeonlyfactors"), y = 11.9, label = "reduction through\nsedimentation in lakes"), fill = "white",hjust = 0.6, color = "black", size = size_label)+
	geom_label(aes(x = as.factor("_withallfactors"), y = 9.8, label = "additional reduction through\nsedimentation in rivers"), hjust = 0.75, fill = "white", color = "black", size = size_label)



p21

l <- data.frame(value = rev(c(1,1,1,1,1,1,1)),
								river = "Legend\n",
								polymer = c("EPS", "PP", "LDPE", "HDPE", "PS", "PVC", "PET"),
								pos = c(0.5,1.5,2.5,3.5,4.5,5.5,6.5))
l$polymer <- factor(l$polymer,
										levels = c("EPS", "PP", "LDPE", "HDPE", "PS", "PVC", "PET"))

legende <- ggplot(data=l, aes(x=river, y=value, fill=polymer)) +
	geom_bar(stat="identity")+
	scale_y_continuous(expand = c(0,0.1,0, 0))+
	scale_x_discrete( expand = c(0.4,0.1))+
	geom_text(aes(y = pos, label = rev(polymer)), color = c( "black"),size = 2)+
	labs(y = "MiP\nin [kg / year]", subtitle = "Legend")+
	scale_fill_manual("",
										values = farben)+
	theme_void()+
	theme(plot.margin=unit(c(1.5,0,0.5,0),"cm"),
				legend.position = "none",
				axis.text.x = element_blank(),
				panel.border = element_blank(),
				panel.ontop = F,
				plot.subtitle = element_text(size = 7))

legende


p211 <- ggdraw(plot_grid(p21, legende, nrow = 1, rel_widths = c(1,0.1)))
ggsave(paste0(main.path, "output_files/plots/outflow2.png"), p211, width = 9, height = 6, units = "cm", dpi = 500, bg = "transparent")

ggsave(paste0(main.path, "output_files/plots/outflow2.pdf"), p211, width = 9, height = 6, units = "cm", dpi = 500, bg = "transparent")



# accumulation ########
farben <- c( 'dodgerblue','#555555',"#CC5700")
d = data.frame(scenario = c("lake", "river"),
							 value = c(df01_total$value[df01_total$compartment == "accumulation" & df01_total$scenario == "_lakeonlyfactors"], df01_total$value[df01_total$compartment == "accumulation" & df01_total$scenario == "_withallfactors"]))
d$value[2] <- d$value[2]-d$value[1]


p2 <- ggplot(data=d, aes(x=scenario, y=value, fill=scenario)) +
	geom_bar(stat="identity")+
	scale_y_continuous(expand = c(0,0,0,1),limits = c(0,I(max(df01_total$value[df01_total$compartment == "accumulation"]) +0.3)), breaks = c(0,2,4))+
	scale_x_discrete(labels = c("in lakes", "in rivers"), expand = c(0.2,0.1))+
	geom_text(aes(label= round(value,1)), vjust=-0.8, color="black",
						position = position_dodge(0.9), size=3.5)+
	labs(y = "total MiP accumulation\nin [tonnes / year]")+
	scale_fill_manual("",
										values = farben[2:3],
										labels = c("Slake", "Sall"),
										breaks = c("lake", "river"))+
	nice +
	theme(plot.margin=unit(c(0.1,0.2,0.1,0.6),"cm")) #trbl


	
p2
#text
t1 <- ggplot() +                     
	annotate("text",
					 x = 1,
					 y = 1,
					 label = "outflow CH /\nflow to unknown") + 
	theme_void()+
	theme(plot.margin = unit(c(0,0,0,0), "cm"),
				text = element_text(face = "plain", size = 11))

t2 <- ggplot() +                     
	annotate("text",
					 x = 1,
					 y = 1,
					 label = "       accumulation in CH") + 
	theme_void()+
	theme(plot.margin = unit(c(0,0,0,0.9), "cm"),
				text = element_text(face = "plain", size = 11))



#put all together

pall <- ggdraw(plot_grid(plot_grid(t1, t2, nrow = 1, rel_widths = c(1,1), labels = c("a)", "b)")), plot_grid(p1, p2, align = "h", nrow = 1, rel_widths = c(1,1)), nrow = 2, rel_heights = c(0.3,2)))

pall
ggsave(paste0(main.path, "output_files/plots/outflow_acc.png"), pall, height = 8.5, width = 17, units = "cm", dpi = 500, bg = "transparent")

ggsave(paste0(main.path, "output_files/plots/outflow.png"), p1, width = 8, height = 8, units = "cm", dpi = 500, bg = "transparent")

ggsave(paste0(main.path, "output_files/plots/outflow.pdf"), p1, width = 8, height = 8, units = "cm", dpi = 500, bg = "transparent")

rm(list = ls())
