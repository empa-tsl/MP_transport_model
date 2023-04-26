####
# plotting pie chart with 15 biggest lakes
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



#load data

polymers <- c("LDPE", "HDPE", "PP", "PS", "EPS", "PVC", "PET")

main.path <- "PhD/mennekes/"
without <- "_withoutfactors"
withf <- "_withallfactors"
lakesonly <- "_lakeonlyfactors"
lake15 <- "_lake15"
rounds <- "801"	

load(paste0(main.path, "output_files/rdata/cont_HDPE_", rounds, without, ".Rdata"))
rm(s_r)
scenarios <- c( without, lakesonly, lake15, withf)

###df 02
df02 <- data.frame(river = rep(NA, 3*length(polymers)),
									 none = NA,
									 lakes = NA,
									 lakes15 = NA,
									 all = NA,
									 polymer = NA)
counter <- 1
for (mat in polymers) {
	c2 <- 2
	for (j in scenarios) {
		load(paste0(main.path, "output_files/rdata/cont_", mat,"_", rounds, j, ".Rdata"))
		df02[counter:I(counter +2), "river"] <- c("rhine", "rhone", "Le doubs") 
		df02[counter:I(counter +2), "polymer"] <- mat
		df02[counter:I(counter +2), c2] <- s_r[c(441464, 441470, 441468), paste0("total_tot_", mat, "_MiP")]
		c2 <- c2 +1
	}
	counter <- counter +3
}
df02
df02$rest <- df02$all
df02$Lake15 <- df02$none- df02$lakes15
df02$Lakeother <- df02$lakes15 - df02$lakes
df02$River <- df02$lakes-df02$all
df02 <- as_tibble(df02)

#colors
blue <- c('#eff3ff','#bdd7e7','#6baed6','#2171b5')
black <- c('#f7f7f7','#cccccc','#969696','#525252')
red <- c('#fee5d9','#fcae91','#fb6a4a','#cb181d')
nice <- theme(legend.position = "none",
							legend.background = element_rect(fill = "transparent", color = NA),
							plot.background = element_rect(fill = "transparent",colour = NA),
							text = element_text(color = "black"),
							plot.margin=unit(c(0.0, -0.7,0.1,-0.6),"cm")) #trbl


###########################################################################
#kreis Diagramme
#EPS
#rhine
ddd <- df02 %>% filter(polymer == "EPS"&river == "rhine") %>% dplyr::select(rest, River, Lakeother, Lake15)  #!
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
										labels = c("remaining", "rivers", "lakes", "lake15"),
										breaks = c("rest", "River", "Lakeother", "Lake15"))+
	geom_label_repel(data = df2,
									 aes(y = pos, label = paste0(percent, " %")),
									 size = 4.5, nudge_x = 1, show.legend = FALSE,
									 segment.colour = NA) +
	guides(fill = guide_legend(title = element_blank())) +
	theme_void()+
	nice
p11
#rhone
ddd <- df02 %>% filter(polymer == "EPS"&river == "rhone") %>% dplyr::select(rest, River, Lakeother, Lake15) 
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
										labels = c("remaining", "rivers", "lakes", "lake15"),
										breaks = c("rest", "River", "Lakeother", "Lake15"))+
	geom_label_repel(data = df2,
									 aes(y = pos, label = paste0(percent, " %")),
									 size = 4.5, nudge_x = 1, show.legend = FALSE, segment.colour = NA) +
	guides(fill = guide_legend(title = element_blank())) +
	theme_void()+
	nice

# le doubs
ddd <- df02 %>% filter(polymer == "EPS"&river == "Le doubs") %>% dplyr::select(rest, River, Lakeother, Lake15)
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
										labels = c(" ", " ", " ", " "),
										breaks = c("rest", "River", "Lakeother", "Lake15"))+
	geom_label_repel(data = df2,
									 aes(y = pos, label = paste0(percent, " %")),
									 size = 4.5, nudge_x = 1, show.legend = FALSE, segment.colour = NA) +
	guides(fill = guide_legend(title = element_blank())) +
	theme_void()+
	nice+
	theme(legend.position = "right")

legend1 <- get_legend(p13)
p13 <- p13+theme(legend.position = "none")


#PS#######
#rhine
ddd <- df02 %>% filter(polymer == "PS"&river == "rhine") %>% dplyr::select(rest, River, Lakeother, Lake15) 
ddd2 <- data.frame(value = as.numeric(ddd),
									 gr = names(ddd))
ddd2$percent <- round(ddd2$value/sum(ddd2$value)*100,1)
ddd2$percent[1] <- 100 - sum(ddd2$percent[2:3])
df2 <- ddd2 %>% 
	mutate(csum = rev(cumsum(rev(value))), 
				 pos = value/2 + lead(csum, 1),
				 pos = if_else(is.na(pos), value/2, pos))

p21 <- ggplot(ddd2, aes(x = "" , y = value, fill = fct_inorder(gr))) +
	geom_col() +
	coord_polar(theta = "y") +
	scale_fill_manual("",
										values = black,
										labels = c("remaining", "rivers", "lakes", "lake15"),
										breaks = c("rest", "River", "Lakeother", "Lake15"))+
	geom_label_repel(data = df2,
									 aes(y = pos, label = paste0(percent, " %")),
									 size = 4.5, nudge_x = 1, show.legend = FALSE, segment.colour = NA) +
	guides(fill = guide_legend(title = element_blank())) +
	theme_void()+
	nice

#rhone
ddd <- df02 %>% filter(polymer == "PS"&river == "rhone") %>% dplyr::select(rest, River, Lakeother, Lake15)
ddd2 <- data.frame(value = as.numeric(ddd),
									 gr = names(ddd))
ddd2$percent <- round(ddd2$value/sum(ddd2$value)*100,1)
ddd2$percent[1] <- 100 - sum(ddd2$percent[2:3])
df2 <- ddd2 %>% 
	mutate(csum = rev(cumsum(rev(value))), 
				 pos = value/2 + lead(csum, 1),
				 pos = if_else(is.na(pos), value/2, pos))

p22 <- ggplot(ddd2, aes(x = "" , y = value, fill = fct_inorder(gr))) +
	geom_col() +
	coord_polar(theta = "y") +
	scale_fill_manual("",
										values = black,
										labels = c("remaining", "rivers", "lakes", "lake15"),
										breaks = c("rest", "River", "Lakeother", "Lake15"))+
	geom_label_repel(data = df2,
									 aes(y = pos, label = paste0(percent, " %")),
									 size = 4.5, nudge_x = 1, show.legend = FALSE, segment.colour = NA) +
	guides(fill = guide_legend(title = element_blank())) +
	theme_void()+
	nice

# le doubs
ddd <- df02 %>% filter(polymer == "PS"&river == "Le doubs") %>% dplyr::select(rest, River, Lakeother, Lake15)
ddd2 <- data.frame(value = as.numeric(ddd),
									 gr = names(ddd))
ddd2$percent <- round(ddd2$value/sum(ddd2$value)*100,1)
ddd2$percent[1] <- 100 - sum(ddd2$percent[2:3])
df2 <- ddd2 %>% 
	mutate(csum = rev(cumsum(rev(value))), 
				 pos = value/2 + lead(csum, 1),
				 pos = if_else(is.na(pos), value/2, pos))

p23 <- ggplot(ddd2, aes(x = "" , y = value, fill = fct_inorder(gr))) +
	geom_col() +
	coord_polar(theta = "y") +
	scale_fill_manual("",
										values = black,
										labels = c(" ", " ", " ", " "),
										breaks = c("rest", "River", "Lakeother", "Lake15"))+
	geom_label_repel(data = df2,
									 aes(y = pos, label = paste0(percent, " %")),
									 size = 4.5, nudge_x = 1, show.legend = FALSE, segment.colour = NA) +
	guides(fill = guide_legend(title = element_blank())) +
	theme_void()+
	nice+
	theme(legend.position = "right")

legend2 <- get_legend(p23)
p23 <- p23+theme(legend.position = "none")



#PET#######
#rhine
ddd <- df02 %>% filter(polymer == "PET"&river == "rhine") %>% dplyr::select(rest, River, Lakeother, Lake15) 
ddd2 <- data.frame(value = as.numeric(ddd),
									 gr = names(ddd))
ddd2$percent <- round(ddd2$value/sum(ddd2$value)*100,1)
ddd2$percent[1] <- 100 - sum(ddd2$percent[2:3])
df2 <- ddd2 %>% 
	mutate(csum = rev(cumsum(rev(value))), 
				 pos = value/2 + lead(csum, 1),
				 pos = if_else(is.na(pos), value/2, pos))

p31 <- ggplot(ddd2, aes(x = "" , y = value, fill = fct_inorder(gr))) +
	geom_col() +
	coord_polar(theta = "y") +
	scale_fill_manual("",
										values = red,
										labels = c("remaining", "rivers", "lakes", "lake15"),
										breaks = c("rest", "River", "Lakeother", "Lake15"))+
	geom_label_repel(data = df2,
									 aes(y = pos, label = paste0(percent, " %")),
									 size = 4.5, nudge_x = 1, show.legend = FALSE, segment.colour = NA) +
	guides(fill = guide_legend(title = element_blank())) +
	theme_void()+
	nice


#rhone
ddd <- df02 %>% filter(polymer == "PET"&river == "rhone") %>% dplyr::select(rest, River, Lakeother, Lake15) 
ddd2 <- data.frame(value = as.numeric(ddd),
									 gr = names(ddd))
ddd2$percent <- round(ddd2$value/sum(ddd2$value)*100,1)
ddd2$percent[1] <- 100 - sum(ddd2$percent[2:3])
df2 <- ddd2 %>% 
	mutate(csum = rev(cumsum(rev(value))), 
				 pos = value/2 + lead(csum, 1),
				 pos = if_else(is.na(pos), value/2, pos))

p32 <- ggplot(ddd2, aes(x = "" , y = value, fill = fct_inorder(gr))) +
	geom_col() +
	coord_polar(theta = "y") +
	scale_fill_manual("",
										values = red,
										labels = c("remaining", "rivers", "lakes", "lake15"),
										breaks = c("rest", "River", "Lakeother", "Lake15"))+
	geom_label_repel(data = df2,
									 aes(y = pos, label = paste0(percent, " %")),
									 size = 4.5, nudge_x = 1, show.legend = FALSE, segment.colour = NA) +
	guides(fill = guide_legend(title = element_blank())) +
	theme_void()+
	nice

# le doubs
ddd <- df02 %>% filter(polymer == "PET"&river == "Le doubs") %>% dplyr::select(rest, River, Lakeother, Lake15)
ddd2 <- data.frame(value = as.numeric(ddd),
									 gr = names(ddd))
ddd2$percent <- round(ddd2$value/sum(ddd2$value)*100,1)
ddd2$percent[1] <- 100 - sum(ddd2$percent[2:3])
df2 <- ddd2 %>% 
	mutate(csum = rev(cumsum(rev(value))), 
				 pos = value/2 + lead(csum, 1),
				 pos = if_else(is.na(pos), value/2, pos))

p33 <- ggplot(ddd2, aes(x = "" , y = value, fill = fct_inorder(gr))) +
	geom_col() +
	coord_polar(theta = "y") +
	scale_fill_manual("",
										values = red,
										labels = c("   outflow in water", "   retained in rivers", "   retained in all other lakes", "   retained in 15 biggest lakes"),
										breaks = c("rest", "River", "Lakeother", "Lake15"))+
	geom_label_repel(data = df2,
									 aes(y = pos, label = paste0(percent, " %")),
									 size = 4.5, nudge_x = 1, show.legend = FALSE, segment.colour = NA) +
	guides(fill = guide_legend(title = element_blank())) +
	theme_void()+
	nice+
	theme(legend.position = "right")

legend3 <- get_legend(p33)
p33 <- p33+theme(legend.position = "none")

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

pall <- ggdraw(plot_grid(plot_grid(plot_grid(t0, ta, tb, tc, rel_heights = c(0.1,1,1,1), ncol = 1), plot_grid(t1, t2,t3,p11,p12,p13,p21,p22,p23,p31,p32,p33, rel_heights = c(0.2,1,1,1), ncol = 3), ncol = 2, rel_widths = c(0.08, 1)), plot_grid(t0, legend1, legend2, legend3, t0, nrow = 1, rel_widths = c(1.5,0.2,0.2,0.7,1.5)), nrow = 2, rel_heights = c(1,0.2)))
Sys.sleep(5)

pall
ggsave(paste0(main.path, "output_files/plots/retain15.png"), pall, height = 19, width = 20, units = "cm", dpi = 500)


# rm(list = ls())
