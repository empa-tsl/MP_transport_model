#derive factors for losses

# according to Bessling 2017 numbers are given in the SI by Mennekes and Nowack 2022
main.path <- "PhD/mennekes/"
#

library(ggplot2)
library(dplyr)
library(tidyverse)
library(cowplot)
library(sf)
library(reshape2)


#make data frame as simple river

d <- data.frame(L_km = rep(1,40), #in km
								flow_velocity = 0.2, #in m/s
								PET = 0,
								PVC = 0,
								PS = 0,
								HDPE = 0,
								LDPE = 0,
								PP = 0,
								EPS = 0)

d <- data.frame(L_km = c(1,3,7,1,8,10,5,0.5,4.5, 0), #in km
								flow_velocity = 0.2, #in m/s
								PET = 0,
								PVC = 0,
								PS = 0,
								HDPE = 0,
								LDPE = 0,
								PP = 0,
								EPS = 0)


#factors according to Bessling 2017
# resXX: XX is the target value in % according to Bessling for the different polymers. resXX shows how well we match the value after 40km (40 km in flow velocity 0.2 m/s = 40000m / 0.2m/s)

#EPS
res00 <- (1- exp(1)^(-0*(40000/0.2)))
fac00 <- function(x){(1- exp(1)^(-0*x))}
fac_EPS <- fac00(1)

#PP
res01 <- (1- exp(1)^(-0.0000000505*(40000/0.2)))
fac01 <- function(x){(1- exp(1)^(-0.0000000505*x))}
fac_PP <- fac01(1)


#LDPE and HDPE
res05 <- (1- exp(1)^(-0.0000002565*(40000/0.2)))
fac05 <- function(x){(1- exp(1)^(-0.0000002565*x))}
fac_HDPE <- fac05(1)
fac_LDPE <- fac05(1)

#PS
res20 <- (1- exp(1)^(-0.0000011158*(40000/0.2)))
fac20 <- function(x){(1- exp(1)^(-0.0000011158*x))}
fac_PS <- fac20(1)

#PVC
res95 <- (1- exp(1)^(-0.000015*(40000/0.2)))
fac95 <- function(x){(1- exp(1)^(-0.000015*x))}
fac_PVC <- fac95(1)

#PET
res99 <- (1- exp(1)^(-0.000025*(40000/0.2)))
fac99 <- function(x){(1- exp(1)^(-0.000025*x))}
fac_PET <- fac99(1)

#list with all functions
fun_list <- list(fac00, fac01, fac05, fac05, fac20, fac95, fac99)
polymers <- c("EPS", "PP", "LDPE", "HDPE", "PS", "PVC", "PET")


#demonstrate the values
#loop per polymer
for (i in 1:length(polymers)) {
	polymer <- polymers[i]
	d[1, polymer] <- 1
	d[ , paste0(polymer, "_actual_pol")] <- 0
	d$length_s = (d$L_km*1000)/d$flow_velocity
	

	
	d$factor <- 1*(1-fun_list[[i]](1))^d$length_s #einbauen in negative Zinsfunktion 1(1-factor(1. Zeitschrit))^lengths in s
	#for loop as calculation
	act <- paste0(polymer, "_actual_pol")
	for (i in 1:nrow(d)) {
		d[2:nrow(d), act] <- ((d[1:I(nrow(d)-1), act] + d[1:I(nrow(d)-1), polymer]) * d$factor[1:I(nrow(d)-1)])
	}
	d[1, act]<-1

}

d
d_p <- d[ , paste0(polymers, "_actual_pol")]
names(d_p) <- polymers
d_p$L <- c(0, cumsum(d$L_km[1:I(nrow(d)-1)]))

d_p2 <- melt(d_p, id.vars = "L")
names(d_p2) <- c("L", "polymer", "value")

nice <- theme_bw()+
	theme(legend.background = element_rect(fill = "transparent", color = NA),
				plot.background = element_rect(fill = "transparent",colour = NA),
				text = element_text(color = "black"),
				axis.text.x = element_text(color = "black", face = "plain", size = 10),
				axis.text.y = element_text(color = "black", face = "plain", size = 11),
				panel.background = element_rect(fill = "transparent")) #trbl


p1 <- ggplot(d_p2, aes(x = L, y = value, color = polymer, linetype = polymer))+
	geom_line()+
	geom_point()+
	nice+
	labs(x = "river lengths in km\nbased on Bessling et al. 2017",
			y = "polymer in suspension")

p1
ggsave(paste0(main.path, "output_files/plots/plot_factor_river.png"), plot = p1, width = 12, height = 8, units = "cm", bg = "transparent", dpi = 500)

fac_rivers <- c(fac_EPS, fac_PP, fac_LDPE, fac_HDPE, fac_PS, fac_PVC, fac_PET)
names(fac_rivers) <- polymers
save(fac_rivers, file = paste0(main.path, "temp_data/factor_rivers.Rdata"))

rm(list = ls())
