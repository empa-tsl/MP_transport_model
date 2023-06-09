#plot along ther rivers.

# first all rivers that belong to the system

library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggrepel)
library(patchwork) #for making all images the same dimensions.
library(cowplot)
library(sf)
library(reshape2)

#load dataset
main.path <- "PhD/mennekes/"
load(paste0(main.path, "temp_data/forPlot.Rdata"))
rounds <- "801"
polymers <- c("EPS", "PP", "LDPE", "HDPE", "PS", "PVC", "PET")

Bern <- 117294




###rhein
d <- rivers.all %>% dplyr::select(id_all, flow_to) %>% st_drop_geometry()

d$x <- 0
d$x[Bern] <- 1
ft <- d$flow_to[which(d$x == 1)]
ft_long <- Bern
for (i in 1:800) {
	d$x[ft] <- 1
	ft_long <- c(ft_long, ft)
	ft <- d$flow_to[ft]
}
sum(d$x)

ft_Bern <- ft_long[1:sum(d$x)]
	


len <- as.numeric(st_length(rivers.all))



#figure##############################


farben <- c('#d73027','#fc8d59','#fee090','#ffffbf','#e0f3f8','#91bfdb','#4575b4')
#ggtheme
nice <- theme_classic()+
	theme(legend.position = "none",
				legend.background = element_rect(fill = "transparent", color = NA),
				plot.background = element_rect(fill = "transparent",colour = NA),
				text = element_text(color = "black"),
				axis.text.x = element_text(color = "black", face = "plain", size = 10),
				axis.text.y = element_text(color = "black", face = "plain", size = 11),
				panel.background = element_rect(fill = "transparent")) #trbl

# rhine
s <- ft_Bern

df01 <- as.data.frame(matrix(NA, nrow = length(s)*length(polymers), ncol = 5)) #*2 for scenarios
names(df01) <- c("id",  "isLake", "value_water", "value_acc", "polymer")

df00_water <- as.data.frame(matrix(NA, nrow = length(s), ncol = length(polymers)))
names(df00_water) <- rev(polymers)

df00_acc <- as.data.frame(matrix(NA, nrow = length(s), ncol = length(polymers)))
names(df00_acc) <- rev(polymers)

#load data
counter <- 1
for (mat in rev(polymers)) {
		load(paste0(main.path, "temp_data/all_burial/cont_", mat,"_", rounds, "_withallfactors", ".Rdata"))
		df00_water[ , counter] <- s_r[s, paste0("water_perS_",mat,"_MiP")] # water values in g/s
		df00_acc[ , counter] <- s_r[s, paste0("accumulation_tot_",mat,"_MiP")] #acc values in g/s
		counter <- counter +1
}
#stack the values for printing
# for (i in 2:length(polymers)) {
# 	df00_water[ , i] <- df00_water[ , I(i-1)] +  df00_water[ ,i]
# 	df00_acc[ , i] <- df00_acc[ , I(i-1)] +  df00_acc[ ,i]
# }

df00_water <- melt(df00_water)
df00_acc <- melt(df00_acc)
df00_acc
df01$isLake <- s_r[s, "isLake"]
df01$id <- s
Langen <- len[s]
# df01$value_water <- rowSums(df00_water)
# df01$value_acc <- rowSums(df00_acc)
df01$value_water <- df00_water$value
df01$value_acc <- df00_acc$value
df01$polymer <- df00_acc$variable
df01$polymer <- factor(df01$polymer, levels = c(polymers))
df01$km <- cumsum(Langen/1000)
rm(s_r)
df01

#start_end lakes
start_end <- data.frame(start = rep(NA, length(s)),
												end = NA)
counter <- 1
y <- F
#if first segment is lake dann muss änderung auch auf T gesetzt werden
if(df01$isLake[1]){
	y <- T
}

for (i in 1:I(length(s)-1)) {
	x <- df01$isLake[i]# check lake für  Runde
	if(x == T & y == T){ #x ist für lake, y überprüft, ob es eine änderung gab
		start_end$start[counter] <- df01$km[i]-0.1 #minus 100m
	}
	if(x == F & y == T){ #x = F bedeuted es ist kein lake, d. h. es ist fluss. Wenn dann noch von see auf fluss geändert hat, dann ist der See vorbei
		start_end$end[counter] <- df01$km[i] - 0.1
		counter <- counter +1
	}
	y = df01$isLake[i] != df01$isLake[i+1]
}

start_end <- start_end[!(is.na(start_end$end)), ]

pR <- ggplot(data = df01)+
	geom_line(aes(x = km, y = value_water, color = polymer))
pR
	geom_rect(data = start_end,
		aes(xmin = start, xmax = end),
		ymin = -Inf, ymax = Inf, alpha = 0.2, fill ="dodgerblue", color = "dodgerblue"
	)+
	geom_area(aes(km, value_water*1000, fill = polymer), position = "identity")+
	geom_line(data = df01[df01$polymer == "EPS", ], aes(km, value_water*1000, color = "total"), size = 1)+
	scale_y_continuous(expand = c(0,0))+
	scale_x_continuous(expand = c(0,0))+
	scale_fill_manual(values = farben)+
	labs(x = "distance [km]",
			 y = "microplastic mass\n[mg/s]")+
	annotate("text",y = 0.93*max(df01$value_water, na.rm = T)*1000, x = 0.05*max(df01$km, na.rm = T), label = "Rhine", hjust = "left", vjust = "left",  size = 5, color = "black")+
	annotate("text",y = 60, x = 305, label = "inflow\nAare", hjust = "right", vjust = "left",  size = 4, color = "black")+
	annotate("text",y = 0.7*max(df01$value_water, na.rm = T)*1000, x = 205, label = "Lake\nConstance", vjust = "left",  size = 4, color = "black")+
	nice


# aare
s <- ft_aare

df01 <- as.data.frame(matrix(NA, nrow = length(s)*length(polymers), ncol = 5)) #*2 for scenarios
names(df01) <- c("id",  "isLake", "value_water", "value_acc", "polymer")

df00_water <- as.data.frame(matrix(NA, nrow = length(s), ncol = length(polymers)))
names(df00_water) <- rev(polymers)

df00_acc <- as.data.frame(matrix(NA, nrow = length(s), ncol = length(polymers)))
names(df00_acc) <- rev(polymers)

#load data
counter <- 1
for (mat in rev(polymers)) {
	load(paste0(main.path, "output_files/rdata/cont_", mat,"_", rounds, "_withallfactors", ".Rdata"))
	df00_water[ , counter] <- s_r[s, paste0("water_perS_",mat,"_MiP")] # water values in g/s
	df00_acc[ , counter] <- s_r[s, paste0("accumulation_tot_",mat,"_MiP")] #acc values in g/s
	counter <- counter +1
}
#stack the values for printing
for (i in 2:length(polymers)) {
	df00_water[ , i] <- df00_water[ , I(i-1)] +  df00_water[ ,i]
	df00_acc[ , i] <- df00_acc[ , I(i-1)] +  df00_acc[ ,i]
}

df00_water <- melt(df00_water)
df00_acc <- melt(df00_acc)
df00_acc
df01$isLake <- s_r[s, "isLake"]
df01$id <- s
Langen <- len[s]
# df01$value_water <- rowSums(df00_water)
# df01$value_acc <- rowSums(df00_acc)
df01$value_water <- df00_water$value
df01$value_acc <- df00_acc$value
df01$polymer <- df00_acc$variable
df01$polymer <- factor(df01$polymer, levels = c(polymers))
df01$km <- cumsum(Langen/1000)
rm(s_r)
df01

df01$value_water[df01$id %in% rhein_aare] <- NA
df01$value_acc[df01$id %in% rhein_aare] <- NA


#start_end lakes
start_end <- data.frame(start = rep(NA, length(s)),
												end = NA)
counter <- 1
y <- F
#if first segment is lake dann muss änderung auch auf T gesetzt werden
if(df01$isLake[1]){
	y <- T
}

for (i in 1:I(length(s)-1)) {
	x <- df01$isLake[i]# check lake für  Runde
	if(x == T & y == T){ #x ist für lake, y überprüft, ob es eine änderung gab
		start_end$start[counter] <- df01$km[i]-0.1 #minus 100m
	}
	if(x == F & y == T){ #x = F bedeuted es ist kein lake, d. h. es ist fluss. Wenn dann noch von see auf fluss geändert hat, dann ist der See vorbei
		start_end$end[counter] <- df01$km[i] - 0.1
		counter <- counter +1
	}
	y = df01$isLake[i] != df01$isLake[i+1]
}

start_end <- start_end[!(is.na(start_end$end)), ]

#find some km

a1 <- df01$km[df01$id== 117235][1]

pA <- ggplot(data = df01)+
	geom_rect(data = start_end,
						aes(xmin = start, xmax = end),
						ymin = -Inf, ymax = Inf, alpha = 0.2, fill ="dodgerblue", color = "dodgerblue"
	)+
	geom_area(aes(km, value_water*1000, fill = polymer), position = "identity")+
	geom_line(data = df01[df01$polymer == "EPS", ], aes(km, value_water*1000, color = "total"), size = 1)+
	scale_y_continuous(expand = c(0,0))+
	scale_x_continuous(expand = c(0,0))+
	scale_fill_manual(values = farben)+
	labs(x = "distance [km]",
			 y = "microplastic mass\n[mg/s]")+
	annotate("text",y = 0.93*max(df01$value_water, na.rm = T)*1000, x = 0.05*max(df01$km, na.rm = T), label = "Aare", hjust = "left", vjust = "left",  size = 5, color = "black")+
	annotate("text",y = 85, x = a1-5, label = "inflow\nReuss and Limmat", hjust = "right", vjust = "right",  size = 4, color = "black")+
	annotate("text",y = 55, x = 320, label = "flows into the Rhine",angle = 90,  size = 4, color = "black")+
	nice

pA


# rhone
s <- ft_rhone

df01 <- as.data.frame(matrix(NA, nrow = length(s)*length(polymers), ncol = 5)) #*2 for scenarios
names(df01) <- c("id",  "isLake", "value_water", "value_acc", "polymer")

df00_water <- as.data.frame(matrix(NA, nrow = length(s), ncol = length(polymers)))
names(df00_water) <- rev(polymers)

df00_acc <- as.data.frame(matrix(NA, nrow = length(s), ncol = length(polymers)))
names(df00_acc) <- rev(polymers)

#load data
counter <- 1
for (mat in rev(polymers)) {
	load(paste0(main.path, "output_files/rdata/cont_", mat,"_", rounds, "_withallfactors", ".Rdata"))
	df00_water[ , counter] <- s_r[s, paste0("water_perS_",mat,"_MiP")] # water values in g/s
	df00_acc[ , counter] <- s_r[s, paste0("accumulation_tot_",mat,"_MiP")] #acc values in g/s
	counter <- counter +1
}
#stack the values for printing
for (i in 2:length(polymers)) {
	df00_water[ , i] <- df00_water[ , I(i-1)] +  df00_water[ ,i]
	df00_acc[ , i] <- df00_acc[ , I(i-1)] +  df00_acc[ ,i]
}

df00_water <- melt(df00_water)
df00_acc <- melt(df00_acc)
df00_acc
df01$isLake <- s_r[s, "isLake"]
df01$id <- s
Langen <- len[s]
# df01$value_water <- rowSums(df00_water)
# df01$value_acc <- rowSums(df00_acc)
df01$value_water <- df00_water$value
df01$value_acc <- df00_acc$value
df01$polymer <- df00_acc$variable
df01$polymer <- factor(df01$polymer, levels = c(polymers))
df01$km <- cumsum(Langen/1000)
rm(s_r)
df01

#start_end lakes
start_end <- data.frame(start = rep(NA, length(s)),
												end = NA)
counter <- 1
y <- F
#if first segment is lake dann muss änderung auch auf T gesetzt werden
if(df01$isLake[1]){
	y <- T
}

for (i in 1:I(length(s)-1)) {
	x <- df01$isLake[i]# check lake für  Runde
	if(x == T & y == T){ #x ist für lake, y überprüft, ob es eine änderung gab
		start_end$start[counter] <- df01$km[i]-0.1 #minus 100m
	}
	if(x == F & y == T){ #x = F bedeuted es ist kein lake, d. h. es ist fluss. Wenn dann noch von see auf fluss geändert hat, dann ist der See vorbei
		start_end$end[counter] <- df01$km[i] - 0.1
		counter <- counter +1
	}
	y = df01$isLake[i] != df01$isLake[i+1]
}

start_end <- start_end[!(is.na(start_end$end)), ]

pRhone <- ggplot(data = df01)+
	geom_rect(data = start_end,
						aes(xmin = start, xmax = end),
						ymin = -Inf, ymax = Inf, alpha = 0.2, fill ="dodgerblue", color = "dodgerblue"
	)+
	geom_area(aes(km, value_water*1000, fill = polymer), position = "identity")+
	geom_line(data = df01[df01$polymer == "EPS", ], aes(km, value_water*1000, color = "total"), size = 1)+
	scale_y_continuous(expand = c(0,0))+
	scale_x_continuous(expand = c(0,0))+
	scale_fill_manual(values = farben)+
	labs(x = "distance [km]",
			 y = "microplastic mass\n[mg/s]")+
	annotate("text",y = 0.93*max(df01$value_water, na.rm = T)*1000, x = 0.05*max(df01$km, na.rm = T), label = "Rhône", hjust = "left", vjust = "left",  size = 5, color = "black")+
	# annotate("text",y = 60, x = 305, label = "inflow\nAare", hjust = "right", vjust = "left",  size = 4, color = "black")+
	annotate("text",y = 0.7*max(df01$value_water, na.rm = T)*1000, x = 205, label = "Lake\nGeneva", size = 4, color = "black")+
	nice

pRhone



# Doubs
s <- ft_doubs

df01 <- as.data.frame(matrix(NA, nrow = length(s)*length(polymers), ncol = 5)) #*2 for scenarios
names(df01) <- c("id",  "isLake", "value_water", "value_acc", "polymer")

df00_water <- as.data.frame(matrix(NA, nrow = length(s), ncol = length(polymers)))
names(df00_water) <- rev(polymers)

df00_acc <- as.data.frame(matrix(NA, nrow = length(s), ncol = length(polymers)))
names(df00_acc) <- rev(polymers)

#load data
counter <- 1
for (mat in rev(polymers)) {
	load(paste0(main.path, "output_files/rdata/cont_", mat,"_", rounds, "_withallfactors", ".Rdata"))
	df00_water[ , counter] <- s_r[s, paste0("water_perS_",mat,"_MiP")] # water values in g/s
	df00_acc[ , counter] <- s_r[s, paste0("accumulation_tot_",mat,"_MiP")] #acc values in g/s
	counter <- counter +1
}
#stack the values for printing
for (i in 2:length(polymers)) {
	df00_water[ , i] <- df00_water[ , I(i-1)] +  df00_water[ ,i]
	df00_acc[ , i] <- df00_acc[ , I(i-1)] +  df00_acc[ ,i]
}

df00_water <- melt(df00_water)
df00_acc <- melt(df00_acc)
df00_acc
df01$isLake <- s_r[s, "isLake"]
df01$id <- s
Langen <- len[s]
# df01$value_water <- rowSums(df00_water)
# df01$value_acc <- rowSums(df00_acc)
df01$value_water <- df00_water$value
df01$value_acc <- df00_acc$value
df01$polymer <- df00_acc$variable
df01$polymer <- factor(df01$polymer, levels = c(polymers))
df01$km <- cumsum(Langen/1000)
rm(s_r)
df01

#start_end lakes
start_end <- data.frame(start = rep(NA, length(s)),
												end = NA)
counter <- 1
y <- F
#if first segment is lake dann muss änderung auch auf T gesetzt werden
if(df01$isLake[1]){
	y <- T
}

for (i in 1:I(length(s)-1)) {
	x <- df01$isLake[i]# check lake für  Runde
	if(x == T & y == T){ #x ist für lake, y überprüft, ob es eine änderung gab
		start_end$start[counter] <- df01$km[i]-0.1 #minus 100m
	}
	if(x == F & y == T){ #x = F bedeuted es ist kein lake, d. h. es ist fluss. Wenn dann noch von see auf fluss geändert hat, dann ist der See vorbei
		start_end$end[counter] <- df01$km[i] - 0.1
		counter <- counter +1
	}
	y = df01$isLake[i] != df01$isLake[i+1]
}

start_end <- start_end[!(is.na(start_end$end)), ]

pD <- ggplot(data = df01)+
	geom_rect(data = start_end,
						aes(xmin = start, xmax = end),
						ymin = -Inf, ymax = Inf, alpha = 0.2, fill ="dodgerblue", color = "dodgerblue"
	)+
	geom_area(aes(km, value_water*1000, fill = polymer), position = "identity")+
	geom_line(data = df01[df01$polymer == "EPS", ], aes(km, value_water*1000, color = "total"), size = 1)+
	scale_y_continuous(expand = c(0,0))+
	scale_x_continuous(expand = c(0,0))+
	scale_fill_manual(values = farben)+
	labs(x = "distance [km]",
			 y = "microplastic mass\n[mg/s]")+
	annotate("text",y = 0.93*max(df01$value_water, na.rm = T)*1000, x = 0.05*max(df01$km, na.rm = T), label = "Doubs", hjust = "left", vjust = "left",  size = 5, color = "black")+
	# annotate("text",y = 60, x = 305, label = "inflow\nAare", hjust = "right", vjust = "left",  size = 4, color = "black")+
	# annotate("text",y = 0.7*max(df01$value_water, na.rm = T)*1000, x = 205, label = "Lake\nGeneva", size = 4, color = "black")+
	nice

pD


### legend colors
#legend
l <- data.frame(value = rev(c(8,7,3,8,3,4,3)),
								river = "Legend\n",
								polymer = polymers,
								pos = c(4,11.5,16.5,22,27.5,31,34.5))
l$polymer <- factor(l$polymer,
										levels = c(polymers))

legende <- ggplot(data=l, aes(x=river, y=value, fill=polymer)) +
	geom_bar(stat="identity")+
	scale_y_continuous(expand = c(0,0.1,0, 7*0.1))+
	scale_x_discrete( expand = c(0.4,0.1))+
	geom_text(aes(y = pos, label = rev(polymer)), color = c("white", "black", "white", "black", "white", "black", "white"))+
	labs(y = "MiP\nin [kg / year]")+
	scale_fill_manual("",
										values = farben)+
	theme_void()+
	theme(plot.margin=unit(c(0.1,0,0.1,0.2),"cm"),
				legend.position = "none",
				axis.text.x = element_text(color = "black", size = 12))

legende

# empty spot
t0 <- ggplot()+
	theme_nothing() +
	theme(plot.background = element_rect(fill = "transparent",colour = NA))
t0


pall <- ggdraw(plot_grid(plot_grid(pD, pRhone, pA, pR, nrow = 4, align = "v"),plot_grid(t0, t0, t0, legende, ncol = 1, rel_heights = c(1,1,1,1)), ncol = 2, rel_widths = c(1,0.15)))


pall

ggsave("PhD/mennekes/output_files/plots/verlauf.png", plot = pall, width = 21, height = 26,units = "cm", dpi = 500, bg = "transparent")

pR
#wegen aare




rm(list = ls())
