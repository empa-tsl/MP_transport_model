#plot along ther rivers: distribution sediment / accumulation etc. 

# first all rivers that belong to the system

library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggrepel)
library(patchwork) #for making all images the same dimensions.
library(cowplot)
library(sf)
library(reshape2)
library(ggpattern)

#load dataset
main.path <- "PhD/mennekes/"
load(paste0(main.path, "temp_data/forPlot.Rdata"))
rounds <- "801"
polymers <- c("EPS", "PP", "LDPE", "HDPE", "PS", "PVC", "PET")

rhein <- 65062
doubs <- 329205
aare <- 79433
rhone <- 325768



###rhein
d <- rivers.all %>% dplyr::select(id_all, flow_to) %>% st_drop_geometry()

d$x <- 0
d$x[rhein] <- 1
ft <- d$flow_to[which(d$x == 1)]
ft_long <- rhein
for (i in 1:800) {
	d$x[ft] <- 1
	ft_long <- c(ft_long, ft)
	ft <- d$flow_to[ft]
}
sum(d$x)

ft_rhine <- ft_long[1:sum(d$x)]



# aare
d <- rivers.all %>% dplyr::select(id_all, flow_to) %>% st_drop_geometry()

d$x <- 0
d$x[aare] <- 1
ft <- d$flow_to[which(d$x == 1)]
ft_long <- aare
for (i in 1:800) {
	d$x[ft] <- 1
	ft_long <- c(ft_long, ft)
	ft <- d$flow_to[ft]
}
sum(d$x)

ft_aare <- ft_long[1:sum(d$x)]

rhein_aare <- ft_aare[ft_aare %in% ft_rhine] #aare fließt in den Rhein




# rhone
d <- rivers.all %>% dplyr::select(id_all, flow_to) %>% st_drop_geometry()

d$x <- 0
d$x[rhone] <- 1
ft <- d$flow_to[which(d$x == 1)]
ft_long <- rhone
for (i in 1:800) {
	d$x[ft] <- 1
	ft_long <- c(ft_long, ft)
	ft <- d$flow_to[ft]
}
sum(d$x)

ft_rhone <- ft_long[1:sum(d$x)]



# doubs
d <- rivers.all %>% dplyr::select(id_all, flow_to) %>% st_drop_geometry()

d$x <- 0
d$x[doubs] <- 1
ft <- d$flow_to[which(d$x == 1)]
ft_long <- doubs
for (i in 1:800) {
	d$x[ft] <- 1
	ft_long <- c(ft_long, ft)
	ft <- d$flow_to[ft]
}
sum(d$x)

ft_doubs <- ft_long[1:sum(d$x)]


len <- as.numeric(st_length(rivers.all))



#figure##############################



farben <- c('#555555', "#CC5700",'dodgerblue' )
#ggtheme
nice <- theme_classic()+
	theme(legend.position = "none",
				legend.background = element_rect(fill = "transparent", color = NA),
				plot.background = element_rect(fill = "transparent",colour = NA),
				text = element_text(color = "black", size = 7),
				legend.text = element_text(size = 7),
				axis.text.x = element_text(color = "black", face = "plain", size = 7),
				axis.text.y = element_text(color = "black", face = "plain", size = 7),
				panel.background = element_rect(fill = "transparent"),
				plot.subtitle = element_text(size = 7)) #trbl

s_lake = 2



# rhine
s <- ft_rhine

df01 <- as.data.frame(matrix(NA, nrow = length(s)*length(polymers), ncol = 6)) #
names(df01) <- c("id",  "isLake", "value_water","value_sed", "value_acc", "polymer")

df00_water <- as.data.frame(matrix(NA, nrow = length(s), ncol = length(polymers)))
names(df00_water) <- rev(polymers)

df00_null <- as.data.frame(matrix(NA, nrow = length(s), ncol = length(polymers)))
names(df00_null) <- rev(polymers)

df00_sed <- as_tibble(matrix(NA, nrow = length(s), ncol = length(polymers)))
names(df00_sed) <- rev(polymers)

#load data
for (mat in rev(polymers)) {
	load(paste0(main.path, "output_files/rdata/cont_", mat,"_", rounds, "_withallfactors", ".Rdata"))
	df00_water[ , mat] <- s_r[s, paste0("water_perS_",mat,"_MiP")] # water values in g/s
	df00_sed[ , mat] <- s_r[s, paste0("sediment_perS_",mat,"_MiP")] #sed values in g/s
	load(paste0(main.path, "output_files/rdata/cont_", mat,"_", rounds, "_withoutfactors", ".Rdata"))
	df00_null[ , mat] <- s_r[s, paste0("water_perS_",mat,"_MiP")] # water values in g/s
}

df00_acc <-df00_null-df00_sed-df00_water
df00_water <- melt(df00_water/df00_null)
df00_sed <- melt(df00_sed/df00_null)
df00_acc <- melt(df00_acc/df00_null)
df01$isLake <- s_r[s, "isLake"]
df01$id <- s
Langen <- len[s]
# df01$value_water <- rowSums(df00_water)
# df01$value_acc <- rowSums(df00_acc)
df01$value_water <- df00_water$value
df01$value_acc <- df00_acc$value
df01$value_sed <- df00_sed$value
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
df02 <- melt(df01, measure.vars = c("value_water", "value_sed", "value_acc"))
df02$variable <- factor(df02$variable, levels = c("value_acc" ,"value_sed",  "value_water"))


#plot
p1pet <- ggplot()+
	geom_area(data = df02 %>% filter(polymer == "PET"), aes(x = km, y = value, fill = variable), stat = "identity")+
	scale_y_continuous(expand = c(0,0), breaks = c(0,0.5,1), labels = c("0", "0.5", "1"))+
	scale_x_continuous(limits = c(0, max(df02$km)), expand = c(0,0))+
	scale_fill_manual("",values = farben,
										labels = c("accumulated (summed up, including tributaries)", "transport in sediments", "transport in suspension"))+
	nice+
	labs(x = "distance [km]",
			 y = "")+
	theme(legend.position = "right")+
	geom_rect(data = start_end,
						aes(xmin = start, xmax = end),
						ymin = -Inf, ymax = Inf, alpha = 0.3,   fill = "white"
	)+
	annotate("text", x = max(df02$km)*0.1, y = 0.2, size = 2, label = "PET")+
	theme(plot.margin = unit(c(0,0,0.1,0), units = "cm"))
legend <- get_legend(p1pet)
p1pet <- p1pet+theme(legend.position = "none")

p1ps <- ggplot()+
	geom_area(data = df02 %>% filter(polymer == "PS"), aes(x = km, y = value, fill = variable), stat = "identity")+
	scale_y_continuous(expand = c(0,0), breaks = c(0,0.5,1), labels = c("0", "0.5", "1"))+
	scale_x_continuous(limits = c(0, max(df02$km)), expand = c(0,0))+
	scale_fill_manual("",values = farben,
										labels = c("accumulated (summed up)", "transport in sediments", "transport in suspension"))+
	nice+
	labs(x = "distance [km]",
			 y = "distribution microplastic")+
	geom_rect(data = start_end,
						aes(xmin = start, xmax = end),
						ymin = -Inf, ymax = Inf, alpha = 0.3,   fill = "white"
	)+
	annotate("text", x = max(df02$km)*0.1, y = 0.2, size = 2, label = "PS")+
	theme(axis.title.x = element_blank(),
				axis.text.x = element_blank(),
				axis.ticks.x = element_blank(),
				plot.margin = unit(c(0,0,0.1,0), units = "cm"))

p1eps <- ggplot()+
	geom_area(data = df02 %>% filter(polymer == "EPS"), aes(x = km, y = value, fill = variable), stat = "identity")+
	scale_y_continuous(expand = c(0,0), breaks = c(0,0.5,1), labels = c("0", "0.5", "1"))+
	scale_x_continuous(limits = c(0, max(df02$km)), expand = c(0,0))+
	scale_fill_manual("",values = farben,
										labels = c("accumulated (summed up)", "transport in sediments", "in suspension"))+
	nice+
	labs(x = "distance [km]",
			 y = " ",
			 subtitle = "Rhine")+
	geom_rect(data = start_end,
						aes(xmin = start, xmax = end),
						ymin = -Inf, ymax = Inf, alpha = 0.3,   fill = "white"
	)+
	annotate("text", x = max(df02$km)*0.1, y = 0.2, size = 2, label = "EPS")+
	theme(axis.title.x = element_blank(),
				axis.text.x = element_blank(),
				axis.ticks.x = element_blank(),
				plot.margin = unit(c(0.1,0,0.1,0), units = "cm"))







# Rhone
s <- ft_rhone

df01 <- as.data.frame(matrix(NA, nrow = length(s)*length(polymers), ncol = 6)) #
names(df01) <- c("id",  "isLake", "value_water","value_sed", "value_acc", "polymer")

df00_water <- as.data.frame(matrix(NA, nrow = length(s), ncol = length(polymers)))
names(df00_water) <- rev(polymers)

df00_null <- as.data.frame(matrix(NA, nrow = length(s), ncol = length(polymers)))
names(df00_null) <- rev(polymers)

df00_sed <- as_tibble(matrix(NA, nrow = length(s), ncol = length(polymers)))
names(df00_sed) <- rev(polymers)

#load data
for (mat in rev(polymers)) {
	load(paste0(main.path, "output_files/rdata/cont_", mat,"_", rounds, "_withallfactors", ".Rdata"))
	df00_water[ , mat] <- s_r[s, paste0("water_perS_",mat,"_MiP")] # water values in g/s
	df00_sed[ , mat] <- s_r[s, paste0("sediment_perS_",mat,"_MiP")] #sed values in g/s
	load(paste0(main.path, "output_files/rdata/cont_", mat,"_", rounds, "_withoutfactors", ".Rdata"))
	df00_null[ , mat] <- s_r[s, paste0("water_perS_",mat,"_MiP")] # water values in g/s
}

df00_acc <-df00_null-df00_sed-df00_water
df00_water <- melt(df00_water/df00_null)
df00_sed <- melt(df00_sed/df00_null)
df00_acc <- melt(df00_acc/df00_null)
df01$isLake <- s_r[s, "isLake"]
df01$id <- s
Langen <- len[s]
# df01$value_water <- rowSums(df00_water)
# df01$value_acc <- rowSums(df00_acc)
df01$value_water <- df00_water$value
df01$value_acc <- df00_acc$value
df01$value_sed <- df00_sed$value
df01$polymer <- df00_acc$variable
df01$polymer <- factor(df01$polymer, levels = c(polymers))
df01$km <- cumsum(Langen/1000)
rm(s_r)

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
df02 <- melt(df01, measure.vars = c("value_water", "value_sed", "value_acc"))
df02$variable <- factor(df02$variable, levels = c("value_acc" ,"value_sed",  "value_water"))


#plot
p2pet <- ggplot()+
	geom_area(data = df02 %>% filter(polymer == "PET"), aes(x = km, y = value, fill = variable), stat = "identity")+
	scale_y_continuous(expand = c(0,0), breaks = c(0,0.5,1), labels = c("0", "0.5", "1"))+
	scale_x_continuous(limits = c(0, max(df02$km)), expand = c(0,0), breaks = c(0,50,150,250))+
	scale_fill_manual("",values = farben,
										labels = c("accumulated (summed up)", "transport in sediments", "in suspension"))+
	nice+
	labs(x = "distance [km]",
			 y = " ")+
	annotate("text", x = max(df02$km)*0.1, y = 0.2, size = 2, label = "PET")+
	theme(legend.position = "none")+
	geom_rect(data = start_end,
						aes(xmin = start, xmax = end),
						ymin = -Inf, ymax = Inf, alpha = 0.3,   fill = "white"
	)+
	theme(plot.margin = unit(c(0,0,0.1,0), units = "cm"))

p2ps <- ggplot()+
	geom_area(data = df02 %>% filter(polymer == "PS"), aes(x = km, y = value, fill = variable), stat = "identity")+
	scale_y_continuous(expand = c(0,0), breaks = c(0,0.5,1), labels = c("0", "0.5", "1"))+
	scale_x_continuous(limits = c(0, max(df02$km)), expand = c(0,0))+
	scale_fill_manual("",values = farben,
										labels = c("accumulated (summed up)", "transport in sediments", "in suspension"))+
	nice+
	labs(x = "distance [km]",
			 y = "distribution microplastic")+
	geom_rect(data = start_end,
						aes(xmin = start, xmax = end),
						ymin = -Inf, ymax = Inf, alpha = 0.3,   fill = "white"
	)+
	annotate("text", x = max(df02$km)*0.1, y = 0.2, size = 2, label = "PS")+
	theme(axis.title.x = element_blank(),
				axis.text.x = element_blank(),
				axis.ticks.x = element_blank(),
				plot.margin = unit(c(0,0,0.1,0), units = "cm"))

p2eps <- ggplot()+
	geom_area(data = df02 %>% filter(polymer == "EPS"), aes(x = km, y = value, fill = variable), stat = "identity")+
	scale_y_continuous(expand = c(0,0), breaks = c(0,0.5,1), labels = c("0", "0.5", "1"))+
	scale_x_continuous(limits = c(0, max(df02$km)), expand = c(0,0), breaks = c(0,50,150,250))+
	scale_fill_manual("",values = farben,
										labels = c("accumulated (summed up)", "transport in sediments", "in suspension"))+
	nice+
	labs(x = "distance [km]",
			 y = " ",
			 subtitle = "Rhône")+
	geom_rect(data = start_end,
						aes(xmin = start, xmax = end),
						ymin = -Inf, ymax = Inf, alpha = 0.3,   fill = "white"
	)+
	annotate("text", x = max(df02$km)*0.1, y = 0.2, size = 2, label = "EPS")+
	theme(axis.title.x = element_blank(),
				axis.text.x = element_blank(),
				axis.ticks.x = element_blank(),
				plot.margin = unit(c(0,0,0.1,0), units = "cm"))






# Doubs
s <- ft_doubs

df01 <- as.data.frame(matrix(NA, nrow = length(s)*length(polymers), ncol = 6)) #
names(df01) <- c("id",  "isLake", "value_water","value_sed", "value_acc", "polymer")

df00_water <- as.data.frame(matrix(NA, nrow = length(s), ncol = length(polymers)))
names(df00_water) <- rev(polymers)

df00_null <- as.data.frame(matrix(NA, nrow = length(s), ncol = length(polymers)))
names(df00_null) <- rev(polymers)

df00_sed <- as_tibble(matrix(NA, nrow = length(s), ncol = length(polymers)))
names(df00_sed) <- rev(polymers)

#load data
for (mat in rev(polymers)) {
	load(paste0(main.path, "output_files/rdata/cont_", mat,"_", rounds, "_withallfactors", ".Rdata"))
	df00_water[ , mat] <- s_r[s, paste0("water_perS_",mat,"_MiP")] # water values in g/s
	df00_sed[ , mat] <- s_r[s, paste0("sediment_perS_",mat,"_MiP")] #sed values in g/s
	load(paste0(main.path, "output_files/rdata/cont_", mat,"_", rounds, "_withoutfactors", ".Rdata"))
	df00_null[ , mat] <- s_r[s, paste0("water_perS_",mat,"_MiP")] # water values in g/s
}

df00_acc <-df00_null-df00_sed-df00_water
df00_water <- melt(df00_water/df00_null)
df00_sed <- melt(df00_sed/df00_null)
df00_acc <- melt(df00_acc/df00_null)
df01$isLake <- s_r[s, "isLake"]
df01$id <- s
Langen <- len[s]
# df01$value_water <- rowSums(df00_water)
# df01$value_acc <- rowSums(df00_acc)
df01$value_water <- df00_water$value
df01$value_acc <- df00_acc$value
df01$value_sed <- df00_sed$value
df01$polymer <- df00_acc$variable
df01$polymer <- factor(df01$polymer, levels = c(polymers))
df01$km <- cumsum(Langen/1000)
rm(s_r)

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
df02 <- melt(df01, measure.vars = c("value_water", "value_sed", "value_acc"))
df02$variable <- factor(df02$variable, levels = c("value_acc" ,"value_sed",  "value_water"))


#plot
p3pet <- ggplot()+
	geom_area(data = df02 %>% filter(polymer == "PET"), aes(x = km, y = value, fill = variable), stat = "identity")+
	scale_y_continuous(expand = c(0,0), breaks = c(0,0.5,1), labels = c("0", "0.5", "1"))+
	scale_x_continuous(limits = c(0, max(df02$km)), expand = c(0,0))+
	scale_fill_manual("",values = farben,
										labels = c("accumulated (summed up)", "transport in sediments", "in suspension"))+
	nice+
	labs(x = "distance [km]",
			 y = " ")+
	geom_rect(data = start_end,
						aes(xmin = start, xmax = end),
						ymin = -Inf, ymax = Inf, alpha = 0.3,   fill = "white"
	)+
	annotate("text", x = max(df02$km)*0.1, y = 0.2, size = 2, label = "PET")+
	theme(plot.margin = unit(c(0,0,0.1,0), units = "cm"))

p3ps <- ggplot()+
	geom_area(data = df02 %>% filter(polymer == "PS"), aes(x = km, y = value, fill = variable), stat = "identity")+
	scale_y_continuous(expand = c(0,0), breaks = c(0,0.5,1), labels = c("0", "0.5", "1"))+
	scale_x_continuous(limits = c(0, max(df02$km)), expand = c(0,0))+
	scale_fill_manual("",values = farben,
										labels = c("accumulated (summed up)", "transport in sediments", "in suspension"))+
	nice+
	labs(x = "distance [km]",
			 y = "distribution microplastic")+
	geom_rect(data = start_end,
						aes(xmin = start, xmax = end),
						ymin = -Inf, ymax = Inf, alpha = 0.3,   fill = "white"
	)+
	annotate("text", x = max(df02$km)*0.1, y = 0.2, size = 2, label = "PS")+
	theme(axis.title.x = element_blank(),
				axis.text.x = element_blank(),
				axis.ticks.x = element_blank(),
				plot.margin = unit(c(0,0,0.1,0), units = "cm"))

p3eps <- ggplot()+
	geom_area(data = df02 %>% filter(polymer == "EPS"), aes(x = km, y = value, fill = variable), stat = "identity")+
	scale_y_continuous(expand = c(0,0), breaks = c(0,0.5,1), labels = c("0", "0.5", "1"))+
	scale_x_continuous(limits = c(0, max(df02$km)), expand = c(0,0))+
	scale_fill_manual("",values = farben,
										labels = c("accumulated (summed up)", "transport in sediments", "in suspension"))+
	nice+
	labs(x = "distance [km]",
			 y = " ",
			 subtitle = "Doubs")+
	geom_rect(data = start_end,
						aes(xmin = start, xmax = end),
						ymin = -Inf, ymax = Inf, alpha = 0.3,   fill = "white"
	)+
	annotate("text", x = max(df02$km)*0.1, y = 0.2, size = 2,  label = "EPS")+
	theme(axis.title.x = element_blank(),
				axis.text.x = element_blank(),
				axis.ticks.x = element_blank(),
				plot.margin = unit(c(0,0,0.1,0), units = "cm"))



















# empty spot
t0 <- ggplot()+
	theme_nothing() +
	theme(plot.background = element_rect(fill = "transparent",colour = NA))


#################
#copy pase from 03.2_plots_retainted.R


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
df02[ , c("rest", "River", "River2", "River1", "Lake")] <- df02[ , c("rest", "River", "River2", "River1", "Lake")]*60*60*24*365
#colors
farben <- c('dodgerblue',  "#CC5700", '#555555','#aaaaaa')


nice <- theme_bw()+
	theme(legend.position = "none",
							legend.background = element_rect(fill = "transparent", color = NA),
							plot.background = element_rect(fill = "transparent",colour = NA),
							text = element_text(color = "black", size = 7),
							legend.text = element_text(size = 7),panel.grid = element_blank(),
				panel.background = element_blank(),
				panel.border = element_blank(),
				axis.title = element_blank(),
				axis.text = element_blank(),
				axis.ticks = element_blank(),
							plot.margin=unit(c(0.1, 0.1,0,0.2),"cm"),
				plot.subtitle = element_text(size = 7)) #trbl
nice2 <- theme(legend.position = "none",
							legend.background = element_rect(fill = "transparent", color = NA),
							plot.background = element_rect(fill = "transparent",colour = NA),
							text = element_text(color = "black", size = 7),
							legend.text = element_text(size = 7),
							plot.margin=unit(c(0.0, -0.7,0.1,-0.5),"cm"))


###########################################################################
#kreis Diagramme
#EPS
#rhine

ddd3 <- df02 %>% filter(polymer == "EPS"&river == "rhine") %>% dplyr::select(rest, River1, River2, Lake) 
ddd32 <- data.frame(value = as.numeric(ddd3),
										gr = names(ddd3))
ddd32$percent <- round(ddd32$value/sum(ddd32$value)*100,1)
ddd32$percent[1] <- 100 - sum(ddd32$percent[2:4])
df2 <- ddd32 %>% 
	mutate(csum = rev(cumsum(rev(value))), 
				 pos = value/2 + lead(csum, 1),
				 pos = if_else(is.na(pos), value/2, pos))


ddd32$gr <- factor(ddd32$gr, levels = rev(c("Lake", "River2", "River1", "rest"))) #order: retained in lakes, retained in rivers, outflow in river sediment, outflow all




p11 <- ggplot(ddd32, aes(x = gr , y = value, fill = gr))+
	geom_text(aes(label=round(value/1000,1)), vjust=0.5, hjust = -0.2, size = 2.5)+ #/1000 to transfer in from g to kg!
	geom_bar(stat = "identity", width = 1)+
	scale_y_continuous(limits = c(0, I(max(ddd32$value)+max(ddd32$value)*0.3)))+
	scale_fill_manual("",
										values = farben,
										labels = c("remaining", "rivers", "river2", "lakes"),
										breaks = c("rest", "River1", "River2", "Lake"))+
	coord_flip()+
	nice

#rhone

ddd3 <- df02 %>% filter(polymer == "EPS"&river == "rhone") %>% dplyr::select(rest, River1, River2, Lake) 
ddd32 <- data.frame(value = as.numeric(ddd3),
										gr = names(ddd3))
ddd32$percent <- round(ddd32$value/sum(ddd32$value)*100,1)
ddd32$percent[1] <- 100 - sum(ddd32$percent[2:4])
df2 <- ddd32 %>% 
	mutate(csum = rev(cumsum(rev(value))), 
				 pos = value/2 + lead(csum, 1),
				 pos = if_else(is.na(pos), value/2, pos))


ddd32$gr <- factor(ddd32$gr, levels = rev(c("Lake", "River2", "River1", "rest"))) #order: retained in lakes, retained in r


p12 <- ggplot(ddd32, aes(x = gr , y = value, fill = gr))+
	geom_text(aes(label=round(value/1000,1)), vjust=0.5, hjust = -0.2, size = 2.5)+
	geom_bar(stat = "identity", width = 1)+
	scale_y_continuous(limits = c(0, I(max(ddd32$value)+max(ddd32$value)*0.3)))+
	scale_fill_manual("",
										values = farben,
										labels = c("remaining", "rivers", "river2", "lakes"),
										breaks = c("rest", "River1", "River2", "Lake"))+
	coord_flip()+
	nice
# le doubs

ddd3 <- df02 %>% filter(polymer == "EPS"&river == "Le doubs") %>% dplyr::select(rest, River1, River2, Lake) 
ddd32 <- data.frame(value = as.numeric(ddd3),
										gr = names(ddd3))
ddd32$percent <- round(ddd32$value/sum(ddd32$value)*100,1)
ddd32$percent[1] <- 100 - sum(ddd32$percent[2:3])
df2 <- ddd32 %>% 
	mutate(csum = rev(cumsum(rev(value))), 
				 pos = value/2 + lead(csum, 1),
				 pos = if_else(is.na(pos), value/2, pos))


ddd32$gr <- factor(ddd32$gr, levels = rev(c("Lake", "River2", "River1", "rest"))) #order: retained in lakes, retained in r


p13 <- ggplot(ddd32, aes(x = gr , y = value, fill = gr))+
	geom_text(aes(label=round(value/1000,1)), vjust=0.5, hjust = -0.2, size = 2.5)+
	geom_bar(stat = "identity", width = 1)+
	scale_y_continuous(limits = c(0, I(max(ddd32$value)+max(ddd32$value)*0.3)))+
	scale_fill_manual("",
										values = farben,
										labels = c("remaining", "rivers", "river2", "lakes"),
										breaks = c("rest", "River1", "River2", "Lake"))+
	coord_flip()+
	nice


#PS#######
#rhine

ddd3 <- df02 %>% filter(polymer == "PS"&river == "rhine") %>% dplyr::select(rest, River1, River2, Lake) 
ddd32 <- data.frame(value = as.numeric(ddd3),
										gr = names(ddd3))
ddd32$percent <- round(ddd32$value/sum(ddd32$value)*100,1)
ddd32$percent[1] <- 100 - sum(ddd32$percent[2:4])
df2 <- ddd32 %>% 
	mutate(csum = rev(cumsum(rev(value))), 
				 pos = value/2 + lead(csum, 1),
				 pos = if_else(is.na(pos), value/2, pos))

ddd32$gr <- factor(ddd32$gr, levels = rev(c("Lake", "River2", "River1", "rest"))) #order: retained in lakes, retained in r



p21 <- ggplot(ddd32, aes(x = gr , y = value, fill = gr))+
	geom_text(aes(label=round(value/1000,1)), vjust=0.5, hjust = -0.2, size = 2.5)+
	geom_bar(stat = "identity", width = 1)+
	scale_y_continuous(limits = c(0, I(max(ddd32$value)+max(ddd32$value)*0.3)))+
	scale_fill_manual("",
										values = farben,
										labels = c("remaining", "rivers", "river2", "lakes"),
										breaks = c("rest", "River1", "River2", "Lake"))+
	coord_flip()+
	nice

#rhone

ddd3 <- df02 %>% filter(polymer == "PS"&river == "rhone") %>% dplyr::select(rest, River1, River2, Lake) 
ddd32 <- data.frame(value = as.numeric(ddd3),
										gr = names(ddd3))
ddd32$percent <- round(ddd32$value/sum(ddd32$value)*100,1)
ddd32$percent[1] <- 100 - sum(ddd32$percent[2:4])
df2 <- ddd32 %>% 
	mutate(csum = rev(cumsum(rev(value))), 
				 pos = value/2 + lead(csum, 1),
				 pos = if_else(is.na(pos), value/2, pos))

ddd32$gr <- factor(ddd32$gr, levels = rev(c("Lake", "River2", "River1", "rest"))) #order: retained in lakes, retained in r



p22 <- ggplot(ddd32, aes(x = gr , y = value, fill = gr))+
	geom_text(aes(label=round(value/1000,1)), vjust=0.5, hjust = -0.2, size = 2.5)+
	geom_bar(stat = "identity", width = 1)+
	scale_y_continuous(limits = c(0, I(max(ddd32$value)+max(ddd32$value)*0.3)))+
	scale_fill_manual("",
										values = farben,
										labels = c("remaining", "rivers", "river2", "lakes"),
										breaks = c("rest", "River1", "River2", "Lake"))+
	coord_flip()+
	nice



# le doubs

ddd3 <- df02 %>% filter(polymer == "PS"&river == "Le doubs") %>% dplyr::select(rest, River1, River2, Lake) 
ddd32 <- data.frame(value = as.numeric(ddd3),
										gr = names(ddd3))
ddd32$percent <- round(ddd32$value/sum(ddd32$value)*100,1)
ddd32$percent[1] <- 100 - sum(ddd32$percent[2:4])
df2 <- ddd32 %>% 
	mutate(csum = rev(cumsum(rev(value))), 
				 pos = value/2 + lead(csum, 1),
				 pos = if_else(is.na(pos), value/2, pos))



ddd32$gr <- factor(ddd32$gr, levels = rev(c("Lake", "River2", "River1", "rest"))) #order: retained in lakes, retained in r

p23 <- ggplot(ddd32, aes(x = gr , y = value, fill = gr))+
	geom_text(aes(label=round(value/1000,1)), vjust=0.5, hjust = -0.2, size = 2.5)+
	geom_bar(stat = "identity", width = 1)+
	scale_y_continuous(limits = c(0, I(max(ddd32$value)+max(ddd32$value)*0.3)))+
	scale_fill_manual("",
										values = farben,
										labels = c("remaining", "rivers", "river2", "lakes"),
										breaks = c("rest", "River1", "River2", "Lake"))+
	coord_flip()+
	nice


###legend

ddd3 <- df02 %>% filter(polymer == "PS"&river == "Le doubs") %>% dplyr::select(rest, River1, River2, Lake) 
ddd32 <- data.frame(value = as.numeric(ddd3),
										gr = c("a","b","c","d"))

ddd32$percent <- round(ddd32$value/sum(ddd32$value)*100,1)
ddd32$percent[1] <- 100 - sum(ddd32$percent[2:4])
df2 <- ddd32 %>% 
	mutate(csum = rev(cumsum(rev(value))), 
				 pos = value/2 + lead(csum, 1),
				 pos = if_else(is.na(pos), value/2, pos))




plegend <- ggplot(ddd32, aes(x = "" , y = value, fill = gr)) +
	geom_col() +
	coord_polar(theta = "y") +
	scale_fill_manual("",
										values = rev(farben),
										labels = rev(c("transport / outflow in suspension", "transport / outflow in sediment", "a) - c) total accumulation (summed up, including tributaries)                          \nd) - f) retained in rivers only                          ", "d) - f) retained in lakes")),
										breaks = c("a", "b", "c", "d"))+
	guides(fill = guide_legend(title = element_blank(), nrow=2,byrow=F, keywidth = 1.2, keyheight = 0.7)) +
	theme_void()+
	nice2+
	theme(legend.position = "right")


legend2 <- get_legend(plegend)
ggdraw(legend2)


#PET#######
#rhine

ddd3 <- df02 %>% filter(polymer == "PET"&river == "rhine") %>% dplyr::select(rest, River1, River2, Lake) 
ddd32 <- data.frame(value = as.numeric(ddd3),
										gr = names(ddd3))
ddd32$percent <- round(ddd32$value/sum(ddd32$value)*100,1)
ddd32$percent[1] <- 100 - sum(ddd32$percent[2:4])
df2 <- ddd32 %>% 
	mutate(csum = rev(cumsum(rev(value))), 
				 pos = value/2 + lead(csum, 1),
				 pos = if_else(is.na(pos), value/2, pos))

ddd32$gr <- factor(ddd32$gr, levels = rev(c("Lake", "River2", "River1", "rest"))) #order: retained in lakes, retained in r




p31 <- ggplot(ddd32, aes(x = gr , y = value, fill = gr))+
	geom_text(aes(label=round(value/1000,1)), vjust=0.5, hjust = -0.2, size = 2.5)+
	geom_bar(stat = "identity", width = 1)+
	scale_y_continuous(limits = c(0, I(max(ddd32$value)+max(ddd32$value)*0.3)), breaks = max(ddd32$value)*0.9, labels = c("[kg / year]"))+
	scale_fill_manual("",
										values = farben,
										labels = c("remaining", "rivers", "river2", "lakes"),
										breaks = c("rest", "River1", "River2", "Lake"))+
	coord_flip()+
	nice+
	theme(axis.text.x = element_text())


#rhone

ddd3 <- df02 %>% filter(polymer == "PET"&river == "rhone") %>% dplyr::select(rest, River1, River2, Lake) 
ddd32 <- data.frame(value = as.numeric(ddd3),
										gr = names(ddd3))
ddd32$percent <- round(ddd32$value/sum(ddd32$value)*100,1)
ddd32$percent[1] <- 100 - sum(ddd32$percent[2:4])
df2 <- ddd32 %>% 
	mutate(csum = rev(cumsum(rev(value))), 
				 pos = value/2 + lead(csum, 1),
				 pos = if_else(is.na(pos), value/2, pos))

ddd32$gr <- factor(ddd32$gr, levels = rev(c("Lake", "River2", "River1", "rest"))) #order: retained in lakes, retained in r




p32 <- ggplot(ddd32, aes(x = gr , y = value, fill = gr))+
	geom_text(aes(label=round(value/1000,1)), vjust=0.5, hjust = -0.2, size = 2.5)+
	geom_bar(stat = "identity", width = 1)+
	scale_y_continuous(limits = c(0, I(max(ddd32$value)+max(ddd32$value)*0.3)), breaks = max(ddd32$value)*0.9, labels = c("[kg / year]"))+
	scale_fill_manual("",
										values = farben,
										labels = c("remaining", "rivers", "river2", "lakes"),
										breaks = c("rest", "River1", "River2", "Lake"))+
	coord_flip()+
	nice+
	theme(axis.text.x = element_text())

# le doubs

ddd3 <- df02 %>% filter(polymer == "PET"&river == "Le doubs") %>% dplyr::select(rest, River1, River2, Lake) 
ddd32 <- data.frame(value = as.numeric(ddd3),
										gr = names(ddd3))
ddd32$percent <- round(ddd32$value/sum(ddd32$value)*100,0)
ddd32$percent[1] <- 100 - sum(ddd32$percent[2:4])
df2 <- ddd32 %>% 
	mutate(csum = rev(cumsum(rev(value))), 
				 pos = value/2 + lead(csum, 1),
				 pos = if_else(is.na(pos), value/2, pos))

ddd32$gr <- factor(ddd32$gr, levels = rev(c("Lake", "River2", "River1", "rest"))) #order: retained in lakes, retained in r




p33 <- ggplot(ddd32, aes(x = gr , y = value, fill = gr))+
	geom_text(aes(label=round(value/1000,1)), vjust=0.5, hjust = -0.2, size = 2.5)+
	geom_bar(stat = "identity", width = 1)+
	scale_y_continuous(limits = c(0, I(max(ddd32$value)+max(ddd32$value)*0.3)), breaks = max(ddd32$value)*0.9, labels = c("[kg / year]"))+
	scale_fill_manual("",
										values = farben,
										labels = c("remaining", "rivers", "river2", "lakes"),
										breaks = c("rest", "River1", "River2", "Lake"))+
	coord_flip()+
	nice+
	theme(axis.text.x = element_text())
p33





#put all together
# 
# pall2 <- ggdraw(plot_grid(t0, p11,p21,p31,t0,p12,p22,p32, t0,p13,p23,p33, t0,rel_heights = c(0.4,1,1,1,0.5,1,1,1,0.5,1,1,1,0.4), ncol = 1))
# 
# 
# 
# pall <- ggdraw(plot_grid(p1eps,p1ps, p1pet, p2eps, p2ps, p2pet, p3eps, p3ps, p3pet, ncol = 1, align = "v", rel_heights = c(1.25,0.9,1.25,1.2,0.9,1.25,1.2,0.9,1.25)))
# 
# 
# pall_all <- ggdraw(plot_grid(plot_grid(pall, pall2, ncol = 2, rel_widths = c(1,0.4)), legends, nrow = 2, rel_heights = c(1,0.1)))

legends <- ggdraw(plot_grid( legend2, nrow = 1, rel_widths = c(1)))

x1 <- ggdraw(plot_grid(p1eps,t0, p11, nrow = 1, rel_widths = c(1,0.08, 0.25), align = "h", labels = c("a)","", "d)"), label_fontface = "bold", label_size = 7, label_x = -0.01))
x2 <- ggdraw(plot_grid(p1ps,t0, p21, nrow = 1, rel_widths = c(1,0.08, 0.25), align = "h"))
x3 <- ggdraw(plot_grid(p1pet,t0, p31, nrow = 1, rel_widths = c(1,0.08, 0.25), align = "h"))

x4 <- ggdraw(plot_grid(p2eps,t0, p12, nrow = 1, rel_widths = c(1,0.08, 0.25), align = "h", labels = c("b)","", "e)"), label_fontface = "bold", label_size = 7, label_x = -0.01))
x5 <- ggdraw(plot_grid(p2ps,t0, p22, nrow = 1, rel_widths = c(1,0.08, 0.25), align = "h"))
x6 <- ggdraw(plot_grid(p2pet,t0, p32, nrow = 1, rel_widths = c(1,0.08, 0.25), align = "h"))

x7 <- ggdraw(plot_grid(p3eps,t0, p13, nrow = 1, rel_widths = c(1,0.08, 0.25), align = "h", labels = c("c)","", "f)"), label_fontface = "bold", label_size = 7, label_x = -0.01))
x8 <- ggdraw(plot_grid(p3ps,t0, p23, nrow = 1, rel_widths = c(1,0.08, 0.25), align = "h"))
x9 <- ggdraw(plot_grid(p3pet,t0, p33, nrow = 1, rel_widths = c(1,0.08, 0.25), align = "h"))

pall_all <- ggdraw(plot_grid(plot_grid(x1,x2, x3, x4,x5,x6,x7,x8,x9, nrow = 9, align = "v", rel_heights = c(1.25,0.9,1.25,1.2,0.9,1.25,1.2,0.9,1.25)), legends, nrow = 2, rel_heights = c(1,0.1)))

ggsave("PhD/mennekes/output_files/plots/retain_distribution2.png", plot = pall_all, width = 18, height = 17,units = "cm", dpi = 500, bg = "transparent")

ggsave("PhD/mennekes/output_files/plots/retain_distribution2.pdf", plot = pall_all, width = 18, height = 17,units = "cm", dpi = 500, bg = "transparent")


rm(list = ls())
