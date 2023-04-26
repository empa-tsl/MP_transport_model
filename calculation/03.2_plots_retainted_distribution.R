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
				plot.subtitle = element_text(size = 10)) #trbl

s_lake = 2.5



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
			 y = "distribution microplastic")+
	theme(legend.position = "right")+
	geom_rect(data = start_end,
						aes(xmin = start, xmax = end),
						ymin = -Inf, ymax = Inf, alpha = 0.4,   fill = "white"
	)+
	annotate("text", x = max(df02$km)*0.1, y = 0.2, size = 3, label = "PET")+
	theme(axis.title.y = element_blank(),plot.margin = unit(c(0,0,0.1,0), units = "cm"))
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
						ymin = -Inf, ymax = Inf, alpha = 0.4,   fill = "white"
	)+
	annotate("text", x = max(df02$km)*0.1, y = 0.2, size = 3, label = "PS")+
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
			 y = "distribution microplastic",
			 subtitle = "Rhine")+
	geom_rect(data = start_end,
						aes(xmin = start, xmax = end),
						ymin = -Inf, ymax = Inf, alpha = 0.4,   fill = "white"
	)+
	annotate("text", x = max(df02$km)*0.1, y = 0.2, size = 3, label = "EPS")+
	theme(axis.title = element_blank(),
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
			 y = "distribution microplastic")+
	annotate("text", x = max(df02$km)*0.1, y = 0.2, size = 3, label = "PET")+
	theme(legend.position = "none")+
	geom_rect(data = start_end,
						aes(xmin = start, xmax = end),
						ymin = -Inf, ymax = Inf, alpha = 0.4,   fill = "white"
	)+
	theme(axis.title.y = element_blank(),
				plot.margin = unit(c(0,0,0.1,0), units = "cm"))

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
						ymin = -Inf, ymax = Inf, alpha = 0.4,   fill = "white"
	)+
	annotate("text", x = max(df02$km)*0.1, y = 0.2, size = 3, label = "PS")+
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
			 y = "distribution microplastic",
			 subtitle = "Rhône")+
	geom_rect(data = start_end,
						aes(xmin = start, xmax = end),
						ymin = -Inf, ymax = Inf, alpha = 0.4,   fill = "white"
	)+
	annotate("text", x = max(df02$km)*0.1, y = 0.2, size = 3, label = "EPS")+
	theme(axis.title = element_blank(),
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
			 y = "distribution microplastic")+
	geom_rect(data = start_end,
						aes(xmin = start, xmax = end),
						ymin = -Inf, ymax = Inf, alpha = 0.4,   fill = "white"
	)+
	annotate("text", x = max(df02$km)*0.1, y = 0.2, size = 3, label = "PET")+
	theme(axis.title.y = element_blank(),
				plot.margin = unit(c(0,0,0.1,0), units = "cm"))

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
						ymin = -Inf, ymax = Inf, alpha = 0.4,   fill = "white"
	)+
	annotate("text", x = max(df02$km)*0.1, y = 0.2, size = 3, label = "PS")+
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
			 y = "distribution microplastic",
			 subtitle = "Doubs")+
	geom_rect(data = start_end,
						aes(xmin = start, xmax = end),
						ymin = -Inf, ymax = Inf, alpha = 0.4,   fill = "white"
	)+
	annotate("text", x = max(df02$km)*0.1, y = 0.2, size = 3,  label = "EPS")+
	theme(axis.title = element_blank(),
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
#colors
farben <- c('dodgerblue',  "#CC5700", '#555555','#cccccc')


nice <- theme(legend.position = "none",
							legend.background = element_rect(fill = "transparent", color = NA),
							plot.background = element_rect(fill = "transparent",colour = NA),
							text = element_text(color = "black", size = 7),
							legend.text = element_text(size = 7),
							plot.margin=unit(c(0.0, -0.7,0.1,0),"cm")) #trbl
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


ggplot(ddd32, aes(x = gr , y = value, fill = gr))+
	geom_bar(stat = "identity")+
	scale_fill_manual("",
										values = farben,
										labels = c("remaining", "rivers", "river2", "lakes"),
										breaks = c("rest", "River1", "River2", "Lake"))+
	nice


p11 <- ggplot(ddd32, aes(x = "" , y = value, fill = fct_inorder(gr))) +
	geom_col() +
	coord_polar(theta = "y") +
	scale_fill_manual("",
										values = farben,
										labels = c("remaining", "rivers", "river2", "lakes"),
										breaks = c("rest", "River1", "River2", "Lake"))+
	# geom_label_repel(data = df2,
	# 								 aes(y = pos, label = paste0(percent, "%")),
	# 								 size = 4.5, nudge_x = 1, force = 5, max.time = 2, show.legend = FALSE, segment.colour = NA) +
	guides(fill = guide_legend(title = element_blank())) +
	theme_void()+
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





p12 <- ggplot(ddd32, aes(x = "" , y = value, fill = fct_inorder(gr))) +
	geom_col() +
	coord_polar(theta = "y") +
	scale_fill_manual("",
										values = farben,
										labels = c("remaining", "rivers", "river2", "lakes"),
										breaks = c("rest", "River1", "River2", "Lake"))+
	# geom_label_repel(data = df2,
	# 								 aes(y = pos, label = paste0(percent, "%")),
	# 								 size = 4.5, nudge_x = 1, force = 5, max.time = 2, show.legend = FALSE, segment.colour = NA) +
	guides(fill = guide_legend(title = element_blank())) +
	theme_void()+
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





p13 <- ggplot(ddd32, aes(x = "" , y = value, fill = fct_inorder(gr))) +
	geom_col() +
	coord_polar(theta = "y") +
	scale_fill_manual("",
										values = farben,
										labels = c("remaining", "rivers", "river2", "lakes"),
										breaks = c("rest", "River1", "River2", "Lake"))+
	# geom_label_repel(data = df2,
	# 								 aes(y = pos, label = paste0(percent, "%")),
	# 								 size = 4.5, nudge_x = 1, force = 5, max.time = 2, show.legend = FALSE, segment.colour = NA) +
	guides(fill = guide_legend(title = element_blank())) +
	theme_void()+
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





p21 <- ggplot(ddd32, aes(x = "" , y = value, fill = fct_inorder(gr))) +
	geom_col() +
	coord_polar(theta = "y") +
	scale_fill_manual("",
										values = farben,
										labels = c("remaining", "rivers", "river2", "lakes"),
									breaks = c("rest", "River1", "River2", "Lake"))+
	# geom_label_repel(data = df2,
	# 								 aes(y = pos, label = paste0(percent, "%")),
	# 								 size = 4.5, nudge_x = 1, force = 5, max.time = 2, show.legend = FALSE, segment.colour = NA) +
	guides(fill = guide_legend(title = element_blank())) +
	theme_void()+
	nice2


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





p22 <- ggplot(ddd32, aes(x = "" , y = value, fill = fct_inorder(gr))) +
	geom_col() +
	coord_polar(theta = "y") +
	scale_fill_manual("",
										values = farben,
										labels = c("remaining", "rivers", "river2", "lakes"),
										breaks = c("rest", "River1", "River2", "Lake"))+
	# geom_label_repel(data = df2,
	# 								 aes(y = pos, label = paste0(percent, "%")),
	# 								 size = 4.5, nudge_x = 1, force = 5, max.time = 2, show.legend = FALSE, segment.colour = NA) +
	guides(fill = guide_legend(title = element_blank())) +
	theme_void()+
	nice2



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





p23 <- ggplot(ddd32, aes(x = "" , y = value, fill = fct_inorder(gr))) +
	geom_col() +
	coord_polar(theta = "y") +
	scale_fill_manual("",
										values = farben,
										labels = c("outflow in suspension", "outflow in sediment", "retained in rivers", "retained in lakes"),
										breaks = c("rest", "River1", "River2", "Lake"))+
	# geom_label_repel(data = df2,
	# 								 aes(y = pos, label = paste0(percent, "%")),
	# 								 size = 4.5, nudge_x = 1, force = 5, max.time = 2, show.legend = FALSE, segment.colour = NA) +
	guides(fill = guide_legend(title = element_blank())) +
	theme_void()+
	nice2


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
										labels = rev(c("outflow in suspension", "outflow in sediment", "retained in rivers", "retained in lakes")),
										breaks = c("a", "b", "c", "d"))+
	# geom_label_repel(data = df2,
	# 								 aes(y = pos, label = paste0(percent, "%")),
	# 								 size = 4.5, nudge_x = 1, force = 5, max.time = 2, show.legend = FALSE, segment.colour = NA) +
	guides(fill = guide_legend(title = element_blank())) +
	theme_void()+
	nice2+
	theme(legend.position = "right")


legend2 <- get_legend(plegend)



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





p31 <- ggplot(ddd32, aes(x = "" , y = value, fill = fct_inorder(gr))) +
	geom_col() +
	coord_polar(theta = "y") +
	scale_fill_manual("",
										values = farben,
										labels = c("remaining", "rivers", "river2", "lakes"),
										breaks = c("rest", "River1", "River2", "Lake"))+
	# geom_label_repel(data = df2,
	# 								 aes(y = pos, label = paste0(percent, "%")),
	# 								 size = 4.5, nudge_x = 1, force = 5, max.time = 2, show.legend = FALSE, segment.colour = NA) +
	guides(fill = guide_legend(title = element_blank())) +
	theme_void()+
	nice


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





p32 <- ggplot(ddd32, aes(x = "" , y = value, fill = fct_inorder(gr))) +
	geom_col() +
	coord_polar(theta = "y") +
	scale_fill_manual("",
										values = farben,
										labels = c("remaining", "rivers", "river2", "lakes"),
										breaks = c("rest", "River1", "River2", "Lake"))+
	# geom_label_repel(data = df2,
									 # aes(y = pos, label = paste0(percent, "%")),
									 # size = 4.5, nudge_x = 1, force = 5, max.time = 2, show.legend = FALSE, segment.colour = NA) +
	guides(fill = guide_legend(title = element_blank())) +
	theme_void()+
	nice

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





p33 <- ggplot(ddd32, aes(x = "" , y = value, fill = fct_inorder(gr))) +
	geom_col() +
	coord_polar(theta = "y") +
	scale_fill_manual("",
										values = farben,
										labels = c("remaining", "rivers", "river2", "lakes"),
										breaks = c("rest", "River1", "River2", "Lake"))+
	# geom_label_repel(data = df2,
	# 								 aes(y = pos, label = paste0(percent, "%")),
	# 								 size = 3, nudge_x = 1, force = 5, max.time = 2, show.legend = FALSE, segment.colour = NA) +
	guides(fill = guide_legend(title = element_blank())) +
	theme_void()+
	nice







#put all together

pall2 <- ggdraw(plot_grid(t0, p11,t0,p31,t0,p12,t0,p32, t0,p13,t0,p33, t0,rel_heights = c(0.1,1,0.2,1,0.2,1,0.2,1,0.2,1,0.2,1,0.1), ncol = 1))

pall2.2 <- ggdraw(plot_grid(t0, t0,p21,t0,t0,t0,p22,t0,t0,t0, p23,t0, t0,rel_heights = c(0.1,0.275,0.5,0.275,0.2,0.275,0.5,0.275,0.2,0.275,0.5,0.275,0.1), ncol = 1))


pall <- ggdraw(plot_grid(p1eps,p1ps, p1pet, p2eps, p2ps, p2pet, p3eps, p3ps, p3pet, ncol = 1, align = "v", rel_heights = c(1.25,0.9,1.25,1.2,0.9,1.25,1.2,0.9,1.25)))

legends <- ggdraw(plot_grid(legend, legend2, nrow = 1, rel_widths = c(1,0.3)))

pall_all <- ggdraw(plot_grid(plot_grid(pall, pall2, pall2.2, ncol = 3, rel_widths = c(1,0.15,0.15)), legends, nrow = 2, rel_heights = c(1,0.2)))


ggsave("PhD/mennekes/output_files/plots/retain_distribution.png", plot = pall_all, width = 17, height = 17,units = "cm", dpi = 500, bg = "transparent")

ggsave("PhD/mennekes/output_files/plots/retain_distribution.pdf", plot = pall_all, width = 17, height = 17,units = "cm", dpi = 500, bg = "transparent")

#wegen aare




rm(list = ls())
