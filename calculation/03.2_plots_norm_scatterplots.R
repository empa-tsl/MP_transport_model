#### plot 03

#scatterplot

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
library(GGally)



#load data

polymers <- c("EPS","PP", "LDPE", "HDPE", "PS", "PVC", "PET")

main.path <- "PhD/mennekes/"
withf <- "_withallfactors"
rounds <- "801"

load(paste0(main.path, "output_files/rdata/cont_HDPE_", rounds, withf, ".Rdata"))
# s_r[s_r$outflow==1, ]

df01 <- as.data.frame(matrix(NA, nrow = nrow(s_r), ncol = I(length(polymers)+1))) #*2 for scenarios

rm(s_r)


names(df01) <- c("id", polymers)


#load data
counter <- 2
for (mat in polymers) {
		load(paste0(main.path, "output_files/rdata/cont_", mat,"_", rounds,  "_withallfactors.Rdata"))
		#rhine
		df01[, counter] <- s_r[ , paste0("water_perS_", mat, "_MiP")]
		counter <- counter+1
}

df01$id <- s_r$id_all

df01 <- df01[s_r$outflow==0, ]

#make relative contamination

df01$EPS <- df01$EPS/max(df01$EPS)
df01$PP <- df01$PP/max(df01$PP)
df01$LDPE <- df01$LDPE/max(df01$LDPE)
df01$HDPE <- df01$HDPE/max(df01$HDPE)
df01$PS <- df01$PS/max(df01$PS)
df01$PVC <- df01$PVC/max(df01$PVC)
df01$PET <- df01$PET/max(df01$PET)

rm(s_r)
df01


plot(df01$EPS, df01$PS)
cor(df01$EPS, df01$PET)


# p <- ggpairs(df01[ , 2:5], title="correlogram with ggpairs()") 
