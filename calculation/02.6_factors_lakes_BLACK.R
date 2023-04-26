###
# this script is used to determine sedimentation curves for the lakes. We aimed for a about 80% removal rate for the lake Geneva (500km2) in average based on existing measurements (see paper)
##sedimentation curve lakes

library(ggplot2)
library(zoo)
flache <- seq(1,800000000, by = 10000) #Area of lakes
#area in m2


lakeGeneva <- 0.95*(1- exp(1)^(-0.005*flache*0.000001)) #lake Geneva
y <- 0.95*(1- exp(1)^(-0.004*flache*0.000001))
y_schwer <- 0.95*(1- exp(1)^(-0.012*flache*0.000001)) #heavier polymers. trible the reduciton
y_sehrleicht <- 0.95*(1- exp(1)^(-0.0025*flache*0.000001)) #very light polymers
x <- flache * 0.000001					
total <- (y+y+y+y*0.6+y*0.8+y_schwer+y_schwer)/7


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
				panel.background = element_rect(fill = "transparent", color = NA),
				plot.subtitle = element_text(size = 10),
				plot.margin = unit(c(0.2,0.2,0.2,0.2), units = "cm"),
				axis.ticks = element_line(color = "white"),
				axis.line = element_line(colour = "white"),
				panel.border = element_rect(color = "white"),
				legend.key = element_rect(fill = "transparent"))


df <- data.frame(x = c(x, x, x),
								 y = c(y*100, y*0.75*100, y_schwer*100),
								 polymer = c(rep("PS", length(x)), rep("PP", length(x)), rep("PET", length(x))))

p <- ggplot(data = df, aes(x = x, y = y, color = polymer, linetype = polymer))+
	geom_line()+
	nice+
	scale_color_manual(values = c("grey90", "orange", "lightblue"))+
	scale_linetype_manual(values = c("solid", "solid", "dashed"))+
	scale_y_continuous(breaks = c(95, 50, 0))+
	labs(x = "lake surface area\nin km²",
			 y = "reduction in %")
	
ggsave("PhD/präsi/für Konferenzen/2023 SETAC/factor_lakes1.png", plot = p, width = 13, height = 8, units = "cm", bg = "transparent", dpi = 500)
#check for the integral
AUC <- function(x, y){
	sum(diff(x)*rollmean(y,2))
}

n <- AUC(flache, y) #normal
s <- AUC(flache, y_schwer)
sl <- AUC(flache, y*0.5)
l <- AUC(flache, y*0.75)
AUC(flache,y)

mean(c(n,n,n,s,s,sl,l))
