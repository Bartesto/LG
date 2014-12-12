rm(list = ls(all = T))
library(dplyr)
library(ggplot2)
library(raster)
library(rgdal)
library(maptools)
library(sp)
library(reshape2)

setwd("Z:\\DEC\\LornaGlenVegetationChange_15122C03\\DATA\\VegMachine\\data\\timeseries")
#Name of raster stack i.e. like timeseries stack (Vegmachine)
dName <- "11078_aoi_i35_Timeseries_89_14_nov_255minusi35.ers"
#Name of shp ---- ensure that it is in same projection as raster stack
sName <- "test_mult_mga51.shp"
#Read in stack
dataR <- stack(dName)
sproj <- CRS(dataR@crs@projargs)#Take CRS from raster stack to apply to shp OK if the two are the same
sitesSHP <- readShapePoly(sName, IDvar = "Site", proj4string = sproj)# Ensure shp is in same CRS as raster
#Make site names (these fit the shp I was working with) change if required
dfName <- paste(rep("site", length = 24), rep(1:24), sep = "_")
dfYears <- as.character(1989:2014)#Years relating to stack ---- change as required
#Use extract and mean function (ensure nl = number of years/layers in stack)
ext.i <- as.data.frame(extract(dataR, sitesSHP,  fun=mean,  nl = 26))
colnames(ext.i) <- as.character(dfYears)#Add years as colnames
ext.i$site <- dfName #Add site names to df
setwd("Z:\\DEC\\LornaGlenVegetationChange_15122C03\\DATA\\Working\\Analysis_preliminary\\20141204")
ext.i <- melt(ext.i, id = "site")
Dwhole <- as.Date(ext.i$variable, "%Y")
Dnum <- as.numeric(format(Dwhole,"%Y"))
ext.i$variable <- Dnum
#ext.f <- filter(ext.i, site == "site_7" & variable >= 2000)
ext.f <- filter(ext.i, site == "site_7")


lm_eqn = function(m) {
        
        l <- list(a = format(coef(m)[1], digits = 2),
                  b = format(abs(coef(m)[2]), digits = 2),
                  r2 = format(summary(m)$r.squared, digits = 3));
        
        if (coef(m)[2] >= 0)  {
                eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
        } else {
                eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(r)^2~"="~r2,l)    
        }
        
        as.character(as.expression(eq));                 
}


#works with line equation (set annotation parameters to suit data)
for (i in 1:length(siteN)) {
        ext.f <- filter(ext.i, site == siteN[i])
        mod.sub <- subset(ext.f, variable >= 2000)
        mod.i <- lm(mod.sub[,3] ~ mod.sub[,2])
        p <- ggplot(data = ext.f, aes(x = variable, y = value , group = 1))+
                geom_line()+
                geom_point()+
                coord_cartesian(ylim = c(10, 150)) +
                ggtitle(siteN[i])+
                geom_smooth(data=subset(ext.f, variable >= 2000), method=lm, se = T)+
                labs(x = "year", y = "index")+
                theme_bw()
        p2 <- p + annotate("text", x = 2005, y = 20, label = lm_eqn(mod.i), colour="black", size = 5, parse=TRUE)
        ggsave(paste0(siteN[i], ".png"), p2, width = 8, height = 6)
}



