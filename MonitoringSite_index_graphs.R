rm(list = ls(all = T))
library(dplyr)
library(ggplot2)
library(raster)
library(rgdal)
library(maptools)
library(sp)

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

#code for 1 site
p <- ggplot(data = ext.f, aes(x = variable, y = value , group = 1))+
        geom_line()+
        geom_point()+
        ggtitle("Site 7")+
        geom_smooth(data=subset(ext.f, variable >= 2000), method=lm, se = FALSE)+
        labs(x = "year", y = "index")+
        theme_bw()
ggsave(paste0("LGS 7 index", ".png"), p, width = 8, height = 6)


#Loop for all sites
siteN <- dfName

for (i in 1:length(siteN)) {
        ext.f <- filter(ext.i, site == siteN[i])
        p <- ggplot(data = ext.f, aes(x = variable, y = value , group = 1))+
                geom_line()+
                geom_point()+
                coord_cartesian(ylim = c(10, 150)) +
                ggtitle(siteN[i])+
                geom_smooth(data=subset(ext.f, variable >= 2000), method=lm, se = T)+
                labs(x = "year", y = "index")+
                theme_bw()
        ggsave(paste0(siteN[i], ".png"), p, width = 8, height = 6)
}

write.csv(ext.i, "Index_values_by_site_89_14.csv", row.names=FALSE)
