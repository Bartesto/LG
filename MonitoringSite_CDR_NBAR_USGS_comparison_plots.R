rm(list = ls(all = T))
library(dplyr)
library(ggplot2)
library(raster)
library(rgdal)
library(maptools)
library(sp)
library(reshape2)

###CODE FOR NBAR###
setwd("Z:\\DEC\\LornaGlenVegetationChange_15122C03\\DATA\\VegMachine\\data\\index\\November_DC\\timeseries")

#Name of raster stack i.e. like timeseries stack (Vegmachine)
dName1 <- "11078_DC_aoi_89_13_15000minusi35_timeseries.ers"

#Name of shp ---- ensure that it is in same projection as raster stack
sName1 <- "test_mult_mga51.shp"

#Read in stack
dataR1 <- stack(dName1)
sproj1 <- CRS(dataR1@crs@projargs)#Take CRS from raster stack to apply to shp OK if the two are the same
sitesSHP1 <- readShapePoly(sName1, IDvar = "Site", proj4string = sproj1)# Ensure shp is in same CRS as raster

#Make site names (these fit the shp I was working with) change if required
dfName1 <- paste(rep("site", length = 24), rep(1:24), sep = "_")
dfYears1 <- as.character(c(1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 2000, 2001, 2002, 2003, 2004,
                           2005, 2006, 2007, 2008, 2009, 2010, 2011, 2013))#Years relating to stack ---- change as required

#Use extract and mean function (ensure nl = number of years/layers in stack)
ext.i1 <- as.data.frame(extract(dataR1, sitesSHP1,  fun=mean,  nl = 23))
colnames(ext.i1) <- as.character(dfYears1)#Add years as colnames
ext.i1$site <- dfName1 #Add site names to df

ext.i1 <- melt(ext.i1, id = "site")
Dwhole1 <- as.Date(ext.i1$variable, "%Y")
Dnum1 <- as.numeric(format(Dwhole1,"%Y"))
ext.i1$variable <- Dnum1
ext.i1$prod <- "NBAR"#new variable created to id data product for ggplot group
ext.i1$norm <- as.numeric(scale(ext.i1[,3]))
siteN1 <- dfName1


###CODE FOR CDR###
setwd("Z:\\DEC\\LornaGlenVegetationChange_15122C03\\DATA\\VegMachine\\data\\index\\November_CDR\\timeseries")

#Name of raster stack i.e. like timeseries stack (Vegmachine)
dName <- "11078_CDR_aoi_89_13_15000minusi35_timeseries.ers"

#Name of shp ---- ensure that it is in same projection as raster stack
sName <- "test_mult_mga51.shp"

#Read in stack
dataR <- stack(dName)
sproj <- CRS(dataR@crs@projargs)#Take CRS from raster stack to apply to shp OK if the two are the same
sitesSHP <- readShapePoly(sName, IDvar = "Site", proj4string = sproj)# Ensure shp is in same CRS as raster



#Make site names (these fit the shp I was working with) change if required
dfName <- paste(rep("site", length = 24), rep(1:24), sep = "_")
dfYears <- as.character(c(1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 
                          1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
                          2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012))#Years relating to stack ---- change as required

#Use extract and mean function (ensure nl = number of years/layers in stack)
ext.i <- as.data.frame(extract(dataR, sitesSHP,  fun=mean,  nl = 24))
colnames(ext.i) <- as.character(dfYears)#Add years as colnames
ext.i$site <- dfName #Add site names to df

ext.i <- melt(ext.i, id = "site")
Dwhole <- as.Date(ext.i$variable, "%Y")
Dnum <- as.numeric(format(Dwhole,"%Y"))
ext.i$variable <- Dnum
ext.i$prod <- "CDR"#new variable created to id data product for ggplot group
ext.i$norm <- as.numeric(scale(ext.i[,3]))
siteN <- dfName

###CODE FOR USGS SUNCORRECTED###
setwd("Z:\\DEC\\LornaGlenVegetationChange_15122C03\\DATA\\VegMachine\\data\\timeseries")

#Name of raster stack i.e. like timeseries stack (Vegmachine)
dName2 <- "11078_aoi_i35_Timeseries_89_14_nov_255minusi35.ers"

#Name of shp ---- ensure that it is in same projection as raster stack
sName2 <- "test_mult_mga51.shp"

#Read in stack
dataR2 <- stack(dName2)
sproj2 <- CRS(dataR2@crs@projargs)#Take CRS from raster stack to apply to shp OK if the two are the same
sitesSHP2 <- readShapePoly(sName2, IDvar = "Site", proj4string = sproj2)# Ensure shp is in same CRS as raster

#Make site names (these fit the shp I was working with) change if required
dfName2 <- paste(rep("site", length = 24), rep(1:24), sep = "_")
dfYears2 <- as.character(1989:2014)#Years relating to stack ---- change as required

#Use extract and mean function (ensure nl = number of years/layers in stack)
ext.i2 <- as.data.frame(extract(dataR2, sitesSHP2,  fun=mean,  nl = 26))
colnames(ext.i2) <- as.character(dfYears2)#Add years as colnames
ext.i2$site <- dfName2 #Add site names to df

ext.i2 <- melt(ext.i2, id = "site")
Dwhole2 <- as.Date(ext.i2$variable, "%Y")
Dnum2 <- as.numeric(format(Dwhole2,"%Y"))
ext.i2$variable <- Dnum2
ext.i2$prod <- "USGS"
ext.i2$norm <- as.numeric(scale(ext.i2[,3]))
siteN2 <- dfName2

##merge all
all <- merge(ext.i, ext.i1, all = TRUE)
all.1 <- merge(all, ext.i2, all = TRUE)


setwd("Z:\\DEC\\LornaGlenVegetationChange_15122C03\\DATA\\VegMachine\\data\\index\\November_CDR\\timeseries\\CDRvNBARvUSGS")
for(i in 1:length(siteN)){
        all.f <- filter(all.1, site == siteN[i])
        p <- ggplot(data=all.f, aes(x = variable, y = norm, group = prod, colour = prod))+
                geom_line()+
                geom_point()+
                ggtitle(paste0(siteN[i], " ", "Comparison CDR v NBAR products"))+
                labs(x = "year", y = "flipped 3+5 index")+
                theme_bw()
        ggsave(paste0(siteN[i], "CDR_v_NBAR", ".png"), p, width = 8, height = 6)
}