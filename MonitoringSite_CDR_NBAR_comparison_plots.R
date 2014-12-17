rm(list = ls(all = T))
library(dplyr)
library(ggplot2)
library(raster)
library(rgdal)
library(maptools)
library(sp)
library(reshape2)

##CODE FOR NBAR
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

siteN1 <- dfName1


##CODE FOR CDR
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
siteN <- dfName


##merge -- puts NBAR and CDR df's on top of each other -- 1 dataset
all <- merge(ext.i, ext.i1, all = TRUE)

#Plots comparison graphs
setwd("Z:\\DEC\\LornaGlenVegetationChange_15122C03\\DATA\\VegMachine\\data\\index\\November_CDR\\timeseries\\CDRvNBAR")
for(i in 1:length(siteN)){
        all.f <- filter(all, site == siteN[i])
        p <- ggplot(data=all.f, aes(x = variable, y = value, group = prod, colour = prod))+
                geom_line()+
                geom_point()+
                ggtitle(paste0(siteN[i], " ", "Comparison CDR v NBAR products"))+
                labs(x = "year", y = "flipped 3+5 index")+
                theme_bw()
        ggsave(paste0(siteN[i], "CDR_v_NBAR", ".png"), p, width = 8, height = 6)
}


