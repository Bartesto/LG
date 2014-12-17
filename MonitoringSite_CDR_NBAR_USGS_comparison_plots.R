rm(list = ls(all = T))

############################################################################################################################
## Takes timeseries data stack (must be .bil) and a shapefile, extracts vaules and plots. This version extracts values from
## 3 stacks (USGS suncorrected, NBAR product from data cube and USGS CDR product), normalises all values and plots all traces
## for comparison.
## Pitfalls - shape file and stack not in same projection, dfYears not updated correctly, argument in extract (nl=) not 
## updated correctly.
## Bart Huntley 17/12/2014

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
                ggtitle(paste0(siteN[i], " ", "Comparison CDR v NBAR v USGS products"))+
                labs(x = "year", y = "normalised flipped 3+5 index")+
                theme_bw()
        ggsave(paste0(siteN[i], "CDR_v_NBAR", ".png"), p, width = 8, height = 6)
}


##AGO DATA
setwd("Z:\\DEC\\LornaGlenVegetationChange_15122C03\\DATA\\VegMachine\\data\\index\\AGO\\timeseries")

#Name of raster stack i.e. like timeseries stack (Vegmachine)
dName3 <- "11078_AGO_aoi_89_06_255minusi35_timeseries.ers"

#Name of shp ---- ensure that it is in same projection as raster stack
sName3 <- "test_mult_mga51.shp"

#Read in stack
dataR3 <- stack(dName3)
sproj3 <- CRS(dataR3@crs@projargs)#Take CRS from raster stack to apply to shp OK if the two are the same
sitesSHP3 <- readShapePoly(sName3, IDvar = "Site", proj4string = sproj3)# Ensure shp is in same CRS as raster

#Make site names (these fit the shp I was working with) change if required
dfName3 <- paste(rep("site", length = 24), rep(1:24), sep = "_")
dfYears3 <- as.character(c(1989, 1991, 1992, 1995,  
                          1998, 2000, 2002, 2004,
                          2005, 2006))#Years relating to stack ---- change as required

#Use extract and mean function (ensure nl = number of years/layers in stack)
ext.i3 <- as.data.frame(extract(dataR3, sitesSHP3,  fun=mean,  nl = 10))
colnames(ext.i3) <- as.character(dfYears3)#Add years as colnames
ext.i3$site <- dfName3 #Add site names to df

ext.i3 <- melt(ext.i3, id = "site")
Dwhole3 <- as.Date(ext.i3$variable, "%Y")
Dnum3 <- as.numeric(format(Dwhole3,"%Y"))
ext.i3$variable <- Dnum3
ext.i3$prod <- "AGO"
ext.i3$norm <- as.numeric(scale(ext.i3[,3]))
siteN3 <- dfName3

#merge in AGO
all.2 <- merge(all.1, ext.i3, all = TRUE)


setwd("Z:\\DEC\\LornaGlenVegetationChange_15122C03\\DATA\\VegMachine\\data\\index\\November_CDR\\timeseries\\CDRvNBARvUSGSvAGO")
for(i in 1:length(siteN)){
        all.f <- filter(all.2, site == siteN[i])
        p <- ggplot(data=all.f, aes(x = variable, y = norm, group = prod, colour = prod))+
                geom_line()+
                geom_point()+
                ggtitle(paste0(siteN[i], " ", "Comparison CDR v NBAR v USGS v AGO products"))+
                labs(x = "year", y = "normalised flipped 3+5 index")+
                theme_bw()
        ggsave(paste0(siteN[i], "CDR_v_NBAR_v_USGS_v_AGO", ".png"), p, width = 8, height = 6)
}

#Plot just AGO points
i=7
ext.fT <- filter(ext.i3, site == siteN3[i])

ggplot(data = ext.fT, aes(x = variable, y = norm , group = 1))+
        geom_line()+
        geom_point()+
        ggtitle("Site 7")+
        #geom_smooth(data=subset(ext.f, variable >= 2000), method=lm, se = FALSE)+
        labs(x = "year", y = "index")+
        theme_bw()

##CODE FOR USGS AGO DATA DATES
setwd("Z:\\DEC\\LornaGlenVegetationChange_15122C03\\DATA\\VegMachine\\data\\index\\USGS_AGO_dates\\timeseries")

#Name of raster stack i.e. like timeseries stack (Vegmachine)
dName4 <- "11078_USGS_AGO_dates_aoi_89_06_255minusi35_timeseries.ers"

#Name of shp ---- ensure that it is in same projection as raster stack
sName4 <- "test_mult_mga51.shp"

#Read in stack
dataR4 <- stack(dName4)
sproj4 <- CRS(dataR4@crs@projargs)#Take CRS from raster stack to apply to shp OK if the two are the same
sitesSHP4 <- readShapePoly(sName4, IDvar = "Site", proj4string = sproj4)# Ensure shp is in same CRS as raster

#Make site names (these fit the shp I was working with) change if required
dfName4 <- paste(rep("site", length = 24), rep(1:24), sep = "_")
dfYears4 <- as.character(c(1989, 1991, 1992, 1995,  
                           1997, 1999, 2002, 2004,
                           2005, 2006))#Years relating to stack ---- change as required

#Use extract and mean function (ensure nl = number of years/layers in stack)
ext.i4 <- as.data.frame(extract(dataR4, sitesSHP4,  fun=mean,  nl = 10))
colnames(ext.i4) <- as.character(dfYears4)#Add years as colnames
ext.i4$site <- dfName4 #Add site names to df

ext.i4 <- melt(ext.i4, id = "site")
Dwhole4 <- as.Date(ext.i4$variable, "%Y")
Dnum4 <- as.numeric(format(Dwhole4,"%Y"))
ext.i4$variable <- Dnum4
ext.i4$prod <- "USGS_AGO"
ext.i4$norm <- as.numeric(scale(ext.i4[,3]))
siteN4 <- dfName4

#merge in AGO
all.3 <- merge(all.2, ext.i4, all = TRUE)


setwd("Z:\\DEC\\LornaGlenVegetationChange_15122C03\\DATA\\VegMachine\\data\\index\\November_CDR\\timeseries\\CDRvNBARvUSGSvAGOvusgsAGO")
for(i in 1:length(siteN)){
        all.f <- filter(all.3, site == siteN[i])
        p <- ggplot(data=all.f, aes(x = variable, y = norm, group = prod, colour = prod))+
                geom_line()+
                geom_point()+
                ggtitle(paste0(siteN[i], " ", "Comparison CDR v NBAR v USGS v AGO v usgsAGO products"))+
                labs(x = "year", y = "normalised flipped 3+5 index")+
                theme_bw()
        ggsave(paste0(siteN[i], "CDR_v_NBAR_v_USGS_v_AGO_v_usgsAGO", ".png"), p, width = 8, height = 6)
}

#Plots AGO just in Black but not in legend
for(i in 1:length(siteN)){
        all.f <- filter(all.3, site == siteN[i] & prod != "AGO")
        ago.i <- filter(ext.i3, site == siteN[i])
        p <- ggplot(data=all.f, aes(x = variable, y = norm, group = prod, colour = prod))+
                geom_line()+
                geom_point()+
                geom_point(data = ago.i, aes(y = norm), colour = "black")+
                ggtitle(paste0(siteN[i], " ", "Comparison CDR v NBAR v USGS v AGO v usgsAGO products"))+
                labs(x = "year", y = "normalised flipped 3+5 index")+
                theme_bw()
        ggsave(paste0(siteN[i], "CDR_v_NBAR_v_USGS_v_AGO_v_usgsAGO", ".png"), p, width = 8, height = 6)
}