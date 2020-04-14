## for associating regions with e.g. higher or lower temperatures or altitudes or other bioclimatic variables

# used to project LHY haplotype data onto various maps in James et al. 2018 Plant Cell & Environ 41(7): 1524-1538)

# after https://pakillo.github.io/R-GIS-tutorial/#raster

library(sp)
library(raster)
library(rasterVis)
library(maptools)
library(rgeos)

## prepare accession data to project onto map
# a dataset (WRLDacc) with 932 accessions (Figure 5a, 5c and Suppl. Figure S6a)
# this dataframe has simple spatial coordinates as lat and long
# WRLDacc<-read.csv('LHY SNP WRLD dataset.csv',header=TRUE)

# can subset the ESP accessions or the Iberian (ESP + POR) accessions (Figure 5c)
# ESPacc<-WRLDacc[WRLDacc$country==’ESP’,]
# IBacc<-WRLDacc[WRLDacc$country %in% c('ESP','POR'),]

# a dataset (WRLD_50_ran) with 200 accessions (50 of each haplotype, randomly selected) used in Figure 4a of James et al.
# this dataframe also has simple spatial coordinates as lat and long
WRLD_50_ran<-read.csv('LHY SNP WRLD_ran50 dataset.csv',header=TRUE)

# ESPacc_ran<-WRLD_50_ran[WRLD_50_ran$country==’ESP’,]
# IBacc_ran<-WRLD_50_ran[WRLD_50_ran$country %in% c('ESP','POR'),]

# first simple subset to get 3 columns
# WRLDlocs<-subset(WRLDacc,select=c("country","long","lat"))
WRLDlocs_ran50<-subset(WRLD_50_ran,select=c("country","long","lat"))

# and then just the longitude and latitude numbers
# coordinates(WRLDlocs)<-c("long","lat")
coordinates(WRLDlocs_ran50)<-c("long","lat")

# test plot of non-spatial data points
# plot(WRLDlocs)
# plot(WRLDlocs_ran50)

## to make the data explicitly spatial
# define the geographical projection : PROJ.4 description at http://www.spatialreference.org/

crs.geo<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") # geographical, datum WGS84

# proj4string(WRLDlocs)<-crs.geo
proj4string(WRLDlocs_ran50)<-crs.geo

# define latitude and longitude for a "world map" that omits America and the Far East that best portrays bulk of Arabidopsis 1001 genomes accessions(Fig 5a of James et al.)
newextWc<-c(-30,100,15,70)

# or define latitude and longitude extremes wider latitude world map (Suppl Fig S6a of James et al.)
newextW<-c(-125,140,15,70)

# or define latitude and longitude extremes for the Iberian peninsula map (Fig 5c of James et al.)
# newextESPOR<-c(-10,4,35,45)

# or for Sweden
# newextS<-c(4.8,20,54,65)

# or for UK
# newextUK<-c(-10.5,2,49.5,61)

# load Bioclimatic variables (e.g. bio 2.5m) from https://worldclim.org/data/worldclim21.html
# save .bil files locally on computer
# this example uses the Elevation variable obtained using getData (see below)

## e.g. defining the bioclimatic raster layers:
## BIO1 Annual Mean Temperature
# tAMT<-raster('/Users/Allan/WRLDmap_Bioclim_raster/bio1.bil')

## BIO2 Annual Mean Diurnal Range
## tminDR<-raster('/Users/Allan/WRLDmap_Bioclim_raster/bio2.bil')

## BIO3 Isothermality
# tminIT<-raster('/Users/Allan/WRLDmap_Bioclim_raster/bio3.bil')

## BIO4 Temperature Seasonality
# tSeason<-raster('/Users/Allan/WRLDmap_Bioclim_raster/bio4.bil')

## BIO5 Max Temperature of Warmest Month
# tminWM<-raster('/Users/Allan/WRLDmap_Bioclim_raster/bio5.bil')

## BIO6 Min Temperature of Coldest Month
# tminCM<-raster('/Users/Allan/WRLDmap_Bioclim_raster/bio6.bil')

## BIO7 Annual Temperature Range
# tAR<-raster('/Users/Allan/WRLDmap_Bioclim_raster/bio7.bil')

## BIO8 Mean Temperature of Wettest Quarter
# tWQ<-raster('/Users/Allan/WRLDmap_Bioclim_raster/bio8.bil')

## BIO9 Mean Temperature of Driest Quarter
# tDQ<-raster('/Users/Allan/WRLDmap_Bioclim_raster/bio9.bil')

## BIO10 Mean Temperature of Warmest Quarter
# tWrmQ<-raster('/Users/Allan/WRLDmap_Bioclim_raster/bio10.bil')

## BIO11 Mean Temperature of Coldest Quarter
# tColdQ<-raster('/Users/Allan/WRLDmap_Bioclim_raster/bio11.bil')

## BIO12 Annual Precipitation
# annP<-raster('/Users/Allan/WRLDmap_Bioclim_raster/bio12.bil')

## BIO13 Precipitation of Wettest Quarter
# pWM<-raster('/Users/Allan/WRLDmap_Bioclim_raster/bio13.bil')

## BIO14 Precipitation of Driest Month
# pDM<-raster('/Users/Allan/WRLDmap_Bioclim_raster/bio14.bil')

## BIO15 Precipitation Seasonality
# pSSN<-raster('/Users/Allan/WRLDmap_Bioclim_raster/bio15.bil')

## BIO16 Precipitation of Wettest Quarter
# pWetQ<-raster('/Users/Allan/WRLDmap_Bioclim_raster/bio16.bil')

## BIO17 Precipitation of Driest Quarter
# pDryQ<-raster('/Users/Allan/WRLDmap_Bioclim_raster/bio17.bil')

## BIO18 Precipitation of Warmest Quarter
# pWarmQ<-raster('/Users/Allan/WRLDmap_Bioclim_raster/bio18.bil')

## BIO19 Precipitation of Coldest Quarter
# pColdQ<-raster('/Users/Allan/WRLDmap_Bioclim_raster/bio19.bil')

## and for elevation
# https://pakillo.github.io/R-GIS-tutorial/#elevation

elevation<-getData("worldclim",var="alt",res=2.5,)

# after https://pakillo.github.io/R-GIS-tutorial/#elevation
# crop based on map boundaries defined in newextWc

elevation.Wc<-crop(elevation,newextWc)

xWc<-terrain(elevation.Wc, opt = c("slope", "aspect"), unit = "degrees")

png('WRLDmap_50ran_LHY_haplotypes.png')

# initial quick maps
plot(xWc)

slope.Wc<-terrain(elevation.Wc,opt="slope")
aspect.Wc<-terrain(elevation.Wc,opt="aspect")
hill.Wc<-hillShade(slope.Wc,aspect.Wc,40,270)
plot(hill.Wc,col=grey(0:100/100),legend=FALSE,main="Elevation",xlab="Longitude",ylab="Laltitude")

# plot(elevation.Wc,col=rainbow(25,alpha=0.35),add=TRUE)

plot(elevation.Wc,col=rainbow(25,alpha=0.35),add=TRUE)

points(WRLDlocs_ran50$long,WRLDlocs_ran50$lat,col=c('mediumblue','green','firebrick1','yellow')[unclass(WRLD_50_ran$genotype_comp)],pch=c(1,1,1,1),cex=1.0,lwd=1.5)

points(WRLDlocs_ran50$long,WRLDlocs_ran50$lat,col=c('gray30','gray30','gray30', 'gray30')[unclass(WRLD_50_ran$genotype_comp)],pch=c(21,21,21,21),cex=0.1)

legend('topright',pch=c(1,1,1,1),col=c('mediumblue','firebrick1','yellow','green'),c('A/G/U/G/A','G/G/U/G/A','G/G/U/G/C','A/U/G/C/A'),bty='o',box.col='black',cex=0.80,pt.lwd=1.5,bg='gray70')

dev.off()
