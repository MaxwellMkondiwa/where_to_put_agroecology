#Mapping yields, area and production
library(foreign)
library(readstata13)
library(dplyr)
library(car)
library(gmodels)
library(pastecs)
library(foreign)
library(readstata13)
library(spdep)
library(spBayes)
library(classInt)
library(RColorBrewer)
library(reshape2)
library(dplyr)
library(tidyr)
library(reshape)
library(tables)
library(MBA)
library(fields)
library(maptools)
library(classInt)
library(Hmisc)
library(nlme)

#setwd ("C:/Users/User/OneDrive/CCRP/AEHubCropLivestock")
library(maptools)
malawi.shp.district=readShapePoly("Data/gaul_malawi_poly.shp",proj4string = CRS("+proj=longlat"))
plot(malawi.shp.district)

##
library(ggplot2)
library(ggmap)
library(scales)
library(rgdal)
library(Cairo)
library(dplyr)

# Districts ----------------------

library(maptools)
library(rgdal)

district.points=readOGR(dsn = "Data", layer = "DistrictPoints")

plot(district.points)

districtdata=as.data.frame(district.points@data)
coordinat=as.data.frame(district.points@coords)
coordinat$DistName=districtdata$ADM2_NAME
names(coordinat)[1]=c("longitude")
names(coordinat)[2]=c("latitude")

# get district locations
district_locations <- coordinat
#

cnames <- aggregate(cbind(longitude, latitude) ~ DistName, data=coordinat, FUN=function(x) mean(range(x)))
#
gaul_malawi<- readOGR(dsn = "Data", layer = "gaul_malawi_poly")
tract <- fortify(gaul_malawi)


Malawidistrictboundaries=readOGR(dsn = "Data", layer = "Malawi district boundaries")
MalawiCities=readOGR(dsn = "Data", layer = "MWICities")


## Cities
Cities=subset(tract,tract$ADM2_NAME%in%c("Lilongwe","Blantyre","Mzimba","Zomba"))
Citiesf=fortify(Cities)
#

### Figure 1
Fig1 <- ggplot() +
  geom_polygon(data = tract, aes(x = long, y = lat, group = group
  ), color = "black", size = 0.25,fill="white")+
  geom_polygon(data= Cities, aes(long,lat, group=group), color="grey", fill="grey")+
  annotate("segment",
           x=35,xend=34.5,y=-10,yend=-11,linetype='solid',arrow = arrow(length = unit(0.03, "npc")))+
  annotate("text", x=35.1, y=-10, label= "Lake\n Malawi") +
  geom_text(data=cnames, aes(longitude, latitude, label = DistName), size=3, fontface="bold")+
  labs(y="Latitude",x="Longitude")
previous_theme <- theme_set(theme_bw())
Fig1


#
library(readxl)

Goatkeepingsystem2007=read_excel("Data/Secondarydata.xlsx",
                                sheet="DistrictData")


Goatkeepingsystem2007D=merge(Malawidistrictboundaries,Goatkeepingsystem2007,by="DISTRICT",all.x=TRUE)

writeOGR(obj=Goatkeepingsystem2007D,dsn="Data",layer="Goatkeepingsystem2007DE",driver="ESRI Shapefile")






# Cities -------------
MalawiCities$DISTRICT=MalawiCities$ADMIN3
Goatkeepingsystem2007C=merge(MalawiCities,Goatkeepingsystem2007,by="DISTRICT",all.x=TRUE)

library(sf)
Goatkeepingsystem2007D_sf=st_as_sf(Goatkeepingsystem2007D)


# Lake Malawi Layer
LakeMalawi= readOGR(dsn = "Data", layer = "LakeMalawi")
LakeMalawisf=st_as_sf(LakeMalawi)

## Cities
Cities=subset(tract,tract$ADM2_NAME%in%c("Lilongwe","Blantyre","Mzimba","Zomba"))


# Free range by district------
GoatkeepingsystemPlot=ggplot() +
  geom_sf(data=Goatkeepingsystem2007D_sf,aes(fill = Freerange))+
  geom_sf(data=LakeMalawisf)+
  scale_fill_viridis_c()+
  labs(fill="Percentage of goats \n under free range")
previous_theme <- theme_set(theme_bw())
GoatkeepingsystemPlot
# 
ggsave("Data/GoatkeepingsystemPlot.png",dpi=300)

# Free range by cities

