# Paper: Institutional quality, climate change impacts, and regional economic growth. Evidence from Peru
# Authors: Gustavo A. García (Universidad EAFIT, Colombia)
#          Paula Restrepo (The World Bank)
#          Juan Manuel Aristizábal (Universidad de Manizales, Colombia)
# Date: 09/2025

#Script for producing the output of Table 5.

#Load libraries
library(pacman)
p_load(readxl, sf, spatialreg, statar,sp, splm, plm, spdep, nlme, tidyverse, summarytools, Hmisc, readxl, modelsummary, statar) 

##Set the current working directory
setwd("")

#Import database
data <- read_excel("data_paper.xlsx") |>
  mutate(IQPA_quintile = ntile(IQPA, n = 5),
         efficiency_quintile = ntile(efficiency, n = 5),
         capacity_quintile = ntile(capacity, n = 5),
         internet_ratio=internet_ratio+0.00001)|> 
  arrange(dpto)

#Transforms the data frame data into a panel data structure (pdata.frame) using dpto as the individual (cross-sectional) index and year as the time index.
datapanel <- pdata.frame(data, c("dpto","year"))

#Load shapefile
map <- st_read("dpto_peru.shp") 

#Creates a neighbors list (nb) from the polygon object map using the queen contiguity criterion (common borders or vertices define neighbors).
nb <- poly2nb(map,queen=T)

#Converts the neighbors list into a row-standardized spatial weights matrix (We), where each row sums to one.
We <- nb2listw(nb, style="W")


#Generating the spatial lag of the variable
datapanel$wgdp_pc_t_1<-slag(datapanel$gdp_pc_t_1, We, 1)
datapanel$wIQPA_quintile<-slag(datapanel$IQPA_quintile, We, 1)
datapanel$wefficiency_quintile<-slag(datapanel$efficiency_quintile, We, 1)
datapanel$wcapacity_quintile<-slag(datapanel$capacity_quintile, We, 1)
datapanel$wprecipitation<-slag(datapanel$precipitation, We, 1)
datapanel$wtemperature<-slag(datapanel$temperature, We, 1)
datapanel$wsecondary_ratio<-slag(datapanel$secondary_ratio, We, 1)
datapanel$winternet_ratio<-slag(datapanel$internet_ratio, We, 1)
datapanel$wdensity<-slag(datapanel$density, We, 1)



##No Spatial Effects - Fixed effect models
#(Table 5 - Column1)
Fe1 <- plm(growth_gdp_pc ~ gdp_pc_t_1 +
             IQPA_quintile + I(IQPA_quintile*gdp_pc_t_1) +
             precipitation + I(precipitation^2) +
             temperature + I(temperature^2) +      
             log(secondary_ratio)+
             log(internet_ratio) +
             log(density),
           data = datapanel, 
           model = "within")
summary(Fe1)

##No Spatial Effects - Fixed effect models
#(Table 5 - Column2)
Fe2 <- plm(growth_gdp_pc ~ gdp_pc_t_1 +
             efficiency_quintile + I(efficiency_quintile*gdp_pc_t_1) +              
             precipitation + I(precipitation^2) +
             temperature + I(temperature^2) +
             log(secondary_ratio)+
             log(internet_ratio) +
             log(density),
           data = datapanel, 
           model = "within")
summary(Fe2)

##No Spatial Effects - Fixed effect models
Re3 <- plm(growth_gdp_pc ~ gdp_pc_t_1 +
             log(secondary_ratio)+
             log(internet_ratio) +
             capacity_quintile + I(capacity_quintile*gdp_pc_t_1) +
             precipitation + I(precipitation^2) +
             temperature + I(temperature^2) +
             log(density),
           data = datapanel, 
           model = "random")
summary(e3)

##Spatial Effects - SAR models
#(Table 5 - Column4)
SAR1_Fe <- spml(growth_gdp_pc ~ gdp_pc_t_1 +
                  IQPA_quintile + I(IQPA_quintile*gdp_pc_t_1) +             
                  precipitation + I(precipitation^2) +
                  temperature + I(temperature^2) +
                  log(secondary_ratio)+
                  log(internet_ratio) +
                  log(density),
                data = datapanel, 
                listw = We, spatial.error="none", lag=TRUE, 
                model = "within")
summary(SAR1_Fe)

##Spatial Effects - SAR models
#(Table 5 - Column5)
SAR2_Fe <- spml(growth_gdp_pc ~ gdp_pc_t_1 +
                  efficiency_quintile + I(efficiency_quintile*gdp_pc_t_1) +
                  log(secondary_ratio)+
                  precipitation + I(precipitation^2) +
                  temperature + I(temperature^2) +               
                  log(internet_ratio) +
                  log(density),
                data = datapanel, 
                listw = We, spatial.error="none", lag=TRUE, 
                model = "within")
summary(SAR2_Fe)

##Spatial Effects - SAR models
#(Table 5 - Column6)
SAR3_Fe <- spml(growth_gdp_pc ~ gdp_pc_t_1 +
                  capacity_quintile + I(capacity_quintile*gdp_pc_t_1) +             
                  precipitation + I(precipitation^2) +
                  temperature + I(temperature^2) +
                  log(secondary_ratio)+
                  log(internet_ratio) +
                  log(density),
                data = datapanel, 
                listw = We, spatial.error="none", lag=TRUE, 
                model = "within")
summary(SAR3_Fe)

##Spatial Effects - SEM models
#(Table 5 - Column7)
SEM1_Fe <- spml(growth_gdp_pc ~ gdp_pc_t_1 +
                  IQPA_quintile + I(IQPA_quintile*gdp_pc_t_1) +
                  precipitation + I(precipitation^2) +
                  temperature + I(temperature^2) +     
                  log(secondary_ratio)+
                  log(internet_ratio) +  
                  log(density),
                data = datapanel, 
                listw = We, spatial.error="b", lag=F, 
                model = "within")
summary(SEM1_Fe)

##Spatial Effects - SEM models
#(Table 5 - Column8)
SEM2_Fe <- spml(growth_gdp_pc ~ gdp_pc_t_1 +
                  efficiency_quintile + I(efficiency_quintile*gdp_pc_t_1) +               
                  precipitation + I(precipitation^2) +
                  temperature + I(temperature^2) +
                  log(secondary_ratio)+
                  log(internet_ratio) +
                  log(density),
                data = datapanel, 
                listw = We, spatial.error="b", lag=F, 
                model = "within")
summary(SEM2_Fe)

##Spatial Effects - SEM models
#(Table 5 - Column9)
SAC1_Fe <- spml(growth_gdp_pc ~ gdp_pc_t_1 +
                  IQPA_quintile + I(IQPA_quintile*gdp_pc_t_1) +              
                  precipitation + I(precipitation^2) +
                  temperature + I(temperature^2) +
                  log(secondary_ratio)+
                  log(internet_ratio) +
                  log(density),
                data = datapanel, 
                listw = We, spatial.error="b", lag=T, 
                model = "within")
summary(SAC1_Fe)

##Spatial Effects - SEM models
#(Table 5 - Column11)
SAC2_Fe <- spml(growth_gdp_pc ~ gdp_pc_t_1 +
                  efficiency_quintile + I(efficiency_quintile*gdp_pc_t_1) +                
                  precipitation + I(precipitation^2) +
                  temperature + I(temperature^2) + 
                  log(secondary_ratio)+
                  log(internet_ratio) +  
                  log(density),
                data = datapanel, 
                listw = We, spatial.error="b", lag=T, 
                model = "within")
summary(SAC2_Fe)

##Spatial Effects - SEM models
#(Table 5 - Column12)
SAC3_Fe <- spml(growth_gdp_pc ~ gdp_pc_t_1 +
                  capacity_quintile + I(capacity_quintile*gdp_pc_t_1) +                
                  precipitation + I(precipitation^2) +
                  temperature + I(temperature^2) +
                  log(secondary_ratio)+
                  log(internet_ratio) +
                  log(density),
                data = datapanel2, 
                listw = We, spatial.error="b", lag=T, 
                model = "within")
summary(SAC3_Fe)

##Spatial Effects - SDM models
#(Table 5 - Column13)
SDM1_Fe <- spml(growth_gdp_pc ~ gdp_pc_t_1 +
                  IQPA_quintile + I(IQPA_quintile*gdp_pc_t_1) +
                  precipitation + I(precipitation^2) +
                  temperature + I(temperature^2) +   
                  log(secondary_ratio)+
                  log(internet_ratio) +
                  log(density)+
                  wgdp_pc_t_1+
                  wIQPA+I(wIQPA*wgdp_pc_t_1)+
                  wprecipitation+I(wprecipitation^2)+
                  wtemperature+I(wtemperature^2)+
                  log(wsecondary_ratio)+
                  log(winternet_ratio)+
                  log(wdensity),
                data = datapanel, 
                listw = We, spatial.error="none", lag=T, 
                model = "within")
summary(SDM1_Fe)

##Spatial Effects - SDM models
#(Table 5 - Column14)
SDM2_Fe <- spml(growth_gdp_pc ~ gdp_pc_t_1 +
                  efficiency_quintile + I(efficiency_quintile*gdp_pc_t_1) +                
                  precipitation + I(precipitation^2) +
                  temperature + I(temperature^2) + 
                  log(secondary_ratio)+
                  log(internet_ratio) +
                  log(density)+
                  wgdp_pc_t_1+
                  wefficiency_quintile+I(wefficiency_quintile*wgdp_pc_t_1)+
                  wprecipitation+I(wprecipitation^2)+
                  wtemperature+I(wtemperature^2)+
                  log(wsecondary_ratio)+log(winternet_ratio)+
                  log(wdensity),
                data = datapanel, 
                listw = We, spatial.error="none", lag=T, 
                model = "within")
summary(SDM2_Fe)

##Spatial Effects - SDM models
#(Table 5 - Column15)
SDM3_Fe <- spml(growth_gdp_pc ~ gdp_pc_t_1 +
                  capacity_quintile + I(capacity_quintile*gdp_pc_t_1) +
                  precipitation + I(precipitation^2) +
                  temperature + I(temperature^2) + 
                  log(secondary_ratio)+
                  log(internet_ratio) +
                  log(density)+
                  wgdp_pc_t_1+
                  wcapacity_quintile+I(wcapacity_quintile*wgdp_pc_t_1)+
                  wprecipitation+I(wprecipitation^2)+
                  wtemperature+I(wtemperature^2)+
                  log(wsecondary_ratio)+log(winternet_ratio)+   
                  log(wdensity),
                data = datapanel, 
                listw = We, spatial.error="none", lag=T, 
                model = "within")
summary(SDM3_Fe)


#Test LM of spatial dependence
#In the first column, at the bottom of Table 5
slmtest(Fe1, listw=We, test=c("lme"))
slmtest(Fe1, listw=We, test=c("rlme"))
slmtest(Fe1, listw=We, test=c("lml"))
slmtest(Fe1, listw=We, test=c("rlml"))

#In the second column, at the bottom of Table 5
slmtest(Fe2, listw=We, test=c("lme"))
slmtest(Fe2, listw=We, test=c("rlme"))
slmtest(Fe2, listw=We, test=c("lml"))
slmtest(Fe2, listw=We, test=c("rlml"))

#In the third column, at the bottom of Table 5
slmtest(Fe3, listw=We, test=c("lme"))
slmtest(Fe3, listw=We, test=c("rlme"))
slmtest(Fe3, listw=We, test=c("lml"))
slmtest(Fe3, listw=We, test=c("rlml"))

#Spatial and non-spatial Hausman tests

##No Spatial Effects - Random effect models
Re1 <- plm(growth_gdp_pc ~ gdp_pc_t_1 +
             IQPA_quintile + I(IQPA_quintile*gdp_pc_t_1) +
             precipitation + I(precipitation^2) +
             temperature + I(temperature^2) +      
             log(secondary_ratio)+
             log(internet_ratio) +
             log(density),
           data = datapanel, 
           model = "random")
summary(Re1)

##No Spatial Effects - Random effect models
Re2 <- plm(growth_gdp_pc ~ gdp_pc_t_1 +
             efficiency_quintile + I(efficiency_quintile*gdp_pc_t_1) +              
             precipitation + I(precipitation^2) +
             temperature + I(temperature^2) +
             log(secondary_ratio)+
             log(internet_ratio) +
             log(density),
           data = datapanel, 
           model = "random")
summary(Re2)

##No Spatial Effects - Random effect models
Re3 <- plm(growth_gdp_pc ~ gdp_pc_t_1 +
             capacity_quintile + I(capacity_quintile*gdp_pc_t_1) +
             precipitation + I(precipitation^2) +
             temperature + I(temperature^2) +
             log(secondary_ratio)+
             log(internet_ratio) +
             log(density),
           data = datapanel, 
           model = "random")
summary(Re3)

##Spatial Effects - SAR models
SAR1_Re <- spml(growth_gdp_pc ~ gdp_pc_t_1 +
                  IQPA_quintile + I(IQPA_quintile*gdp_pc_t_1) +             
                  precipitation + I(precipitation^2) +
                  temperature + I(temperature^2) +
                  log(secondary_ratio)+
                  log(internet_ratio) +
                  log(density),
                data = datapanel, 
                listw = We, spatial.error="none", lag=TRUE, 
                model = "random")
summary(SAR1_Re)

##Spatial Effects - SAR models
SAR2_Re <- spml(growth_gdp_pc ~ gdp_pc_t_1 +
                  efficiency_quintile + I(efficiency_quintile*gdp_pc_t_1) +
                  log(secondary_ratio)+
                  precipitation + I(precipitation^2) +
                  temperature + I(temperature^2) +               
                  log(internet_ratio) +
                  log(density),
                data = datapanel, 
                listw = We, spatial.error="none", lag=TRUE, 
                model = "random")
summary(SAR2)

##Spatial Effects - SAR models
SAR3_Re <- spml(growth_gdp_pc ~ gdp_pc_t_1 +
                  capacity_quintile + I(capacity_quintile*gdp_pc_t_1) +             
                  precipitation + I(precipitation^2) +
                  temperature + I(temperature^2) +
                  log(secondary_ratio)+
                  log(internet_ratio) +
                  log(density),
                data = datapanel, 
                listw = We, spatial.error="none", lag=TRUE, 
                model = "random")
summary(SAR3)

##Spatial Effects - SEM models
SEM1_Re <- spml(growth_gdp_pc ~ gdp_pc_t_1 +
                  IQPA_quintile + I(IQPA_quintile*gdp_pc_t_1) +
                  precipitation + I(precipitation^2) +
                  temperature + I(temperature^2) +     
                  log(secondary_ratio)+
                  log(internet_ratio) +  
                  log(density),
                data = datapanel, 
                listw = We, spatial.error="b", lag=F, 
                model = "random")
summary(SEM1_Re)

##Spatial Effects - SEM models
SEM2_Re <- spml(growth_gdp_pc ~ gdp_pc_t_1 +
                  efficiency_quintile + I(efficiency_quintile*gdp_pc_t_1) +               
                  precipitation + I(precipitation^2) +
                  temperature + I(temperature^2) +
                  log(secondary_ratio)+
                  log(internet_ratio) +
                  log(density),
                data = datapanel, 
                listw = We, spatial.error="b", lag=F, 
                model = "random")
summary(SEM2_Re)

##Spatial Effects - SEM models
SAC1_Re <- spml(growth_gdp_pc ~ gdp_pc_t_1 +
                  IQPA_quintile + I(IQPA_quintile*gdp_pc_t_1) +              
                  precipitation + I(precipitation^2) +
                  temperature + I(temperature^2) +
                  log(secondary_ratio)+
                  log(internet_ratio) +
                  log(density),
                data = datapanel, 
                listw = We, spatial.error="b", lag=T, 
                model = "random")
summary(SAC1_Re)

##Spatial Effects - SEM models
SAC2_Re <- spml(growth_gdp_pc ~ gdp_pc_t_1 +
                  efficiency_quintile + I(efficiency_quintile*gdp_pc_t_1) +                
                  precipitation + I(precipitation^2) +
                  temperature + I(temperature^2) + 
                  log(secondary_ratio)+
                  log(internet_ratio+) +  
                  log(density),
                data = datapanel, 
                listw = We, spatial.error="b", lag=T, 
                model = "random")
summary(SAC2_Re)

##Spatial Effects - SEM models
SAC3_Re <- spml(growth_gdp_pc ~ gdp_pc_t_1 +
                  capacity_quintile + I(capacity_quintile*gdp_pc_t_1) +                
                  precipitation + I(precipitation^2) +
                  temperature + I(temperature^2) +
                  log(secondary_ratio)+
                  log(internet_ratio) +
                  log(density),
                data = datapanel, 
                listw = We, spatial.error="b", lag=T, 
                model = "random")
summary(SAC3_Re)

##Spatial Effects - SDM models
SDM1_Re <- spml(growth_gdp_pc ~ gdp_pc_t_1 +
                  IQPA_quintile + I(IQPA_quintile*gdp_pc_t_1) +
                  precipitation + I(precipitation^2) +
                  temperature + I(temperature^2) +   
                  log(secondary_ratio)+
                  log(internet_ratio) +
                  log(density)+
                  wgdp_pc_t_1+
                  wIQPA+I(wIQPA*wgdp_pc_t_1)+
                  wprecipitation+I(wprecipitation^2)+
                  wtemperature+I(wtemperature^2)+
                  log(wsecondary_ratio)+
                  log(winternet_ratio)+
                  log(wdensity),
                data = datapanel, 
                listw = We, spatial.error="none", lag=T, 
                model = "random")
summary(SDM1_Re)

##Spatial Effects - SDM models
SDM2_Re <- spml(growth_gdp_pc ~ gdp_pc_t_1 +
                  efficiency_quintile + I(efficiency_quintile*gdp_pc_t_1) +                
                  precipitation + I(precipitation^2) +
                  temperature + I(temperature^2) + 
                  log(secondary_ratio)+
                  log(internet_ratio) +
                  log(density)+
                  wgdp_pc_t_1+
                  wefficiency_quintile+I(wefficiency_quintile*wgdp_pc_t_1)+
                  wprecipitation+I(wprecipitation^2)+
                  wtemperature+I(wtemperature^2)+
                  log(wsecondary_ratio)+log(winternet_ratio)+
                  log(wdensity),
                data = datapanel, 
                listw = We, spatial.error="none", lag=T, 
                model = "random")
summary(SDM2_Re)

##Spatial Effects - SDM models
SDM3_Re <- spml(growth_gdp_pc ~ gdp_pc_t_1 +
                  capacity_quintile + I(capacity_quintile*gdp_pc_t_1) +
                  precipitation + I(precipitation^2) +
                  temperature + I(temperature^2) + 
                  log(secondary_ratio)+
                  log(internet_ratio) +
                  log(density)+
                  wgdp_pc_t_1+
                  wcapacity_quintile+I(wcapacity_quintile*wgdp_pc_t_1)+
                  wprecipitation+I(wprecipitation^2)+
                  wtemperature+I(wtemperature^2)+
                  log(wsecondary_ratio)+log(winternet_ratio)+   
                  log(wdensity),
                data = datapanel, 
                listw = We, spatial.error="none", lag=T, 
                model = "random")
summary(SDM3_Re)

##Hausman Test (No spatial effects)
##H:0 The random effects (RE) estimators are consistent and efficient
#IQPA
phtest(Fe1, Re1)

#Efficiency
phtest(Fe2, Re2)

#Capacity
phtest(Fe3, Re3)

##Hausman Test (Spatial effects)
## SAR
##IQPA
H1 <- sphtest(x = SAR1_Re, x2 = SAR1_Fe)
H1

##Efficiency
H2 <- sphtest(x = SAR2_Re, x2 = SAR2_Fe)
H2

##Capacity
H3 <- sphtest(x = SAR3_Re, x2 = SAR3_Fe)
H3

## SEM
##IQPA
H4 <- sphtest(x = SEM1_Re, x2 = SEM1_Fe)
H4

##Efficiency
H5 <- sphtest(x = SEM2_Re, x2 = SEM2_Fe)
H5

##Capacity
H6 <- sphtest(x = SEM3_Re, x2 = SEM3_Fe)
H6

## SAC
##IQPA
H7 <- sphtest(x = SAC1_Re, x2 = SAC1_Fe)
H7

##Efficiency
H8 <- sphtest(x = SAC2_Re, x2 = SAC2_Fe)
H8

##Capacity
H9 <- sphtest(x = SAC3_Re, x2 = SAC3_Fe)
H9

## SDM
##IQPA
H10 <- sphtest(x = SDM1_Re, x2 = SDM1_Fe)
H10

##Efficiency
H11 <- sphtest(x = SDM2_Re, x2 = SDM2_Fe)
H11

##Capacity
H12 <- sphtest(x = SDM3_Re, x2 = SDM3_Fe)
H12

## Direct and Indirect effects #sarfe#
## Convert the spatial weights matrix to a sparse format for efficient computation
W <- as(We, "CsparseMatrix")

## Compute traces of powers of W using Monte Carlo approximation,
# needed for calculating direct, indirect, and total spatial effects
trMC <- trW(W, type="MC")

##SAR Models (Table 6 - Block 1)
impacts_SAR1_Fe<-impacts(SAR1_Fe, tr=trMC, R=100)
summary(impacts_SAR1_Fe, zstats=TRUE, short=TRUE)

##SAR Models (Table 6 - Block 2)
impacts_SAR2_Fe<-impacts(SAR2_Fe, tr=trMC, R=100)
summary(impacts_SAR2_Fe, zstats=TRUE, short=TRUE)

##SAR Models (Table 6 - Block 3)
impacts_SAR3_Fe<-impacts(SAR3_Fe, tr=trMC, R=100)
summary(impacts_SAR3_Fe, zstats=TRUE, short=TRUE)

##SAC Models (Table 7 - Block 1)
impacts_SAC1_Fe<-impacts(SAC1_Fe, tr=trMC, R=100)
summary(impacts_SAC1_Fe, zstats=TRUE, short=TRUE)

##SAC Models (Table 7 - Block 2)
impacts_SAC2_Fe<-impacts(SAC2_Fe, tr=trMC, R=100)
summary(impacts_SAC2_Fe, zstats=TRUE, short=TRUE)

##SAC Models (Table 7 - Block 3)
impacts_SAC3_Fe<-impacts(SAC3_Fe, tr=trMC, R=100)
summary(impacts_SAC3_Fe, zstats=TRUE, short=TRUE)

##SDM Models (Table 8 - Block 1)
impacts_SDM1_Fe<-impacts(SDM1_Fe, tr=trMC, R=100)
summary(impacts_SDM1_Fe, zstats=TRUE, short=TRUE)

##SDM Models (Table 8 - Block 2)
impacts_SDM2_Fe<-impacts(SDM2_Fe, tr=trMC, R=100)
summary(impacts_SDM2_Fe, zstats=TRUE, short=TRUE)

##SDM Models (Table 8 - Block 3)
impacts_SDM3_Fe<-impacts(SDM3_Fe, tr=trMC, R=100)
summary(impacts_SDM3_Fe, zstats=TRUE, short=TRUE)
