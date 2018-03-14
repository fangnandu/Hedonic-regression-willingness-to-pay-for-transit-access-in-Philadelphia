#1.1 the kitchen sink model- mapping the denpendent variable
setwd
install.packages('ggmap')
install.packages('sf')
install.packages('tidyverse')
install.packages('ggplot')
install.packages('ggplot2')
install.packages('readr')
library(ggplot2)
library(ggmap)
library(sf)
library(tidyverse)
library(readr)
#load the csv file
hed<-read.csv("saleprice_R.txt")
#load the basemap(pay attention to the disable of maptheme)
baseMap <- get_map(location = c(lon = -75.137259, lat = 39.977981), 
                   source = "stamen", 
                   zoom = 11, 
                   maptype= 'toner')
ggmap(baseMap)
#load the home sales on the map
ggmap(baseMap) + 
  geom_point(data = hed, 
             aes(x=longitude, y=latitude, color=inf_prc_ft), 
             size = .1) + 
  labs(title="Price per square foot, Philadelphia") +
  scale_color_gradient(low="yellow", high="blue")
#try to make this map be the raster map
InterpolatedMap <- 
  ggmap(baseMap) +
  stat_summary_2d(geom = "tile",
                  bins = 80,
                  data=hed, 
                  aes(x = longitude, y = latitude, z = inf_prc_ft)
  ) +
  scale_fill_gradient(low = "yellow", 
                      high = "blue", 
                      guide = guide_legend(title = "Price per Square Foot")) +
  labs(title="Price per square foot, Philadelphia",
       subtitle="Interpolated")
InterpolatedMap

#the kitchen sink model-scatterplots

ggplot(data = hed, aes(x =inf_prc_ft , y =d_crime)) +
  geom_point(color="#33ccff", size=0.1)+
  labs(title="the scatterplot of price per square foot and crime",
    x="Price per square foot",
       y="crime")

ggplot(data = hed, aes(x =inf_prc_ft , y =d_cbd)) +
  geom_point(color="#33ccff", size=0.1)+
  labs(title="the scatterplot of price per square foot and distance to cbd",
       x="Price per square foot",
       y="Distance to cbd")     


ggplot(data = hed, aes(x =inf_prc_ft , y =d_septa)) +
  geom_point(color="#33ccff", size=0.1)+
  labs(title="the scatterplot of price per square foot and distance to speta",
       x="Price per square foot",
       y="Distance to septa")

ggplot(data = hed, aes(x =inf_prc_ft , y =d_business)) +
  geom_point(color="#33ccff", size=0.1)+
  labs(title="the scatterplot of price per square foot and distance to business",
       x="Price per square foot",
       y="distance to business")

#1.3 the kitchen sink model-regression model

ggplot(hed, aes(inf_prc_ft)) + geom_histogram(bins=100) +
  labs(x="Price per square foot",
       y="Count")

ggplot(hed, aes(log(inf_prc_ft))) + geom_histogram(bins=100) +
  labs(title="the histogram for log of price per square foot",
       x="Log of price per square foot",
       y="Count")

regA <- lm(log(inf_prc_ft) ~ d_septa, data = hed )
summary(regA)

reg4 <- lm(log(inf_prc_ft) ~ d_septa+sale_price+livable_ar+d_parks+MED_YR_HS+pct_non_wh+d_business+d_crime+d_cbd+density+d_h_ramps+d_off_site+P_ADV_MA_1+d_septa+d_abate+d_vacant+d_walk, data = hed )
summary(reg4)
regfinal<-lm(formula = log(inf_prc_ft) ~ d_septa + sale_price  + 
     livable_ar + d_parks + MED_YR_HS + 
     pct_non_wh + d_business + d_crime + d_cbd + density + d_h_ramps + 
     d_off_site + P_ADV_MA_1 + d_septa + d_abate + d_vacant + 
     d_walk, data = hed)
summary(regfinal)
names(regfinal)


regfinal_residuals <- data.frame(regfinal$residuals)
hedLonLat <- data.frame(hed$longitude, hed$latitude)
residualsToMap <- cbind(hedLonLat, regfinal_residuals )
colnames(residualsToMap) <- c("longitude", "latitude", "residual")





reg10<-lm(formula = log(inf_prc_ft) ~ d_septa   + 
                d_parks  + 
               pct_non_wh + d_business + d_crime + d_cbd + density + 
               d_walk, data = hed)
summary(reg10)
a<-100 * (exp(reg10$coefficient) - 1)
a*1000

regfinal_twovalue <- data.frame(reg10$residuals, reg10$fitted.values)

#test1 for the homoskedasticity
ggplot(data = regfinal_twovalue, aes(x =reg10.residuals, y =reg10.fitted.values)) +
  geom_point(color="#33ccff", size=0.1)+
  labs(title="the scatterplot of residuals and fitted.values",
       x="residual",
       y="fitted.values")
#test2 for the residual normality
ggplot(regfinal_twovalue, aes(reg10.residuals)) + geom_histogram(bins=20) +
  labs(title="the histogram of residuals",
       x="residuals",
       y="Count")
#test3 for the independence
regfinal_residuals <- data.frame(regfinal_twovalue$reg10.residuals)
hedLonLat <- data.frame(hed$longitude, hed$latitude)
residualsToMap <- cbind(hedLonLat, regfinal_residuals )
colnames(residualsToMap) <- c("longitude", "latitude", "residual")


invert <- function(x) rgb(t(255-col2rgb(x))/255)    
baseMap_invert <- as.raster(apply(baseMap, 2, invert))
class(baseMap_invert) <- class(baseMap)
attr(baseMap_invert, "bb") <- attr(baseMap, "bb")
ggmap(baseMap_invert)


ggmap(baseMap_invert) + 
  geom_point(data = residualsToMap, 
             aes(x=longitude, y=latitude, color=residual), 
             size = 0.00000001) + 
  labs(title="Residual, Philadelphia") +
  scale_color_gradient(low="green", high="pink")
install.packages('spdep')
library(spdep)
#moran's i
coords <- cbind(hed$longitude, hed$latitude)
spatialWeights <- knn2nb(knearneigh(coords, 4))
moran.test(reg10$residuals, nb2listw(spatialWeights, style="W"))

#2.1

ggmap(baseMap_invert) + 
  geom_point(aes(x=longitude, y=latitude, color=factor(lt_qrtMi)), 
             size = .1, 
             data = hed) + 
  scale_colour_manual(
    values = c("orange","darkgreen"),
    labels=c("> 1/4 mi.","< 1/4 mi"),
    name="group") +
  labs(title="Home sales 1/4mi from and away transit stops")  +
  theme(legend.position = "bottom",legend.key=element_rect(fill = NA),legend.key.size = unit(0.3,"line") )+     
          guides(colour = guide_legend(override.aes = list(size=5)))

        
hed2 <-
  hed %>%
  filter(QrtMiDist <= 1320)
ggmap(baseMap_invert) + 
  geom_point(aes(x=longitude, y=latitude, color=factor(lt_qrtMi)), 
             size = .1, 
             data = hed2) + 
  scale_colour_manual(
    values = c("orange","darkgreen"),
    labels=c("Control","Treatment"),
    name="Group") +
  labs(title="Control and treatment group sales") +
  theme(legend.position = "bottom")


hed2 <-
  hed2 %>%
  mutate(QrtMiDist2 = ifelse(lt_qrtMi== 0, QrtMiDist * -1, QrtMiDist ))
library(ggplot2)
discontinuityPlot <- 
  ggplot(hed2, aes(QrtMiDist2, inf_prc_ft, group=lt_qrtMi)) + 
  geom_point(size=1) +
  stat_smooth(method = "lm", se = FALSE, size = 1) + #what kind of regression function line should be used?
  geom_vline(xintercept = 0, colour = "red", linetype= "longdash", size=1) + 
  geom_vline(xintercept = max(hed2$QrtMiDist2), colour = "darkgreen", linetype= "longdash", size=2) +
  annotate("text", x = 50, size=6, y = 400, angle=90, colour="red", label = "Quarter Mile Boundary") +
  annotate("text", x = max(hed2$QrtMiDist2) + 50, size=6, y = 400, angle=90, colour="darkgreen", label = "Subway") +
  labs(title="Discontinuity of willingness to pay for transit in Philadelphia",
       subtitle="the difference of price identifies the willingness to pay for transit",
       caption="The obvious diffrence of prices makes the assumption robust",
       X="Price per square foot",
       y="Price")
discontinuityPlot


#2.2
install.packages('sf')
library(sf)

transitStops <- st_read("Phila_transit_stops.shp")
library(dplyr)
library(tidyr)
hedSummary <-
  hed2 %>%
  group_by(lt_qrtMi, STATION) %>%
  summarize(MeanPrice = mean(inf_prc_ft)) %>%
  spread(lt_qrtMi, MeanPrice) %>%
  mutate(difference = .[[3]] - .[[2]]) %>%
  select(STATION,difference) %>%
  filter(STATION != "30TH STREET") %>%
  left_join(transitStops, by="STATION") 

library(ggmap)
ggmap(baseMap_invert) + 
  geom_point(data=hedSummary, 
             aes(LONGTUDE, LATITUDE, size=factor(ntile(difference,5))),
             colour="red") + 
  scale_size_manual(values = c(1,2,3,4,5),
                    labels=as.character(quantile(hedSummary$difference,
                                                 c(.1,.2,.4,.6,.8),na.rm=T)),
                    name="Difference\n(Quintile\nBreaks)") +
  labs(title="The transit discontinuity - Graduated Symbol Map",
       subtitle= "the difference in home prices within or out 1/4 miles by each station",
       caption="       The center stations have the most obvious transit discontinuity")
#2.3

hist(hed2$inf_prc_ft)

reg1 <- lm(log(inf_prc_ft) ~ lt_qrtMi, data = hed2 )
summary(reg1)

reg2 <- lm(log(inf_prc_ft) ~ STATION + lt_qrtMi, data = hed2 )
summary(reg2)
reg3<- lm(log(inf_prc_ft) ~ pct_non_wh + lt_qrtMi, data = hed2 )
summary(reg3)

install.packages('stargazer')
library(stargazer)
regression_results <- stargazer(reg1, reg2, reg3, type="html", single.row=FALSE, digits=4, align=TRUE,  column.labels=c("Just the fixed effect","w/ station fixed effects", "w/ other variables"))

regfinal_residuals <- data.frame(regfinal_twovalue$reg10.residuals)
hedLonLat <- data.frame(hed$longitude, hed$latitude)
residualsToMap <- cbind(hedLonLat, regfinal_residuals )
colnames(residualsToMap) <- c("longitude", "latitude", "residual")

#reg1
regfinal_twovalue <- data.frame(reg1$residuals, reg1$fitted.values)
regfinal_residuals <- data.frame(regfinal_twovalue$reg1.residuals)
hedLonLat <- data.frame(hed2$longitude, hed2$latitude)
residualsToMap <- cbind(hedLonLat, regfinal_residuals )
colnames(residualsToMap) <- c("longitude", "latitude", "residual")


ggmap(baseMap_invert) + 
  geom_point(data = residualsToMap, 
             aes(x=longitude, y=latitude, color=residual), 
             size = 0.00000001) + 
  labs(title="Residual of reg 1, Philadelphia") +
  scale_color_gradient(low="green", high="pink")
install.packages('spdep')
library(spdep)
#moran's i
coords <- cbind(hed2$longitude, hed2$latitude)
spatialWeights <- knn2nb(knearneigh(coords, 4))
moran.test(reg1$residuals, nb2listw(spatialWeights, style="W"))

#reg 2 
regfinal_twovalue <- data.frame(reg2$residuals, reg2$fitted.values)
regfinal_residuals <- data.frame(regfinal_twovalue$reg2.residuals)
hedLonLat <- data.frame(hed2$longitude, hed2$latitude)
residualsToMap <- cbind(hedLonLat, regfinal_residuals )
colnames(residualsToMap) <- c("longitude", "latitude", "residual")


ggmap(baseMap_invert) + 
  geom_point(data = residualsToMap, 
             aes(x=longitude, y=latitude, color=residual), 
             size = 0.00000001) + 
  labs(title="Residual of reg 2, Philadelphia") +
  scale_color_gradient(low="green", high="pink")
install.packages('spdep')
library(spdep)
#moran's i
coords <- cbind(hed2$longitude, hed2$latitude)
spatialWeights <- knn2nb(knearneigh(coords, 4))
moran.test(reg2$residuals, nb2listw(spatialWeights, style="W"))

#reg 3
regfinal_twovalue <- data.frame(reg3$residuals, reg3$fitted.values)
regfinal_residuals <- data.frame(regfinal_twovalue$reg3.residuals)
hedLonLat <- data.frame(hed2$longitude, hed2$latitude)
residualsToMap <- cbind(hedLonLat, regfinal_residuals )
colnames(residualsToMap) <- c("longitude", "latitude", "residual")


ggmap(baseMap_invert) + 
  geom_point(data = residualsToMap, 
             aes(x=longitude, y=latitude, color=residual), 
             size = 0.00000001) + 
  labs(title="Residual of reg 3, Philadelphia") +
  scale_color_gradient(low="green", high="pink")
install.packages('spdep')
library(spdep)
#moran's i
coords <- cbind(hed2$longitude, hed2$latitude)
spatialWeights <- knn2nb(knearneigh(coords, 4))
moran.test(reg3$residuals, nb2listw(spatialWeights, style="W"))
