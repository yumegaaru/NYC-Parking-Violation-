load("C:/Users/jaeye/Dropbox/521/hw6-Team06/manh.Rdata")
load("C:/Users/jaeye/Dropbox/521/hw6-Team06/nyc_parking.Rdata")
load("C:/Users/jaeye/Dropbox/521/hw6-Team06/pluto.Rdata")

library(sf)
library(raster)
library(dplyr)
library(ggplot2)
library(readr)
library(dplyr)
library(magrittr)
library(stringr)
library(caret)
library(tidyr)
library(caretEnsemble)






## In order to Enlarge the Data Set, I needed to integrate the format of the data
nyc$address = 
  nyc$address %>% str_replace_all(.,"BLVD", "BL") %>% 
  str_replace_all(.,"STREET","ST") %>%
  str_replace_all(., "AVENUE", "AVE") %>%
  str_replace_all(.,"EAST","E") %>% 
  str_replace_all(., "WEST", "W") %>%
  str_replace_all(., "\\sDRIVE$","\\sDR$") %>%
  str_replace_all(.,"\\sPLACE$","\\sPL$")

pluto$address = 
  pluto$address %>% str_replace_all(.,"BLVD", "BL") %>% 
  str_replace_all(.,"STREET","ST") %>%
  str_replace_all(., "AVENUE", "AVE") %>%
  str_replace_all(.,"EAST","E") %>% 
  str_replace_all(., "WEST", "W") %>%
  str_replace_all(., "\\sDRIVE$","\\sDR$") %>%
  str_replace_all(.,"\\sPLACE$","\\sPL$")

final = inner_join(
  mutate(nyc, address=tolower(address)), 
  mutate(pluto, address=tolower(address)),
  by="address"
)

# Sampling 22nd District(Central Park), The coordinates are from gooogle map. I just basically extracted every corner of central park
## c(40.800299, -73.958244) Left Top  c(40.800431, -73.957805)
## c(40.797046, -73.949734) Right top   c(40.797028, -73.949769)
## c(40.765014, -73.972625) Right bottom c(40.764738, -73.973668)
## c(40.768475, -73.981494) Left Bottom c(40.767930, -73.981216)

bd_22 =matrix(c(40.800299, -73.958244,40.800431, -73.957805,40.797046, -73.949734,40.797028, -73.949769,40.765014, 
                       -73.972625,40.764738, -73.973668,40.768475, -73.981494,40.767930, -73.981216),ncol=2,byrow=TRUE)
bd_22 = as.data.frame(bd_22)
min_max = data.frame(
  xmin = min(bd_22$V1), xmax = max(bd_22$V1), ymin =min(bd_22$V2), ymax = max(bd_22$V2)
)

X = seq(min_max$xmin, min_max$xmax, 0.000015)
Y = seq(min_max$ymin, min_max$ymax, 0.000015)

grid_22 = expand.grid(X=X, Y=Y) %>%
  as.matrix() %>%
  st_multipoint() %>%
  st_sfc() %>%
  st_set_crs(st_crs(manh))

# Outputs the empty set!!
manh_pts_22 = st_intersection(st_geometry(manh),grid_22) %>% st_cast("POINT")

manh_xy_22 = st_coordinates(manh_pts_22) %>% as.data.frame()

manh_xy_22 = manh_xy_22 %>% mutate(
  precinct = 22,
  address = NA,
  X=X,
  Y=Y
)


#############Start Running Models#####################################################3
library(caret)
library(caretEnsemble)
library(dplyr)
dplyr::select
# Making precinct variable into categorical var.
final_rf = final %>% select(-address) %>% mutate(precinct = as.factor(precinct)) 

##Dummy Variable
#dmy<-dummyVars("~precinct",data=final_rf)
#tsrf<-data.frame(predict(dmy,newdata=final_rf))



###Test
library(mlbench)
library(pROC)
data(Sonar)

my_control<-trainControl(
  method = "boot",
  number=25, #Number of resampling iterations
  savePredictions = "final", #how much you would want the result to be saved, "final" = predictions for the optimal tuning parameter
  classProbs = TRUE,
  index = createResample(final_rf$precinct,1000),
  summaryFunction = twoClassSummary
)

# Which Model to Use and fit the model
model_list<-caretList(precinct~.,data=final_rf,
                      trControl = my_control,
                      methodList = c("ORFsvm", "ORFridge"))

# Making Prediction with cross-validation set(manh_xy)
predict<-predict(model_list, newdata=manh_xy))





