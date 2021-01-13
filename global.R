library(shiny)
library(shinydashboard)
library(shinythemes)
#library(SparkR)
library(rCAT)
library(DT)
library(dplyr)
library(tibble)
library(tidyr)
library(plyr)
library(data.table)
library(stringr)
library(ggplot2)
library(RColorBrewer)
library(forcats)

data_folder <- "/Users/aaron/Documents/GitHub/Javelin-Database/data"

filenames = list.files(data_folder, pattern = "*.csv", full.names = T)
dataname = basename(filenames)
dataname <-  str_remove_all(dataname, ".csv")

table1 <-  lapply(filenames, fread, header=TRUE, stringsAsFactors=FALSE)

labels <-  t(data.frame(strsplit(dataname, "_")))
fullnames <-  data.frame(dataname, labels)

Collateddata = NULL
for (k in 1:length(table1)){
  fullnames1 <- fullnames[k,]
  data1 = as.data.frame(table1[[k]])
  Round = as.data.frame(as.numeric(data1[1,2:7]))
  Distance = as.data.frame(as.character(data1[3,2:7]))
  ImpulseContact = as.data.frame(as.numeric(data1[5,2:7]))
  ImpulsePhase = as.data.frame(as.numeric(data1[6,2:7]))
  DeliveryPhase = as.data.frame(as.numeric(data1[7,2:7]))
  ReleasePhase = as.data.frame(as.numeric(data1[8,2:7]))
  AttitudeAngle = as.data.frame(as.numeric(data1[10,2:7]))
  ReleaseAngle = as.data.frame(as.numeric(data1[11,2:7]))
  AttackAngle = as.data.frame(as.numeric(data1[12,2:7]))
  YawAngle = as.data.frame(as.numeric(data1[13,2:7]))
  PKAc = as.data.frame(as.numeric(data1[16,2:7]))
  PKAm = as.data.frame(as.numeric(data1[17,2:7]))
  Difference = as.data.frame(as.numeric(data1[18,2:7]))
  ElbowAngle = as.data.frame(as.numeric(data1[20,2:7]))
  JavForearmAngle = as.data.frame(as.numeric(data1[21,2:7]))
  ISL = as.data.frame(as.numeric(data1[23,2:7]))
  BSL = as.data.frame(as.numeric(data1[24,2:7]))
  ISLBSL = round(((ISL-BSL)/BSL*100),2)
  NADrawLength = as.data.frame(as.numeric(data1[26,2:7]))
  PullLength = as.data.frame(as.numeric(data1[27,2:7]))
  DrawDistance = as.data.frame(as.numeric(data1[28,2:7]))
  ReleaseDistance = as.data.frame(as.numeric(data1[2,2:7]))
  
  ExportData = cbind.data.frame(fullnames1, Round, Distance, ImpulseContact, ImpulsePhase, DeliveryPhase, ReleasePhase,
                                AttitudeAngle,ReleaseAngle, AttackAngle,YawAngle, PKAc, PKAm, Difference, ElbowAngle, 
                                JavForearmAngle, ISL, BSL, ISLBSL, NADrawLength, PullLength, DrawDistance,
                                ReleaseDistance, row.names = NULL)
  
  filename = paste0(ExportData[1,1])
  Collateddata[[filename]] <- ExportData

  
}

Collateddatadf = bind_rows(Collateddata)
  
col_names_collated <- c(
  "dataname", "Name",  "Competition",  "Round", "Distance",  "ImpulseContact", "ImpulsePhase", "DeliveryPhase", "ReleasePhase",
  "AttitudeAngle","ReleaseAngle", "AttackAngle","YawAngle", "PKAc", "PKAm", "Difference", "ElbowAngle", 
  "JavForearmAngle", "ISL", "BSL", "ISLBSL", "NADrawLength", "PullLength", "DrawDistance",
  "ReleaseDistance")  
  
  
colnames(Collateddatadf) <-  col_names_collated
  
  
Collateddatadf <- Collateddatadf %>%dplyr::mutate("Round&Distance" = paste0(Collateddatadf$Round, " ",Collateddatadf$Distance))

Collateddatadf$Distance = as.numeric(Collateddatadf$Distance)  
  
  
  
#imported_data <- read.csv("C:/Users/aaron.beach/..../R/Hammer Conversion/sample2.csv", header=FALSE, stringsAsFactors=FALSE)


