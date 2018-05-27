##############################################################
# Title: Geospatial analysis of student achievement
#        based on EQAO math scores in Ontario
# Author: Anthony Ionno
# Date: 2018-05-26
##############################################################

#########################
# R Libraries
#########################

# Reading packages
library(dplyr);library(magrittr);library(ggplot2);library(readr)
library(readxl);library(extrafont);library(tmap);library(rgdal);
library(raster);library(tmaptools)

#############################
# Preprocessing
#############################

# Student grades and LI information 

# Reading geospatial information on student achievement and LI proportions
df<-read_xlsx("2018-05-12_Data/School information and student demographics/sif_data_table_2015_2016_en.xlsx",
              sheet=1)
# Code lists to remove NA values
code.list<-c("A/D","NA","N/R","N/A","S. R.","N/D")
code.list2<-c("NA","SP")

# Creating and then merging separate grade 3 and 6 data-frames, simplifying
# df's to only contain information needed for analysis
grade3math<-df %>%
  select(`School Name`,`Percentage of Grade 3 Students Achieving the Provincial Standard in Mathematics`,
         `Percentage of Children Who Live in Low-Income Households`,
         Latitude,
         Longitude,
         Enrolment,
         `Postal Code`
  ) %>%
  filter(!(`Percentage of Grade 3 Students Achieving the Provincial Standard in Mathematics` %in% code.list),
         !(`Percentage of Children Who Live in Low-Income Households` %in% code.list2))%>%
  mutate(Grade="Grade 3",FSA=substr(`Postal Code`,1,3))
names(grade3math)[2]<-"Score"
names(grade3math)[3]<-"LI"
grade3math$Score<-as.numeric(grade3math$Score)
grade3math$LI<-as.numeric(grade3math$LI)

grade6math<-df %>%
  select(`School Name`,`Percentage of Grade 6 Students Achieving the Provincial Standard in Mathematics`,
         `Percentage of Children Who Live in Low-Income Households`,
         Latitude,
         Longitude,
         Enrolment,
         `Postal Code`) %>%
  filter(!(`Percentage of Grade 6 Students Achieving the Provincial Standard in Mathematics` %in% code.list),
         !(`Percentage of Children Who Live in Low-Income Households` %in% code.list2))%>%
  mutate(Grade="Grade 6",FSA=substr(`Postal Code`,1,3))
names(grade6math)[2]<-"Score"
names(grade6math)[3]<-"LI"
grade6math$LI<-as.numeric(grade6math$LI)
grade6math$Score<-as.numeric(grade6math$Score)

# Class size information #

class.size<-read_xlsx("2018-05-12_Data/Primary Class Size/2016-10oct-20_-ontario_open_data_-_all_classes_1.xlsx",sheet=9)
head(class.size)
# A tibble: 6 x 9
# BOARDNAME                SCHOOLNAME CLASS_NUMBER    JK
# <chr>                     <chr>        <dbl> <dbl>
#   1 Algoma District School Board Anna McCrea Public School            1    12
# 2 Algoma District School Board Anna McCrea Public School            2     0
# 3 Algoma District School Board Anna McCrea Public School            3     0
# 4 Algoma District School Board Anna McCrea Public School            4     0
# 5 Algoma District School Board Anna McCrea Public School            5     0
# 6 Algoma District School Board Anna McCrea Public School            6     0
# # ... with 5 more variables: K <dbl>, G1 <dbl>, G2 <dbl>, G3 <dbl>,
# #   G4TO8 <dbl>
names(class.size)
# [1] "BOARDNAME"    "SCHOOLNAME"   "CLASS_NUMBER" "JK"           "K"           
# [6] "G1"           "G2"           "G3"           "G4TO8" 
class.size<-class.size %>%
  group_by(SCHOOLNAME) %>%
  summarise(G3ClassSize=sum(G3),G4TO8ClassSize=sum(G4TO8))
names(class.size)[1]<-"School Name"

# Merging class size, achievement and LI information
grade3math<-left_join(grade3math,class.size[,1:2])
nrow(grade3math)
# [1] 3322
# Note: Lose 436 GR3 schools due to school names being labelled differently
# between primary class size and student achievement datasets.
grade3math<-grade3math[complete.cases(grade3math),] 
# [1] 2886
grade6math<-left_join(grade6math,class.size[,c(1,3)])
nrow(grade6math)
# [1] 3140
# Note: Lose 439 GR6 schools due to school names being labelled differently
# between primary class size and student achievement datasets.
grade6math<-grade6math[complete.cases(grade6math),] 
nrow(grade6math)
# [1] 2701

# Creating new weighted achievement variable by FSA
grade3math2<- grade3math %>%
  mutate(NumStudents=Score*G3ClassSize) %>%
  group_by(FSA) %>%
  summarise(FSAScore=sum(NumStudents)/sum(G3ClassSize))

# Geospatial shapefile information #
shp <- shapefile("G:/Blog/2018-01-27_Toronto-Paramedic-Services-Incident-Post/Data/lfsa000b16a_e.shp") 
Ontario_shp<-shp[shp@data$PRNAME=="Ontario",] 
names(Ontario_shp@data)<-c("FSA","ID","PRNAME")

# Merging student achievement data with geo spatial data
Ontario_shp@data<-left_join(Ontario_shp@data,grade3math2)
names(Ontario_shp@data)[4]<-"Percent of Grade 3 Students"
Ontario_shp@data$`Percent of Grade 3 Students`<-Ontario_shp@data$`Percent of Grade 3 Students`*100

Ontario_shp@data<-left_join(Ontario_shp@data,grade6math2)
names(Ontario_shp@data)[5]<-"Percent of Grade 6 Students"
Ontario_shp@data$`Percent of Grade 6 Students`<-Ontario_shp@data$`Percent of Grade 6 Students`*100

################################
# Analysis
################################

Ontario_FSA_Map<-
  tm_shape(Ontario_shp) +
  tm_polygons("Percent of Grade 3 Students",
              palette=c("#F46D43","#FFFFBF","#66C2A5"),
              border.col = "grey45") +
  tm_layout(legend.bg.color = "grey90", legend.bg.alpha=.5, legend.frame=TRUE)
Ontario_leaflet<-tmap_leaflet(Ontario_FSA_Map)
Ontario_leaflet %>%
  leaflet::setView(lat = 50,lng = -85,zoom = 5)

Ontario_FSA_Map2<-
  tm_shape(Ontario_shp) +
  tm_polygons("Percent of Grade 6 Students",
              palette=c("#F46D43","#FFFFBF","#66C2A5"),
              border.col = "grey45") +
  tm_layout(legend.bg.color = "grey90", legend.bg.alpha=.5, legend.frame=TRUE)
Ontario_leaflet<-tmap_leaflet(Ontario_FSA_Map2)
Ontario_leaflet %>%
  leaflet::setView(lat = 50,lng = -85,zoom = 5)
