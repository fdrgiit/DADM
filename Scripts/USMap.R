rm(list=ls())
cat("\014")
require(dplyr)
require(ggplot2)
require(gridExtra)

################### Clean data ###################

## data comes from https://collegescorecard.ed.gov/data/

## read most recent data which is 2013
data2013<-read.csv(file = "E:/documents/CourseWork/DADM/CollegeScorecard_Raw_Data/CollegeScorecard_Raw_Data/MERGED2013_PP.csv" , na.strings = "NULL")
## I am having an issue with the first column name not reading in correctly
## compensate by renaminig the col (not optimal but I need a work around).
names(data2013)[1]<-"UNITID"

## narrow data
schools<-dplyr::select(data2013,
                       UNITID,         ## Institution ID
                       INSTNM,         ## Institution name
                       PREDDEG,        ## Predominate degree
                       CURROPER,       ## Currently operating flag
                       TUITIONFEE_IN,  ## In-state tuition and fees
                       LATITUDE,
                       LONGITUDE,
                       DISTANCEONLY,
                       STABBR)   ## Distance only flag 

## filter data
schools<-filter(schools,
                PREDDEG==3 &                        ## Predominate degree is BS
                  CURROPER==1 &                       ## Currently operating
                  DISTANCEONLY==0 &                   ## Not distance
                  is.na(TUITIONFEE_IN)==FALSE &       ## Key measurements aren't missing
                  is.na(LATITUDE)==FALSE &
                  is.na(LONGITUDE)==FALSE &
                  LATITUDE>25 & LATITUDE<50 &         ## Location is US 48
                  LONGITUDE>(-130) & LONGITUDE<(-60) &
                 (STABBR=='NY' | STABBR=='NJ' | STABBR=='CT' | STABBR=='DE' | STABBR=='DC' | STABBR=='MD' | STABBR=='PA' | STABBR=='VT' | STABBR=='MA' | STABBR=='RI' | STABBR=='OH' | STABBR=='VA' | STABBR=='WV' | STABBR=='ME' | STABBR=='NH' | STABBR=='NC' | STABBR=='GA' | STABBR=='SC' | STABBR=='FL')
                )

## add a tuition factor for tuition range
cuts<-c(0, 10000, 20000, 30000, 40000, 50000)
labs<-c("$0-9.999K","$10K-19.999K", "$20K-29.999K", "$30K-39.999K", "$40K+")
schools$tuition<-cut(schools$TUITIONFEE_IN, 
                     cuts, right=FALSE, labels=labs)

################### Plot US Map ###################

## define colors to use for each tuition range
tuitionColors <- c("#C0FF00", "#FFFFC0", "#C0C0FF", "#C00000", "#753656")
names(tuitionColors) <- levels(schools$tuition)

us <- NULL
us <- ggplot(schools) 
us <- us + borders("state", colour="pink", fill="gray0")
us <- us + geom_point(mapping=aes(x=LONGITUDE, y=LATITUDE, 
                                  colour=tuition),  size=3)
us <- us + ggtitle("IN STATE TUITION FEE DISTRIBUTION") 
us <- us + scale_colour_manual(name = "In State Tuition",values = tuitionColors)
us <- us + theme(legend.position="top", legend.box="horizontal", 
                 legend.text=element_text(size=10), 
                 legend.title=element_text(size=10))
us
## add a black and white version

tuitionColorsBW <- c("gray80", "gray60", "gray40", "gray20", "gray0")
names(tuitionColorsBW) <- levels(schools$tuition)

bw <- NULL
bw <- ggplot(schools) 
bw <- bw + borders("state", colour="black", fill="white")
bw <- bw + geom_point(mapping=aes(x=LONGITUDE, y=LATITUDE, 
                                  color=tuition),  size=3, shape=20)
bw <- bw + ggtitle("Black and White Version") 
bw <- bw + scale_color_manual(name ="In State Tuition", values=tuitionColorsBW)
bw <- bw + theme(legend.position="top", legend.box="horizontal", 
                 legend.text=element_text(size=10), 
                 legend.title=element_text(size=10))
bw

