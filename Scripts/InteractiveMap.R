library(dplyr)
library(leaflet)
library(htmltools)

df <- read.csv('E:/documents/CourseWork/DADM/CollegeScorecard_Raw_Data/CollegeScorecard_Raw_Data/MERGED2013_PP.csv', na.strings = "NULL")
names(df)[1]<-"UNITID"
schools <- dplyr::select(df,
                         UNITID,         ## institution ID
                         INSTNM,         ## institution name
                         CITY,           ## institution city
                         STABBR,         ## institution state abbrev
                         ZIP,            ## institution zip
                         PREDDEG,        ## predominate degree
                         CURROPER,       ## currently operating flag
                         TUITIONFEE_IN,  ## in-state tuition and fees
                         LATITUDE,       ## latitude
                         LONGITUDE,      ## longitude
                         DISTANCEONLY,   ## distance only flag
                         DEATH_YR4_RT,   ## student death 
                         COMP_ORIG_YR4_RT, ## student completion
                         GRAD_DEBT_MDN   ## median debt
)   


uniInfo <- paste(schools[['INSTNM']], "<br>", schools[['CITY']], ", ", 
                 schools[['STABBR']], schools[['ZIP']], "<br> Median debt: $", 
                 schools[['GRAD_DEBT_MDN']], sep='')

schools$info <- uniInfo

## filter data
schools<-filter(schools,
                PREDDEG==3 &                        ## Predominate degree is BS
                  CURROPER==1 &                       ## Currently operating
                  DISTANCEONLY==0 &                   ## Not distance
                  is.na(TUITIONFEE_IN)==FALSE &       ## Key measurements aren't missing
                  is.na(LATITUDE)==FALSE &
                  is.na(LONGITUDE)==FALSE &
                  LATITUDE>20 & LATITUDE<50 &         ## Location is US 48
                  LONGITUDE>(-130) & LONGITUDE<(-60))




blingIcon <- makeIcon(
  iconUrl = "http://ih0.redbubble.net/image.57273688.8586/tpr,875x875,s.6u2.png",
  iconWidth = 50, iconHeight = 50,
)



map <- leaflet(schools) %>% 
  setView(-93.65, 42.0285, zoom = 4) %>%
  addTiles() %>%
  addMarkers(~LONGITUDE, ~LATITUDE, popup=~info,
             options = popupOptions(closeButton = TRUE),
             clusterOptions = markerClusterOptions(), 
             icon = blingIcon)


map