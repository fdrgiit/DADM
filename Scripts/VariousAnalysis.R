library(RSQLite)
db <- dbConnect(dbDriver("SQLite"), "database.sqlite")

library(xtable)
print.table <- function(table) {
  html <- print(xtable(table), type="html", print.results=FALSE, include.rownames=FALSE)
  cat(paste0("<div style='width:800; overflow:auto; border-width: 2;'><style>td {padding: 3px;} th {padding: 3px;}</style>", html, "</div>"))
}
library(dplyr)
tables <- dbGetQuery(db, "SELECT Name FROM sqlite_master WHERE type='table'")
colnames(tables) <- c("Name")
tables <- tables %>%
  rowwise() %>%
  mutate(RowCount=dbGetQuery(db, paste0("SELECT COUNT(Id) RowCount FROM ", Name))$RowCount[1])
print.table(tables)

library(ggvis)
dbGetQuery(db, "SELECT Year, COUNT(Id) Number_of_Schools FROM Scorecard 
           WHERE (STABBR=='NY' OR STABBR=='NJ' OR STABBR=='CT' OR STABBR=='DE' OR STABBR=='DC' OR STABBR=='MD' OR STABBR=='PA' OR STABBR=='VT' OR STABBR=='MA' OR STABBR=='RI' OR STABBR=='OH' OR STABBR=='VA' OR STABBR=='WV' OR STABBR=='ME' OR STABBR=='NH' OR STABBR=='NC' OR STABBR=='GA' OR STABBR=='SC' OR STABBR=='FL')
           AND Year > 2003
           GROUP BY Year") %>%
  ggvis(~Year, ~Number_of_Schools) %>%
  layer_bars(fill:="#677536")

dbGetQuery(db, "SELECT CONTROL, COUNT(Id) Number_of_Schools FROM Scorecard 
           WHERE (STABBR=='NY' OR STABBR=='NJ' OR STABBR=='CT' OR STABBR=='DE' OR STABBR=='DC' OR STABBR=='MD' OR STABBR=='PA' OR STABBR=='VT' OR STABBR=='MA' OR STABBR=='RI' OR STABBR=='OH' OR STABBR=='VA' OR STABBR=='WV' OR STABBR=='ME' OR STABBR=='NH' OR STABBR=='NC' OR STABBR=='GA' OR STABBR=='SC' OR STABBR=='FL')
           AND Year > 2003
           GROUP BY CONTROL") %>%
  ggvis(~CONTROL, ~Number_of_Schools) %>%
  layer_bars(fill:="#677596")

earnings <- dbGetQuery(db, "
SELECT s11.INSTNM College,
       s11.CONTROL CollegeType,
       s11.STABBR State,
       s11.md_earn_wne_p10 e50,
       s11.pct10_earn_wne_p10 e10,
       s11.pct25_earn_wne_p10 e25,
       s11.pct75_earn_wne_p10 e75,
       s11.pct90_earn_wne_p10 e90
FROM Scorecard s11
INNER JOIN Scorecard s13 ON s11.UNITID=s13.UNITID
WHERE s11.Year=2011
  AND s13.Year=2013
  AND s11.pct75_earn_wne_p10 IS NOT NULL
  AND s11.pct75_earn_wne_p10 != 'PrivacySuppressed'
  AND s11.PREDDEG = 'Predominantly bachelor''s-degree granting'
  AND (s11.STABBR=='NY' OR s11.STABBR=='NJ' OR s11.STABBR=='CT' OR s11.STABBR=='DE' OR s11.STABBR=='DC' OR s11.STABBR=='MD' OR s11.STABBR=='PA' OR s11.STABBR=='VT' OR s11.STABBR=='MA' OR s11.STABBR=='RI' OR s11.STABBR=='OH' OR s11.STABBR=='VA' OR s11.STABBR=='WV' OR s11.STABBR=='ME' OR s11.STABBR=='NH' OR s11.STABBR=='NC' OR s11.STABBR=='GA' OR s11.STABBR=='SC' OR s11.STABBR=='FL')
  AND s13.CCBASIC NOT LIKE '%Special%'
ORDER BY s11.pct75_earn_wne_p10 DESC")
earnings <- cbind(Rank=1:nrow(earnings), earnings)
earnings$College <- paste(earnings$Rank, earnings$College, sep=". ")
earnings$College <- factor(earnings$College, levels=rev(earnings$College))

library(ggplot2)
g_control<-ggplot(earnings, aes(x=e50, color=CollegeType, fill=CollegeType, group=CollegeType)) +
  geom_density(alpha=0.3) +
  theme_light(base_size=16) +
  xlab("Median Earnings 10 Years after Matriculation") + ylab("")

earning<-earnings[which(earnings$CollegeType=='Private nonprofit'),]

g_nonprofit<-ggplot(earning[1:20,], aes(x=College, ymin=e10, lower=e25, middle=e50, upper=e75, ymax=e90)) +
  geom_boxplot(stat="identity", fill="#53c1ee") + 
  geom_text(aes(x=College, y=e75-2000, ymax=e75, hjust=0.95, label=paste0("$", e75)), size=4) + 
  theme_light(base_size=16) +
  theme(axis.text.y = element_text(hjust=0, color="black"), axis.text.x=element_blank()) +
  coord_flip() +
  xlab("") + ylab("") +
  ggtitle("Non-Profit")

earning<-earnings[which(earnings$CollegeType=='Public'),]

g_public<-ggplot(earning[1:20,], aes(x=College, ymin=e10, lower=e25, middle=e50, upper=e75, ymax=e90)) +
  geom_boxplot(stat="identity", fill="#53c1ee") + 
  geom_text(aes(x=College, y=e75-2000, ymax=e75, hjust=0.95, label=paste0("$", e75)), size=4) + 
  theme_light(base_size=16) +
  theme(axis.text.y = element_text(hjust=0, color="black"), axis.text.x=element_blank()) +
  coord_flip() +
  xlab("") + ylab("") +
  ggtitle("Public")

earning<-earnings[which(earnings$CollegeType=='Private for-profit'),]

g_profit<-ggplot(earning[1:20,], aes(x=College, ymin=e10, lower=e25, middle=e50, upper=e75, ymax=e90)) +
  geom_boxplot(stat="identity", fill="#53c1ee") + 
  geom_text(aes(x=College, y=e75-2000, ymax=e75, hjust=0.95, label=paste0("$", e75)), size=4) + 
  theme_light(base_size=16) +
  theme(axis.text.y = element_text(hjust=0, color="black"), axis.text.x=element_blank()) +
  coord_flip() +
  xlab("") + ylab("") +
  ggtitle("Profit")

top <- "Top Quartile Earnings 10 Years After Matriculation (2013)"
top_grob = textGrob(top, gp=gpar(fontsize=32, 
                                 fontface="bold"))
bottom <- paste("College Scorecard", sep="")

bottom_grob = textGrob(bottom, gp=gpar(fontsize=20))
grid.arrange(g_control, g_profit, g_public, g_nonprofit, nrow=2, ncol=2, 
             top = top_grob, bottom = bottom_grob)

sat <- dbGetQuery(db, "
SELECT INSTNM College,
                  SATMTMID Math,
                  SATVRMID Verbal,
                  SATWRMID Writing
                  FROM Scorecard
                  WHERE Year=2013
                  AND (STABBR=='NY' OR STABBR=='NJ' OR STABBR=='CT' OR STABBR=='DE' OR STABBR=='DC' OR STABBR=='MD' OR STABBR=='PA' OR STABBR=='VT' OR STABBR=='MA' OR STABBR=='RI' OR STABBR=='OH' OR STABBR=='VA' OR STABBR=='WV' OR STABBR=='ME' OR STABBR=='NH' OR STABBR=='NC' OR STABBR=='GA' OR STABBR=='SC' OR STABBR=='FL')
                  AND SATMTMID IS NOT NULL
                  AND SATMTMID != 'PrivacySuppressed'
                  AND SATVRMID IS NOT NULL
                  AND SATVRMID != 'PrivacySuppressed'
                  AND SATWRMID IS NOT NULL
                  AND SATWRMID != 'PrivacySuppressed'")

library(tidyr)
g_plot<-ggplot(sat %>% gather(Section, Score, -College), aes(x=Score, color=Section, fill=Section, group=Section)) +
  geom_density(alpha=0.3) +
  theme_light(base_size=16) +
  xlab("SAT Score") +
  ylab("")

#library(dplyr)
topSatMath <- sat %>% arrange(desc(Math)) %>% head(n=20)
topSatMath <- cbind(Rank=1:nrow(topSatMath), topSatMath)
topSatMath$College <- paste(topSatMath$Rank, topSatMath$College, sep=". ")
topSatMath$College <- factor(topSatMath$College, levels=rev(topSatMath$College))

g_math<-ggplot(topSatMath, aes(x=College, y=Math)) +
  geom_bar(stat="identity", fill="#53cfff") + 
  geom_text(aes(x=College, y=Math-5, ymax=Math, hjust=1, label=Math), size=4) + 
  theme_light(base_size=16) +
  theme(axis.text.y = element_text(hjust=0, color="black"), axis.text.x=element_blank()) +
  xlab("") + ylab("") +
  coord_flip(ylim=c(600, 800)) +
  ggtitle("Top SAT Math Scores")

topSatVerbal <- sat %>% arrange(desc(Verbal)) %>% head(n=20)
topSatVerbal <- cbind(Rank=1:nrow(topSatVerbal), topSatVerbal)
topSatVerbal$College <- paste(topSatVerbal$Rank, topSatVerbal$College, sep=". ")
topSatVerbal$College <- factor(topSatVerbal$College, levels=rev(topSatVerbal$College))

g_verbal<-ggplot(topSatVerbal, aes(x=College, y=Verbal)) +
  geom_bar(stat="identity", fill="#53cfff") +
  geom_text(aes(x=College, y=Verbal-5, ymax=Verbal, hjust=0.95, label=Verbal), size=4) + 
  theme_light(base_size=16) +
  theme(axis.text.y = element_text(hjust=0, color="black"), axis.text.x=element_blank()) +
  xlab("") + ylab("") +
  coord_flip(ylim=c(600, 800)) +
  ggtitle("Top SAT Verbal Scores")

topSatWritten <- sat %>% arrange(desc(Writing)) %>% head(n=20)
topSatWritten <- cbind(Rank=1:nrow(topSatWritten), topSatWritten)
topSatWritten$College <- paste(topSatWritten$Rank, topSatWritten$College, sep=". ")
topSatWritten$College <- factor(topSatWritten$College, levels=rev(topSatWritten$College))

g_Writing<-ggplot(topSatWritten, aes(x=College, y=Writing)) +
  geom_bar(stat="identity", fill="#53cfff") +
  geom_text(aes(x=College, y=Writing-5, ymax=Writing, hjust=0.95, label=Writing), size=4) + 
  theme_light(base_size=16) +
  theme(axis.text.y = element_text(hjust=0, color="black"), axis.text.x=element_blank()) +
  xlab("") + ylab("") +
  coord_flip(ylim=c(600, 800)) +
  ggtitle("Top SAT Writing Scores")

top <- "SAT Statistics(2013)"
top_grob = textGrob(top, gp=gpar(fontsize=32, 
                                 fontface="bold"))
bottom <- paste("2013 data for schools", sep="")

bottom_grob = textGrob(bottom, gp=gpar(fontsize=20))
grid.arrange(g_plot, g_math, g_verbal, g_Writing, nrow=2, ncol=2, 
             top = top_grob, bottom = bottom_grob)

lowestAdmissionRates <- dbGetQuery(db, "
SELECT INSTNM College,
ADM_RATE*100.0 AdmissionRate,
Year
FROM Scorecard
WHERE Year>2003
AND ADM_RATE IS NOT NULL
AND ADM_RATE != 0.0
AND PREDDEG='Predominantly bachelor''s-degree granting'
AND (STABBR=='NY' OR STABBR=='NJ' OR STABBR=='CT' OR STABBR=='DE' OR STABBR=='DC' OR STABBR=='MD' OR STABBR=='PA' OR STABBR=='VT' OR STABBR=='MA' OR STABBR=='RI' OR STABBR=='OH' OR STABBR=='VA' OR STABBR=='WV' OR STABBR=='ME' OR STABBR=='NH' OR STABBR=='NC' OR STABBR=='GA' OR STABBR=='SC' OR STABBR=='FL')
ORDER BY ADM_RATE")

lowestAdmissionRate<-lowestAdmissionRates[which(lowestAdmissionRates$Year=='2013'),]

lowestAdmissionRate <- cbind(Rank=1:nrow(lowestAdmissionRate), lowestAdmissionRate)
lowestAdmissionRate$College <- paste(lowestAdmissionRate$Rank, lowestAdmissionRate$College, sep=". ")
lowestAdmissionRate$College <- factor(lowestAdmissionRate$College, levels=rev(lowestAdmissionRate$College))

g_2013<-ggplot(lowestAdmissionRate[1:20,], aes(x=College, y=AdmissionRate)) +
  geom_bar(stat="identity", fill="#53cfff") +
  geom_text(aes(x=College, y=AdmissionRate-0.55, ymax=AdmissionRate, hjust=0.95, label=paste0(AdmissionRate, "%")), size=4) + 
  theme_light(base_size=16) +
  theme(axis.text.y = element_text(hjust=0, color="black"), axis.text.x=element_blank()) +
  xlab("") + ylab("") +
  coord_flip() +
  ggtitle("Lowest Admission Rates 2013")

lowestAdmissionRate<-lowestAdmissionRates[which(lowestAdmissionRates$Year=='2010'),]

lowestAdmissionRate <- cbind(Rank=1:nrow(lowestAdmissionRate), lowestAdmissionRate)
lowestAdmissionRate$College <- paste(lowestAdmissionRate$Rank, lowestAdmissionRate$College, sep=". ")
lowestAdmissionRate$College <- factor(lowestAdmissionRate$College, levels=rev(lowestAdmissionRate$College))

g_2010<-ggplot(lowestAdmissionRate[1:20,], aes(x=College, y=AdmissionRate)) +
  geom_bar(stat="identity", fill="#53cfff") +
  geom_text(aes(x=College, y=AdmissionRate-0.55, ymax=AdmissionRate, hjust=0.95, label=paste0(AdmissionRate, "%")), size=4) + 
  theme_light(base_size=16) +
  theme(axis.text.y = element_text(hjust=0, color="black"), axis.text.x=element_blank()) +
  xlab("") + ylab("") +
  coord_flip() +
  ggtitle("Lowest Admission Rates 2010")

lowestAdmissionRate<-lowestAdmissionRates[which(lowestAdmissionRates$Year=='2007'),]

lowestAdmissionRate <- cbind(Rank=1:nrow(lowestAdmissionRate), lowestAdmissionRate)
lowestAdmissionRate$College <- paste(lowestAdmissionRate$Rank, lowestAdmissionRate$College, sep=". ")
lowestAdmissionRate$College <- factor(lowestAdmissionRate$College, levels=rev(lowestAdmissionRate$College))

g_2007<-ggplot(lowestAdmissionRate[1:20,], aes(x=College, y=AdmissionRate)) +
  geom_bar(stat="identity", fill="#53cfff") +
  geom_text(aes(x=College, y=AdmissionRate-0.55, ymax=AdmissionRate, hjust=0.95, label=paste0(AdmissionRate, "%")), size=4) + 
  theme_light(base_size=16) +
  theme(axis.text.y = element_text(hjust=0, color="black"), axis.text.x=element_blank()) +
  xlab("") + ylab("") +
  coord_flip() +
  ggtitle("Lowest Admission Rates 2007")

lowestAdmissionRate<-lowestAdmissionRates[which(lowestAdmissionRates$Year=='2004'),]

lowestAdmissionRate <- cbind(Rank=1:nrow(lowestAdmissionRate), lowestAdmissionRate)
lowestAdmissionRate$College <- paste(lowestAdmissionRate$Rank, lowestAdmissionRate$College, sep=". ")
lowestAdmissionRate$College <- factor(lowestAdmissionRate$College, levels=rev(lowestAdmissionRate$College))

g_2004<-ggplot(lowestAdmissionRate[1:20,], aes(x=College, y=AdmissionRate)) +
  geom_bar(stat="identity", fill="#53cfff") +
  geom_text(aes(x=College, y=AdmissionRate-0.55, ymax=AdmissionRate, hjust=0.95, label=paste0(AdmissionRate, "%")), size=4) + 
  theme_light(base_size=16) +
  theme(axis.text.y = element_text(hjust=0, color="black"), axis.text.x=element_blank()) +
  xlab("") + ylab("") +
  coord_flip() +
  ggtitle("Lowest Admission Rates 2004")

top <- "Admission Rate(2004 - 2013)"
top_grob = textGrob(top, gp=gpar(fontsize=32, 
                                 fontface="bold"))
bottom <- paste("2013 data for schools", sep="")

bottom_grob = textGrob(bottom, gp=gpar(fontsize=20))
grid.arrange(g_2013, g_2010, g_2007, g_2004, nrow=2, ncol=2, 
             top = top_grob, bottom = bottom_grob)

engineeringDegrees <- dbGetQuery(db, "
SELECT INSTNM College,
                                 ((PCIP14*100)+(PCIP15*100)) AS PercentEngineeringDegrees,
                                 CONTROL CollegeType
                                 FROM Scorecard
                                 WHERE Year=2013
                                 AND PREDDEG='Predominantly bachelor''s-degree granting'
                                 AND CCBASIC NOT LIKE '%Special Focus%'
                                 AND (STABBR=='NY' OR STABBR=='NJ' OR STABBR=='CT' OR STABBR=='DE' OR STABBR=='DC' OR STABBR=='MD' OR STABBR=='PA' OR STABBR=='VT' OR STABBR=='MA' OR STABBR=='RI' OR STABBR=='OH' OR STABBR=='VA' OR STABBR=='WV' OR STABBR=='ME' OR STABBR=='NH' OR STABBR=='NC' OR STABBR=='GA' OR STABBR=='SC' OR STABBR=='FL')
                                 ORDER BY PercentEngineeringDegrees DESC")
engineeringDegrees <- cbind(Rank=1:nrow(engineeringDegrees), engineeringDegrees)
engineeringDegrees$College <- paste(engineeringDegrees$Rank, engineeringDegrees$College, sep=". ")
engineeringDegrees$College <- factor(engineeringDegrees$College, levels=rev(engineeringDegrees$College))

g_2013<-ggplot(engineeringDegrees[1:20,], aes(x=College, y=PercentEngineeringDegrees)) +
  geom_bar(stat="identity", fill="#53cfff") +
  geom_text(aes(x=College, y=PercentEngineeringDegrees-2, ymax=PercentEngineeringDegrees, hjust=0.95, label=PercentEngineeringDegrees), size=6) + 
  theme_light(base_size=16) +
  theme(axis.text.y = element_text(hjust=0, color="black"), axis.text.x=element_blank()) +
  xlab("") + ylab("") +
  coord_flip() +
  ggtitle("2013")

engineeringDegrees <- dbGetQuery(db, "
                                 SELECT INSTNM College,
                                 ((PCIP14*100)+(PCIP15*100)) AS PercentEngineeringDegrees,
                                 CONTROL CollegeType,
                                 Year
                                 FROM Scorecard
                                 WHERE Year>2003
                                 AND PREDDEG='Predominantly bachelor''s-degree granting'
                                 AND (STABBR=='NY' OR STABBR=='NJ' OR STABBR=='CT' OR STABBR=='DE' OR STABBR=='DC' OR STABBR=='MD' OR STABBR=='PA' OR STABBR=='VT' OR STABBR=='MA' OR STABBR=='RI' OR STABBR=='OH' OR STABBR=='VA' OR STABBR=='WV' OR STABBR=='ME' OR STABBR=='NH' OR STABBR=='NC' OR STABBR=='GA' OR STABBR=='SC' OR STABBR=='FL')
                                 ORDER BY PercentEngineeringDegrees DESC")

engineeringDegree<-engineeringDegrees[which(engineeringDegrees$Year=='2010'),]

engineeringDegree <- cbind(Rank=1:nrow(engineeringDegree), engineeringDegree)
engineeringDegree$College <- paste(engineeringDegree$Rank, engineeringDegree$College, sep=". ")
engineeringDegree$College <- factor(engineeringDegree$College, levels=rev(engineeringDegree$College))

g_2010<-ggplot(engineeringDegree[1:20,], aes(x=College, y=PercentEngineeringDegrees)) +
  geom_bar(stat="identity", fill="#53cfff") +
  geom_text(aes(x=College, y=PercentEngineeringDegrees-2, ymax=PercentEngineeringDegrees, hjust=0.95, label=PercentEngineeringDegrees), size=6) + 
  theme_light(base_size=16) +
  theme(axis.text.y = element_text(hjust=0, color="black"), axis.text.x=element_blank()) +
  xlab("") + ylab("") +
  coord_flip() +
  ggtitle("2010")

engineeringDegree<-engineeringDegrees[which(engineeringDegrees$Year=='2007'),]

engineeringDegree <- cbind(Rank=1:nrow(engineeringDegree), engineeringDegree)
engineeringDegree$College <- paste(engineeringDegree$Rank, engineeringDegree$College, sep=". ")
engineeringDegree$College <- factor(engineeringDegree$College, levels=rev(engineeringDegree$College))

g_2007<-ggplot(engineeringDegree[1:20,], aes(x=College, y=PercentEngineeringDegrees)) +
  geom_bar(stat="identity", fill="#53cfff") +
  geom_text(aes(x=College, y=PercentEngineeringDegrees-2, ymax=PercentEngineeringDegrees, hjust=0.95, label=PercentEngineeringDegrees), size=6) + 
  theme_light(base_size=16) +
  theme(axis.text.y = element_text(hjust=0, color="black"), axis.text.x=element_blank()) +
  xlab("") + ylab("") +
  coord_flip() +
  ggtitle("2007")

engineeringDegree<-engineeringDegrees[which(engineeringDegrees$Year=='2004'),]

engineeringDegree <- cbind(Rank=1:nrow(engineeringDegree), engineeringDegree)
engineeringDegree$College <- paste(engineeringDegree$Rank, engineeringDegree$College, sep=". ")
engineeringDegree$College <- factor(engineeringDegree$College, levels=rev(engineeringDegree$College))

g_2004<-ggplot(engineeringDegree[1:20,], aes(x=College, y=PercentEngineeringDegrees)) +
  geom_bar(stat="identity", fill="#53cfff") +
  geom_text(aes(x=College, y=PercentEngineeringDegrees-2, ymax=PercentEngineeringDegrees, hjust=0.95, label=PercentEngineeringDegrees), size=6) + 
  theme_light(base_size=16) +
  theme(axis.text.y = element_text(hjust=0, color="black"), axis.text.x=element_blank()) +
  xlab("") + ylab("") +
  coord_flip() +
  ggtitle("2004")

top <- "Passed Engineering Students(2004 - 2013)"
top_grob = textGrob(top, gp=gpar(fontsize=32, 
                                 fontface="bold"))
bottom <- paste("College Scorecard", sep="")

bottom_grob = textGrob(bottom, gp=gpar(fontsize=20))
grid.arrange(g_2013, g_2010, g_2007, g_2004, nrow=2, ncol=2, 
             top = top_grob, bottom = bottom_grob)

enrollments <- dbGetQuery(db, "
SELECT INSTNM College,
                          UGDS UndergradEnrollment,
                          CONTROL CollegeType,
                          Year
                          FROM Scorecard
                          WHERE Year>2003
                          AND PREDDEG='Predominantly bachelor''s-degree granting'
                          AND (STABBR=='NY' OR STABBR=='NJ' OR STABBR=='CT' OR STABBR=='DE' OR STABBR=='DC' OR STABBR=='MD' OR STABBR=='PA' OR STABBR=='VT' OR STABBR=='MA' OR STABBR=='RI' OR STABBR=='OH' OR STABBR=='VA' OR STABBR=='WV' OR STABBR=='ME' OR STABBR=='NH' OR STABBR=='NC' OR STABBR=='GA' OR STABBR=='SC' OR STABBR=='FL')
                          AND UGDS IS NOT NULL
                          AND UGDS>0
                          ORDER BY UGDS DESC")

enrollment<-enrollments[which(enrollments$Year=='2013'),]

enrollment <- cbind(Rank=1:nrow(enrollment), enrollment)
enrollment$College <- paste(enrollment$Rank, enrollment$College, sep=". ")
enrollment$College <- factor(enrollment$College, levels=rev(enrollment$College))

g_2013<-ggplot(enrollment[1:20,], aes(x=College, y=UndergradEnrollment)) +
  geom_bar(stat="identity", fill="#53cfff") +
  geom_text(aes(x=College, y=UndergradEnrollment-1000, ymax=UndergradEnrollment, hjust=0.95, label=UndergradEnrollment), size=4) + 
  theme_light(base_size=16) +
  theme(axis.text.y = element_text(hjust=0, color="black"), axis.text.x=element_blank()) +
  xlab("") + ylab("") +
  coord_flip() +
  ggtitle("2013")

enrollment<-enrollments[which(enrollments$Year=='2010'),]

enrollment <- cbind(Rank=1:nrow(enrollment), enrollment)
enrollment$College <- paste(enrollment$Rank, enrollment$College, sep=". ")
enrollment$College <- factor(enrollment$College, levels=rev(enrollment$College))

g_2010<-ggplot(enrollment[1:20,], aes(x=College, y=UndergradEnrollment)) +
  geom_bar(stat="identity", fill="#53cfff") +
  geom_text(aes(x=College, y=UndergradEnrollment-1000, ymax=UndergradEnrollment, hjust=0.95, label=UndergradEnrollment), size=4) + 
  theme_light(base_size=16) +
  theme(axis.text.y = element_text(hjust=0, color="black"), axis.text.x=element_blank()) +
  xlab("") + ylab("") +
  coord_flip() +
  ggtitle("2010")

enrollment<-enrollments[which(enrollments$Year=='2007'),]

enrollment <- cbind(Rank=1:nrow(enrollment), enrollment)
enrollment$College <- paste(enrollment$Rank, enrollment$College, sep=". ")
enrollment$College <- factor(enrollment$College, levels=rev(enrollment$College))

g_2007<-ggplot(enrollment[1:20,], aes(x=College, y=UndergradEnrollment)) +
  geom_bar(stat="identity", fill="#53cfff") +
  geom_text(aes(x=College, y=UndergradEnrollment-1000, ymax=UndergradEnrollment, hjust=0.95, label=UndergradEnrollment), size=4) + 
  theme_light(base_size=16) +
  theme(axis.text.y = element_text(hjust=0, color="black"), axis.text.x=element_blank()) +
  xlab("") + ylab("") +
  coord_flip() +
  ggtitle("2007")

enrollment<-enrollments[which(enrollments$Year=='2004'),]

enrollment <- cbind(Rank=1:nrow(enrollment), enrollment)
enrollment$College <- paste(enrollment$Rank, enrollment$College, sep=". ")
enrollment$College <- factor(enrollment$College, levels=rev(enrollment$College))

g_2004<-ggplot(enrollment[1:20,], aes(x=College, y=UndergradEnrollment)) +
  geom_bar(stat="identity", fill="#53cfff") +
  geom_text(aes(x=College, y=UndergradEnrollment-1000, ymax=UndergradEnrollment, hjust=0.95, label=UndergradEnrollment), size=4) + 
  theme_light(base_size=16) +
  theme(axis.text.y = element_text(hjust=0, color="black"), axis.text.x=element_blank()) +
  xlab("") + ylab("") +
  coord_flip() +
  ggtitle("2004")

top <- "Undergraduate Enrollment(2004 - 2013)"
top_grob = textGrob(top, gp=gpar(fontsize=32, 
                                 fontface="bold"))
bottom <- paste("College Scorecard", sep="")

bottom_grob = textGrob(bottom, gp=gpar(fontsize=20))
grid.arrange(g_2013, g_2010, g_2007, g_2004, nrow=2, ncol=2, 
             top = top_grob, bottom = bottom_grob)


cost <- dbGetQuery(db, "
SELECT INSTNM College,
COSTT4_A Cost,
CONTROL CollegeType
FROM Scorecard
WHERE Year=2013
AND PREDDEG='Predominantly bachelor''s-degree granting'
AND COSTT4_A IS NOT NULL
AND (STABBR='NY' OR STABBR='NJ' OR STABBR='CT' OR STABBR='DE' OR STABBR='DC' OR STABBR='MD' OR STABBR='PA' OR STABBR='VT' OR STABBR='MA' OR STABBR='RI' OR STABBR='OH' OR STABBR='VA' OR STABBR='WV' OR STABBR='ME' OR STABBR='NH' OR STABBR='NC' OR STABBR='GA' OR STABBR='SC' OR STABBR='FL')
AND region NOT LIKE '%Service%'
AND CCBASIC NOT LIKE '%Special Focus%'
ORDER BY COSTT4_A ASC")

cost <- cbind(Rank=1:nrow(cost), cost)
cost$College <- paste(cost$Rank, cost$College, sep=". ")
cost$College <- factor(cost$College, levels=rev(cost$College))

g_2013_1<-ggplot(cost, aes(x=Cost, color=CollegeType, fill=CollegeType, group=CollegeType)) +
  geom_density(alpha=0.3) +
  theme_light(base_size=16) +
  xlab("") + ylab("Cost of Attendance")

g_2013<-ggplot(cost[1:20,], aes(x=College, y=Cost)) +
  geom_bar(stat="identity", fill="#53cfff") +
  geom_text(aes(x=College, y=Cost-500, ymax=Cost, hjust=0.95, label=paste0("$", Cost)), size=4) + 
  theme_light(base_size=16) +
  theme(axis.text.y = element_text(hjust=0, color="black"), axis.text.x=element_blank()) +
  xlab("") + ylab("") +
  coord_flip() +
  ggtitle("2013")

cost <- dbGetQuery(db, "
SELECT INSTNM College,
COSTT4_A Cost,
CONTROL CollegeType
FROM Scorecard
WHERE Year=2010
AND PREDDEG='Predominantly bachelor''s-degree granting'
AND COSTT4_A IS NOT NULL
AND (STABBR='NY' OR STABBR='NJ' OR STABBR='CT' OR STABBR='DE' OR STABBR='DC' OR STABBR='MD' OR STABBR='PA' OR STABBR='VT' OR STABBR='MA' OR STABBR='RI' OR STABBR='OH' OR STABBR='VA' OR STABBR='WV' OR STABBR='ME' OR STABBR='NH' OR STABBR='NC' OR STABBR='GA' OR STABBR='SC' OR STABBR='FL')
AND region NOT LIKE '%Service%'
ORDER BY COSTT4_A ASC") 

cost <- cbind(Rank=1:nrow(cost), cost)
cost$College <- paste(cost$Rank, cost$College, sep=". ")
cost$College <- factor(cost$College, levels=rev(cost$College))

g_2010_1<-ggplot(cost, aes(x=Cost, color=CollegeType, fill=CollegeType, group=CollegeType)) +
  geom_density(alpha=0.3) +
  theme_light(base_size=16) +
  xlab("") + ylab("Cost of Attendance")

g_2010<-ggplot(cost[1:20,], aes(x=College, y=Cost)) +
  geom_bar(stat="identity", fill="#53cfff") +
  geom_text(aes(x=College, y=Cost-500, ymax=Cost, hjust=0.95, label=paste0("$", Cost)), size=4) + 
  theme_light(base_size=16) +
  theme(axis.text.y = element_text(hjust=0, color="black"), axis.text.x=element_blank()) +
  xlab("") + ylab("") +
  coord_flip() +
  ggtitle("2010")

top <- "Affordable Colleges(2010 - 2013)"
top_grob = textGrob(top, gp=gpar(fontsize=32, 
                                 fontface="bold"))
bottom <- paste("College Scorecard", sep="")

bottom_grob = textGrob(bottom, gp=gpar(fontsize=20))
grid.arrange(g_2013, g_2013_1, g_2010, g_2010_1, nrow=2, ncol=2, 
             top = top_grob, bottom = bottom_grob)

cost <- dbGetQuery(db, "
SELECT INSTNM College,
COSTT4_A Cost,
CONTROL CollegeType
FROM Scorecard
WHERE Year=2010
AND PREDDEG='Predominantly bachelor''s-degree granting'
AND COSTT4_A IS NOT NULL
AND (STABBR='NY' OR STABBR='NJ' OR STABBR='CT' OR STABBR='DE' OR STABBR='DC' OR STABBR='MD' OR STABBR='PA' OR STABBR='VT' OR STABBR='MA' OR STABBR='RI' OR STABBR='OH' OR STABBR='VA' OR STABBR='WV' OR STABBR='ME' OR STABBR='NH' OR STABBR='NC' OR STABBR='GA' OR STABBR='SC' OR STABBR='FL')
AND region NOT LIKE '%Service%'
ORDER BY COSTT4_A ASC") 

