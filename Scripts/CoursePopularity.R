data2013<-read.csv(file = "E:/documents/CourseWork/DADM/CollegeScorecard_Raw_Data/CollegeScorecard_Raw_Data/CollegeAll.csv" , na.strings = "NULL")

## the name of the first column is not reading in correctly
## compensate by renaming the col (not optimal but I need a work around).
names(data2013)[1]<-"UNITID"

data2013_nrow<-nrow(data2013)
data2013_ncol<-ncol(data2013)

library(dplyr)

schools<-select(data2013, 
                UNITID, 
                INSTNM, 
                UGDS,
                Agriculture=PCIP01,
                Conservation=PCIP03,
                Architecture=PCIP04,
                GroupStudies=PCIP05,
                CommAndJournalism=PCIP09,
                CommTechnologies=PCIP10,
                Computers=PCIP11,
                Culinary=PCIP12,
                Education=PCIP13,
                Engineering=PCIP14,
                EngTechnologies=PCIP15,
                Languages=PCIP16,
                Family=PCIP19,
                Legal=PCIP22,
                English=PCIP23,
                LiberalArts=PCIP24,
                Library=PCIP25,
                Biology=PCIP26,
                MathAndStat=PCIP27,
                Military=PCIP29,
                MultiDiscipline=PCIP30,
                ParksAndRec=PCIP31,
                Philosophy=PCIP38,
                Theology=PCIP39,
                PhysicalScience=PCIP40,
                ScienceTech=PCIP41,
                Psychology=PCIP42,
                ProtectServices=PCIP43,
                PublicAdmin=PCIP44,
                SocialServices=PCIP45,
                Construction=PCIP46,
                Mechanical=PCIP47,
                PrecisionProd=PCIP48,
                Transportation=PCIP49,
                PerformingArts=PCIP50,
                Health=PCIP51,
                Business=PCIP52,
                History=PCIP54,
                Yr)

schools_nrow<-nrow(schools)
schools_ncol<-ncol(schools)

programs<-select(schools, UNITID, INSTNM, UGDS, prcntDgrs=4, Yr)
programs$prog<-names(schools)[4]

## repeat for each remaining program, binding the rows to the programs dataset
for (i in 5:41) {
  temp<-select(schools, UNITID, INSTNM, UGDS, prcntDgrs=i, Yr)  
  temp$prog<-names(schools)[i]
  programs<-rbind(programs, temp)
}

programs_nrow<-nrow(programs)
programs_ncol<-ncol(programs)

programs<-filter(programs, is.na(UGDS)==FALSE & is.na(prcntDgrs)==FALSE)

programs_nrow<-nrow(programs)
programs_ncol<-ncol(programs)


grandTotal<-sum((schools$UGDS[which(schools$Yr=='2013')]), na.rm=TRUE)

##(programs$UGDS[which(programs$Yr=='2013')])*(programs$prcntDgrs[which(programs$Yr=='2013')])

## for each program, 
## sum the school's enrollment * percent degrees awarded
progTotals<-aggregate(((programs$UGDS[which(programs$Yr=='2013')])*(programs$prcntDgrs[which(programs$Yr=='2013')])),
                      by=list(Program=(programs$prog[which(programs$Yr=='2013')])), FUN=sum)

## progTotals has two vars: Program and x (total)
## rename x for clarity
names(progTotals)[2]<-"Enrollment"

## add a third column indicating percent of total enrollment
progTotals$Percent<-round(progTotals$Enrollment/grandTotal*100, 2)

## round ProgramEnrollment to make it an even number for display
progTotals$Enrollment<-round(progTotals$Enrollment, 0)

## rank by highest enrollment first
progTotals<-arrange(progTotals, desc(Percent), desc(Enrollment), Program)
print(progTotals, type="html")

library(ggplot2)

progTotals$Program <- factor(progTotals$Program, levels = progTotals$Program)

## This is simply to prevent the values from displaying as 
## scientific notation in the labels.
options(scipen=10)

g <- ggplot(progTotals, aes(x=Program, y=Enrollment)) 
g <- g + geom_bar(stat='identity')   
g <- g + labs(x="", y="")
g <- g + ggtitle("Total Enrollment by Field of Study in 2013")
## Add percent labels to end of bars.
## In stmt below, y is the label position and adding 150000
## pushed it beyond end of bar so it can be seen.
g <- g + geom_text(aes(label = paste(as.character(Percent), "%"), y = Enrollment + 150000), size = 3) 
g <- g + coord_flip() ## makes bars horizontal
g
options(scipen=0)