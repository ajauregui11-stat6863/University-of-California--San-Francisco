library(readr)
library(lubridate) #to read the function parse_date_time
library(dplyr) # for %>%
library(tidyr) # for %>%
library(ggplot2)
library(gmodels)
library(knitr)
library(arsenal)
library(psych)
library(tableone)
library(officer)
library(flextable)
library(magrittr)
library(StatMeasures)
library(car)
library(rmarkdown)

aim3Dat_all <- read_csv("C:/Users/Adam/Documents/BSRI/TIP Stuff/aim3Dat_all.csv", 
                        col_types = 
                          cols(ATF_PMTCTStartDate = 
                                 col_date(format = "%d/%m/%Y"),
                            ATF_PMTCTStartDateUNK = 
                                 col_date(format = "%d/%m/%Y"),
                            ATF_LastCD4CountDate =
                                 col_date(format = "%d/%m/%Y"),
                            ATF_LastCD4CountDateUNK = 
                                 col_date(format = "%d/%m/%Y"),
                            ATF_LastViralLoadDate = 
                                 col_date(format = "%d/%m/%Y"),
                            ATF_LastViralLoadDateUNK = 
                                 col_date(format = "%d/%m/%Y"),
                            ATF_ARTStartDate = 
                                 col_date(format = "%d/%m/%Y"),
                            ATF_ARTStartDateUNK = 
                                 col_date(format = "%d/%m/%Y"),
                            ATF_HIVStatusOthTestDate =
                                 col_date(format = "%d/%m/%Y"),
                            ATF_HIVStatusOthTestDate2 = 
                                 col_date(format = "%d/%m/%Y"),
                            ATF_HIVStatusDelvAdmitDate =
                                 col_date(format = "%d/%m/%Y"),
                            ATF_HIVStatusBookingDate =
                                 col_date(format = "%d/%m/%Y"),
                            ATF_BookingDate =
                                 col_date(format = "%d/%m/%Y"),
                            ATF_EstDateDelivery = 
                                 col_date(format = "%d/%m/%Y"),
                            ATF_TodayDate = 
                                 col_date(format = "%d/%m/%Y"),
                            ATF_AdmDate = 
                                 col_date(format = "%d/%m/%Y"),
                            ATF_DschgDate =
                                 col_date(format = "%d/%m/%Y"),
                            ATF_ComponentTxDate1 = 
                                 col_date(format = "%d/%m/%Y"),
                            ATF_ComponentTxDate2 = 
                                 col_date(format = "%d/%m/%Y"),
                            ATF_ComponentTxDate3 = 
                                 col_date(format = "%d/%m/%Y"),
                            ATF_ComponentTxDate4 = 
                                 col_date(format = "%d/%m/%Y"),
                            ATF_ComponentTxDate5 = 
                                 col_date(format = "%d/%m/%Y"),
                            ATF_ComponentTxDate6 = 
                                 col_date(format = "%d/%m/%Y"),
                            ATF_ComponentTxDate7 = 
                                 col_date(format = "%d/%m/%Y")))
View(aim3Dat_all)
#collapse hospital LOCATION column from four to three values
aim3Dat_all$LOCATION<-gsub(pattern="Groote Schuur Hospital",
                           replacement="Groote/Mowbray",
                           aim3Dat_all$LOCATION)
aim3Dat_all$LOCATION<-gsub(pattern="Mowbray Maternity Hospita",
                           replacement="Groote/Mowbray",
                           aim3Dat_all$LOCATION)
aim3Dat_all$LOCATION<-gsub(pattern="Chris Hani- Baragwanath H",
                           replacement="Chris Hani",
                           aim3Dat_all$LOCATION)
aim3Dat_all$LOCATION<-gsub(pattern="King Edward VIII Hospital",
                           replacement="King Edward",
                           aim3Dat_all$LOCATION)

###discarding rows that don't meet inclusion criteria
#aim3_inclusion<-aim3Dat_all[which(
  #aim3Dat_all$ATF_GestAgeWks < 26 & 
   # aim3Dat_all$ATF_Age > 17 & 
    #aim3Dat_all$ATF_Transfused=='01 Yes'),]

#isolate the sections#
#find out what the column names are and their indices
#colnames(aim3Dat_all)
aim3_sec0<-aim3Dat_all[c(48,1:47)]
aim3_sec0<-aim3_sec0[c(1,3,5,9:11)]
aim3_sec1<-aim3Dat_all[c(48,9,49:75)]
aim3_sec1<-aim3_sec1[c(-c(5:22,25))]
aim3_sec2<-aim3Dat_all[c(48,9,76:83)]
aim3_sec2<-aim3_sec2[c(1,2,6:8,10)]
aim3_sec3<-aim3Dat_all[c(48,9,84:107,185:206)]
aim3_sec3<-aim3_sec3[c(1,2,10:26)]
aim3_sec4<-aim3Dat_all[c(48,9,108:118,128:131)]
aim3_sec4<-aim3_sec4[c(1:10,14:16)]
aim3_sec5<-aim3Dat_all[c(48,9,210:236)]
aim3_sec5<-aim3_sec5[c(1:4,28)]
aim3_sec6<-aim3Dat_all[c(48,9,99,251:388)]
aim3_sec6<-aim3_sec6[c(1:2,4:11,13,14,21,22,23,34:44,52:56)]
aim3_sec7<-aim3Dat_all[c(48,9,260,313:388)]
aim3_sec7<-aim3_sec7[c(1:4,6,8,10,12,14,16,18,20,22,24,26,28,30,55:79)]

#split date variables with mixed date and time entries into two columns
aim3_sec0<-aim3_sec0 %>% 
  separate(CHARTABSTRATIONDATE,
           c("ChartAbstrationDate","ChartAbstrationTime"),
           sep=" ",extra="merge")
aim3_sec0<-aim3_sec0 %>% 
  separate(CONSENTDATE,
           c("ConsentDate","ConsentTime"),
           sep=" ",extra="merge")
aim3_sec0<-aim3_sec0 %>% 
  separate(CREATEDDATE,
           c("CreatedDate","CreatedTime"),
           sep=" ",extra="merge")

#change time variables to one format (military time)
aim3_sec0$ChartAbstrationTime<-
  parse_date_time(aim3_sec0$ChartAbstrationTime,c("HMS","HMS p"))
aim3_sec0$CreatedTime<-
  parse_date_time(aim3_sec0$CreatedTime,c("HMS","HMS p"))
aim3_sec0$ConsentTime<-
  parse_date_time(aim3_sec0$ConsentTime,c("HMS","HMS p"))

#discard the first part of these new time variables
aim3_sec0<-aim3_sec0 %>% 
  separate(ChartAbstrationTime,
           c("useless1","ChartAbstrationTime"),
           sep=" ",extra="merge")
aim3_sec0<-aim3_sec0 %>% 
  separate(CreatedTime,
           c("useless2","CreatedTime"),
           sep=" ",extra="merge")
aim3_sec0<-aim3_sec0 %>% 
  separate(ConsentTime,
           c("useless3","ConsentTime"),
           sep=" ",extra="merge")
aim3_sec0<-subset(aim3_sec0,
                    select=-c(useless1,useless2,useless3))
View(aim3_sec0)

attach(aim3_sec0)
#check if 'SUBJECT ID' and 'studyId' column are equal
all(SUBJECTID==studyId) #(TRUE)
#check for duplicates
which(duplicated(studyId)) 
#none are duplicates
detach(aim3_sec0)

#check if 'ChartAbstrationDate' and 'ConsentDate' are equal
#first convert them into dates
aim3_sec0$CreatedDate<-as.Date(aim3_sec0$CreatedDate,
                               format="%d/%m/%Y")
aim3_sec0$ChartAbstrationDate<-as.Date(aim3_sec0$ChartAbstrationDate,
                                      format="%d/%m/%Y")
aim3_sec0$ConsentDate<-as.Date(aim3_sec0$ConsentDate,
                                  format="%d/%m/%Y")
all(aim3_sec0$ChartAbstrationDate==aim3_sec0$ConsentDate) 
#(FALSE)
#which indeces are FALSE?
which(!(aim3_sec0$ChartAbstrationDate==aim3_sec0$ConsentDate))
#which indeces are NA?
which(is.na(aim3_sec0$ChartAbstrationDate==aim3_sec0$ConsentDate))
#drop time columns and SUBJECT ID column
aim3_sec0<-aim3_sec0[-c(3,5,7,9)]
#rearrange
aim3_sec0<-aim3_sec0[c(1,5,2,3,4)]

###section1###
#convert ATF_Date into a date column
aim3_sec1$ATF_TodayDate<-as.Date(aim3_sec1$ATF_TodayDate,
                                 format="%Y-%m-%d")
#check nonmissing values of nationalityDK, raceDK, gravidityDK,
  #parityDK
lapply(aim3_sec1[c(6,8,10)],function(x) which(!(is.na(x))))
#since the above vectors contain nothing relevant, let's delete them
aim3_sec1<-aim3_sec1[-c(6,8,10)]

###section2###
aim3_sec2$ATF_AdmDate<-as.Date(
  aim3_sec2$ATF_AdmDate,format="%d/%m/%Y"
)
aim3_sec2$ATF_DschgDate<-as.Date(
  aim3_sec2$ATF_DschgDate,format="%d/%m/%Y"
)
stay.length<-as.integer(aim3_sec2$ATF_DschgDate)-
  as.integer(aim3_sec2$ATF_AdmDate)
aim3_sec2$Stay_Length<-stay.length

###section3###
aim3_sec3$ATF_CompsThisPregType<-gsub("CompsThisPreg","",
                                      aim3_sec3$ATF_CompsThisPregType)
aim3_sec3$ATF_CompsThisAdmType<-gsub("CompsThisAdm","",
                                     aim3_sec3$ATF_CompsThisAdmType)
lapply(aim3_sec3[c(9,14,15,18)],function(x) which(!(is.na(x))))
#as categorical
gest.wks<-cut(aim3_sec3$ATF_GestAgeWks,
              breaks=c(-Inf,25,32,Inf),
              label=c("<26","26-32",">32"),
              right=FALSE)
#after running lapply, delete columns 14 and 18 since they are empty
aim3_sec3<-aim3_sec3[-c(14,18)]

#section4#
which(!(is.na(aim3_sec4$ATF_PMTCTStartDateUNK)))
#SAS frequency list
attach(aim3_sec4)
summary(freqlist(
  table(ATF_HIVStatusAtBooking,ATF_HIVStatusDelvAdmission,
        ATF_HIVStatusOthTest,ATF_HIVStatusOthTest2,
        useNA="ifany")
))
detach(aim3_sec4)

#section 5#
aim3_sec5$ATF_HemorrhageCause<-gsub("HemorrhageCause","",
                                    aim3_sec5$ATF_HemorrhageCause)
which(!(is.na(aim3_sec5$HemorrhageCauseOtherSpecify)))

#section 6#
aim3_sec6$ATF_MedicalRationaleForTx<-gsub(
  "MedRationaleTx","",aim3_sec6$ATF_MedicalRationaleForTx)
aim3_sec6$ATF_LastHemoglobinPriorTx<-as.numeric(
  aim3_sec6$ATF_LastHemoglobinPriorTx)
aim3_sec6$ATF_HbAfterTx<-as.numeric(
  aim3_sec6$ATF_HbAfterTx)
aim3_sec6$DeltaTx<-aim3_sec6$ATF_HbAfterTx - 
  aim3_sec6$ATF_LastHemoglobinPriorTx
lapply(aim3_sec6[c(9,10,13,14,19,20,22,24,25,28,30,31)], 
                  function(x) which(!(is.na(x))))
#delete columns 9, 13, 19, 24, 30 since they are empty
aim3_sec6<-aim3_sec6[-c(9,13,19,24,30)]

#section 7#
aim3_sec7$ATF_ComponentsTXDate1<-as.Date(
  aim3_sec7$ATF_ComponentsTXDate1,format="%d/%m/%Y")
aim3_sec7$ATF_ComponentsTXDate2<-as.Date(
  aim3_sec7$ATF_ComponentsTXDate2,format="%d/%m/%Y")
aim3_sec7$ATF_ComponentsTXDate3<-as.Date(
  aim3_sec7$ATF_ComponentsTXDate3,format="%d/%m/%Y")
aim3_sec7$ATF_ComponentsTXDate4<-as.Date(
  aim3_sec7$ATF_ComponentsTXDate4,format="%d/%m/%Y")
aim3_sec7$ATF_ComponentsTXDate5<-as.Date(
  aim3_sec7$ATF_ComponentsTXDate5,format="%d/%m/%Y")
aim3_sec7$ATF_ComponentsTXDate6<-as.Date(
  aim3_sec7$ATF_ComponentsTXDate6,format="%d/%m/%Y")
aim3_sec7$ATF_ComponentsTXDate7<-as.Date(
  aim3_sec7$ATF_ComponentsTXDate7,format="%d/%m/%Y")

#check all columns
x<-sapply(aim3Dat_all,unique)