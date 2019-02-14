library(readxl)
library(lubridate)
library(dplyr)
library(tidyr)
#library(naniar)
library(summarytools)
library(arsenal)
library(stringr)
library(flextable)
MergedAIM2_QX <- read_excel("BSRI/TIP Stuff/MergedAIM2_QX.xlsx/MergedAIM2_QX (typos gone).xlsx")
View(MergedAIM2_QX)

#get dimensions and view structure
#dim(MergedAIM2_QX)
#glimpse(MergedAIM2_QX)

#look for typos in the variables of the data frame
#sapply(MergedAIM2_QX[1:50],unique)
#Collection Date: UTC
MergedAIM2_QX<-MergedAIM2_QX %>%
  separate(`Collection Date`,
           c("Collection_Date","utc"),
           sep=" ",extra="merge")
MergedAIM2_QX$Collection_Date<-as.Date(
  MergedAIM2_QX$Collection_Date,format="%Y-%m-%d")
#Registration Date: UTC
MergedAIM2_QX<-MergedAIM2_QX %>% 
  separate(`Registration Date`,
           c("Registration_Date","utc"),
           sep=" ",extra="merge")
MergedAIM2_QX$Registration_Date<-as.Date(
  MergedAIM2_QX$Registration_Date,format="%Y-%m-%d"
)
#TodayDate: change to date format d/m/y
MergedAIM2_QX$TodayDate<-as.Date(MergedAIM2_QX$TodayDate,
                                 format="%d/%m/%Y")
#sapply(MergedAIM2_QX[51:150],unique)
MergedAIM2_QX<-MergedAIM2_QX %>% 
  separate("BLOODCOLLECTIONDATE",
           c("BLOODCOLLECTIONDATE","utc"),
           sep=" ",extra="merge")
MergedAIM2_QX$BLOODCOLLECTIONDATE<-ymd(
  MergedAIM2_QX$BLOODCOLLECTIONDATE)
MergedAIM2_QX<-MergedAIM2_QX %>% 
  separate("CHARTABSTRATIONDATE",
           c("CHARTABSTRATIONDATE","utc"),
           sep=" ",extra="merge")
MergedAIM2_QX$CHARTABSTRATIONDATE<-ymd(
  MergedAIM2_QX$CHARTABSTRATIONDATE)
MergedAIM2_QX<-MergedAIM2_QX %>% 
  separate("CONSENTDATE",
           c("CONSENTDATE","utc"),
           sep=" ",extra="merge")
MergedAIM2_QX$CONSENTDATE<-ymd(
  MergedAIM2_QX$CONSENTDATE)
MergedAIM2_QX<-MergedAIM2_QX %>% 
  separate("CREATEDDATE",
           c("CREATEDDATE","utc"),
           sep=" ",extra="merge")
MergedAIM2_QX$CREATEDDATE<-ymd(
  MergedAIM2_QX$CREATEDDATE)
#AAF_TodayDate: change to date format d/m/y
MergedAIM2_QX$AAF_TodayDate<-as.Date(MergedAIM2_QX$AAF_TodayDate,
                                     format="%d/%m/%Y")
#SecondCountry: change value of "sa" (and maybe "SSA") to "SA"
#AAF_PostalCode: all are 4 digits except for two (904 and 745)
#AAF_PostalCode_2ndRes: all begin with the digit "1" except for two (2143 and 7785)
#AAF_Gravidity: values of 62 and 92
#AAF_Parity: values of 91 and 61
#AAF_NumVisitsRefAnteClinic: values of 62, 61, 64, 81, and 98
#sapply(MergedAIM2_QX[151:250],unique)
#AAF_DateHbObained: change to date format d/m/y
MergedAIM2_QX$AAF_DateHbObtained<-as.Date(MergedAIM2_QX$AAF_DateHbObtained,
                                          format="%d/%m/%Y")
#AAF_IronSulphateDate: change to date format d/m/y
MergedAIM2_QX$AAF_IronSulphateDate<-as.Date(MergedAIM2_QX$AAF_IronSulphateDate,
                                            format="%d/%m/%Y")
#AAF_FolicAcidDate: change to date format d/m/y
MergedAIM2_QX$AAF_FolicAcidDate<-as.Date(MergedAIM2_QX$AAF_FolicAcidDate,
                                         format="%d/%m/%Y")
#AAF_VitaminCDate: change to date format d/m/y
MergedAIM2_QX$AAF_VitaminCDate<-as.Date(MergedAIM2_QX$AAF_VitaminCDate,
                                        format="%d/%m/%Y")
#AAF_VitaminADate: change to date format d/m/y
MergedAIM2_QX$AAF_VitaminADate<-as.Date(MergedAIM2_QX$AAF_VitaminADate,
                                        format="%d/%m/%Y")
#AAF_VitaminB12Date: change to date format d/m/y
MergedAIM2_QX$AAF_VitaminB12Date<-as.Date(MergedAIM2_QX$AAF_VitaminB12Date,
                                          format="%d/%m/%Y")
#AAF_ThiamineDate: change to date format d/m/y
MergedAIM2_QX$AAF_ThiamineDate<-as.Date(MergedAIM2_QX$AAF_ThiamineDate,
                                        format="%d/%m/%Y")
#AAF_OtherIronDate: change to date format d/m/y
MergedAIM2_QX$AAF_OtherIronDate<-as.Date(MergedAIM2_QX$AAF_OtherIronDate,
                                         format="%d/%m/%Y")
#AAF_RefForAnemiaDate: change to date format d/m/y
MergedAIM2_QX$AAF_RefForAnemiaDate<-as.Date(MergedAIM2_QX$AAF_RefForAnemiaDate,
                                            format="%d/%m/%Y")
#AAF_1stSpecialistAnteAnemiaVisit: change to date format d/m/y
MergedAIM2_QX$AAF_1stSpecialistAnteAnemiaVisit<-as.Date(
  MergedAIM2_QX$AAF_1stSpecialistAnteAnemiaVisit,format="%d/%m/%Y")
#AAF_ReasonForReferral: fix spelling of "Reason" (some spelled as "Raeson")
#AAF_AACRx_IronSulphate_Date: change to date format d/m/y
MergedAIM2_QX$AAF_AACRx_IronSulphate_Date<-as.Date(
  MergedAIM2_QX$AAF_AACRx_IronSulphate_Date,format="%d/%m/%Y")
#AAF_AACRx_FolicAcid_Date: change to date format d/m/y
MergedAIM2_QX$AAF_AACRx_FolicAcid_Date<-as.Date(
  MergedAIM2_QX$AAF_AACRx_FolicAcid_Date,format="%d/%m/%Y")
#AAF_AACRx_VitC_Date: change to date format d/m/y
MergedAIM2_QX$AAF_AACRx_VitC_Date<-as.Date(
  MergedAIM2_QX$AAF_AACRx_VitC_Date,format="%d/%m/%Y")
#AAF_AACRx_VitA_Date: change to date format d/m/y
MergedAIM2_QX$AAF_AACRx_VitA_Date<-as.Date(
  MergedAIM2_QX$AAF_AACRx_VitA_Date,format="%d/%m/%Y"
)
#AAF_AACRx_VitB12_Date: change to date format d/m/y
MergedAIM2_QX$AAF_AACRx_VitA_Date<-as.Date(
  MergedAIM2_QX$AAF_AACRx_VitA_Date,format="%d/%m/%Y"
)
#AAF_AACRx_Thiamine_Date: change to date format d/m/y
MergedAIM2_QX$AAF_AACRx_Thiamine_Date<-as.Date(
  MergedAIM2_QX$AAF_AACRx_Thiamine_Date,format="%d/%m/%Y"
)
#AAF_AACRx_OthIronSup_Date: change to date format d/m/y
MergedAIM2_QX$AAF_AACRx_OthIronSup_Date<-as.Date(
  MergedAIM2_QX$AAF_AACRx_OthIronSup_Date,format="%d/%m/%Y"
)
#sapply(MergedAIM2_QX[251:350],unique)
#AAF_CurrLMP: change to date format d/m/y
MergedAIM2_QX$AAF_CurrLMP<-as.Date(
  MergedAIM2_QX$AAF_CurrLMP,format="%d/%m/%Y"
)
#AAF_CurrEstDateDelv: change to date format d/m/y
MergedAIM2_QX$AAF_CurrEstDateDelv<-as.Date(
  MergedAIM2_QX$AAF_CurrEstDateDelv,format="%d/%m/%Y"
)
#AAF_CurrBookingDate: change to date format d/m/y
MergedAIM2_QX$AAF_CurrBookingDate<-as.Date(
  MergedAIM2_QX$AAF_CurrBookingDate,format="%d/%m/%Y"
)
#AAF_AACRespRate: values range between 16 and 24, outlier of 820
#BookingHIVStatusDate: change to date format d/m/y
MergedAIM2_QX$BookingHIVStatusDate<-as.Date(
  MergedAIM2_QX$BookingHIVStatusDate,format="%d/%m/%Y"
)
#DeliveryAdmitStatusDate: change to date format d/m/y
MergedAIM2_QX$DeliveryAdmitStatusDate<-as.Date(
  MergedAIM2_QX$DeliveryAdmitStatusDate,format="%d/%m/%Y"
)
#OtherTestDurPregHIVStatusDate: change to date format d/m/y
MergedAIM2_QX$OtherTestDurPregHIVStatusDate<-as.Date(
  MergedAIM2_QX$OtherTestDurPregHIVStatusDate,format="%d/%m/%Y"
)
#OtherTestDurPregHIVStatusDate2: change to date format d/m/y
MergedAIM2_QX$OtherTestDurPregHIVStatusDate2<-as.Date(
  MergedAIM2_QX$OtherTestDurPregHIVStatusDate2,format="%d/%m/%Y"
)
#AAF_LastCD4CountDate: change to date format d/m/y
MergedAIM2_QX$AAF_LastCD4CountDate<-as.Date(
  MergedAIM2_QX$AAF_LastCD4CountDate,format="%d/%m/%Y"
)
#AAF_viralLoadDate: change to date format d/m/y
MergedAIM2_QX$AAF_ViralLoadDate<-as.Date(
  MergedAIM2_QX$AAF_ViralLoadDate,format="%d/%m/%Y"
)
#AAF_ARTStartDate: change to date format d/m/y
MergedAIM2_QX$AAF_ARTStartDate<-as.Date(
  MergedAIM2_QX$AAF_ARTStartDate,format="%d/%m/%Y"
)
#sapply(MergedAIM2_QX[351:450],unique)
#AAF_ARTPMTCTStartDate: change to date format d/m/y
MergedAIM2_QX$AAF_ARTPMTCTStartDate<-as.Date(
  MergedAIM2_QX$AAF_ARTPMTCTStartDate,format="%d/%m/%Y"
)
#AAF_StartDateAZT: change to date format d/m/y
MergedAIM2_QX$AAF_StartDateAZT<-as.Date(
  MergedAIM2_QX$AAF_ARTPMTCTStartDate,format="%d/%m/%Y"
)
#AAF_EndDateAZT: change to date format d/m/y
MergedAIM2_QX$AAF_EndDateAZT<-as.Date(
  MergedAIM2_QX$AAF_EndDateAZT,format="%d/%m/%Y"
)
#AAF_DateTransfusion: change to date format d/m/y
MergedAIM2_QX$AAF_DateTransfusion<-as.Date(
  MergedAIM2_QX$AAF_DateTransfusion,format="%d/%m/%Y"
)
#Transcription_Date: change to date format d/m/y
MergedAIM2_QX$Transcription_Date<-as.Date(
  MergedAIM2_QX$Transcription_Date,format="%d/%m/%Y"
)
sapply(MergedAIM2_QX[c(401:545)],unique)
#v01_Visit_Date: change to date format d/m/y
MergedAIM2_QX$v01_Visit_Date<-as.Date(
  MergedAIM2_QX$v01_Visit_Date)
#v01_IVI_Iron_date: change to date format d/m/y
MergedAIM2_QX$v01_IVI_Iron_date<-as.Date(
  MergedAIM2_QX$v01_IVI_Iron_date,format="%d/%m/%Y"
)
#v02_Visit_Date: change to date format d/m/y
MergedAIM2_QX$v02_Visit_Date<-as.Date(
  MergedAIM2_QX$v02_Visit_Date,format="%d/%m/%Y"
)
#v02_IVI_Iron_date: change to date format d/m/y
MergedAIM2_QX$v02_IVI_Iron_date<-as.Date(
  MergedAIM2_QX$v02_IVI_Iron_date,format="%d/%m/%Y"
)
#v02_Transfusion_date: change to date format d/m/y
MergedAIM2_QX$v02_Transfusion_date<-as.Date(
  MergedAIM2_QX$v02_Transfusion_date,format="%d/%m/%Y"
)
#v03_Visit_Date: change to date format d/m/y
MergedAIM2_QX$v03_Visit_Date<-as.Date(
  MergedAIM2_QX$v03_Visit_Date,format="%d/%m/%Y"
)
#v03_IVI_Iron_date: "" format d/m/y
MergedAIM2_QX$v03_IVI_Iron_date<-as.Date(
  MergedAIM2_QX$v03_IVI_Iron_date,format="%d/%m/%Y"
)
#v03_Transfusion_date: "" format d/m/y
MergedAIM2_QX$v03_Transfusion_date<-as.Date(
  MergedAIM2_QX$v03_Transfusion_date,format="%d/%m/%Y"
)
#v04_Visit_Date: "" format d/m/y
MergedAIM2_QX$v04_Visit_Date<-as.Date(
  MergedAIM2_QX$v04_Visit_Date,format="%d/%m/%Y"
)
#sapply(MergedAIM2_QX[451:550],unique)
#v04_IVI_Iron_date: d/m/y
MergedAIM2_QX$v04_IVI_Iron_date<-as.Date(
  MergedAIM2_QX$v04_IVI_Iron_date,format="%d/%m/%Y"
)
#v04_Tranfuion_date: d/m/y
MergedAIM2_QX$v04_Transfusion_date<-as.Date(
  MergedAIM2_QX$v04_Transfusion_date,format="%d/%m/%Y"
)
#v05_Visit_Date: d/m/y
MergedAIM2_QX$v05_Visit_Date<-as.Date(
  MergedAIM2_QX$v05_Visit_Date,format="%d/%m/%Y"
)
#v05_IVI_Iron_date: d/m/y
MergedAIM2_QX$v05_IVI_Iron_date<-as.Date(
  MergedAIM2_QX$v05_IVI_Iron_date,format="%d/%m/%Y"
)
#v05_Transfusion_date: d/m/y
MergedAIM2_QX$v05_Transfusion_date<-as.Date(
  MergedAIM2_QX$v05_Transfusion_date,format="%d/%m/%Y"
)
#v06_Visit_Date: d/m/y
MergedAIM2_QX$v06_Visit_Date<-as.Date(
  MergedAIM2_QX$v06_Visit_Date,format="%d/%m/%Y"
)
#v06_IVI_Iron_date: d/m/y
MergedAIM2_QX$v06_IVI_Iron_date<-as.Date(
  MergedAIM2_QX$v06_IVI_Iron_date,format="%d/%m/%Y"
)
#v06_Transfusion_date: d/m/y
MergedAIM2_QX$v06_Transfusion_date<-as.Date(
  MergedAIM2_QX$v06_Transfusion_date,format="%d/%m/%Y"
)
#v07_Visit_Date: d/m/y
MergedAIM2_QX$v07_Visit_Date<-as.Date(
  MergedAIM2_QX$v07_Visit_Date,format="%d/%m/%Y"
)
#v07_IVI_Iron_date: d/m/y
MergedAIM2_QX$v07_IVI_Iron_date<-as.Date(
  MergedAIM2_QX$v07_IVI_Iron_date,format="%d/%m/%Y"
)
#v07_Transfusion_date: d/m/y
MergedAIM2_QX$v07_Transfusion_date<-as.Date(
  MergedAIM2_QX$v07_Transfusion_date,format="%d/%m/%Y"
)
#v08_Visit_Date: d/m/y
MergedAIM2_QX$v08_Visit_Date<-as.Date(
  MergedAIM2_QX$v08_Visit_Date,format="%d/%m/%Y"
)
#v08_IVI_Iron_date: d/m/y
MergedAIM2_QX$v08_IVI_Iron_date<-as.Date(
  MergedAIM2_QX$v08_IVI_Iron_date,format="%d/%m/%Y"
)
#v08_Transfusion_date: d/m/y
MergedAIM2_QX$v08_Transfusion_date<-as.Date(
  MergedAIM2_QX$v08_Transfusion_date,format="%d/%m/%Y"
)
#v09_Visit_Date: d/m/y
MergedAIM2_QX$v09_Visit_Date<-as.Date(
  MergedAIM2_QX$v09_Visit_Date,format="%d/%m/%Y"
)
#v09_IVI_Iron_date: d/m/y
MergedAIM2_QX$v09_IVI_Iron_date<-as.Date(
  MergedAIM2_QX$v09_IVI_Iron_date,format="%d/%m/%Y"
)
#v09_Transfusion_date: d/m/y
MergedAIM2_QX$v09_Transfusion_date<-as.Date(
  MergedAIM2_QX$v09_Transfusion_date,format="%d/%m/%Y"
)
#v10_Visit_Date: d/m/y
MergedAIM2_QX$v10_Visit_Date<-as.Date(
  MergedAIM2_QX$v10_Visit_Date,format="%d/%m/%Y"
)
#v10_IVI_Iron_date: d/m/y
MergedAIM2_QX$v10_IVI_Iron_date<-as.Date(
  MergedAIM2_QX$v10_IVI_Iron_date,format="%d/%m/%Y"
)
#v10_Transfusion_date: d/m/y
MergedAIM2_QX$v10_Transfusion_date<-as.Date(
  MergedAIM2_QX$v10_Transfusion_date,format="%d/%m/%Y")
#v01_Visit_DateA: d/m/y
#MergedAIM2_QX$v01_Visit_DateA<-as.Date(
# MergedAIM2_QX$v01_Visit_DateA,format="%d/%m/%Y")
#v01_Visit_DateB: remove the UTC from the date values, then change to d/m/y,
#                 also a few of the years have a year of 1930, a posible typo
MergedAIM2_QX<-MergedAIM2_QX %>% 
  separate("v01_Visit_DateB",
           c("v01_Visit_DateB","utc"),
           sep=" ",extra="merge")
MergedAIM2_QX$v01_Visit_DateB<-ymd(
  MergedAIM2_QX$v01_Visit_DateB)
#sapply(MergedAIM2_QX[551:650],unique)
#v01_Date: remove the UTC from the date values, then change to d/m/y
MergedAIM2_QX<-MergedAIM2_QX %>% 
  separate("v01_Date",
           c("v01_Date","utc"),
           sep=" ",extra="merge")
MergedAIM2_QX$v01_Date<-ymd(
  MergedAIM2_QX$v01_Date)
#v02_Visit_DateA
#MergedAIM2_QX$v02_Visit_DateA<-as.Date(
# MergedAIM2_QX$v02_Visit_DateA,format="%d/%m/%Y")
#v02_Visit_DateB: remove the UTC from the date values, then change to d/m/y,
#                 then check the years for typos
MergedAIM2_QX<-MergedAIM2_QX %>% 
  separate("v02_Visit_DateB",
           c("v02_Visit_DateB","utc"),
           sep=" ",extra="merge")
MergedAIM2_QX$v02_Visit_DateB<-ymd(
  MergedAIM2_QX$v02_Visit_DateB)
#v02_Date: remove the UTC from the date values, then change to d/m/y
MergedAIM2_QX<-MergedAIM2_QX %>% 
  separate("v02_Date",
           c("v02_Date","utc"),
           sep=" ",extra="merge")
MergedAIM2_QX$v02_Date<-ymd(
  MergedAIM2_QX$v02_Date)
#v03_Visit_DateA
#MergedAIM2_QX$v03_Visit_DateA<-as.Date(
# MergedAIM2_QX$v03_Visit_DateA,format="%d/%m/%Y")
#v03_Visit_DateB: UTC
MergedAIM2_QX<-MergedAIM2_QX %>% 
  separate("v03_Visit_DateB",
           c("v03_Visit_DateB","utc"),
           sep=" ",extra="merge")
MergedAIM2_QX$v03_Visit_DateB<-ymd(
  MergedAIM2_QX$v03_Visit_DateB)
#v03_Date: "... UTC..."
MergedAIM2_QX<-MergedAIM2_QX %>% 
  separate("v03_Date",
           c("v03_Date","utc"),
           sep=" ",extra="merge")
MergedAIM2_QX$v03_Date<-ymd(
  MergedAIM2_QX$v03_Date)
#v04_Visit_DateA
#MergedAIM2_QX$v04_Visit_DateA<-as.Date(
# MergedAIM2_QX$v04_Visit_DateA,format="%d/%m/%Y")
#v04_Visit_DateB: UTC
MergedAIM2_QX<-MergedAIM2_QX %>% 
  separate("v04_Visit_DateB",
           c("v04_Visit_DateB","utc"),
           sep=" ",extra="merge")
MergedAIM2_QX$v04_Visit_DateB<-ymd(
  MergedAIM2_QX$v04_Visit_DateB)
#v04_Date: UTC
MergedAIM2_QX<-MergedAIM2_QX %>% 
  separate("v04_Date",
           c("v04_Date","utc"),
           sep=" ",extra="merge")
MergedAIM2_QX$v04_Date<-ymd(
  MergedAIM2_QX$v04_Date)
#v05_Visit_DateA
#MergedAIM2_QX$v05_Visit_DateA<-as.Date(
# MergedAIM2_QX$v05_Visit_DateA,format="%d/%m/%Y")
#v05_Visit_DateB: UTC
MergedAIM2_QX<-MergedAIM2_QX %>% 
  separate("v05_Visit_DateB",
           c("v05_Visit_DateB","utc"),
           sep=" ",extra="merge")
MergedAIM2_QX$v05_Visit_DateB<-ymd(
  MergedAIM2_QX$v05_Visit_DateB)
#v05_Date: UTC
MergedAIM2_QX<-MergedAIM2_QX %>% 
  separate("v05_Date",
           c("v05_Date","utc"),
           sep=" ",extra="merge")
MergedAIM2_QX$v05_Date<-ymd(
  MergedAIM2_QX$v05_Date)
#v06_Visit_DateA
#MergedAIM2_QX$v06_Visit_DateA<-as.Date(
# MergedAIM2_QX$v06_Visit_DateA,format="%d/%m/%Y")
#v06_Visit_DateB: UTC
MergedAIM2_QX<-MergedAIM2_QX %>% 
  separate("v06_Visit_DateB",
           c("v06_Visit_DateB","utc"),
           sep=" ",extra="merge")
MergedAIM2_QX$v06_Visit_DateB<-ymd(
  MergedAIM2_QX$v06_Visit_DateB)
#v06_Date: UTC
MergedAIM2_QX<-MergedAIM2_QX %>% 
  separate("v06_Date",
           c("v06_Date","utc"),
           sep=" ",extra="merge")
MergedAIM2_QX$v06_Date<-ymd(
  MergedAIM2_QX$v06_Date)
#v07_Date: UTC
MergedAIM2_QX<-MergedAIM2_QX %>% 
  separate("v07_Date",
           c("v07_Date","utc"),
           sep=" ",extra="merge")
MergedAIM2_QX$v07_Date<-ymd(
  MergedAIM2_QX$v07_Date)
#v08_Date: UTC
MergedAIM2_QX<-MergedAIM2_QX %>% 
  separate("v08_Date",
           c("v08_Date","utc"),
           sep=" ",extra="merge")
MergedAIM2_QX$v08_Date<-ymd(
  MergedAIM2_QX$v08_Date)
#v09_Date: UTC
MergedAIM2_QX<-MergedAIM2_QX %>% 
  separate("v09_Date",
           c("v09_Date","utc"),
           sep=" ",extra="merge")
MergedAIM2_QX$v09_Date<-ymd(
  MergedAIM2_QX$v09_Date)
#v10_Date: UTC
MergedAIM2_QX<-MergedAIM2_QX %>% 
  separate("v10_Date",
           c("v10_Date","utc"),
           sep=" ",extra="merge")
MergedAIM2_QX$v10_Date<-ymd(
  MergedAIM2_QX$v10_Date)
#AAF_TodayDateN
#MergedAIM2_QX$AAF_TodayDateN<-as.Date(
# MergedAIM2_QX$AAF_TodayDateN,format="%d/%m/%Y")
#FirstFUVisit
#MergedAIM2_QX$FirstFUVisit<-as.Date(
 # MergedAIM2_QX$FirstFUVisit,format="%d/%m/%Y")
#Collection_Date1: UTC
MergedAIM2_QX<-MergedAIM2_QX %>% 
  separate("Collection_Date1",
           c("Collection_Date1","utc"),
           sep=" ",extra="merge")
MergedAIM2_QX$Collection_Date1<-ymd(
  MergedAIM2_QX$Collection_Date1)
#Registration_Date1: UTC
MergedAIM2_QX<-MergedAIM2_QX %>% 
  separate("Registration_Date1",
           c("Registration_Date1","utc"),
           sep=" ",extra="merge")
MergedAIM2_QX$Registration_Date1<-ymd(
  MergedAIM2_QX$Registration_Date1)
#Haptoglobin1: change from character to numeric variable
#HIV_Viral_Load_cps_mL1: contains a value of "LDL" while the rest are numbers
#Collection_Date2: UTC
MergedAIM2_QX<-MergedAIM2_QX %>% 
  separate("Collection_Date2",
           c("Collection_Date2","utc"),
           sep=" ",extra="merge")
MergedAIM2_QX$Collection_Date2<-ymd(
  MergedAIM2_QX$Collection_Date2)
#Registration_Date2: UTC
MergedAIM2_QX<-MergedAIM2_QX %>% 
  separate("Registration_Date2",
           c("Registration_Date2","utc"),
           sep=" ",extra="merge")
MergedAIM2_QX$Registration_Date2<-ymd(
  MergedAIM2_QX$Registration_Date2)
#Pct_Saturation2: contains a value of "UTC" while the rest are numbers
#sapply(MergedAIM2_QX[651:759],unique)
#Collection_Date3: UTC
MergedAIM2_QX<-MergedAIM2_QX %>% 
  separate("Collection_Date3",
           c("Collection_Date3","utc"),
           sep=" ",extra="merge")
MergedAIM2_QX$Collection_Date3<-ymd(
  MergedAIM2_QX$Collection_Date3)
#Registration_Date3: UTC
MergedAIM2_QX<-MergedAIM2_QX %>% 
  separate("Registration_Date3",
           c("Registration_Date3","utc"),
           sep=" ",extra="merge")
MergedAIM2_QX$Registration_Date3<-ymd(
  MergedAIM2_QX$Registration_Date3)
#Pct_Saturation: contains a value of "UTC" while the rest are numbers
#HIV_Viral_Load_cps_mL: contains a value of "LDL" while the rest are numbers
#IRON_DEFIC: recode "yes" value to "Yes"

#get counts of unique ID values
attach(MergedAIM2_QX)
#AIM2 Lab Data
sum(!(is.na(`Patient Number`))) #non-NA counts
length(unique(na.omit(`Patient Number`))) #unique counts
#AIM2 Patient Questionnaire
sum(!(is.na(StudyID))) #non-NA counts
length(unique(na.omit(StudyID))) #unique counts 
sum(!(is.na(CaseID))) #non-NA counts
length(unique(na.omit(CaseID))) #unique counts 
#AIM2 Antenatal Anaemia Form (AAF)
sum(!(is.na(SUBJECTID))) #non-NA counts
length(unique(na.omit(SUBJECTID))) #unique counts 
sum(!(is.na(AAF_StudyID))) #non-NA counts
length(unique(na.omit(AAF_StudyID))) #unique counts 
#AIM2 Prospective Visits
sum(!(is.na(Study_ID))) #non-NA counts
length(unique(na.omit(Study_ID))) #unique counts 
detach(MergedAIM2_QX)

#find duplicate rows, if they exist
#AIM2 Lab Data
aim2.lab<-MergedAIM2_QX[c(1:42)]
dups.lab<-duplicated(aim2.lab)
table(dups.lab)
#AIM2 Patient Questionnaire
aim2.pq<-MergedAIM2_QX[c(43:56,58:93)]
dups.pq<-duplicated(aim2.pq)
table(dups.pq)
#Antenatal Anaemia Form
aim2.aaf<-MergedAIM2_QX[c(115:395)]
dups.AAF<-duplicated(aim2.aaf)
table(dups.AAF)
#Prospective Visit
aim2.pv<-MergedAIM2_QX[c(397,400:545)]
dups.pv<-duplicated(aim2.pv)
table(dups.pv)
#scan the dups for a few individuals to make sure no surprises
pq.dupsTF<-aim2.pq
pq.dupsTF$dups.pq<-dups.pq
pq.dupsF<-subset(pq.dupsTF,dups.pq=="FALSE")
pq.dupsT<-subset(pq.dupsTF,dups.pq=="TRUE")
pq.dupsF<-subset(pq.dupsF,select=-c(dups.pq))
all(pq.dupsF==aim2.pq.nodups,na.rm = TRUE)

#remove duplicate observations
aim2.pq.nodups<-distinct(aim2.pq)
sum(is.na(aim2.pq.nodups$StudyID))
aim2.aaf.nodups<-distinct(aim2.aaf)
sum(is.na(aim2.aaf.nodups$AAF_StudyID))
aim2.pv.nodups<-distinct(aim2.pv)
sum(is.na(aim2.pv.nodups$Study_ID))

#investigate rows with missing ID values
aim2.pq.nodups.naids<-subset(aim2.pq.nodups,is.na(StudyID))
aim2.aaf.nodups.naids<-subset(aim2.aaf.nodups,is.na(AAF_StudyID))
aim2.pv.nodups.naids<-subset(aim2.pv.nodups,is.na(Study_ID))

#remove rows with missing ID values
aim2.pq.ultimate<-subset(aim2.pq.nodups,!(is.na(StudyID)))
aim2.aaf.ultimate<-subset(aim2.aaf.nodups,!(is.na(AAF_StudyID)))
aim2.pv.ultimate<-subset(aim2.pv.nodups,!(is.na(Study_ID)))

#find women who have been transfused
#grep("^v.*Transfusion$",colnames(MergedAIM2_QX))
#attach(MergedAIM2_QX)
#table(AAF_TransfusedCurrPreg,useNA = "ifany") #Were they transfused?
#table(EverHadBloodTx,useNA = "ifany") #Ever had a transfusion?
#detach(MergedAIM2_QX)
#tx.df<-subset(MergedAIM2_QX,
#              AAF_TransfusedCurrPreg=="01 Ye" | 
#                EverHadBloodTx=="01 Ye")
#lapply(tx.df[c(411,425,439,453,467,481,495,509,523,537)],
#       table,useNA="ifany") #tranfusion at visit 1, 2, ..., 10
#summary(freqlist(table(tx.df$AAF_TransfusedCurrPreg,
#                       tx.df$EverHadBloodTx,
#                       useNA = "ifany"),
#                 labelTranslations = c("Tx. Curr. Preg.",
#                                       "Ever Had Tx.")))
  #see if these women have booking dates similar to AIM3
#bloop<-sort(tx.df$AAF_CurrBookingDate)
#blahp<-sort(aim3_chki.tx.conf$ATF_BookingDate)
#bloop
#blahp
  #none do, but do they have similar HIV book dates?
#boop<-sort(tx.df$BookingHIVStatusDate)
#bahp<-sort(aim3_chki.tx.conf$ATF_HIVStatusBookingDate)
#boop
#bahp

#sort the IDs
#aim2.aaf.ultimate<-aim2.aaf.ultimate[
 # order(aim2.aaf.ultimate$AAF_StudyID),]
#aim2.pq.ultimate<-aim2.pq.ultimate[
#  order(aim2.pq.ultimate$StudyID),]
#aim2.pv.ultimate<-aim2.pv.ultimate[
#  order(aim2.pv.ultimate$Study_ID),]

#merge 3 forms into one data set with all matching IDs
#aim2.matched<-inner_join(aim2.aaf.ultimate,
#                         aim2.pq.ultimate,
#                         by=c("AAF_StudyID"="StudyID"))
#aim2.matched<-inner_join(aim2.matched,
 #                        aim2.pv.ultimate,
#                         by=c("AAF_StudyID"="Study_ID"))

#find all the columns that are completely empty
#aim2.na<-MergedAIM2_QX[,colSums(is.na(MergedAIM2_QX))==nrow(MergedAIM2_QX)]
#sapply(aim2.na,function(x) sum(is.na(x)))

#keep the columns that have values
aim2.pq.ultimate<-aim2.pq.ultimate[,colSums(is.na(aim2.pq.ultimate))!=nrow(aim2.pq.ultimate)]
aim2.aaf.ultimate<-aim2.aaf.ultimate[,colSums(is.na(aim2.aaf.ultimate))!=nrow(aim2.aaf.ultimate)]
aim2.pv.ultimate<-aim2.pv.ultimate[,colSums(is.na(aim2.pv.ultimate))!=nrow(aim2.pv.ultimate)]

#investigate what the visit dates should be
aim2.dates<-MergedAIM2_QX[,grep(".*date.*",
                                colnames(MergedAIM2_QX),ignore.case = TRUE)]
aim2.dates$studyid<-MergedAIM2_QX$`Patient Number`
aim2.dates<-aim2.dates%>%select(studyid,everything())
write.csv(aim2.dates,"AIM2 All Date Columns.csv")
#glimpse(aim2.dates)
#investigation
#attach(aim2.dates)
#unique(TodayDate)

#recodes
aim2.aaf.ultimate$AAF_ReasonForReferral<-gsub("^ReasonRef","",
                                              aim2.aaf.ultimate$AAF_ReasonForReferral)
aim2.aaf.ultimate$AAF_ReasonForReferral<-gsub("^RaesonRef","",
                                              aim2.aaf.ultimate$AAF_ReasonForReferral)
aim2.aaf.ultimate$AAF_ReasonForReferral<-gsub("?.ReasonRef.?","",
                                              aim2.aaf.ultimate$AAF_ReasonForReferral)
aim2.aaf.ultimate$AAF_DxForAnaemia<-gsub("^DxForAnemia_","",
                                         aim2.aaf.ultimate$AAF_DxForAnaemia)
aim2.aaf.ultimate$AAF_DxForAnaemia<-gsub("?.DxForAnemia.?","",
                                         aim2.aaf.ultimate$AAF_DxForAnaemia)
aim2.aaf.ultimate$AAF_ChronicAnaemiaCause<-gsub("^AnaemiaCause_","",
                                                aim2.aaf.ultimate$AAF_ChronicAnaemiaCause)
aim2.aaf.ultimate$AAF_PrevDelvComps<-str_trim(aim2.aaf.ultimate$AAF_PrevDelvComps,
                                              side=c("both"))
aim2.aaf.ultimate$AAF_PrevDelvComps<-gsub("^PrevDelvComps_","",
                                          aim2.aaf.ultimate$AAF_PrevDelvComps)
aim2.aaf.ultimate$AAF_ThisPregComps<-gsub("^ThisPregComps_","",
                                          aim2.aaf.ultimate$AAF_ThisPregComps)

#investigate possible typos
attach(aim2.aaf.ultimate)
#find unique values of variables with typos
unique(AAF_Gravidity) #62,92
unique(AAF_Parity) #91, 61
unique(AAF_NumVisitsRefAnteClinic) #62,61,64,81,98
unique(AAF_NumCSect) #39,36,29,28,20,25,40,31,33,19,24
unique(AAF_NumPrevStillbirths) #60
unique(AAF_NumPrevMiscarriages) #60
#find row indices that contain the possible typo
which(AAF_Gravidity=="62" | AAF_Gravidity=="92")
which(AAF_Parity=="61" | AAF_Parity=="91")
which(AAF_NumVisitsRefAnteClinic=="62" | 
        AAF_NumVisitsRefAnteClinic=="61" | 
        AAF_NumVisitsRefAnteClinic=="64" | 
        AAF_NumVisitsRefAnteClinic=="81" | 
        AAF_NumVisitsRefAnteClinic=="98")
which(AAF_NumCSect=="39" | AAF_NumCSect=="36" | 
        AAF_NumCSect=="29" | AAF_NumCSect=="28" | 
        AAF_NumCSect=="20" | AAF_NumCSect=="25" | 
        AAF_NumCSect=="40" | AAF_NumCSect=="31" | 
        AAF_NumCSect=="33" | AAF_NumCSect=="19" | 
        AAF_NumCSect=="24")
which(AAF_NumPrevStillbirths=="60")
which(AAF_NumPrevMiscarriages=="60")
#find column indices of variables with possible typos
grep(".*Gravidity",colnames(aim2.aaf.ultimate))
grep(".*Parity",colnames(aim2.aaf.ultimate))
grep(".*NumVisitsRefAnteClinic",colnames(aim2.aaf.ultimate))
grep(".*NumCSect",colnames(aim2.aaf.ultimate))
grep(".*NumPrevStillbirths",colnames(aim2.aaf.ultimate))
grep(".*NumPrevMiscarriages",colnames(aim2.aaf.ultimate))
#see data frame of typos
aim2.aaf.typos<-aim2.aaf.ultimate[c(31,32,47,49,61,113,114,116,123,181,
                    223,226,227,228,238,241,261,269,
                    309,313,318,339,357,366,386,
                    427,473,482),
                  c(17:23,90:92,108:111)]
write.csv(aim2.aaf.typos,"AIM2_AAF_typos data frame.csv")

#find HIV+ with some variables
attach(aim2.aaf.ultimate)
summary(freqlist(table(BookingHIVStatus,
                       DelveryAdmissionHIVStatus,
                       useNA = "ifany"),
                 labelTranslations=c("Booking HIV Status",
                                     "Delivery Admission HIV Status")))
summary(freqlist(table(BookingHIVStatus,
                       AAF_OnARTPriorThisPreg,
                       useNA = "ifany"),
                 labelTranslations=c("Booking HIV Status",
                                     "ART Use Prior Preg.?")))
summary(freqlist(table(BookingHIVStatus,
                       AAF_ARTPMTCTThisPreg,
                       useNA = "ifany"),
                 labelTranslations=c("Booking HIV Status",
                                     "ART PMTCT This Preg.?")))
summary(freqlist(table(BookingHIVStatus,
                       AAF_HIVPosPrecedePreg,
                       useNA = "ifany"),
                 labelTranslations=c("Booking HIV Status",
                                     "HIV Pos. Preceding Preg.?")))
summary(freqlist(table(BookingHIVStatus,
                       AAF_PMTCTUsedPrecedePreg,
                       useNA = "ifany"),
                 labelTranslations=c("Booking HIV Status",
                                     "PMTCT Use Preceding Preg.?")))
summary(freqlist(table(BookingHIVStatus,
                       AAF_ARTUsedPrecedePreg,
                       useNA = "ifany"),
                 labelTranslations=c("Booking HIV Status",
                                     "ART Used Preceding Preg.?")))
detach(aim2.aaf.ultimate)

#summaries
dfSummary(aim2.pq.ultimate,style = 'grid', plain.ascii = FALSE)
dfSummary(aim2.aaf.ultimate,
                            style='grid',plain.scii=FALSE)
dfSummary(aim2.pv.ultimate,
                           style='grid',plain.ascii=FALSE)

