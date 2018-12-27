library(readr)
library(ggplot2)
library(arsenal)
library(tableone)
library(lubridate)
library(StatMeasures)
library(dplyr)
library(tidyr)
aim3.dat.clean <- read_csv("BSRI/TIP Stuff/aim3_Dat_clean_correct (version 1).csv", 
                         col_types = cols(ATF_AdmDate = col_date(format = "%d/%m/%Y"), 
                                          ATF_BookingDate = col_date(format = "%m/%d/%Y"), 
                                          ATF_ComponentsTXDate1 = col_date(format = "%d/%m/%Y"), 
                                          ATF_ComponentsTXDate2 = col_date(format = "%d/%m/%Y"), 
                                          ATF_ComponentsTXDate3 = col_date(format = "%d/%m/%Y"), 
                                          ATF_ComponentsTXDate4 = col_date(format = "%d/%m/%Y"), 
                                          ATF_ComponentsTXDate5 = col_date(format = "%d/%m/%Y"), 
                                          ATF_ComponentsTXDate6 = col_date(format = "%d/%m/%Y"), 
                                          ATF_ComponentsTXDate7 = col_date(format = "%d/%m/%Y"), 
                                          ATF_DschgDate = col_date(format = "%d/%m/%Y"), 
                                          ATF_EstDateDelivery = col_date(format = "%m/%d/%Y"), 
                                          ATF_HIVStatusBookingDate = col_date(format = "%m/%d/%Y"), 
                                          ATF_HIVStatusDelvAdmitDate = col_date(format = "%m/%d/%Y"), 
                                          ATF_HIVStatusOthTestDate = col_date(format = "%m/%d/%Y"), 
                                          ATF_PMTCTStartDate = col_date(format = "%m/%d/%Y"), 
                                          ATF_TodayDate = col_date(format = "%m/%d/%Y"), 
                                          ChartAbstrationDate = col_date(format = "%m/%d/%Y"), 
                                          ConsentDate = col_date(format = "%m/%d/%Y"), 
                                          CreatedDate = col_date(format = "%m/%d/%Y")))
View(aim3.dat.clean)

#subset out Groote/Schurr hospitals
aim3_chris.king<-subset(aim3.dat.clean,
                              LOCATION=="Chris Hani" | 
                                LOCATION=="King Edward")

#add gestation weeks
gest.wks.tri<-cut(aim3_chris.king$ATF_GestAgeWks,
              breaks=c(-Inf,13,25,Inf),
              label=c("<14","14-25",">25"),
              right=FALSE)
aim3_chris.king$gest.wks.tri<-gest.wks.tri
aim3_chris.king<-aim3_chris.king[c(1:16,116,17:115)]
aim3_chris.king<-aim3_chris.king[-c(112)]

#rename column
colnames(aim3_chris.king)[
  colnames(aim3_chris.king)=="ATF_Transfused.x"
]<-"ATF_Transfused"

#recalculate stay length column
aim3_chris.king$Stay_Length<-as.numeric(
  aim3_chris.king$ATF_DschgDate)-as.numeric(
    aim3_chris.king$ATF_AdmDate)

#remove the observations that don't have recorded tx's
aim3_chki.tx.conf<-subset(x=aim3_chris.king,
                          subset=!(rowSums(aim3_chris.king[c(113:115)])==0))

#Joans' CSV
aim3Dat_18Dec18 <- read_csv("BSRI/TIP Stuff/aim3Dat_18Dec18.csv", 
                            col_types = cols(AdmDate = col_date(format = "%d/%m/%Y"), 
                                             DschgDate = col_date(format = "%d/%m/%Y"), 
                                             Est_Delivery = col_date(format = "%d/%m/%Y"), 
                                             TXDate1 = col_date(format = "%d/%m/%Y"), 
                                             TXDate2 = col_date(format = "%d/%m/%Y"), 
                                             TXDate3 = col_date(format = "%d/%m/%Y"), 
                                             TXDate4 = col_date(format = "%d/%m/%Y"), 
                                             TXDate5 = col_date(format = "%d/%m/%Y"), 
                                             TXDate6 = col_date(format = "%d/%m/%Y"), 
                                             TXDate7 = col_date(format = "%d/%m/%Y")))
View(aim3Dat_18Dec18)

#Joan's CSV
aim3_Joan<-inner_join(aim3_chki.tx.conf,
                     aim3Dat_18Dec18,
                     by="studyId")
aim3_Joan<-aim3_Joan[c(1:14,116,117,15:115,118:157)]
write.csv(aim3_Joan,"aim3_Dec18_nis560.csv")
aim3_Joan$ATF_AdmDate==aim3_Joan$AdmDate
which(!(aim3_Joan$ATF_AdmDate==aim3_Joan$AdmDate))
aim3_Joan_Adam<-subset(aim3_Joan,
                       !(aim3_Joan$ATF_AdmDate==aim3_Joan$AdmDate))
write.csv(aim3_Joan_Adam,"aim3_ADM_DSCH.csv")

#match mine to the original to document changes
aim3_joined<-inner_join(aim3_chki.tx.conf,
                        aim3Dat_all,
                        by="studyId")
which(!(aim3_joined$ATF_AdmDate.x==aim3_joined$ATF_AdmDate.y))
which(!(aim3_joined$ATF_DschgDate.x==aim3_joined$ATF_DschgDate.y))
aim3_joined.differ<-subset(aim3_joined,
                           (!(aim3_joined$ATF_AdmDate.x==
                                aim3_joined$ATF_AdmDate.y) | 
                              !(aim3_joined$ATF_DschgDate.x==
                                  aim3_joined$ATF_DschgDate.y)))
write.csv(aim3_joined,"aim3_joined(2).csv")
#if an observation has an entry in the last 3 columns 
#(redTotal, pltTotal, and ffpTotal) that is not zero, then we can 
#change that observation to a "01 Yes"
#aim3_chris.king<-within(aim3_chris.king,
 #                ATF_Transfused[
  #                !(redTotal==0) | 
   #                 !(pltTotal==0) | 
    #                !(ffpTotal==0)]<-"01 Yes")
#table(aim3_chris.king$ATF_Transfused,useNA = "ifany")
#which(is.na(aim3_chris.king$ATF_Transfused))
#remove observations that were not transfused
#aim3_chris.king<-subset(aim3_chris.king,
 #                       !(is.na(ATF_Transfused)))

#check atf component date columns for typos
lapply(aim3_chki.tx.conf[c(12,13,14,70,73,76,79,82,85,88)],
       unique)

#order data frame
attach(aim3_chki.tx.conf)
aim3.sort2<-aim3_chki.tx.conf[order(Stay_Length),]
detach(aim3_chki.tx.conf)

#investigation of RBC, platelet, and plasma counts
ft.rowsums <- regulartable(data = head(aim3_chris.king[
  c(91:97,112)]))
ft.rowsums <- regulartable(data = head(aim3_chris.king[
      c(91:97,112)])) %>%
  set_formatter(ft.rowsums,
                studyId=function(x) sprintf("%.0f",x)) %>%
  theme_zebra %>% 
  autofit
ft.rowsums # See flextable in RStudio viewer
tmp <- tempfile(fileext = ".docx") # Create a temp file
read_docx() %>%                        # Create a docx file
  body_add_flextable(ft.rowsums) %>% 
  print(target = tmp)
browseURL(tmp) # open word document
ft.rowsums <- regulartable(data = head(aim3_chris.king[
  c(98:104,113)]))
ft.rowsums <- regulartable(data = head(aim3_chris.king[
  c(98:104,113)])) %>%
    set_formatter(ft.rowsums,
                  studyId=function(x) sprintf("%.0f",x)) %>%
    theme_zebra %>% 
    autofit
  ft.rowsums # See flextable in RStudio viewer
  tmp <- tempfile(fileext = ".docx") # Create a temp file
  read_docx() %>%                        # Create a docx file
    body_add_flextable(ft.rowsums) %>% 
    print(target = tmp)
  browseURL(tmp) # open word document
ft.rowsums <- regulartable(data = head(aim3_chris.king[
    c(105:111,114)]))
ft.rowsums <- regulartable(data = head(aim3_chris.king[
    c(105:111,114)])) %>%
      set_formatter(ft.rowsums,
                    studyId=function(x) sprintf("%.0f",x)) %>%
      theme_zebra %>% 
      autofit
ft.rowsums # See flextable in RStudio viewer
    tmp <- tempfile(fileext = ".docx") # Create a temp file
    read_docx() %>%                        # Create a docx file
      body_add_flextable(ft.rowsums) %>% 
      print(target = tmp)
    browseURL(tmp) # open word document
#check
grep("^red*",colnames(aim3_chris.king))
grep("^plt*",colnames(aim3_chris.king))
grep("^ffp*",colnames(aim3_chris.king))
redT<-rowSums(aim3_chris.king[c(91:97)],na.rm=TRUE)
plateT<-rowSums(aim3_chris.king[c(98:104)],na.rm=TRUE)
plasmaT<-rowSums(aim3_chris.king[c(105:111)],na.rm = TRUE)
all(redT==aim3_chris.king$redTotal)
all(plateT==aim3_chris.king$pltTotal)
all(plasmaT==aim3_chris.king$ffpTotal)
#verify total by type
#create flextable
ft.rowsums <- regulartable(data = head(aim3_chris.king[
  c(72,91,75,92,78,93)]))
ft.rowsums <- regulartable(data = head(aim3_chris.king[
  c(72,91,75,92,78,93)])) %>%
    set_formatter(ft.rowsums,
                  studyId=function(x) sprintf("%.0f",x)) %>%
    theme_zebra %>% 
    autofit
ft.rowsums # See flextable in RStudio viewer
  tmp <- tempfile(fileext = ".docx") # Create a temp file
  read_docx() %>%                        # Create a docx file
    body_add_flextable(ft.rowsums) %>% 
    print(target = tmp)
  browseURL(tmp) # open word document
#verification through crosstabs
attach(aim3_chris.king)
col1<-red1
col2<-ATF_ComponentsTxType1
summary(freqlist(table(col1,col2,useNA = "ifany")))
col1<-red2
col2<-ATF_ComponentsTxType2
summary(freqlist(table(col1,col2,useNA = "ifany")))
col1<-red3
col2<-ATF_ComponentsTxType3
summary(freqlist(table(col1,col2,useNA = "ifany")))
col1<-red4
col2<-ATF_ComponentsTxType4
summary(freqlist(table(col1,col2,useNA = "ifany")))
col1<-red5
col2<-ATF_ComponentsTxType5
summary(freqlist(table(col1,col2,useNA = "ifany")))
col1<-red6
col2<-ATF_ComponentsTxType6
summary(freqlist(table(col1,col2,useNA = "ifany")))
col1<-red7
col2<-ATF_ComponentsTxType7
summary(freqlist(table(col1,col2,useNA = "ifany")))
col1<-plt1
col2<-ATF_ComponentsTxType1
summary(freqlist(table(col1,col2,useNA = "ifany")))
col1<-plt2
col2<-ATF_ComponentsTxType2
summary(freqlist(table(col1,col2,useNA = "ifany")))
col1<-plt3
col2<-ATF_ComponentsTxType3
summary(freqlist(table(col1,col2,useNA = "ifany")))
col1<-plt4
col2<-ATF_ComponentsTxType4
summary(freqlist(table(col1,col2,useNA = "ifany")))
col1<-plt5
col2<-ATF_ComponentsTxType5
summary(freqlist(table(col1,col2,useNA = "ifany")))
col1<-plt6
col2<-ATF_ComponentsTxType6
summary(freqlist(table(col1,col2,useNA = "ifany")))
col1<-plt7
col2<-ATF_ComponentsTxType7
summary(freqlist(table(col1,col2,useNA = "ifany")))
col1<-ffp1
col2<-ATF_ComponentsTxType1
summary(freqlist(table(col1,col2,useNA = "ifany")))
col1<-ffp2
col2<-ATF_ComponentsTxType2
summary(freqlist(table(col1,col2,useNA = "ifany")))
col1<-ffp3
col2<-ATF_ComponentsTxType3
summary(freqlist(table(col1,col2,useNA = "ifany")))
col1<-ffp4
col2<-ATF_ComponentsTxType4
summary(freqlist(table(col1,col2,useNA = "ifany")))
col1<-ffp5
col2<-ATF_ComponentsTxType5
summary(freqlist(table(col1,col2,useNA = "ifany")))
col1<-ffp6
col2<-ATF_ComponentsTxType6
summary(freqlist(table(col1,col2,useNA = "ifany")))
col1<-ffp7
col2<-ATF_ComponentsTxType7
summary(freqlist(table(col1,col2,useNA = "ifany")))
detach(aim3_chris.king)
#sums
grep("Total$",colnames(aim3_chris.king))
table(aim3_chris.king$ATF_Transfused,
      rowSums(aim3_chris.king[c(112:114)]),useNA = "ifany")
summary(freqlist(table(aim3_chris.king$redTotal,
                       aim3_chris.king$pltTotal,
                       aim3_chris.king$ffpTotal),
                 labelTranslations = c("RBC Total",
                                       "Platelet Total",
                                       "Plasma Total")))

#further investigations#

#check if transfusion dates are between admission and discharge date
adm.dsch.int<-interval(aim3_chki.tx.conf$ATF_AdmDate,
                       aim3_chki.tx.conf$ATF_DschgDate)
adm.tx.dsch.f<-aim3_chki.tx.conf$ATF_ComponentsTXDate1 %within% adm.dsch.int
aim3_chki.tx.conf$adm.tx.dsch.f<-adm.tx.dsch.f
aim3_adm.tx.dsch.false<-subset(aim3_chki.tx.conf,
                               adm.tx.dsch.f=="FALSE" | 
                                 is.na(adm.tx.dsch.f))
write.csv(aim3_adm.tx.dsch.false,"aim3_transfusion not within(2).csv")

#the removed observations
aim3_0<-subset(x=aim3_chris.king,
                       subset=rowSums(aim3_chris.king[c(112:114)])==0)
