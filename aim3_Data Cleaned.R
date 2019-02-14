library(readr)
library(ggplot2)
library(arsenal)
library(tableone)
library(lubridate)
library(StatMeasures)
library(dplyr)
library(tidyr)
library(stringr)
library(magrittr)
library(officer)
library(flextable)
library(scales)
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

#recode Q6.5 "Blood Tx Rationale"
MedicalRationale<-
  paste(aim3_chki.tx.conf$ATF_MedicalRationaleForTx,
        aim3_chki.tx.conf$MedicalRationaleOtherSpecify)
MedicalRationale<-str_trim(MedicalRationale,side=c("both"))
MedicalRationale<-gsub(" NA$","",MedicalRationale)
table(MedicalRationale) #get counts beforehand
MedicalRationale<-gsub("^_Anaesthetic.*","ANESTH",
                       MedicalRationale)
MedicalRationale<-gsub(".*Anaesthetic.*","ANESTH",
                       MedicalRationale)
MedicalRationale<-gsub(".*DAYS$","BOTH",
                       MedicalRationale)
MedicalRationale<-gsub(".*Other ICA$","HEMOR",
                       MedicalRationale)
MedicalRationale<-gsub("^_Chronic.*","ANEM",
                       MedicalRationale)
MedicalRationale<-gsub(".*Su.*","SURG",ignore.case = TRUE,
                       MedicalRationale)
MedicalRationale<-gsub("^_Other AN.*","ANEM",
                       MedicalRationale)
MedicalRationale<-gsub(".*Other AN.*","BOTH",
                       MedicalRationale)
MedicalRationale<-gsub(".*PALE$","HEMOR",
                       MedicalRationale)
MedicalRationale<-gsub(".*AND.*","BOTH",
                       MedicalRationale)
MedicalRationale<-gsub(".*ACUTE.*","ANEM",
                       MedicalRationale)
MedicalRationale<-gsub(".*SALPINGECTOMY$","SURG",
                       MedicalRationale)
MedicalRationale<-gsub(".*ECTOPIC.*","HEMOR",
                       MedicalRationale)
MedicalRationale<-gsub(".*HYPOCHYLAEMIA$",
                       "HEMOR",
                       MedicalRationale)
MedicalRationale<-gsub(".*MISCARRIAGE$",
                       "HEMOR",
                       MedicalRationale)
MedicalRationale<-gsub(".*IUD$","HEMOR",
                       MedicalRationale)
MedicalRationale<-gsub(".*PRODUCTS$","HEMOR",
                       MedicalRationale)
MedicalRationale<-gsub(".*ABORT$","HEMOR",
                       MedicalRationale)
MedicalRationale<-gsub(".*PRAGENIA$","HEMOR",
                       MedicalRationale)
MedicalRationale<-gsub(".*PLACENTA$","HEMOR",
                       MedicalRationale)
MedicalRationale<-gsub(".*MALARIA$","OTHER",
                       MedicalRationale)
MedicalRationale<-gsub(".*SEP.*","OTHER",
                       MedicalRationale)
MedicalRationale<-gsub(".*THROMOCYTOPAGENIA",
                       "OTHER",MedicalRationale)
MedicalRationale<-gsub(".*CURRETAGE","SURG",
                       MedicalRationale)
MedicalRationale<-gsub(".*UTERUS$","SURG",
                       MedicalRationale)
MedicalRationale<-gsub(".*TOP.*","SURG",
                       MedicalRationale)
MedicalRationale<-gsub("^_OBHemorrhage$","HEMOR",
                       MedicalRationale)
MedicalRationale<-gsub("_OBHemorrhage.*","BOTH",
                       MedicalRationale)
MedicalRationale<-gsub("_Other$","OTHER",
                       MedicalRationale)
MedicalRationale<-gsub(".*Unknown$","OTHER",
                       MedicalRationale)
MedicalRationale<-ifelse(
  MedicalRationale=="HEMOR","HEMOR",
  ifelse(MedicalRationale=="BOTH","BOTH",
         ifelse(MedicalRationale=="SURG","SURG",
                ifelse(MedicalRationale=="ANESTH","ANESTH",
                       ifelse(MedicalRationale=="OTHER","OTHER",
                              ifelse(MedicalRationale=="NA","NA","ANEM"
                                     ))))))
summary(freqlist(table(MedicalRationale,
                       useNA = "ifany"),
                 labelTranslations = "Medical Rationale"))
MedicalRationale[MedicalRationale=="NA"]<-NA

#recode Q5.2 Hemorrhage Cause
HemorrhageCause<-
  paste(aim3_chki.tx.conf$ATF_HemorrhageCause,
        aim3_chki.tx.conf$HemorrhageCauseOtherSpecify)
HemorrhageCause<-str_trim(HemorrhageCause,side=c("both"))
HemorrhageCause<-gsub(" NA$","",HemorrhageCause)
table(HemorrhageCause) #get counts beforehand
HemorrhageCause<-gsub("_Unknown$","UNK",HemorrhageCause)
HemorrhageCause<-gsub("^_Ante.*","HEMOR",HemorrhageCause)
HemorrhageCause<-gsub("^_Bleed.*","SURG",HemorrhageCause)
HemorrhageCause<-gsub("^_Blled.*","SURG",HemorrhageCause)
HemorrhageCause<-gsub("^_Complete.*","C_ABORT",HemorrhageCause)
HemorrhageCause<-gsub("^_Eltopic.*","ECTOPIC",HemorrhageCause)
HemorrhageCause<-gsub("^_PlacentalAbr.*","ABRUPTION",HemorrhageCause)
HemorrhageCause<-gsub("^_PlacentaPre.*","PREVIA",HemorrhageCause)
HemorrhageCause<-gsub("^.*IUD$","DEATH",HemorrhageCause)
HemorrhageCause<-gsub("^_Incomplete.*","I_ABORT",HemorrhageCause)
HemorrhageCause<-gsub(".*INEVITABLE.*","I_ABORT",HemorrhageCause)
HemorrhageCause<-gsub(".*PALE.*","I_ABORT",HemorrhageCause)
HemorrhageCause<-gsub(".*ABORTION$","I_ABORT",HemorrhageCause)
HemorrhageCause<-gsub("^_Threat.*","T_ABORT",HemorrhageCause)
HemorrhageCause<-gsub(".*ANEA.*","ANEM",HemorrhageCause)
HemorrhageCause<-gsub("_Other TOP","C_ABORT",HemorrhageCause)
HemorrhageCause<-gsub(".*CERV.*","CERVIX",HemorrhageCause)
HemorrhageCause<-gsub(".*APH.*","HEMOR",HemorrhageCause)
HemorrhageCause<-gsub("^_Other$","UNK",HemorrhageCause)
HemorrhageCause<-gsub(".*Other.*","DEATH",HemorrhageCause)
summary(freqlist(table(HemorrhageCause,
                       useNA = "ifany"),
                 labelTranslations = "Hemorrhage Cause"))
HemorrhageCause[HemorrhageCause=="NA"]<-NA

#recode of Complications this Admission
CompsThisAdm<-
  paste(aim3_chki.tx.conf$ATF_CompsThisAdmType,
        aim3_chki.tx.conf$ComplicationsThisAdmissionSP)
CompsThisAdm<-str_trim(CompsThisAdm,side=c("both"))
table(CompsThisAdm) #get counts beforehand
CompsThisAdm<-gsub(" NA$","",CompsThisAdm)
CompsThisAdm<-gsub(".*Renal.*","RENAL",ignore.case = TRUE,CompsThisAdm)
CompsThisAdm<-gsub(".*Sep.*","SEPSIS",ignore.case=TRUE,CompsThisAdm)
CompsThisAdm<-gsub(".*Shock.*","SHOCK",ignore.case=TRUE,CompsThisAdm)
CompsThisAdm<-gsub("^_SymptAnaemia$","ANEM",CompsThisAdm)
CompsThisAdm<-gsub(".*Resp.*","RESPIRATORY",CompsThisAdm)
CompsThisAdm<-gsub(".*Hep.*","HEPATIC",CompsThisAdm)
CompsThisAdm<-gsub(".*HYPO.*","RENAL",CompsThisAdm)
CompsThisAdm<-gsub(".*DIARR.*","GASTRO",CompsThisAdm)
CompsThisAdm<-gsub(".*ATRIAC.*","CARDIAC",CompsThisAdm)
CompsThisAdm<-gsub("^_Other$","OTHER",CompsThisAdm)
CompsThisAdm<-gsub(".*MALARIA.*","OTHER",CompsThisAdm)
CompsThisAdm<-gsub(".*LOW.*","ANEM",CompsThisAdm)
CompsThisAdm<-ifelse(
  CompsThisAdm=="ANEM","ANEM",
  ifelse(CompsThisAdm=="CARDIAC","CARDIAC",
         ifelse(CompsThisAdm=="GASTRO","GASTRO",
                ifelse(CompsThisAdm=="HEPATIC","HEPATIC",
                       ifelse(CompsThisAdm=="RESPIRATORY","RESPIRATORY",
                              ifelse(CompsThisAdm=="OTHER","OTHER",
                                     ifelse(CompsThisAdm=="RENAL","RENAL",
                                            ifelse(CompsThisAdm=="SEPSIS","SEPSIS",
                                                   ifelse(CompsThisAdm=="SHOCK","SHOCK",
                                                          ifelse(CompsThisAdm=="NA","NA","OBGYN"
                                                                 )))))))))
)
summary(freqlist(table(CompsThisAdm,
                       useNA = "ifany"),
                 labelTranslations = "Complications this Admission"))
CompsThisAdm[CompsThisAdm=="NA"]<-NA

#recode of Complications This Pregnancy
CompsThisPreg<-
  paste(aim3_chki.tx.conf$ATF_CompsThisPregType,
        aim3_chki.tx.conf$ComplicationsThisPregnancySP)
CompsThisPreg<-str_trim(CompsThisPreg,side=c("both"))
table(CompsThisPreg) #get counts beforehand
CompsThisPreg<-gsub(" NA$","",CompsThisPreg)
CompsThisPreg<-gsub("^_Gestro.*","GPH",CompsThisPreg)
CompsThisPreg<-gsub("^_Intra.*","DEATH",CompsThisPreg)
CompsThisPreg<-gsub("^_Multip.*","MULTIP",CompsThisPreg)
CompsThisPreg<-gsub("^_Malp.*","MALPOSITION",CompsThisPreg)
CompsThisPreg<-gsub("^_Unknown$","UNK",CompsThisPreg)
CompsThisPreg<-gsub("^_Other$","OTHER",CompsThisPreg)
CompsThisPreg<-gsub("^_PlacentaPrevia$","PREVIA",CompsThisPreg)
CompsThisPreg<-gsub("^_ThreatenedAbort$","T_ABORT",CompsThisPreg)
CompsThisPreg<-gsub(".*_IntrauterineDeath$","T_ABORT",CompsThisPreg)
CompsThisPreg<-gsub(".*RENAL.*","RENAL",CompsThisPreg)
CompsThisPreg<-gsub(".*ANE.*","ANEM",CompsThisPreg)
CompsThisPreg<-gsub(".*ANA.*","ANEM",CompsThisPreg)
CompsThisPreg<-gsub(".*ANM.*","ANEM",CompsThisPreg)
CompsThisPreg<-gsub(".*APH.*","HEMOR",CompsThisPreg)
CompsThisPreg<-gsub(".*ABRUPT.*","PLACENTA",CompsThisPreg)
CompsThisPreg<-gsub(".*BLEED.*","HEMOR",CompsThisPreg)
CompsThisPreg<-gsub(".*ECTO.*","ECTOPIC",CompsThisPreg)
CompsThisPreg<-gsub(".*ETOPIC.*","ECTOPIC",CompsThisPreg)
CompsThisPreg<-gsub(".*CARDIAC.*","CARDIAC",CompsThisPreg)
CompsThisPreg<-gsub(".*CHEST.*","CARDIAC",CompsThisPreg)
CompsThisPreg<-gsub(".*SEP.*","SEPSIS",CompsThisPreg)
CompsThisPreg<-gsub(".*ICA.*","I_ABORT",CompsThisPreg)
CompsThisPreg<-gsub(".*CERV.*","CERVIX",CompsThisPreg)
CompsThisPreg<-gsub(".*DIAL.*","RENAL",CompsThisPreg)
CompsThisPreg<-gsub(".*PLAC.*","PLACENTA",CompsThisPreg)
CompsThisPreg<-gsub(".*WARTS.*","OTHER",CompsThisPreg)
CompsThisPreg<-gsub(".*TB.*","OTHER",CompsThisPreg)
CompsThisPreg<-gsub(".*REFER.*","OTHER",CompsThisPreg)
CompsThisPreg<-gsub(".*MENOR.*","HEMOR",CompsThisPreg)
CompsThisPreg<-gsub(".*HYP.*","HYPERT",CompsThisPreg)
CompsThisPreg<-gsub(".*EXT.*","ECTOPIC",CompsThisPreg)
CompsThisPreg<-gsub(".*DEA.*","DEATH",CompsThisPreg)
CompsThisPreg<-gsub(".*FMNF.*","DEATH",CompsThisPreg)
CompsThisPreg<-gsub(".*FETA.*","DEATH",CompsThisPreg)
CompsThisPreg<-gsub(".*INC.*","I_ABORT",CompsThisPreg)
CompsThisPreg<-gsub(".*NEV.*","I_ABORT",CompsThisPreg)
CompsThisPreg<-gsub(".*Unknown$","UNK",CompsThisPreg)
CompsThisPreg<-gsub("^_Other.*","C_ABORT",CompsThisPreg)
CompsThisPreg<-gsub("^NA .*","C_ABORT",CompsThisPreg)
summary(freqlist(table(CompsThisPreg,
                       useNA = "ifany"),
                 labelTranslations = "Complications This Pregnancy"))
CompsThisPreg[CompsThisPreg=="NA"]<-NA

#recode 97 Unknown as NA
aim3_chki.tx.conf$ATF_TransfusePriorToCurr[
  aim3_chki.tx.conf$ATF_TransfusePriorToCurr=="97 Unknown"]<-NA
aim3_chki.tx.conf$ATF_OnHematinicRxDuringPreg[
  aim3_chki.tx.conf$ATF_OnHematinicRxDuringPreg=="97 Unknown"]<-NA
aim3_chki.tx.conf$ATF_TxHighestLevelDiscuss[
  aim3_chki.tx.conf$ATF_TxHighestLevelDiscuss=="97 Unknown"]<-NA
aim3_chki.tx.conf$ATF_HbMethodUsed[
  aim3_chki.tx.conf$ATF_HbMethodUsed=="97 Unknown"]<-NA
aim3_chki.tx.conf$ATF_WhereResultsObtainedPriorTx[
  aim3_chki.tx.conf$ATF_WhereResultsObtainedPriorTx=="97 Unknown"]<-NA
aim3_chki.tx.conf$ATF_IdAnemicDuringCurrPreg[
  aim3_chki.tx.conf$ATF_IdAnemicDuringCurrPreg=="97 Unknown"]<-NA
aim3_chki.tx.conf$ATF_BleedingThisPreg[
  aim3_chki.tx.conf$ATF_BleedingThisPreg=="97 Unknown"]<-NA
aim3_chki.tx.conf$ATF_MedicalRationaleForTx[
  aim3_chki.tx.conf$ATF_MedicalRationaleForTx=="97 Unknown"]<-NA
aim3_chki.tx.conf$ATF_PMTCTThisPreg[
  aim3_chki.tx.conf$ATF_PMTCTThisPreg=="97 Unknown"]<-NA
aim3_chki.tx.conf$ATF_OnARTPriorThisPreg[
  aim3_chki.tx.conf$ATF_OnARTPriorThisPreg=="97 Unknown"]<-NA
aim3_chki.tx.conf$ATF_OnARTPriorThisPreg[
  aim3_chki.tx.conf$ATF_OnARTPriorThisPreg=="Skip"]<-NA
aim3_chki.tx.conf$ATF_HIVStatusAtBooking[
  aim3_chki.tx.conf$ATF_HIVStatusAtBooking=="97 Unknown"]<-NA
aim3_chki.tx.conf$ATF_HIVStatusDelvAdmission[
  aim3_chki.tx.conf$ATF_HIVStatusDelvAdmission=="97 Unknown"]<-NA
aim3_chki.tx.conf$ATF_HIVStatusOthTest[
  aim3_chki.tx.conf$ATF_HIVStatusOthTest=="97 Unknown"]<-NA
aim3_chki.tx.conf$ATF_HIVStatusOthTest2[
  aim3_chki.tx.conf$ATF_HIVStatusOthTest2=="97 Unknown"]<-NA
aim3_chki.tx.conf$ATF_PMTCTThisPreg[
  is.na(aim3_chki.tx.conf$ATF_PMTCTThisPreg)]<-"02 No"

#Combine HIV Statuses into new column called HIV New
attach(aim3_chki.tx.conf)
hiv.mat<-cbind(ATF_HIVStatusAtBooking,
               ATF_HIVStatusDelvAdmission,
               ATF_HIVStatusOthTest)
detach(aim3_chki.tx.conf)
hiv.mat[hiv.mat=="01 HIV+"]<-1
hiv.mat[hiv.mat=="02 HIV-"]<--2
hiv.mat<-as.numeric(hiv.mat)
dim(hiv.mat)<-c(560,3)
hivstatus<-rowSums(hiv.mat,na.rm = TRUE)
hivstatus[hivstatus=="0"]<-NA
hivstatus[hivstatus=="-2" | 
            hivstatus=="-4"]<-"02 HIV-"
hivstatus[hivstatus=="1" | 
            hivstatus=="2"]<-"01 HIV+"
#QC of coding
attach(aim3_chki.tx.conf)
summary(freqlist(table(hivstatus,
                       ATF_HIVStatusAtBooking,
                       ATF_HIVStatusDelvAdmission,
                       ATF_HIVStatusOthTest,
                       useNA = "ifany"),
                 labelTranslations = c("HIV_new",
                                       "@Book",
                                       "@Adm.",
                                       "@Oth. Test")))
detach(aim3_chki.tx.conf)

#investigate two deaths
deaths<-subset(aim3_chki.tx.conf,aim3_chki.tx.conf$ATF_AliveAtDschg=="02 No")
deaths<-deaths[,colSums(is.na(deaths))!=nrow(deaths)]

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
#gestation age is NA
aim3_gest.na<-subset(x=aim3_chki.tx.conf,
                     subset=is.na(aim3_chki.tx.conf$ATF_GestAgeWks))
write.csv(aim3_gest.na,"aim3_gestation age is NA.csv")