
###Table 1: Characteristics of the Study Population###
#
#Section 1 - Demographics
#1. Age:
ggplot(aim3_chki.tx.conf,aes(ATF_Age)) + 
  geom_histogram(binwidth=1) + 
  labs(x="Age in Years",y="counts",title="Age of Mothers")
ggplot(aim3_chki.tx.conf,aes(LOCATION,ATF_Age)) + 
  geom_boxplot(fill=c("chocolate","khaki")) +
  labs(x="Location",y="Years",
       title="Age of Mothers by Location")
#
#6. Race / ethnic origin:
ggplot(aim3_chki.tx.conf,aes(ATF_Race)) +
  geom_bar(fill=c("black","white","brown","yellow","gray")) +
  labs(x="Race",y="counts",title="Race of Mothers") +
  scale_x_discrete(labels=c("Black","White","Coloured","Asian","NA")) +
  geom_text(stat='count',aes(label=..count..,vjust=-.2))
ggplot(aim3_chki.tx.conf, aes(fill=ATF_Race, x=LOCATION)) + 
  geom_bar() + 
  scale_fill_manual(values=c("01 Black"="black",
                             "02 White"="white",
                             "03 Coloured"="brown",
                             "04 Asian"="yellow",
                             "NA"="gray"),
                    name="Race") +
  labs(x="Hospital",y="counts",title="Race of Mothers by Location") 
#
#Print out Table 1
#(aggregated)
table1.df<-data.frame(Age_of_Mother=aim3_chki.tx.conf$ATF_Age,
                      Race=aim3_chki.tx.conf$ATF_Race,
                      Gravidity=aim3_chki.tx.conf$ATF_Gravidity,
                      Parity=aim3_chki.tx.conf$ATF_Parity,
                      Gestation_Age_Weeks=
                        aim3_chki.tx.conf$ATF_GestAgeWks,
                      Booking_Status_At_Admission=
                        aim3_chki.tx.conf$ATF_BookingStatusAtAdm,
                      Prenatal_Visits=
                        aim3_chki.tx.conf$ATF_NumVisitsAntenatalClinic,
                      Gestation_Age_Weeks_Grouped=
                        aim3_chki.tx.conf$gest.wks.tri,
                      Length_of_Stay=
                        aim3_chki.tx.conf$Stay_Length,
                      Alive_at_Discharge=
                        aim3_chki.tx.conf$ATF_AliveAtDschg)
listvars<-c("Age_of_Mother","Race","Gravidity","Parity",
            "Gestation_Age_Weeks",
            "Booking_Status_At_Admission",
            "Gestation_Age_Weeks_Grouped",
            "Length_of_Stay",
            "Prenatal_Visits","Alive_at_Discharge")
catvars<-c("Race","Booking_Status_At_Admission",
           "Gestation_Age_Weeks_Grouped",
           "Alive_at_Discharge")
nonnormalvars<-c("Gravidity","Parity","Prenatal_Visits",
                 "Length_of_Stay")
table1<-CreateTableOne(vars=listvars,
                       data = table1.df,
                       factorVars = catvars,
                       includeNA = TRUE)
table1.doc<-print(table1,nonnormal = nonnormalvars)
#(by hospital)
table1.df<-data.frame(Location=aim3_chki.tx.conf$LOCATION,
                      Age_of_Mother=aim3_chki.tx.conf$ATF_Age,
                      Race=aim3_chki.tx.conf$ATF_Race,
                      Gravidity=aim3_chki.tx.conf$ATF_Gravidity,
                      Parity=aim3_chki.tx.conf$ATF_Parity,
                      Gestation_Age_Weeks=
                        aim3_chki.tx.conf$ATF_GestAgeWks,
                      Booking_Status_At_Admission=
                        aim3_chki.tx.conf$ATF_BookingStatusAtAdm,
                      Prenatal_Visits=
                        aim3_chki.tx.conf$ATF_NumVisitsAntenatalClinic,
                      Gestation_Age_Weeks_Grouped=
                        aim3_chki.tx.conf$gest.wks.tri,
                      Length_of_Stay=
                        aim3_chki.tx.conf$Stay_Length,
                      Alive_at_Discharge=
                        aim3_chki.tx.conf$ATF_AliveAtDschg)
listvars<-c("Age_of_Mother","Race","Gravidity","Parity",
            "Gestation_Age_Weeks",
            "Booking_Status_At_Admission",
            "Gestation_Age_Weeks_Grouped",
            "Length_of_Stay",
            "Prenatal_Visits","Alive_at_Discharge")
catvars<-c("Location",
           "Race","Booking_Status_At_Admission",
           "Gestation_Age_Weeks_Grouped",
           "Alive_at_Discharge")
nonnormalvars<-c("Gravidity","Parity","Prenatal_Visits",
                 "Length_of_Stay")
table1<-CreateTableOne(vars=listvars,data = table1.df,
                       factorVars = catvars,includeNA = TRUE,
                       strata = "Location",test=FALSE)
print(table1,nonnormal = nonnormalvars)

#
#Anemia,all  #cross-check with Q6.5 "chronic anemia)
#2. Had the patient been identified as anemic during current pregnancy?
#create logical vector to pinpoint rows that contain the string 
#"anemia" so we can validate that everyone who marked "01 Yes"
#that they were identified as anemic during the current 
#pregnancy also marked that they had "chronic anemia"
#aim3_sec6$ATF_MedicalRationaleForTx2<-grepl(
# 'Anemia',ATF_MedicalRationaleForTx)
#create a data frame from the TRUE observations
#anemia.df<-subset(aim3_sec6,ATF_MedicalRationaleForTx2=="TRUE")
#check if all of the anemia are accounted for
#table(anemia.df$ATF_IdAnemicDuringCurrPreg,useNA = "ifany")
#there were 92 instances when anemia was marked yes, but 6 instances
#where anemia was marked as NA. Let's change that
#aim3_sec6<-within(aim3_sec6,
#                 ATF_IdAnemicDuringCurrPreg[
#                  is.na(ATF_IdAnemicDuringCurrPreg) & 
#                   ATF_MedicalRationaleForTx2=="TRUE"]<-
#                "01 Yes")
#aim3_sec6<-subset(aim3_sec6,select=-c(ATF_MedicalRationaleForTx2))
#which(ATF_HgLessthan10gdLAnteCl=="01 Yes" & 
#       ATF_IdAnemicDuringCurrPreg=="02 No")
#which(ATF_HgLessthan10gdLAnteCl=="01 Yes" & 
#       is.na(ATF_IdAnemicDuringCurrPreg))
#Anemia etiology
#detach(aim3_sec6)
#
#Section 5 -Bleeding in Current Pregnancy
#Hemorrhage, all (Q5.1a); Estim blood loss (mean, SD)
#attach(aim3_sec5)
#Hemorrhage Etiology
#aim3_sec5$HemmorageCause.tf<-grepl("Bleed",ATF_HemorrhageCause)
#bleed.df<-subset(aim3_sec5,HemmorageCause.tf=="TRUE")
#summary(freqlist(table(bleed.df$ATF_HemorrhageCause,useNA = "ifany")))
#summary(freqlist(table(LOCATION,bleed.df$ATF_HemorrhageCause,
#                      useNA = "ifany")))
#detach(aim3_sec5)
#Print out Table 2
table2.df<-data.frame(Transfused=
                        aim3_chki.tx.conf$ATF_Transfused,
                      Blood_Transfusion_Rationale=
                        aim3_chki.tx.conf$ATF_MedicalRationaleForTx,
                      Anemic_Current_Pregnancy=
                        aim3_chki.tx.conf$ATF_IdAnemicDuringCurrPreg,
                      Anemia_Etiology=
                        aim3_chki.tx.conf$ATF_ChronicAnemiaType,
                      Bleeding_this_Pregnancy=
                        aim3_chki.tx.conf$ATF_BleedingThisPreg,
                      Hemorrhage_Cause=
                        aim3_chki.tx.conf$ATF_HemorrhageCause)
listvars<-c("Transfused",
            "Blood_Transfusion_Rationale","Anemic_Current_Pregnancy",
            "Anemia_Etiology","Bleeding_this_Pregnancy",
            "Hemorrhage_Cause")
catvars<-c("Transfused",
           "Blood_Transfusion_Rationale","Anemic_Current_Pregnancy",
           "Anemia_Etiology","Bleeding_this_Pregnancy",
           "Hemorrhage_Cause")
table2<-CreateTableOne(vars=listvars,data=table2.df,factorVars = catvars,
                       includeNA = TRUE)
table2

###Table 3 Complications During Current Pregnancy###
table3.df<-data.frame(Complications_this_Pregnancy=
                        aim3_chki.tx.conf$ATF_CompsThisPreg,
                      Type_of_Complications_this_Pregnancy=
                        aim3_chki.tx.conf$ATF_CompsThisPregType,
                      Complications_this_Admission=
                        aim3_chki.tx.conf$ATF_CompsThisAdm,
                      Type_of_Complications_this_Admission=
                        aim3_chki.tx.conf$ATF_CompsThisAdmType)
listvars<-c("Complications_this_Pregnancy",
            "Type_of_Complications_this_Pregnancy",
            "Complications_this_Admission",
            "Type_of_Complications_this_Admission")
catvars<-c("Complications_this_Pregnancy",
           "Type_of_Complications_this_Pregnancy",
           "Complications_this_Admission",
           "Type_of_Complications_this_Admission")
table3<-CreateTableOne(vars=listvars,data = table3.df,
                       factorVars = catvars,
                       includeNA = TRUE)
table3


###Miscellaneous###
#
#Section 6 - Transfusion
#1. Had the patient been transfused before the current transfusion?
#Anemia Treatment
#make table 4
table4.df<-data.frame(Previous_Transfusion=
                        aim3_chki.tx.conf$ATF_TransfusePriorToCurr,
                      Previous_Transfusion_When=
                        aim3_chki.tx.conf$ATF_TransfusedPriorWhen,
                      Anemia_Treatment=
                        aim3_chki.tx.conf$ATF_OnHematinicRxDuringPreg,
                      Who_Ordered_Transfusion=
                        aim3_chki.tx.conf$ATF_TxHighestLevelDiscuss,
                      Patient_Transfused_Where=
                        aim3_chki.tx.conf$ATF_PtTransfusedAt,
                      Where_Patient_Transfused_Start=
                        aim3_chki.tx.conf$ATF_WherePtTxStart,
                      PreTransfusion_Hemoglobin=
                        aim3_chki.tx.conf$ATF_LastHemoglobinPriorTx,
                      Hemoglobin_Method_Used=
                        aim3_chki.tx.conf$ATF_HbMethodUsed,
                      Where_Results_Obtained_Before_Transfusion=
                        aim3_chki.tx.conf$ATF_WhereResultsObtainedPriorTx)
listvars<-c("Previous_Transfusion","Previous_Transfusion_When",
            "Anemia_Treatment","Who_Ordered_Transfusion",
            "Patient_Transfused_Where",
            "Where_Patient_Transfused_Start",
            "PreTransfusion_Hemoglobin","Hemoglobin_Method_Used",
            "Where_Results_Obtained_Before_Transfusion")
catvars<-c("Previous_Transfusion","Previous_Transfusion_When",
           "Anemia_Treatment","Who_Ordered_Transfusion",
           "Patient_Transfused_Where",
           "Where_Patient_Transfused_Start",
           "Hemoglobin_Method_Used",
           "Where_Results_Obtained_Before_Transfusion")
table4<-CreateTableOne(vars=listvars,data = table4.df,
                       factorVars = catvars,
                       includeNA = TRUE)
table4
#
#Figure 1. Bar graphs of blood products administered
#red blood cells
bloodproducts<-subset(aim3_chki.tx.conf,select=c(1,2,112:114))
bloodproducts$redTotal<-as.character(bloodproducts$redTotal)
bloodproducts$redTotal[bloodproducts$redTotal=="5" | 
                         bloodproducts$redTotal=="6" | 
                         bloodproducts$redTotal=="7"]<-"5"
#(we note that there are 44 0's, but we make it NA because 
#we don't want to graph them)
ggplot(bloodproducts,aes(redTotal)) +
  geom_bar(fill=c("white","red","red1","red2","red3","red4")) +
  labs(x="# of Units",y="count",
       title="# of Patients with x Units of Red Blood Cells") +
  scale_x_discrete(labels=c("0","1","2","3","4","5+")) +
  geom_text(stat='count',aes(label=..count..,vjust=-.5))
#platelets
bloodproducts$pltTotal<-as.character(bloodproducts$pltTotal)
ggplot(bloodproducts,aes(pltTotal)) +
  geom_bar(fill=c("white","purple","purple2","purple4")) +
  labs(x="# of Units",y="count",
       title="# of Patients with x Units of Platelets") +
  scale_x_discrete(labels=c("0","1","2","4")) +
  geom_text(stat='count',aes(label=..count..,vjust=-.5))
#plasma
bloodproducts$ffpTotal<-as.character(bloodproducts$ffpTotal)
ggplot(bloodproducts,aes(ffpTotal)) +
  geom_bar(fill=c("white",
                  "slategray","slategray2","slategray3","slategray4")) +
  labs(x="# of Units",y="count",
       title="# of Patients with x Units of Plasma") +
  scale_x_discrete(labels=c("0","1","2","3","4")) +
  geom_text(stat='count',aes(label=..count..,vjust=-.5))
###Fig 2.  Hemoglobin increments###
#make the data set for the figure 2
hgb.increments<-data.frame(Location=aim3_chki.tx.conf$LOCATION,
                           Pretransfusion_Hgb=
                             aim3_chki.tx.conf$ATF_LastHemoglobinPriorTx,
                           Posttransfusion_Hgb=
                             aim3_chki.tx.conf$ATF_HbAfterTx,
                           Delta_Hgb=
                             aim3_chki.tx.conf$ATF_HbAfterTx - 
                             aim3_chki.tx.conf$ATF_LastHemoglobinPriorTx,
                           RBC_unit=aim3_chki.tx.conf$redTotal)
hgb.increments$DeltaHgb_div_RBCUnit<-
  hgb.increments$Delta_Hgb/hgb.increments$RBC_unit
#create a data frame without the NA or Inf observations 
#from the last column
hgb.inc.able<-subset(hgb.increments,!(is.na(DeltaHgb_div_RBCUnit)) & 
                       !(is.infinite(DeltaHgb_div_RBCUnit)))
#get the means of everything
sapply(hgb.inc.able,mean)
aggregate(hgb.inc.able[,2:6],list(hgb.inc.able$Location),mean)
#create a data frame so we can plot this in ggplot
groupall.df<-data.frame(Location=c(
  "Chris Hani","Chris Hani","Chris Hani","Chris Hani",
  "King Edward VIII","King Edward VIII",
  "King Edward VIII","King Edward VIII",
  "Overall","Overall","Overall","Overall"),
  Hgb_Tx=c(
    "Pre","Post","Delta","Delta/Unit",
    "Pre","Post","Delta","Delta/Unit",
    "Pre","Post","Delta","Delta/Unit"),
  means=c(6.65,9.26,2.6,1.09,
          6.93,8.79,1.86,.83,
          6.78,9.03,2.24,.96))
#create plot
groupall.df$Hgb_Tx<-factor(groupall.df$Hgb_Tx,
                           levels=c("Pre","Post","Delta","Delta/Unit"),
                           labels=c("Pre Hb (g/dL)","Post Hb (g/dL)",
                                    "Delta Hb (g/dL)",
                                    "Delta Hb (g/dL)/unit"))
ggplot(groupall.df,aes(x=Location,y=means,group=Hgb_Tx)) + 
  geom_point(aes(shape=Hgb_Tx,color=Hgb_Tx)) + 
  labs(x="Hospital",y="Mean Hemoglobin Level",
       title="Hemoglobin Increments") + 
  scale_y_continuous(breaks=seq(from=1,to=10))

#Crosstabs#

#Q6.2 (anemic curr.) and Q6.5 (med. rationale)
aim3_chki.tx.conf$Hemorrhage<-aim3_chki.tx.conf$ATF_MedicalRationaleForTx
aim3_chki.tx.conf$Hemorrhage<-gsub(
  ".*Hemorrhage.*","Hemorrhage",aim3_chki.tx.conf$Hemorrhage)
aim3_chki.tx.conf$Hemorrhage<-ifelse(
  aim3_chki.tx.conf$Hemorrhage=="Hemorrhage","Hemorrhage",
  ifelse(aim3_chki.tx.conf$Hemorrhage=="NA","NA","Not Hemorrhage"))
<-summary(freqlist(table(aim3_chki.tx.conf$ATF_IdAnemicDuringCurrPreg,
                       aim3_chki.tx.conf$Hemorrhage,
                       useNA = "ifany"),
                 labelTranslations = c("Anemic Curr. Preg.",
                                       "Bleeding This Preg.")))
#Q6.2 (anemic curr.) and Q5.1a (bleeding this preg.)
summary(freqlist(table(aim3_chki.tx.conf$ATF_IdAnemicDuringCurrPreg,
                       aim3_chki.tx.conf$ATF_BleedingThisPreg,
                       useNA = "ifany"),
                 labelTranslations = c("Anemic Curr. Preg.",
                                       "Bleeding This Preg.")))
#new group variable
aim3_chki.tx.conf$anemic.and.bleeding<-NA
aim3_chki.tx.conf$anemic.and.bleeding<-as.character(
  aim3_chki.tx.conf$anemic.and.bleeding
)
aim3_chki.tx.conf$anemic.and.bleeding[
  aim3_chki.tx.conf$ATF_IdAnemicDuringCurrPreg=="01 Yes" & 
    aim3_chki.tx.conf$ATF_BleedingThisPreg=="01 Yes"
  ]<-"Both"
aim3_chki.tx.conf$anemic.and.bleeding[
  aim3_chki.tx.conf$ATF_IdAnemicDuringCurrPreg=="02 No" | 
    aim3_chki.tx.conf$ATF_IdAnemicDuringCurrPreg=="97 Unknown" | 
    is.na(aim3_chki.tx.conf$ATF_IdAnemicDuringCurrPreg) & 
    aim3_chki.tx.conf$ATF_BleedingThisPreg=="01 Yes"
  ]<-"Bleeding_Only"
aim3_chki.tx.conf$anemic.and.bleeding[
  aim3_chki.tx.conf$ATF_IdAnemicDuringCurrPreg=="01 Yes" & 
    aim3_chki.tx.conf$ATF_BleedingThisPreg=="02 No"
  ]<-"Anemia_Only"
aim3_chki.tx.conf$anemic.and.bleeding[
  aim3_chki.tx.conf$ATF_IdAnemicDuringCurrPreg=="02 No" | 
    aim3_chki.tx.conf$ATF_IdAnemicDuringCurrPreg=="97 Unknown" | 
    is.na(aim3_chki.tx.conf$ATF_IdAnemicDuringCurrPreg) & 
    aim3_chki.tx.conf$ATF_BleedingThisPreg=="02 No"
  ]<-"Neither"
#anemic.and.bleeding with Q5.1a (bleeding) and Q6.2 (anemic curr. preg.)
bleeding.this.preg.nona<-aim3_chki.tx.conf$ATF_BleedingThisPreg
bleeding.this.preg.nona[bleeding.this.preg.nona=="97 Unknown"]<-NA
summary(freqlist(table(bleeding.this.preg.nonaaim3_chki.tx.conf$anemic.and.bleedin),
                 labelTranslations = c("Anemic*Bleeding",
                                       "Bleeding this Preg.")))
summary(freqlist(table(aim3_chki.tx.conf$anemic.and.bleeding,
                       aim3_chki.tx.conf$ATF_IdAnemicDuringCurrPreg),
                 labelTranslations = c("Anemic*Bleeding",
                                       "Anemic Curr. Preg.")))
#Medical Rationale and Anemic Current Pregnancy
#aim3_chki.tx.conf$medrat.cat<-aim3_chki.tx.conf$ATF_MedicalRationaleForTx
#aim3_chki.tx.conf$medrat.cat<-gsub("^_ChronicAnemia.*",
#                               "C",
#                              aim3_chki.tx.conf$medrat.cat)
#aim3_chki.tx.conf$medrat.cat<-gsub(".*ChronicAnemia.*",
#                               "B",
#                              aim3_chki.tx.conf$medrat.cat)
#aim3_chki.tx.conf$medrat.cat<-gsub("^_Other.*",
#                               "N",
#                              aim3_chki.tx.conf$medrat.cat)
#aim3_chki.tx.conf$medrat.cat<-gsub("_Unknown",
#                               "N",
#                              aim3_chki.tx.conf$medrat.cat)
#aim3_chki.tx.conf$medrat.cat[is.na(aim3_chki.tx.conf$medrat.cat)]<-"N"
#aim3_chki.tx.conf$medrat.cat<-ifelse(
# aim3_chki.tx.conf$medrat.cat=="B","B",
#ifelse(aim3_chki.tx.conf$medrat.cat=="N","N",
#      ifelse(aim3_chki.tx.conf$medrat.cat=="C","C","S"))
#) 
#summary(freqlist(table(aim3_chki.tx.conf$ATF_IdAnemicDuringCurrPreg,
#                      aim3_chki.tx.conf$medrat.cat,
#                     useNA = "ifany"),
#              labelTranslations = c("Anemic Curr. Preg.",
#                                   "4-level Tx. Rat.")))
#Q6.5 (blood tx. rationale) and new group variable
anemic.and.bleeding.nona<-subset(
  aim3_chki.tx.conf,!(is.na(anemic.and.bleeding))
)
summary(freqlist(table(anemic.and.bleeding.nona$anemic.and.bleeding,
                       anemic.and.bleeding.nona$ATF_MedicalRationaleForTx,
                       
                       useNA = "ifany"),
                 labelTranslations = c("Anemic*Bleeding",
                                       "Med. Rationale Tx.")))
#Q6.5 (blood tx. rationale) and gest.wks.tri
summary(freqlist(table(aim3_chki.tx.conf$gest.wks.tri,
                       aim3_chki.tx.conf$ATF_MedicalRationaleForTx),
                 labelTranslations = c("Gest. Age. Group",
                                       "Med. Rationale Tx.")))
#"Other" specified for Blood Tx. Rationale
other.tx.rat<-as.data.frame(freqlist(table(aim3_chki.tx.conf$ATF_MedicalRationaleForTx,
                       aim3_chki.tx.conf$MedicalRationaleOtherSpecify),
                 labelTranslations = c("Med. Rat. w/ 'Other'",
                                       "'Other' Specify")))
#"Other" Specified for Table 2
other.hem.caus<-as.data.frame(freqlist(table(aim3_chki.tx.conf$ATF_HemorrhageCause,
                       aim3_chki.tx.conf$HemorrhageCauseOtherSpecify),
                 labelTranslations = c(
                   "Hemorrhage Cause w/ 'Other'","Other Specify")))
#"Other" Specified for Table 3
other.comp.preg<-as.data.frame(freqlist(table(aim3_chki.tx.conf$ATF_CompsThisPregType,
                       aim3_chki.tx.conf$ComplicationsThisPregnancySP),
                 labelTranslations = c(
                   "Preg. Complications w/ 'Other'",
                   "'Other' Specified")))
other.comp.adm<-as.data.frame(freqlist(table(aim3_chki.tx.conf$ATF_CompsThisAdmType,
                       aim3_chki.tx.conf$ComplicationsThisAdmissionSP),
                 labelTranslations = c(
                   "Adm. Complications w/ 'Other'",
                   "'Other' Specified")))

#write to csv
write.csv(other.tx.rat,"Other Blood Tx Rationale.csv")
write.csv(other.hem.caus,"Other Hemorrhage Causes.csv")
write.csv(other.comp.preg,"Other Complications this Pregnancy.csv")
write.csv(other.comp.adm,"Other Complications this Admission.csv")