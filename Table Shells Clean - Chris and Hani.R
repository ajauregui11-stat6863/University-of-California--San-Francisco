
###Table 1: Characteristics of the Study Population###
#
#Section 1 - Demographics
#1. Age:
#ggplot(aim3_chki.tx.conf,aes(ATF_Age)) + 
#  geom_histogram(binwidth=1) + 
#  labs(x="Age in Years",y="counts",title="Age of Mothers")
#ggplot(aim3_chki.tx.conf,aes(LOCATION,ATF_Age)) + 
# geom_boxplot(fill=c("chocolate","khaki")) +
#  labs(x="Location",y="Years",
#       title="Age of Mothers by Location")
#
#6. Race / ethnic origin:
#ggplot(aim3_chki.tx.conf,aes(ATF_Race)) +
#  geom_bar(fill=c("black","white","brown","yellow","gray")) +
#  labs(x="Race",y="counts",title="Race of Mothers") +
#  scale_x_discrete(labels=c("Black","White","Coloured","Asian","NA")) +
#  geom_text(stat='count',aes(label=..count..,vjust=-.2))
#ggplot(aim3_chki.tx.conf, aes(fill=ATF_Race, x=LOCATION)) + 
#  geom_bar() + 
#  scale_fill_manual(values=c("01 Black"="black",
#                             "02 White"="white",
#                             "03 Coloured"="brown",
#                             "04 Asian"="yellow",
#                             "NA"="gray"),
#                    name="Race") +
#  labs(x="Hospital",y="counts",title="Race of Mothers by Location") 
#
#Print out Table 1
#(aggregated)
table1.df<-data.frame(Age=aim3_chki.tx.conf$ATF_Age,
                      Race=aim3_chki.tx.conf$ATF_Race,
                      Gravidity=aim3_chki.tx.conf$ATF_Gravidity,
                      Parity=aim3_chki.tx.conf$ATF_Parity,
                      LengthofStay=aim3_chki.tx.conf$Stay_Length,
                      AliveatDischarge=aim3_chki.tx.conf$ATF_AliveAtDschg,
                      GestationAgeWeeks=
                        aim3_chki.tx.conf$ATF_GestAgeWks,
                      GestationAgeTrimester=
                        aim3_chki.tx.conf$gest.wks.tri,
                      BookingStatusatAdmission=
                        aim3_chki.tx.conf$ATF_BookingStatusAtAdm,
                      PrenatalVisits=
                        aim3_chki.tx.conf$ATF_NumVisitsAntenatalClinic,
                      HIVStatus=hivstatus,
                      ARTUsePriortoPregnancy=
                        aim3_chki.tx.conf$ATF_OnARTPriorThisPreg)
listvars<-c("Age","Race",
            "Gravidity","Parity",
            "LengthofStay",
            "AliveatDischarge",
            "GestationAgeWeeks",
            "GestationAgeTrimester",
            "BookingStatusatAdmission",
            "PrenatalVisits",
            "HIVStatus",
            "ARTUsePriortoPregnancy")
catvars<-c("Race",
           "BookingStatusatAdmission",
           "GestationAgeTrimester",
           "AliveatDischarge",
           "HIVStatus",
           "ARTUsePriortoPregnancy")
nonnormalvars<-c("Gravidity","Parity",
                 "LengthofStay",
                 "PrenatalVisits")
table1<-CreateTableOne(vars=listvars,
                       data = table1.df,
                       factorVars = catvars,
                       includeNA = TRUE)
table1.doc<-print(table1,nonnormal = nonnormalvars)
#(by hospital)
table1.df<-data.frame(Location=aim3_chki.tx.conf$LOCATION,
                      Age=aim3_chki.tx.conf$ATF_Age,
                      Race=aim3_chki.tx.conf$ATF_Race,
                      Gravidity=aim3_chki.tx.conf$ATF_Gravidity,
                      Parity=aim3_chki.tx.conf$ATF_Parity,
                      LengthofStay=aim3_chki.tx.conf$Stay_Length,
                      AliveatDischarge=aim3_chki.tx.conf$ATF_AliveAtDschg,
                      GestationAgeWeeks=
                        aim3_chki.tx.conf$ATF_GestAgeWks,
                      GestationAgeTrimester=
                        aim3_chki.tx.conf$gest.wks.tri,
                      BookingStatusatAdmission=
                        aim3_chki.tx.conf$ATF_BookingStatusAtAdm,
                      PrenatalVisits=
                        aim3_chki.tx.conf$ATF_NumVisitsAntenatalClinic,
                      HIVStatus=hivstatus,
                      ARTUsePriortoPregnancy=
                        aim3_chki.tx.conf$ATF_OnARTPriorThisPreg)
listvars<-c("Age","Race",
            "Gravidity","Parity",
            "LengthofStay",
            "AliveatDischarge",
            "GestationAgeWeeks",
            "GestationAgeTrimester",
            "BookingStatusatAdmission",
            "PrenatalVisits",
            "HIVStatus",
            "ARTUsePriortoPregnancy")
catvars<-c("Location","Race",
           "BookingStatusatAdmission",
           "GestationAgeTrimester",
           "AliveatDischarge",
           "HIVStatus",
           "ARTUsePriortoPregnancy")
nonnormalvars<-c("Gravidity","Parity",
                 "LengthofStay",
                 "PrenatalVisits")
table1<-CreateTableOne(vars=listvars,data = table1.df,
                       factorVars = catvars,includeNA = TRUE,
                       strata = "Location",test=FALSE)
print(table1,nonnormal = nonnormalvars)

#flextable table1
kableone <- function(x, ...) {
  capture.output(x <- print(x))
  knitr::kable(x, ...)
}
kableone(table1)
ft <- flextable(data = table1) %>% 
  theme_booktabs() %>% 
  set_header_labels( n = "#", Mean = "\u03D1", SD = "\u03C3") %>% 
  autofit()
ft
read_docx() %>% 
  body_add_flextable(ft) %>% 
  print(target = ".../flextable.docx")

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
#overall
table2.df<-data.frame(Location=aim3_chki.tx.conf$LOCATION,
                      GestationAgeTrimester=
                        aim3_chki.tx.conf$gest.wks.tri,
                      BleedingthisPregnancy=
                        aim3_chki.tx.conf$ATF_BleedingThisPreg,
                      HemorrhageCause=
                        HemorrhageCause,
                      AnemicCurrentPregnancy=
                        aim3_chki.tx.conf$ATF_IdAnemicDuringCurrPreg,
                      AnemiaEtiology=
                        aim3_chki.tx.conf$ATF_ChronicAnemiaType,
                      BloodTransfusionRationale=
                        MedicalRationale)
listvars<-c("BleedingthisPregnancy",
            "HemorrhageCause",
            "AnemicCurrentPregnancy",
            "BloodTransfusionRationale",
            "AnemiaEtiology")
catvars<-c("Location","GestationAgeWeeks",
           "BleedingthisPregnancy",
           "HemorrhageCause",
           "AnemicCurrentPregnancy",
           "BloodTransfusionRationale",
           "AnemiaEtiology")
table2<-CreateTableOne(vars=listvars,data=table2.df,factorVars = catvars,
                       includeNA = TRUE,test=FALSE)
table2
#stratified by hospital
table2.df<-data.frame(Location=aim3_chki.tx.conf$LOCATION,
                      GestationAgeTrimester=
                        aim3_chki.tx.conf$gest.wks.tri,
                      BleedingthisPregnancy=
                        aim3_chki.tx.conf$ATF_BleedingThisPreg,
                      HemorrhageCause=
                        HemorrhageCause,
                      AnemicCurrentPregnancy=
                        aim3_chki.tx.conf$ATF_IdAnemicDuringCurrPreg,
                      AnemiaEtiology=
                        aim3_chki.tx.conf$ATF_ChronicAnemiaType,
                      BloodTransfusionRationale=
                        MedicalRationale)
listvars<-c("BleedingthisPregnancy",
            "HemorrhageCause",
            "AnemicCurrentPregnancy",
            "BloodTransfusionRationale",
            "AnemiaEtiology")
catvars<-c("Location","GestationAgeTrimester",
           "BleedingthisPregnancy",
           "HemorrhageCause",
           "AnemicCurrentPregnancy",
           "BloodTransfusionRationale",
           "AnemiaEtiology")
table2<-CreateTableOne(vars=listvars,data=table2.df,factorVars = catvars,
                       strata="Location",includeNA = TRUE,test=FALSE)
table2
#stratified by trimester
table2.df<-data.frame(Location=aim3_chki.tx.conf$LOCATION,
                      GestationAgeTrimester=
                        aim3_chki.tx.conf$gest.wks.tri,
                      BleedingthisPregnancy=
                        aim3_chki.tx.conf$ATF_BleedingThisPreg,
                      HemorrhageCause=
                        HemorrhageCause,
                      AnemicCurrentPregnancy=
                        aim3_chki.tx.conf$ATF_IdAnemicDuringCurrPreg,
                      AnemiaEtiology=
                        aim3_chki.tx.conf$ATF_ChronicAnemiaType,
                      BloodTransfusionRationale=
                        MedicalRationale)
listvars<-c("BleedingthisPregnancy",
            "HemorrhageCause",
            "AnemicCurrentPregnancy",
            "BloodTransfusionRationale",
            "AnemiaEtiology")
catvars<-c("Location","GestationAgeTrimester",
           "BleedingthisPregnancy",
           "HemorrhageCause",
           "AnemicCurrentPregnancy",
           "BloodTransfusionRationale",
           "AnemiaEtiology")
table2<-CreateTableOne(vars=listvars,data=table2.df,factorVars = catvars,
                       strata="GestationAgeTrimester",includeNA = TRUE,test=FALSE)
table2
#stratified by trimester and hospital
table2.df<-data.frame(Location=aim3_chki.tx.conf$LOCATION,
                      GestationAgeTrimester=
                        aim3_chki.tx.conf$gest.wks.tri,
                      BleedingthisPregnancy=
                        aim3_chki.tx.conf$ATF_BleedingThisPreg,
                      HemorrhageCause=
                        HemorrhageCause,
                      AnemicCurrentPregnancy=
                        aim3_chki.tx.conf$ATF_IdAnemicDuringCurrPreg,
                      AnemiaEtiology=
                        aim3_chki.tx.conf$ATF_ChronicAnemiaType,
                      BloodTransfusionRationale=
                        MedicalRationale)
listvars<-c("BleedingthisPregnancy",
            "HemorrhageCause",
            "AnemicCurrentPregnancy",
            "BloodTransfusionRationale",
            "AnemiaEtiology")
catvars<-c("Location","GestationAgeTrimester",
           "BleedingthisPregnancy",
           "HemorrhageCause",
           "AnemicCurrentPregnancy",
           "BloodTransfusionRationale",
           "AnemiaEtiology")
table2<-CreateTableOne(vars=listvars,data=table2.df,factorVars = catvars,
                       strata=c("GestationAgeTrimester","Location"),
                       includeNA = TRUE,test=FALSE)
table2


###Table 3 Complications During Current Pregnancy###
#table3.df<-data.frame(Q3.10_Complications_this_Pregnancy=
#                        aim3_chki.tx.conf$ATF_CompsThisPreg,
#                      Q3.10a_Type_of_Complications_this_Pregnancy=
#                        CompsThisPreg,
#                      Q3.11_Complications_this_Admission=
#                        aim3_chki.tx.conf$ATF_CompsThisAdm,
#                      Q3.11a_Type_of_Complications_this_Admission=
#                        CompsThisAdm)
#listvars<-c("Q3.10_Complications_this_Pregnancy",
#            "Q3.10a_Type_of_Complications_this_Pregnancy",
#            "Q3.11_Complications_this_Admission",
#            "Q3.11a_Type_of_Complications_this_Admission")
#catvars<-c("Q3.10_Complications_this_Pregnancy",
#           "Q3.10a_Type_of_Complications_this_Pregnancy",
#           "Q3.11_Complications_this_Admission",
#           "Q3.11a_Type_of_Complications_this_Admission")
#table3<-CreateTableOne(vars=listvars,data = table3.df,
#                       factorVars = catvars,
#                       includeNA = TRUE)
#table3


###Miscellaneous###
#
#Section 6 - Transfusion
#1. Had the patient been transfused before the current transfusion?
#Anemia Treatment
#make table 4
table4.df<-data.frame(PreviousTransfusion=
                        aim3_chki.tx.conf$ATF_TransfusePriorToCurr,
                      PreviousTransfusionWhen=
                        aim3_chki.tx.conf$ATF_TransfusedPriorWhen,
                      OnHematinicTherapyDuringPregnancy=
                        aim3_chki.tx.conf$ATF_OnHematinicRxDuringPreg,
                      HighestLevelDiscussedTransfusion=
                        aim3_chki.tx.conf$ATF_TxHighestLevelDiscuss,
                      PatientTransfusedWhere=
                        aim3_chki.tx.conf$ATF_PtTransfusedAt,
                      WherePatientWhenTransfusionStarted=
                        aim3_chki.tx.conf$ATF_WherePtTxStart,
                      PreTransfusionHemoglobin=
                        aim3_chki.tx.conf$ATF_LastHemoglobinPriorTx,
                      HemoglobinMethodUsed=
                        aim3_chki.tx.conf$ATF_HbMethodUsed,
                      WereResultsObtainedBeforeTransfusion=
                        aim3_chki.tx.conf$ATF_WhereResultsObtainedPriorTx)
listvars<-c("PreviousTransfusion",
            "PreviousTransfusionWhen",
            "OnHematinicTherapyDuringPregnancy",
            "HighestLevelDiscussedTransfusion",
            "PatientTransfusedWhere",
            "WherePatientWhenTransfusionStarted",
            "PreTransfusionHemoglobin",
            "HemoglobinMethodUsed",
            "WereResultsObtainedBeforeTransfusion")
catvars<-c("PreviousTransfusion",
           "PreviousTransfusionWhen",
           "OnHematinicTherapyDuringPregnancy",
           "HighestLevelDiscussedTransfusion",
           "PatientTransfusedWhere",
           "WherePatientWhenTransfusionStarted",
           "HemoglobinMethodUsed",
           "WereResultsObtainedBeforeTransfusion")
table4<-CreateTableOne(vars=listvars,data = table4.df,
                       factorVars = catvars,
                       includeNA = TRUE)
table4
#stratified by hospital
table4.df<-data.frame(Location=aim3_chki.tx.conf$LOCATION,
                      GestationAgeTrimester=
                        aim3_chki.tx.conf$gest.wks.tri,
                      PreviousTransfusion=
                        aim3_chki.tx.conf$ATF_TransfusePriorToCurr,
                      PreviousTransfusionWhen=
                        aim3_chki.tx.conf$ATF_TransfusedPriorWhen,
                      OnHematinicTherapyDuringPregnancy=
                        aim3_chki.tx.conf$ATF_OnHematinicRxDuringPreg,
                      HighestLevelDiscussedTransfusion=
                        aim3_chki.tx.conf$ATF_TxHighestLevelDiscuss,
                      PatientTransfusedWhere=
                        aim3_chki.tx.conf$ATF_PtTransfusedAt,
                      WherePatientWhenTransfusionStarted=
                        aim3_chki.tx.conf$ATF_WherePtTxStart,
                      PreTransfusionHemoglobin=
                        aim3_chki.tx.conf$ATF_LastHemoglobinPriorTx,
                      HemoglobinMethodUsed=
                        aim3_chki.tx.conf$ATF_HbMethodUsed,
                      WereResultsObtainedBeforeTransfusion=
                        aim3_chki.tx.conf$ATF_WhereResultsObtainedPriorTx)
listvars<-c("PreviousTransfusion",
            "PreviousTransfusionWhen",
            "OnHematinicTherapyDuringPregnancy",
            "HighestLevelDiscussedTransfusion",
            "PatientTransfusedWhere",
            "WherePatientWhenTransfusionStarted",
            "PreTransfusionHemoglobin",
            "HemoglobinMethodUsed",
            "WereResultsObtainedBeforeTransfusion")
catvars<-c("Location",
           "PreviousTransfusion",
           "PreviousTransfusionWhen",
           "OnHematinicTherapyDuringPregnancy",
           "HighestLevelDiscussedTransfusion",
           "PatientTransfusedWhere",
           "WherePatientWhenTransfusionStarted",
           "HemoglobinMethodUsed",
           "WereResultsObtainedBeforeTransfusion")
table4<-CreateTableOne(vars=listvars,data = table4.df,
                       factorVars = catvars,
                       strata="Location",
                       includeNA = TRUE,test=FALSE)
table4
#stratified by gestation age trimester
table4.df<-data.frame(Location=aim3_chki.tx.conf$LOCATION,
                      GestationAgeTrimester=
                        aim3_chki.tx.conf$gest.wks.tri,
                      PreviousTransfusion=
                        aim3_chki.tx.conf$ATF_TransfusePriorToCurr,
                      PreviousTransfusionWhen=
                        aim3_chki.tx.conf$ATF_TransfusedPriorWhen,
                      OnHematinicTherapyDuringPregnancy=
                        aim3_chki.tx.conf$ATF_OnHematinicRxDuringPreg,
                      HighestLevelDiscussedTransfusion=
                        aim3_chki.tx.conf$ATF_TxHighestLevelDiscuss,
                      PatientTransfusedWhere=
                        aim3_chki.tx.conf$ATF_PtTransfusedAt,
                      WherePatientWhenTransfusionStarted=
                        aim3_chki.tx.conf$ATF_WherePtTxStart,
                      PreTransfusionHemoglobin=
                        aim3_chki.tx.conf$ATF_LastHemoglobinPriorTx,
                      HemoglobinMethodUsed=
                        aim3_chki.tx.conf$ATF_HbMethodUsed,
                      WereResultsObtainedBeforeTransfusion=
                        aim3_chki.tx.conf$ATF_WhereResultsObtainedPriorTx)
listvars<-c("PreviousTransfusion",
            "PreviousTransfusionWhen",
            "OnHematinicTherapyDuringPregnancy",
            "HighestLevelDiscussedTransfusion",
            "PatientTransfusedWhere",
            "WherePatientWhenTransfusionStarted",
            "PreTransfusionHemoglobin",
            "HemoglobinMethodUsed",
            "WereResultsObtainedBeforeTransfusion")
catvars<-c("GestationAgeTrimester",
           "PreviousTransfusion",
           "PreviousTransfusionWhen",
           "OnHematinicTherapyDuringPregnancy",
           "HighestLevelDiscussedTransfusion",
           "PatientTransfusedWhere",
           "WherePatientWhenTransfusionStarted",
           "HemoglobinMethodUsed",
           "WereResultsObtainedBeforeTransfusion")
table4<-CreateTableOne(vars=listvars,data = table4.df,
                       factorVars = catvars,
                       strata="GestationAgeTrimester",
                       includeNA = TRUE,test=FALSE)
table4
#stratified by hospital and gestation age trimester
table4.df<-data.frame(Location=aim3_chki.tx.conf$LOCATION,
                      GestationAgeTrimester=
                        aim3_chki.tx.conf$gest.wks.tri,
                      PreviousTransfusion=
                        aim3_chki.tx.conf$ATF_TransfusePriorToCurr,
                      PreviousTransfusionWhen=
                        aim3_chki.tx.conf$ATF_TransfusedPriorWhen,
                      OnHematinicTherapyDuringPregnancy=
                        aim3_chki.tx.conf$ATF_OnHematinicRxDuringPreg,
                      HighestLevelDiscussedTransfusion=
                        aim3_chki.tx.conf$ATF_TxHighestLevelDiscuss,
                      PatientTransfusedWhere=
                        aim3_chki.tx.conf$ATF_PtTransfusedAt,
                      WherePatientWhenTransfusionStarted=
                        aim3_chki.tx.conf$ATF_WherePtTxStart,
                      PreTransfusionHemoglobin=
                        aim3_chki.tx.conf$ATF_LastHemoglobinPriorTx,
                      HemoglobinMethodUsed=
                        aim3_chki.tx.conf$ATF_HbMethodUsed,
                      WereResultsObtainedBeforeTransfusion=
                        aim3_chki.tx.conf$ATF_WhereResultsObtainedPriorTx)
listvars<-c("PreviousTransfusion",
            "PreviousTransfusionWhen",
            "OnHematinicTherapyDuringPregnancy",
            "HighestLevelDiscussedTransfusion",
            "PatientTransfusedWhere",
            "WherePatientWhenTransfusionStarted",
            "PreTransfusionHemoglobin",
            "HemoglobinMethodUsed",
            "WereResultsObtainedBeforeTransfusion")
catvars<-c("Location",
           "GestationAgeTrimester",
           "PreviousTransfusion",
           "PreviousTransfusionWhen",
           "OnHematinicTherapyDuringPregnancy",
           "HighestLevelDiscussedTransfusion",
           "PatientTransfusedWhere",
           "WherePatientWhenTransfusionStarted",
           "HemoglobinMethodUsed",
           "WereResultsObtainedBeforeTransfusion")
table4<-CreateTableOne(vars=listvars,data = table4.df,
                       factorVars = catvars,
                       strata=c("Location","GestationAgeTrimester"),
                       includeNA = TRUE,test = FALSE)
table4


#
#Figure 1. Bar graphs of blood products administered
#red blood cells
bloodproducts<-subset(aim3_chki.tx.conf,select=c(1,2,113:115))
bloodproducts$redTotal<-as.character(bloodproducts$redTotal)
bloodproducts$redTotal[bloodproducts$redTotal=="5" | 
                         bloodproducts$redTotal=="6" | 
                         bloodproducts$redTotal=="7"]<-"5"
ggplot(bloodproducts,aes(x=redTotal,y=(..count..)/sum(..count..))) +
  geom_bar(fill="black") +
  labs(x="Units",y="Percent",
       title="Patients with X Units of Red Blood Cells") +
  scale_x_discrete(labels=c("0","1","2","3","4","5+")) +
  geom_text(stat='count',aes(label=..count..,vjust=-.2)) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_y_continuous(labels=percent)
#platelets
#bloodproducts$pltTotal<-as.character(bloodproducts$pltTotal)
#ggplot(bloodproducts,aes(pltTotal)) +
 # geom_bar(fill=c("white","purple","purple2","purple4")) +
  #labs(x="# of Units",y="count",
   #    title="# of Patients with x Units of Platelets") +
  #scale_x_discrete(labels=c("0","1","2","4")) +
  #geom_text(stat='count',aes(label=..count..,vjust=-.2))
#plasma
#bloodproducts$ffpTotal<-as.character(bloodproducts$ffpTotal)
#ggplot(bloodproducts,aes(ffpTotal)) +
 # geom_bar(fill=c("white",
  #                "slategray","slategray2","slategray3","slategray4")) +
  #labs(x="# of Units",y="count",
   #    title="# of Patients with x Units of Plasma") +
  #scale_x_discrete(labels=c("0","1","2","3","4")) +
  #geom_text(stat='count',aes(label=..count..,vjust=-.2))
###Fig 2.  Hemoglobin increments###
#make the data set for the figure 2
hgb.increments<-data.frame(studyID=aim3_chki.tx.conf$studyId,
                           Location=aim3_chki.tx.conf$LOCATION,
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
#show distribution plot overall and by hospital
#single histogram function
plot_histogram <- function(df, feature) {
  plt <- ggplot(df, aes(x=eval(parse(text=feature)))) +
    geom_histogram(aes(y = ..density..), alpha=0.7, fill="#33AADE", color="black") +
    geom_density(alpha=0.3, fill="red") +
    geom_vline(aes(xintercept=mean(eval(parse(text=feature)))), color="black", linetype="dashed", size=1) +
    labs(x=feature, y = "Density")
  print(plt)
}
plot_histogram(hgb.inc.able,'DeltaHgb_div_RBCUnit')
#multiple histogram function
plot_multi_histogram <- function(df, feature, label_column) {
  plt <- ggplot(df, aes(x=eval(parse(text=feature)), 
                        fill=eval(parse(text=label_column)))) +
    geom_histogram(alpha=0.7, position="identity", 
                   aes(y = ..density..), color="black") +
    geom_density(alpha=0.7) +
    geom_vline(aes(xintercept=mean(eval(parse(text=feature)))), 
               color="black", linetype="dashed", size=1) +
    labs(x=feature, y = "Density")
  plt + guides(fill=guide_legend(title=label_column))
}
plot_multi_histogram(hgb.inc.able,'DeltaHgb_div_RBCUnit','Location')
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
  scale_y_continuous(breaks=seq(from=1,to=10)) +
  theme(plot.title = element_text(hjust = 0.5))

#Crosstabs#

#Q6.2 (anemic curr.) and Q6.5 (med. rationale)
Hemorrhage<-aim3_chki.tx.conf$ATF_MedicalRationaleForTx
Hemorrhage<-gsub(".*Hemorrhage.*","Hemorrhage",Hemorrhage)
Hemorrhage<-ifelse(Hemorrhage=="Hemorrhage","Hemorrhage",
  ifelse(Hemorrhage=="NA","NA","Not Hemorrhage"))
summary(freqlist(table(aim3_chki.tx.conf$ATF_IdAnemicDuringCurrPreg,
                       Hemorrhage,
                       useNA = "ifany"),
                 labelTranslations = c("Anemic Curr. Preg.",
                                       "Hemorrhage?")))

#Q6.2 (anemic curr.) and Q5.1a (bleeding this preg.)
summary(freqlist(table(aim3_chki.tx.conf$ATF_IdAnemicDuringCurrPreg,
                       aim3_chki.tx.conf$ATF_BleedingThisPreg,
                       useNA = "ifany"),
                 labelTranslations = c("Anemic Curr. Preg.",
                                       "Bleeding This Preg.")))
#new group variable
anemic.and.bleeding<-NA
anemic.and.bleeding[
  aim3_chki.tx.conf$ATF_IdAnemicDuringCurrPreg=="01 Yes" & 
    aim3_chki.tx.conf$ATF_BleedingThisPreg=="01 Yes"
  ]<-"Both"
anemic.and.bleeding[
  aim3_chki.tx.conf$ATF_BleedingThisPreg=="01 Yes" & 
    (aim3_chki.tx.conf$ATF_IdAnemicDuringCurrPreg=="02 No" | 
    aim3_chki.tx.conf$ATF_IdAnemicDuringCurrPreg=="97 Unknown" | 
    is.na(aim3_chki.tx.conf$ATF_IdAnemicDuringCurrPreg))
  ]<-"Bleeding_Only"
anemic.and.bleeding[
  aim3_chki.tx.conf$ATF_IdAnemicDuringCurrPreg=="01 Yes" & 
    aim3_chki.tx.conf$ATF_BleedingThisPreg=="02 No"
  ]<-"Anemia_Only"
anemic.and.bleeding[
  (aim3_chki.tx.conf$ATF_BleedingThisPreg=="02 No" | 
     is.na(aim3_chki.tx.conf$ATF_BleedingThisPreg)) & 
    (aim3_chki.tx.conf$ATF_IdAnemicDuringCurrPreg=="02 No" |
    is.na(aim3_chki.tx.conf$ATF_IdAnemicDuringCurrPreg))]<-"Neither"
anemic.and.bleeding[
  is.na(aim3_chki.tx.conf$ATF_BleedingThisPreg) & 
    aim3_chki.tx.conf$ATF_IdAnemicDuringCurrPreg=="01 Yes"]<-"Neither"
#QC of Anemia*Bleeding
summary(freqlist(table(anemic.and.bleeding,
                       aim3_chki.tx.conf$ATF_BleedingThisPreg,
                       aim3_chki.tx.conf$ATF_IdAnemicDuringCurrPreg,
                       useNA = "ifany"),
                 labelTranslations = c("Anemic*Bleeding",
                                       "Bleeding this Preg.",
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

#Crosstab of Anemia*Bleeding and Q6.5 Medical Rationale for Tx. (collapsed)
summary(freqlist(table(anemic.and.bleeding,
                       MedicalRationale,
                       useNA = "ifany"),
                 labelTranslations = c("Anemic*Bleeding",
                                       "Med. Rationale Tx.")))

#Gestation Age Weeks Trimester and Q3.6 Booking Status at Admission
summary(freqlist(table(aim3_chki.tx.conf$gest.wks.tri,
                       aim3_chki.tx.conf$ATF_BookingStatusAtAdm,
                       useNA = "ifany")),
        labelTranslations = c("Gest. Age Grouped",
                              "Booking Status at Adm.")

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

#frequency of HIV Status
attach(aim3_chki.tx.conf)
summary(freqlist(table(ATF_HIVStatusAtBooking,
                       ATF_HIVStatusDelvAdmission,
                       ATF_HIVStatusOthTest,
                       useNA = "ifany"),
                 labelTranslations = c("@ Book",
                                       "@ Adm.",
                                       "@ Other Preg. Tests")))
detach(aim3_chki.tx.conf)

#frequency of prior ART status
summary(freqlist(table(aim3_chki.tx.conf$ATF_OnARTPriorThisPreg,
                       useNA = "ifany"),
                 labelTranslations = "On ART Prior to Preg.?"))

#frequency of PMTCT
summary(freqlist(table(aim3_chki.tx.conf$ATF_PMTCTThisPreg,
                       useNA = "ifany"),
                 labelTranslations = "On PMTCT this Preg.?"))

#Crosstab of HIV Status with Anemic*Bleeding
attach(aim3_chki.tx.conf)
#for each HIV Status column
summary(freqlist(table(ATF_HIVStatusAtBooking,
                       ATF_HIVStatusDelvAdmission,
                       ATF_HIVStatusOthTest,
                       anemic.and.bleeding,
                       useNA = "ifany"),
                 labelTranslations = c("@ Book",
                                       "@ Adm.",
                                       "@ Other Preg. Tests",
                                       "Anemic*Bleeding")))
detach(aim3_chki.tx.conf)

#Crosstab of HIV_New and Anemia*Bleeding
summary(freqlist(table(hivstatus,
                       anemic.and.bleeding,
                       useNA = "ifany"),
                 labelTranslations = c("HIV_New",
                                       "Anemic*Bleeding")))

#Crosstab of HIV_New and prior ART Status
attach(aim3_chki.tx.conf)
summary(freqlist(table(hivstatus,
                       ATF_OnARTPriorThisPreg,
                       useNA = "ifany"),
                 labelTranslations = c("HIV_New",
                                       "On ART Prior to Preg?")))

#Crosstab of HIV_New and PMTCT
summary(freqlist(table(hivstatus,
                       ATF_PMTCTThisPreg,
                       useNA = "ifany"),
                 labelTranslations = c("HIV_New",
                                       "On PMTCT this Preg.?")))
detach(aim3_chki.tx.conf)

#Crosstab of Grouped Transfusion Rationale by Gestation Age (0-25 and 26+)
gest.wks.bi<-cut(aim3_chki.tx.conf$ATF_GestAgeWks,
                  breaks=c(-Inf,26,Inf),
                  label=c("<26",">25"),
                  right=FALSE)
summary(freqlist(table(gest.wks.bi,
                       anemic.and.bleeding,
                       useNA = "ifany"),
                 labelTranslations=c("Anemia*Bleeding",
                                     "Gestation Age (0-25 and 26+)")))

#Crosstab of HIV Status New and ART and PMTCT
attach(aim3_chki.tx.conf)
summary(freqlist(table(hivstatus,
                       ATF_OnARTPriorThisPreg,
                       ATF_PMTCTThisPreg,
                       useNA = "ifany"),
                 labelTranslations = c("HIV Status New",
                                       "ART Use Prior Preg.",
                                       "PMTCT This Preg.")))
detach(aim3_chki.tx.conf)

#Crosstab of Gestation Age Trimester and Obstetric Complications
summary(freqlist(table(aim3_chki.tx.conf$gest.wks.tri,
                       CompsThisPreg,
                       useNA = "ifany"),
                 labelTranslations=c("Gestation Age Trimester",
                                     "Obstetric Complications")))

#Crosstab of Gestation Age Trimester and Hemorrhage Causes
summary(freqlist(table(aim3_chki.tx.conf$gest.wks.tri,
                       HemorrhageCause,
                       useNA = "ifany"),
                 labelTranslations=c("Gestation Age Trimester",
                                     "Obstetric Complications")))

#Crosstab of Gestation Age Trimester and Medical Rationale
summary(freqlist(table(aim3_chki.tx.conf$gest.wks.tri,
                       MedicalRationale,
                       useNA = "ifany"),
                 labelTranslations=c("Gestation Age Trimester",
                                     "Medical Rationale for Tx.")))