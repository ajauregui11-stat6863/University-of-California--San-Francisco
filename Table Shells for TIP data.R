###Table 1: Characteristics of the Study Population###
#
#Section 1 - Demographics
#1. Age:
attach(aim3_sec1)
sum(is.na(ATF_Age))
describe(ATF_Age,fast=TRUE)
tapply(ATF_Age,LOCATION,describe,fast=TRUE) #by hospital
ggplot(aim3_sec1,aes(ATF_Age)) + 
  geom_histogram(binwidth=1) + 
  labs(x="Age in Years",y="counts",title="Age of Mothers")
ggplot(aim3_sec1,aes(LOCATION,ATF_Age)) + 
  geom_boxplot(fill=c("chocolate","green","khaki")) +
  labs(x="Location",y="Age in Years",title="Age of Mothers by Location")
#
#6. Race / ethnic origin:
race.tab<-table(ATF_Race,useNA = "ifany")
summary(freqlist(race.tab))
ggplot(aim3_sec1,aes(ATF_Race)) +
  geom_bar(fill=c("black","white","brown","yellow","gray")) +
  labs(x="Race",y="counts",title="Race of Mothers") +
  scale_x_discrete(labels=c("Black","White","Coloured","Asian","NA")) +
  geom_text(stat='count',aes(label=..count..,vjust=-.5))
race.hosp.tab<-table(LOCATION,ATF_Race,useNA = "ifany") #by hospital
summary(freqlist(race.hosp.tab))
ggplot(aim3_sec1, aes(fill=ATF_Race, x=LOCATION)) + 
  geom_bar() + 
  scale_fill_manual(values=c("01 Black"="black",
                           "02 White"="white",
                           "03 Coloured"="brown",
                           "04 Asian"="yellow",
                           "NA"="gray")) +
  labs(x="Hospital",y="counts",title="Race of Mothers by Location")
#
#7. Gravidity
median(ATF_Gravidity)
tapply(X=ATF_Gravidity,INDEX=LOCATION,FUN=median,na.rm=T) #by hospital
#
#8. Parity
sum(is.na(ATF_Parity))
median(ATF_Parity,na.rm=T)
tapply(X=ATF_Parity,INDEX=LOCATION,FUN=median,na.rm=T) #by hospital
detach(aim3_sec1)
#
#Section 3 - Pregnancy
#4. Gestational age at time of transfusion:
attach(aim3_sec3)
sum(is.na(ATF_GestAgeWks))
#as quantitative variable
mean(ATF_GestAgeWks,na.rm=T)
tapply(ATF_GestAgeWks,LOCATION,mean,na.rm=T)
#as quartiles
quantile(ATF_GestAgeWks,na.rm=T)
tapply(ATF_GestAgeWks,LOCATION,quantile,na.rm=T)
#as <26, 26-32, 32+
gest.wks<-cut(ATF_GestAgeWks,
              breaks=c(-Inf,25,32,Inf),
              label=c("<26","26-32",">32"),
              right=FALSE)
#
#
#Print out Table 1
#(aggregated)
table1.df<-data.frame(Age=aim3_sec1$ATF_Age,
                      Race=aim3_sec1$ATF_Race,
                      Gravidity=aim3_sec1$ATF_Gravidity,
                      Parity=aim3_sec1$ATF_Parity,
                      Gestation_Age_Weeks=aim3_sec3$ATF_GestAgeWks,
                      Booking_Status=aim3_sec3$ATF_BookingStatusAtAdm,
                      Prenatal_Visits=
                        aim3_sec3$ATF_NumVisitsAntenatalClinic,
                      Gestation_Age_Weeks_Grouped=gest.wks,
                      Length_of_Stay=stay.length,
                      Alive_at_Discharge=aim3_sec2$ATF_AliveAtDschg)
listvars<-c("Age","Race","Gravidity","Parity","Gestation_Age_Weeks",
            "Booking_Status","Gestation_Age_Weeks_Grouped","Length_of_Stay",
            "Prenatal_Visits","Alive_at_Discharge")
catvars<-c(#"Location",
           "Race","Booking_Status","Gestation_Age_Weeks_Grouped",
           "Alive_at_Discharge")
nonnormalvars<-c("Gravidity","Parity","Prenatal_Visits","Length_of_Stay")
table1<-CreateTableOne(vars=listvars,data = table1.df,factorVars = catvars,
                       includeNA = TRUE)
table1.doc<-print(table1,nonnormal = nonnormalvars)
#(by hospital)
table1.df<-data.frame(Location=aim3_sec1$LOCATION,
  Age=aim3_sec1$ATF_Age,
  Race=aim3_sec1$ATF_Race,
  Gravidity=aim3_sec1$ATF_Gravidity,
  Parity=aim3_sec1$ATF_Parity,
  Gestation_Age_Weeks=aim3_sec3$ATF_GestAgeWks,
  Booking_Status=aim3_sec3$ATF_BookingStatusAtAdm,
  Prenatal_Visits=aim3_sec3$ATF_NumVisitsAntenatalClinic,
  Gestation_Age_Weeks_Grouped=gest.wks,
  Length_of_Stay=stay.length,
  Alive_at_Discharge=aim3_sec2$ATF_AliveAtDschg)
listvars<-c("Age","Race","Gravidity","Parity","Gestation_Age_Weeks",
            "Booking_Status","Gestation_Age_Weeks_Grouped","Length_of_Stay",
            "Prenatal_Visits","Alive_at_Discharge")
catvars<-c("Location",
  "Race","Booking_Status","Gestation_Age_Weeks_Grouped",
  "Alive_at_Discharge")
nonnormalvars<-c("Gravidity","Parity","Prenatal_Visits","Length_of_Stay")
table1<-CreateTableOne(vars=listvars,data = table1.df,factorVars = catvars,
                       includeNA = TRUE,strata = "Location",test=FALSE)
print(table1,nonnormal = nonnormalvars)


####Table 2### 
#
#Section 6 - Transfusion
#Primary indication for blood transfusion  
#4. Was the patient transfused?
attach(aim3_sec6)
#get a subset of the rows that are marked '02 No' or missing
atf_transfused.weird<-subset(aim3_sec6,ATF_Transfused=="02 No" | 
                               is.na(ATF_Transfused))
#if an observation has an entry in the last 3 columns 
  #(redTotal, pltTotal, and ffpTotal) that is not zero, then we can 
  #change that observation to a "01 Yes"
#aim3_sec6<-within(aim3_sec6,
 #                 ATF_Transfused[
  #                  !(redTotal==0) & !(pltTotal==0) & !(ffpTotal==0)]<-
   #                 "01 Yes")
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
detach(aim3_sec6)
#
#Section 5 -Bleeding in Current Pregnancy
#Hemorrhage, all (Q5.1a); Estim blood loss (mean, SD)
attach(aim3_sec5)
#Hemorrhage Etiology
#aim3_sec5$HemmorageCause.tf<-grepl("Bleed",ATF_HemorrhageCause)
#bleed.df<-subset(aim3_sec5,HemmorageCause.tf=="TRUE")
#summary(freqlist(table(bleed.df$ATF_HemorrhageCause,useNA = "ifany")))
#summary(freqlist(table(LOCATION,bleed.df$ATF_HemorrhageCause,
 #                      useNA = "ifany")))
detach(aim3_sec5)
#Print out Table 2
table2.df<-data.frame(Primary_Indication_for_Blood_Transfusion=
                        aim3_sec6$ATF_Transfused,
                      Blood_Transfusion_Rationale=
                        aim3_sec6$ATF_MedicalRationaleForTx,
                      Anemic_Current_Pregnancy=
                        aim3_sec6$ATF_IdAnemicDuringCurrPreg,
                      Anemia_Etiology=
                        aim3_sec6$ATF_ChronicAnemiaType,
                      Hemorrhage=
                        aim3_sec5$ATF_BleedingThisPreg,
                      Hemorrhage_Etiology=
                        aim3_sec5$ATF_HemorrhageCause)
listvars<-c("Primary_Indication_for_Blood_Transfusion",
            "Blood_Transfusion_Rationale","Anemic_Current_Pregnancy",
            "Anemia_Etiology","Hemorrhage","Hemorrhage_Etiology")
catvars<-c("Primary_Indication_for_Blood_Transfusion",
           "Blood_Transfusion_Rationale","Anemic_Current_Pregnancy",
           "Anemia_Etiology","Hemorrhage","Hemorrhage_Etiology")
table2<-CreateTableOne(vars=listvars,data=table2.df,factorVars = catvars,
                       includeNA = TRUE)
table2

###Table 3 Complications During Current Pregnancy###
#
#Section 3 - Pregnancy
#other (non-hemorrhagic) obstetric complication (n, %)
#10. Complications during THIS pregnancy?
attach(aim3_sec3)
detach(aim3_sec3)
#make table 3
table3.df<-data.frame(Other_Nonhemorrhagic_Obstetric_Complication=
                        aim3_sec3$ATF_CompsThisPreg,
                      Nonhemorrhagic_Obstetric_Complications=
                        aim3_sec3$ATF_CompsThisPregType,
                      Other_Obstetric_Complication=
                        aim3_sec3$ATF_CompsThisAdm,
                      Obstetric_Complications=
                        aim3_sec3$ATF_CompsThisAdmType)
listvars<-c("Other_Nonhemorrhagic_Obstetric_Complication",
            "Nonhemorrhagic_Obstetric_Complications",
            "Other_Obstetric_Complication",
            "Obstetric_Complications")
catvars<-c("Other_Nonhemorrhagic_Obstetric_Complication",
           "Nonhemorrhagic_Obstetric_Complications",
           "Other_Obstetric_Complication",
           "Obstetric_Complications")
table3<-CreateTableOne(vars=listvars,data = table3.df,factorVars = catvars,
                       includeNA = TRUE)
table3


###Miscellaneous###
#
#Section 6 - Transfusion
#1. Had the patient been transfused before the current transfusion?
attach(aim3_sec6)
#Anemia Treatment
#make table 4
table4.df<-data.frame(Previous_Transfusion=
                        aim3_sec6$ATF_TransfusePriorToCurr,
                      Previous_Transfusion_When=
                        aim3_sec6$ATF_TransfusedPriorWhen,
                      Anemia_Treatment=
                        aim3_sec6$ATF_OnHematinicRxDuringPreg,
                      Who_ordered_transfusion=
                        aim3_sec6$ATF_TxHighestLevelDiscuss,
                      Transfused_where_pt.a=
                        aim3_sec6$ATF_PtTransfusedAt,
                      Transfused_where_pt.b=
                        aim3_sec6$ATF_WherePtTxStart,
                      PreTransfusion_Hemoglobin=
                        aim3_sec6$ATF_LastHemoglobinPriorTx,
                      Hemoglobin_Method_Used=
                        aim3_sec6$ATF_HbMethodUsed,
                      Results_Obtained_Before_Transfusion=
                        aim3_sec6$ATF_WhereResultsObtainedPriorTx)
listvars<-c("Previous_Transfusion","Previous_Transfusion_When",
             "Anemia_Treatment","Who_ordered_transfusion",
             "Transfused_where_pt.a","Transfused_where_pt.b",
             "PreTransfusion_Hemoglobin","Hemoglobin_Method_Used",
             "Results_Obtained_Before_Transfusion")
catvars<-c("Previous_Transfusion","Previous_Transfusion_When",
            "Anemia_Treatment","Who_ordered_transfusion",
            "Transfused_where_pt.a","Transfused_where_pt.b",
            "Hemoglobin_Method_Used","Results_Obtained_Before_Transfusion")
table4<-CreateTableOne(vars=listvars,data = table4.df,factorVars = catvars,
                       includeNA = TRUE)
table4
#
#Figure 1. Bar graphs of blood products administered
#red blood cells
bloodproducts<-subset(aim3_sec7,select=c(1,2,40:42))
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
#check for outliers
plot(aim3_sec6$ATF_LastHemoglobinPriorTx)
plot(aim3_sec6$ATF_HbAfterTx)
boxplot.stats(aim3_sec6$ATF_LastHemoglobinPriorTx)
which(grepl(77.5,aim3_sec6$ATF_LastHemoglobinPriorTx))
#make the data set for the figure 2
hgb.increments<-data.frame(Location=aim3_sec6$LOCATION,
                           Pretransfusion_Hgb=
                             aim3_sec6$ATF_LastHemoglobinPriorTx,
                           Posttransfusion_Hgb=aim3_sec6$ATF_HbAfterTx,
                           Delta_Hgb=aim3_sec6$ATF_HbAfterTx - 
                             aim3_sec6$ATF_LastHemoglobinPriorTx,
                           RBC_unit=aim3_sec7$redTotal)
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
  "Groote/Mowbray","Groote/Mowbray","Groote/Mowbray","Groote/Mowbray",
  "King Edward VIII","King Edward VIII",
  "King Edward VIII","King Edward VIII",
  "Overall","Overall","Overall","Overall"),
  Hgb_Tx=c(
    "Pre","Post","Delta","Delta/Unit",
    "Pre","Post","Delta","Delta/Unit",
    "Pre","Post","Delta","Delta/Unit",
    "Pre","Post","Delta","Delta/Unit"),
  means=c(6.6,9.26,2.66,1.12,
          6.61,8.92,2.3,1.14,
          6.93,8.79,1.86,.83,
          6.74,9.02,2.28,1))
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

#Q6.2 and Q6.5
aim3_sec6$Hemorrhage<-aim3_sec6$ATF_MedicalRationaleForTx
aim3_sec6$Hemorrhage<-gsub(
  ".*Hemorrhage.*","Hemorrhage",aim3_sec6$Hemorrhage)
aim3_sec6$Hemorrhage<-ifelse(
  aim3_sec6$Hemorrhage=="Hemorrhage","Hemorrhage",
  ifelse(aim3_sec6$Hemorrhage=="NA","NA","Not Hemorrhage"))
summary(freqlist(table(aim3_sec6$ATF_IdAnemicDuringCurrPreg,
                       aim3_sec6$Hemorrhage,
                       useNA = "ifany"),
                 labelTranslations = c("Anemic Curr. Preg.",
                                       "Bleeding This Preg.")))
aim3_sec6$Anemic.Curr<-aim3_sec6$ATF_IdAnemicDuringCurrPreg
aim3_sec6$Anemic.Curr[aim3_sec6$Anemic.Curr=="97 Unknown"]<-NA
summary(freqlist(table(aim3_sec6$Anemic.Curr,
                       aim3_sec6$Hemorrhage),
                       labelTranslations = c("Anemic Curr. Preg.",
                                             "Bleeding This Preg.")))
#Q6.2 and Q5.1a
summary(freqlist(table(aim3_sec6$ATF_IdAnemicDuringCurrPreg,
                       aim3_sec5$ATF_BleedingThisPreg,
                       useNA = "ifany"),
                 labelTranslations = c("Anemic Curr. Preg.",
                                       "Bleeding This Preg.")))
#"Other" Specified for Table 2
summary(freqlist(table(aim3_sec5$ATF_HemorrhageCause,
                       aim3_sec5$HemorrhageCauseOtherSpecify),
                 labelTranslations = c(
                   "Hemorrhage Cause w/ 'Other'","Other Specify")))
#"Other" Specified for Table 3
summary(freqlist(table(aim3_sec3$ATF_CompsThisPregType,
                       aim3_sec3$ComplicationsThisPregnancySP),
                 labelTranslations = c(
                   "Preg. Complications w/ 'Other'","'Other' Specified")))
summary(freqlist(table(aim3_sec3$ATF_CompsThisAdmType,
                       aim3_sec3$ComplicationsThisAdmissionSP),
                 labelTranslations = c(
                   "Adm. Complications w/ 'Other'","'Other' Specified")))