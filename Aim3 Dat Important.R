#merge all sections into one big data frame
aim3_Dat.imp<-merge(aim3_sec0,aim3_sec1,by=c("studyId","LOCATION"))
aim3_Dat.imp<-merge(aim3_Dat.imp,aim3_sec2,by=c("studyId","LOCATION"))
aim3_Dat.imp<-merge(aim3_Dat.imp,aim3_sec3,by=c("studyId","LOCATION"))
aim3_Dat.imp<-merge(aim3_Dat.imp,aim3_sec4,by=c("studyId","LOCATION"))
aim3_Dat.imp<-merge(aim3_Dat.imp,aim3_sec5,by=c("studyId","LOCATION"))
aim3_Dat.imp<-merge(aim3_Dat.imp,aim3_sec6,by=c("studyId","LOCATION"))
aim3_Dat.imp<-merge(aim3_Dat.imp,aim3_sec7,by=c("studyId","LOCATION"))

#find all the date column indices so that we can move them to the front
datecols<-sapply(aim3_Dat.imp, 
                 function(x) !all(is.na(as.Date(as.character(x),
                                                format="%Y-%m-%d"))))
which(datecols,arr.ind = TRUE)
aim3_Dat.imp<-aim3_Dat.imp[c(
  1,2,3,4,5,6,12,13,18,20,32,34,36,40,71,73,75,77,79,81,83,
  7:11,14:17,19,21:31,33,35,37:39,41:70,72,74,76,78,80,82,84:109)]

#check for outliers on the date columns
lapply(aim3_Dat.imp[c(3:21)],function(x) outliers(as.numeric(x)))

#create flextable object
ft.aim3 <- regulartable(data = aim3_Dat.imp)
ft.aim3 <- regulartable(data = aim3_Dat.imp) %>%
  set_formatter(ft.aim3,
                studyId=function(x) sprintf("%.0f",x)) %>%
  theme_zebra %>% 
  autofit
ft.aim3 # See flextable in RStudio viewer
tmp <- tempfile(fileext = ".docx") # Create a temp file
read_docx() %>%                        # Create a docx file
  body_add_flextable(ft.aim3) %>% 
  print(target = tmp)
browseURL(tmp)