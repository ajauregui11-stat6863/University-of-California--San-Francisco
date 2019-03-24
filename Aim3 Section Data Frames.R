#check for outliers
lapply(aim3_important[c(3:5)],function(x) outliers(as.numeric(x)))
# Create flextable object
ft.aim3_sec0 <- regulartable(data = aim3_sec0) %>%
  set_formatter(ft.aim3_sec0,
                studyId=function(x) sprintf("%.0f",x)) %>%
  theme_zebra %>% 
  autofit
ft.aim3_sec0 # See flextable in RStudio viewer
tmp.0 <- tempfile(fileext = ".docx") # Create a temp file
read_docx() %>%                        # Create a docx file
  body_add_flextable(ft.aim3_sec0) %>% 
  print(target = tmp.0)
browseURL(tmp.0) # open word document

#check for outliers
outliers(as.numeric(aim3_sec1$ATF_TodayDate)) #date column
lapply(aim3_sec1[c(4,6,7)],function(x) outliers(x)) #number columns
unique(aim3_sec1$ATF_Race)
#outliers data frame
sec1.outlier.df<-aim3_sec1[c(38,48,213,360,405,1,3,94,270,315,7,26),]
#create Flextable object
ft.aim3_sec1<- regulartable(data = sec1.outlier.df)
ft.aim3_sec1<- regulartable(data = sec1.outlier.df) %>%
  set_formatter(ft.aim3_sec1,
                studyId=function(x) sprintf("%.0f",x)) %>%
  theme_zebra %>% 
  autofit
ft.aim3_sec1 # See flextable in RStudio viewer
tmp.1 <- tempfile(fileext = ".docx") # Create a temp file
read_docx() %>%                        # Create a docx file
  body_add_flextable(ft.aim3_sec1) %>% 
  print(target = tmp.1)
browseURL(tmp.1) # open word document

#outliers
lapply(aim3_sec2[c(4,5)],function(x) outliers(as.numeric(x))) #date columns
outliers(aim3_sec2$Stay_Length)
lapply(aim3_sec2[c(2,3,6)],unique)
#outliers data frame
sec2.outliers.df<-aim3_sec2[c(9,281,356,641,17,212,352,417,5,9,16,17,22,
                              28,54,57,60,64,91,92,104,110,181,197,207,
                              212,213,257,274,281,291,308,338,348,352,
                              356,357,361,366,379,386,391,417,533,554,
                              586,610,619),]
#create flextable object
ft.aim3_sec2<- regulartable(data = sec2.outliers.df)
ft.aim3_sec2<- regulartable(data = sec2.outliers.df) %>%
  set_formatter(ft.aim3_sec2,
                studyId=function(x) sprintf("%.0f",x)) %>%
  theme_zebra %>% 
  autofit
ft.aim3_sec2 # See flextable in RStudio viewer
tmp.2 <- tempfile(fileext = ".docx") # Create a temp file
read_docx() %>%                        # Create a docx file
  body_add_flextable(ft.aim3_sec2) %>% 
  print(target = tmp.2)
browseURL(tmp.2) # open word document

#outliers
lapply(aim3_sec3[c(5,7)],function(x) outliers(as.numeric(x))) #dates
lapply(aim3_sec3[c(3,8)],function(x) outliers(x)) #numbers
lapply(aim3_sec3[c(2,6,9:16)],unique)
#outliers data frame
sec3.outliers.df<-aim3_sec3[c(119,213,547,217,222,212),c(1,2,3,5,7,8)]
#create flextable object
ft.aim3_sec3<- regulartable(data = sec3.outliers.df)
ft.aim3_sec3<- regulartable(data = sec3.outliers.df) %>%
  set_formatter(ft.aim3_sec3,
                studyId=function(x) sprintf("%.0f",x)) %>%
  theme_zebra %>% 
  autofit
ft.aim3_sec3 # See flextable in RStudio viewer
tmp.3 <- tempfile(fileext = ".docx") # Create a temp file
read_docx() %>%                        # Create a docx file
  body_add_flextable(ft.aim3_sec3) %>% 
  print(target = tmp.3)
browseURL(tmp.3) # open word document

#outliers
lapply(aim3_sec4[c(4,6,8,10)],function(x) outliers(as.numeric(x))) #dates
#outliers data frame
sec4.outliers.df<-aim3_sec4[c(615,437,594,608,553),c(1,2,4,6,8,10)]
#create flextable object
ft.aim3_sec4<- regulartable(data = sec4.outliers.df)
ft.aim3_sec4<- regulartable(data = sec4.outliers.df) %>%
  set_formatter(ft.aim3_sec4,
                studyId=function(x) sprintf("%.0f",x)) %>%
  theme_zebra %>% 
  autofit
ft.aim3_sec4 # See flextable in RStudio viewer
tmp.4 <- tempfile(fileext = ".docx") # Create a temp file
read_docx() %>%                        # Create a docx file
  body_add_flextable(ft.aim3_sec4) %>% 
  print(target = tmp.4)
browseURL(tmp.4) # open word document

#outliers
lapply(aim3_sec6[c(18,23,27)],function(x) outliers(x)) #numbers
lapply(aim3_sec6[-c(1,18,23,27)],unique)
#outliers data frame
sec6.outliers.df<-aim3_sec6[c(7,57,70,106,222,329,413,522,575,
                              91,108,137,223,364,418,603,622,
                              127,247,256,557,582),
                            c(1,2,18,23,27)]
#create flextable object
ft.aim3_sec6<- regulartable(data=sec6.outliers.df)
ft.aim3_sec6<- regulartable(data=sec6.outliers.df) %>%
  set_formatter(ft.aim3_sec6,
                studyId=function(x) sprintf("%.0f",x)) %>%
  theme_zebra %>% 
  autofit
ft.aim3_sec6 # See flextable in RStudio viewer
tmp.6 <- tempfile(fileext = ".docx") # Create a temp file
read_docx() %>%                        # Create a docx file
  body_add_flextable(ft.aim3_sec6) %>% 
  print(target = tmp.6)
browseURL(tmp.6) # open word document

#check for outliers
lapply(aim3_sec7[c(4,6,8,10,12,14,16)],
       function(x) outliers(as.numeric(x))) #dates
#outliers data frame
sec7.outliers.df<-aim3_sec7[c(4,21,383,599,33,370,413,528,599,221,205,
                              40),c(1,4,6,8,10,12,14,16)]
#create flextable object
ft.aim3_sec7<- regulartable(data=sec7.outliers.df)
ft.aim3_sec7<- regulartable(data=sec7.outliers.df) %>%
  set_formatter(ft.aim3_sec7,
                studyId=function(x) sprintf("%.0f",x)) %>%
  theme_zebra %>% 
  autofit
ft.aim3_sec7 # See flextable in RStudio viewer
tmp.7 <- tempfile(fileext = ".docx") # Create a temp file
read_docx() %>%                        # Create a docx file
  body_add_flextable(ft.aim3_sec7) %>% 
  print(target = tmp.7)
browseURL(tmp.7) # open word document
