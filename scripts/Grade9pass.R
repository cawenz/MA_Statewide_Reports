#---------------------------------------------------------------------------------------------------
# Date: 2021-12-02
#
# Purpose: Create the Grade 9 Passing Long Dataframe
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# INSTALL PACKAGES & LOAD LIBRARIES
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(tidyverse)
library(readxl)
library(googlesheets4)
library(ggplot2)
library(hrbrthemes)
library(ggthemes)
library(janitor)
library(readxl)
#
gradeninepassfolder = list.files("./raw_data/GradeNinePass", full.names = T)
#
grade9data = lapply(gradeninepassfolder, function(i) {
  x=read_excel(i,
               col_types = c("text", "text", "numeric", 
                             "numeric", "numeric"), skip = 1
  )
  x$file = i
  x
})
#
passingnames <- c("district",  "discode", "n_students", 
              "n_passing", "percent_passing", "filename")   
#
#
grade9bind <- lapply(grade9data, setNames, passingnames)
#
grade9passingallraw <- bind_rows(grade9bind)%>%
  mutate(fn=str_sub(filename, start=37, end=-6L))

grade9passingall <- left_join(grade9passingallraw ,justgoodnames)%>%
  replace_na(list(dis="missing"))%>%
  mutate(dis=
           ifelse(dis %in% "missing", district, dis))%>%
  mutate(district=
           ifelse(discode %in% "39010000",
                  "Greater Commonwealth Virtual District",
                  ifelse(discode %in% "04600000",   
                         "Lynn Community Charter",
                         ifelse(discode %in% "04720000", 
                                "New Bedford Global Learning Charter (District)",
                                ifelse(discode %in% "04730000",
                                       "North Star Academy Charter School",
                                       ifelse(discode %in% "04900000", 
                                              "Uphams Corner Charter (District)",
                                              ifelse(discode %in% "35040000", 
                                                     "City On A Hill Charter Public School Dudley Square (District)",
                                                     ifelse(discode %in% "04230000",
                                                            "Barnstable Horace Mann Charter (District)",
                                                            ifelse(discode %in% "04220000", 
                                                                   "Roxbury Charter High Public (District)",
                                                                   ifelse(discode %in% "04340000",
                                                                          "Champion Charter (District)",
                                                                          ifelse(discode %in% "04420000", 
                                                                                 "Frederick Douglass Charter",
                                                                                 ifelse(discode %in% "04510000",
                                                                                        "Robert M. Hughes Academy Charter Public (District)",
                                                                                        ifelse(discode %in% "04600000", 
                                                                                               "Lynn Community Charter",
                                                                                               ifelse(discode %in% "35030000", 
                                                                                                      "Collegiate Charter School Of Lowell (District)", dis
                                                                                               ))))))))))))))%>%
  select(-dis)







#
grade9passingall <- separate(grade9passingall, fn, into=c("year", "Group", "Subject"), 
                             sep="_", remove=F)%>%
  mutate(group=
           ifelse(grepl("All", Group), "All", Group)
           )%>%
  mutate(subject=
           ifelse(grepl("All", Subject), "All", Subject)
           )







#
grade9passL <- grade9passingall %>%
  select(year, discode, district, group, subject, n_students:percent_passing)
#
write.csv(grade9passL, file="output/grade9pass.csv", row.names=F)
