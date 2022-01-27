#---------------------------------------------------------------------------------------------------
# Date: 2021-12-02
#
# Purpose: Create the Enrollment by Grade Long Dataframe
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
enrollbygradefolder = list.files("./raw_data/EnrollmentandStudentIndicators/EnrollmentbyGrade_District", full.names = T)

#
enrollbygradedata = lapply(enrollbygradefolder, function(i) {
  x=read_excel(i,
               col_types = c("text", "text", "numeric", 
                             "numeric", "numeric", "numeric", 
                             "numeric", "numeric", "numeric", 
                             "numeric", "numeric", "numeric", "numeric", 
                             "numeric", "numeric", "numeric", "numeric", "numeric"), 
               skip = 1
  )
  x$file = i
  x
})
#
enrollgradenames <- c("district",  "discode", "PK", "K", 
                  "grade1", "grade2", "grade3", "grade4",
                  "grade5", "grade6", "grade7", "grade8",
                  "grade9", "grade10", "grade11", "grade12",
                  "SP", "total", "filename")
#
#
enrollgradebind <- lapply(enrollbygradedata, setNames, enrollgradenames)
#
enrollgradeall <- bind_rows(enrollgradebind)%>%
  mutate(sy=str_sub(filename, start=-17L, end=-11L))%>%
  mutate(year=str_sub(filename, start=-9L, end=-6L))

ebg <- enrollgradeall %>%
  select(-filename)%>%
  select(sy, year, district:total)%>%
  mutate(district=str_to_title(district))

justgoodnames <- enrollgradeall %>%
  filter(sy== "SY12-13")%>%
  select(district, discode)%>%
  rename(dis=district)


fixnames <- left_join(ebg, justgoodnames)%>%
  filter(is.na(dis))%>%
  group_by(discode) %>%
  slice(1)


enrollbygrade <- left_join(ebg, justgoodnames)%>%
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
  select(-dis)%>%
  mutate(year=(as.numeric(year)))%>%
  arrange(year)

# rm(ebg, enrollbygradedata, enrollbygradefolder, 
#    enrollgradeall, fixnames, enrollgradebind, justgoodnames)

write.csv(enrollbygrade, "output/enrollbygrade.csv", row.names=F)

