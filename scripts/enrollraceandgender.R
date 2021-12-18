#---------------------------------------------------------------------------------------------------
# Date: 2021-12-02
#
# Purpose: Enrollment by race and gender
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
library(blscrapeR)

enrollracegenderfolder = list.files("./raw_data/EnrollmentandStudentIndicators/EnrollmentbyRaceGender_District", full.names = T)
#
enrollracegenderdata = lapply(enrollracegenderfolder, function(i) {
  x=read_excel(i,
               skip = 1
  )
  x$file = i
  x
})
#
enrollracegendernames <- c("district",  "discode", "black","asian","latinx",
                   "white", "native","hawaiipi","multi","male","female", "filename", "nonbinary")

#
#
enrollracegenderdata[[27]]

enrollracegender<- lapply(salariesdata, setNames, salariesnames)
#
enrollracegenderdataall <- bind_rows(enrollracegenderdata)%>%
  mutate(year=as.numeric(str_sub(file, start=-9L, end=-6L)))%>%
  mutate(district=str_to_title(`District Name`))

enrollracegendernames <- c("DISTRICT",  "discode", "black","asian","latinx",
                           "white", "native","hawaiipi","multi","male",
                           "female", "filename", "nonbinary", "year", "district")

enrollracegenderdataall <- setNames(enrollracegenderdataall, enrollracegendernames)

enrollbyracegender <- enrollracegenderdataall %>%
  select(year, district, discode, black:female, nonbinary)


enrollracegendernamesfix <- enrollbygrade %>%
  filter(year == 2018)%>%
  select(discode, district)%>%
  rename(dis=district)

racegender <- left_join(enrollbyracegender, enrollracegendernamesfix )%>%
  replace_na(list(dis="missing"))%>%
  mutate(district=
           ifelse(dis %in% "missing", district, dis))%>%
  select(-dis)


rglong <- racegender %>%
  pivot_longer(cols=black:nonbinary, names_to="group", values_to="value")

ggplot(data=subset(rglong, discode=="00000000" & (group == "white" | group == "latinx")),
       aes(x=year, y=value, group=group, shape=group, color=group))+
  geom_line()+
  scale_x_continuous(breaks=seq(1994,2022,3))




