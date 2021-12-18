#---------------------------------------------------------------------------------------------------
# Date: 2021-12-02
#
# Purpose: Teacher Salaries by District
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
bls_key <- "ffea8bbd93c84f12a45c987e3d8c7df3"
#
#
salariesfolder = list.files("./raw_data/Educators/TeacherSalaries_District", full.names = T)
#
salariesdata = lapply(salariesfolder, function(i) {
  x=read_excel(i,
               skip = 1
  )
  x$file = i
  x
})
#
salariesnames <- c("district",  "discode", "total_salary","avg_salary",
                   "n_FTE", "filename")
#
#
salariesdata[[1]]

salariesbind <- lapply(salariesdata, setNames, salariesnames)
#
salariesall <- bind_rows(salariesbind)%>%
  mutate(year=as.numeric(str_sub(filename, start=-9L, end=-6L)))%>%
  mutate(district=str_to_title(district))


salariesNames <- enrollbygrade %>%
  filter(year == 2018)%>%
  select(discode, district)%>%
  rename(dis=district)

teachersalary <- left_join(salariesall, salariesNames)%>%
  replace_na(list(dis="missing"))%>%
  mutate(district=
           ifelse(dis %in% "missing", district, dis))%>%
  select(year,district:n_FTE)

rm(salariesNames, salariesall, salariesbind, salariesdata, salariesfolder)

write.csv(teachersalary, "output/teachersalary.csv", row.names=F)


# ggplot(data=subset(teachersalary, district=="Holyoke"),
#        aes(x=year, y=avg_salary))+
#   geom_line()

inflate_var <- c("CWUR0100SA0")


inflate1 <- bls_api(inflate_var,
              startyear = 1997, endyear = 2017, registrationKey=bls_key)%>%
  dateCast()%>%
  select(-footnotes)

inflate2 <- bls_api("CWUR0100SA0",
                    startyear = 2017, endyear = 2021, registrationKey=bls_key)%>%
  dateCast()%>%
  select(-latest, -footnotes)

inflation <- bind_rows(inflate1, inflate2)

avginflate <- inflation %>%
  group_by(year)%>%
  summarize(avgCPI=mean(value))


teachersalaryadj <- left_join(teachersalary, avginflate)%>%
  mutate(adjust97=
           (avg_salary/avgCPI)*164.7667
  )%>%
  mutate(adjust21=
           (avg_salary/avgCPI)*278.9320
  )
           
           
write.csv(teachersalaryadj, "output/teachersalaryadj.csv", row.names=F)          

ggplot(data=subset(teachersalaryadj, (district=="Holyoke" |
                                      district=="Easthampton"|
                                      district=="Chicopee" |
                                      district == "West Springfield"|
                                      district == "Hampshire"
                                      )),
       aes(x=year, y=adjust21, color=district, shape=district))+
  # geom_line()+
  geom_smooth(method="loess", formula=y~x, se=F)+
  scale_y_continuous(labels=scales::dollar_format())+
  # scale_x_continuous(breaks=seq(200, 2020, 2))+
  ggtitle("Average Teacher Salary in 2021 dollars")+
  theme_ipsum_rc()+
  theme(
    axis.title.y = element_blank(), 
    axis.title.x = element_blank(), 
    legend.title = element_blank()
  )
  

registrationKey=bls_key


rm(salariesNames, salariesall, salariesbind, salariesdata, salariesfolder)