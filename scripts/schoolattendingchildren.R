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


schoolattendfolder = list.files("./raw_data/EnrollmentandStudentIndicators/schoolattendingchildren", full.names = T)

schoolattenddata = lapply(schoolattendfolder, function(i) {
  x=read_excel(i, 
               skip = 1
  )
  x$file = i
  x
})

schoolattendnames <- c(
  "town", 
  "town_code", 
  "local_public", 
  "academic_regional", 
  "vocational_technical", 
  "collaborative", 
  "charter", 
  "out_district_public", 
  "home", 
  "in_state_private",
  "out_state_private",
  "total",
  "total_public",
  "per_public", 
  "file"
)

schoolattenddata <- lapply(schoolattenddata, setNames, schoolattendnames)

schoolattendall <- bind_rows(schoolattenddata)%>%
  mutate(year=str_sub(file, start=-9L, end=-6L))%>%
  select(-file)%>%
  relocate(year)%>%
  mutate(code=
           ifelse(nchar(town_code)==4, paste0(town_code, "0000"), town_code
           ))%>%
  relocate(code, .after=town_code)%>%
  mutate(town=
           ifelse(town %in% 
                    "Great Barrington (non-op", "Great Barrington (non-op)", town))%>%
 mutate(nonop=
          ifelse(grepl("non-op",town, ignore.case = T), T, F))%>%
  relocate(nonop, .after = town)%>%
 mutate(town=
          str_remove_all(town,"non-op")
          )%>%
  mutate(town=
           str_remove_all(town, "Non-Op"))%>%
  mutate(town=
           str_remove_all(town, "Non-op"))%>%
 mutate(towntest=
          gsub("[()]", "", town)
 )%>%
  relocate(towntest, .after=town)%>%
 mutate(towntest=
          str_trim(towntest, side="both")
          )%>%
  # mutate(towntest=
  #          ifelse(nonop==T, paste0(towntest, " (Non-Op)"), towntest))%>%
  select(-town)%>%
  rename(town=towntest)%>%
  mutate(year=as.numeric(year))%>%
  relocate(total_public, .before=total)

means <- schoolattendall %>%
  group_by(year)%>%
  summarize(across(local_public:out_state_private, mean))


schoolattendlong <- schoolattendall %>%
  replace_na(list(charter=0, home=0))%>%
  pivot_longer(cols=local_public:total_public, 
               names_to="type", 
               values_to="value")%>%
  mutate(per_total=
           round(
             ((value/total)*100),
             digits=1
           ))
  # mutate(type=factor(type, levels=c(
  #                                 "local_public", 
  #                                 "academic_regional",
  #                                 "vocational_technical",
  #                                 "in_state_private",
  #                                 "charter",
  #                                 "collaborative",
  #                                 "out_district_public",
  #                                 "out_state_private",
  #                                 "home",
  #                                 "total_public")))
    
    
    
         
ggplot(data=subset(schoolattendlong, 
                   code=="00000000" & 
                   (type=="charter" | type=="total_public")
                  ), 
       aes(x=year, y=per_total, group=type))+
geom_line()+
theme_ipsum_rc()
 

          
 ggplot(subset(
   schoolattendlong,
   (type != "local_public" & type != "total_public") 
   & code=="00000000"
   ), 
   aes(x=year, y=per_total, group=type, color=type))+
   geom_smooth(se=F)+
   hrbrthemes::theme_ipsum_rc()+
   scale_fill_brewer(palette = "YlGnBu")
   
   
   

