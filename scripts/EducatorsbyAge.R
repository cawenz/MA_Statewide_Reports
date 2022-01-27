library(readxl)
library(tidyverse)



teachbyagefolder = list.files("./raw_data/Educators/EducatorsbyAge", full.names = T)



#
teachbyagedata = lapply(teachbyagefolder, function(i) {
  x=read_excel(i,
               col_types = c("text", "text", 
                             "numeric", "numeric", 
                             "numeric", "numeric", 
                             "numeric", "numeric", 
                             "numeric", "numeric"),
               skip = 1
  )
  x$file = i
  x
})


names(teachbyagedata[[1]])


#
teachbyagenames <- c("District", "DisCode", 
                      "Less_than_26", "26_to_32", 
                      "33_to_40", "41_to_48", 
                      "49_to_56", "57_to_64", 
                      "Over_64", "Total_FTE", "file")
#
#
teachbyagebind<- lapply(teachbyagedata, setNames, teachbyagenames)
#
teachbyageall <- bind_rows(teachbyagebind)%>%
  # mutate(sy=str_sub(file, start=-17L, end=-11L))%>%
  mutate(year=str_sub(file, start=-9L, end=-6L))%>%
  relocate(year)%>%
  select(-file)%>%
  janitor::clean_names()%>%
  rename(discode=dis_code)

teachbyage <- teachbyageall %>%
  pivot_longer(cols=less_than_26:over_64, names_to="category", values_to ="value")%>%
  mutate(category=
           str_replace_all(str_remove(category, "x"), "_", " ")
           )%>%
  mutate(percent=
           round(((value/total_fte)*100), digits=1))


educatorsbyage <- left_join(teachbyage, justgoodnames)%>%
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
  select(-district)%>%
  relocate(dis)%>%
  rename(district=dis)%>%
  mutate(year=as.numeric(year))%>%
  mutate(category=factor(category, 
                      levels=c("over 64",
                               "57 to 64",
                               "49 to 56",
                               "41 to 48",
                               "33 to 40",
                               "26 to 32",
                               "less than 26"
                      )))
                               
                          
library(RColorBrewer)

ggplot(subset(educatorsbyage,discode=="01370000"), aes(x=year, y=percent, fill=category))+
  geom_area()+
  hrbrthemes::theme_ipsum_rc()+
  scale_fill_brewer(palette = "YlGnBu")
  # scale_fill_brewer(palette = "Set2")
