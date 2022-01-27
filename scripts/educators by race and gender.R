library(readxl)
library(tidyverse)



teachbyracegenderfolder = list.files("./raw_data/Educators/staffracegender_District", full.names = T)


#
teachbyracegenderdata = lapply(teachbyracegenderfolder, function(i) {
  x=read_excel(i,
               col_types = c("text", "text", 
                             "numeric", "numeric", 
                             "numeric", "numeric", 
                             "numeric", "numeric", 
                             "numeric", "numeric", 
                             "numeric", "numeric"),
               skip = 1
  )
  x$file = i
  x
})


names(teachbyracegenderdata [[1]])


#
teachbyracegendernames <- c("District", "DisCode", 
                            "Black", "Asian", "Latinx",
                            "White", "Native American", 
                            "Native Hawaiian", "Multi Race",
                            "Female", "Male", 
                            "FTE_n", "file")
#
#
teachbyracegenderbind<- lapply(teachbyracegenderdata, setNames, teachbyracegendernames)
#
teachbyracegenderall <- bind_rows(teachbyracegenderbind)%>%
  # mutate(sy=str_sub(file, start=-17L, end=-11L))%>%
  mutate(year=str_sub(file, start=-9L, end=-6L))%>%
  relocate(year)%>%
  select(-file)%>%
  janitor::clean_names()%>%
  rename(discode=dis_code)

teachbyracegender<- teachbyracegenderall %>%
  pivot_longer(cols=black:male, names_to="category", values_to ="value")%>%
  mutate(demo=
           ifelse(grepl("male", category), "gender", "race"))


educatorsbyracegender <- left_join(teachbyracegender, justgoodnames)%>%
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
  
  
  library(RColorBrewer)

ggplot(subset(educatorsbyracegender,discode=="00000000" & demo=="race"), aes(x=year, y=value, fill=category))+
  geom_area()+
  hrbrthemes::theme_ipsum_rc()+
  scale_fill_brewer(palette = "YlGnBu")
# scale_fill_brewer(palette = "Set2")
