##the normalization function is created
nor <-function(x) { (x -min(x))/(max(x)-min(x))}

# What are the variables that define HPS? 
#1. %of non-white students 

library(readr)
d <- read_csv("output/districtchar.csv")%>%
  filter(Year == 2021 & 
        !grepl("(District)", District) & 
        !grepl("Vocational", District) & 
        !grepl("Agricult", District) & 
        DisCode != "04680000")


gradesoff <- read_excel("raw_data/GradesOffered_District.xlsx", 
                                     skip = 1)%>%
  rename(DisCode=`District Code`)%>%
  rename(GradeList=`Grade List`)%>%
  select(DisCode, GradeList)

districts <- left_join(d, gradesoff)%>%
  mutate(n_grades=str_count(GradeList,",")+1)%>%
  mutate(n_pergrade=round(enrollment/n_grades, digits=0))%>%
  #remove districts that are just high schools
  filter(GradeList != "9,10,11,12")%>%
  mutate(nonwhite_per=100-white_per)


imp <- districts %>%
  select(District, DisCode, n_pergrade,
         ell_per, ecodis_per, swd_per,
         
         black_per, hispanic_per, asian_per, white_per)
         
         
        



impnor <- imp %>%
  mutate(across(n_pergrade:white_per, nor))








library(FNN)

neighbors <- get.knn(data=impnor[,3:10], k=10, algorithm = "kd_tree")
knn10 <- as.data.frame(neighbors$nn.index)

# knnx.index(imp[,5:10], imp[,5:10], k=11, algorithm="CR")

disknn<- bind_cols(impnor, knn10)
disknn$names <- rownames(disknn)

knnlong <- disknn %>%
  pivot_longer(cols=V1:V10, values_to="index", names_to="n_neigh")


matchnames <- disknn %>%
  select(names, District) %>%
  mutate(names=as.numeric(names))%>%
  rename(neighbor=District)%>%
  rename(index=names)


indexed <- left_join(knnlong, matchnames)


print(neighbors$nn.index[107,])
  


library(mclust)

impcl <- Mclust(data=imp[3:9],G=1:10)
summary(impcl, parameters = T)
plot(impcl)

imp$cl <- impcl$classification

df <- data.frame(impcl$z)

withz <- bind_cols(imp,df)
  
  districts2019 %>%
  # filter(!grepl("Charter", District)) %>%
  select("District", "DisCode", "n_students", "ell_%", 
         "swd_%", 
         "ecodis_%", "white_per", "hispanic_per")
  
  
  
  
hpscluster <- imp %>%
  filter(cl == 1)

hpsknn <- get.knn(data=hpscluster[,3:9], k=10, algorithm = "cover_tree")
hpsknn5 <- as.data.frame(hpsknn$nn.index)

hps <- bind_cols(hpscluster, hpsknn5)%>%
  unite(col=ns, V1:V10, sep=",", remove=F)%>%
  mutate(lns=paste0("c(", ns, ")"))


[hps[19,22]

hps$names <- rownames(hps)

hpslong <- hps %>%
  pivot_longer(cols=V1:V10, values_to="index", names_to="n_neigh")%>%
  mutate(index=as.numeric(hpslong$index))
  

hpsnames <- hps %>%
  select(names, District)%>%
  mutate(names=as.numeric(hps$names))%>%
  rename(index=names)%>%
  rename(match=District)

final <- left_join(hpslong, hpsnames)
  


