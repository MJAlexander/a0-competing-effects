## Mortality descriptives
## This creates rates reported in Tables 1-3
## Author: LR

library(reshape)
library(tidyverse)
library(here)

####share of births by race/gestational status####

#split into extremely preterm <28, very preterm (28-<32), later preterm (32-<37), full-term
#and also dichotomous all preterm and full-term

denom <- read.csv(here("data","births03-06.csv"))
denom2 <- read.csv(here("data","births07-13.csv"))
denom <- bind_rows(denom, denom2)


denom$preemie <-  cut(x = denom$gest, breaks = c(0,27,31, 36, 48))
levels(denom$preemie) <- c("extremely preterm","very preterm","later preterm","full-term")
denom <- subset(denom,denom$gest <99)

share <- denom %>% 
  group_by(year, race,preemie) %>% 
  summarise(total = sum(births))

share <- cast(share,race + year~preemie,sum)
share$total <- share$`extremely preterm`+share$`very preterm`+share$`later preterm`+share$`full-term`


####first-week, neonatal, postneonatal, and overall infant mortality####
dfm_all <- readRDS(here("data","dfm_all.RDS"))
d <- dfm_all %>% 
  dplyr::rename(year = birth_year) %>% 
  filter(mother_race==6|mother_race==7, year!=2013) %>%
  mutate(race = ifelse(mother_race==6, "NHW", "NHB"))

d$mortfirstwk <- ifelse(d$death_age < 7,1,0)
d$mortneonat <- ifelse(d$death_age < 28,1,0)
d$mortpost <- ifelse(d$mortneonat == 1,0,1)
d <- subset(d,d$gest_age < 99)

week1 <- d %>% 
  group_by(year, race) %>% 
  summarise(first_week = sum(mortfirstwk))

neonatal <- d %>% 
  group_by(year, race) %>% 
  summarise(neonatal = sum(mortneonat))


post <- d %>% 
  group_by(year, race) %>% 
  summarise(postneonatal = sum(mortpost))


mortality <- merge(week1,neonatal,by=c("year","race"))
mortality <- merge(mortality,post,by=c("year","race"))
mortality$total <- mortality$neonatal + mortality$postneonatal

#add births
#note that the births counts here are slightly different than the total births by gestational age in df 'share'
#because those exclude births with unknown gestational age

db <- narcan::live_births

db <- db %>% 
  group_by(m_race_eth, year) %>% 
  summarise(births = sum(births))

db <- subset(db,db$m_race_eth=="non_hisp_black" | db$m_race_eth=="non_hisp_white")
colnames(db) <- c("race","year","births")
db$race <- ifelse(db$race == "non_hisp_black","NHB","NHW")

mortality <- merge(mortality,db,by=c("year","race"))

#rates
mortality$firstweekmort <- mortality$first_week*1000/mortality$births
mortality$neonatalmort <- mortality$neonatal*1000/mortality$births
mortality$postneonatalmort <- mortality$postneonatal*1000/mortality$births
mortality$overall <- mortality$total*1000/mortality$births

####mortality rates by both age and gest status####
#first, split out gest status
d$preemie <-  cut(x = d$gest_age, breaks = c(0,27,31, 36, 48))
levels(d$preemie) <- c("extremely preterm","very preterm","later preterm","full-term")

#extremely preterm
##overall
extpre <- subset(d,d$preemie=="extremely preterm")
extprem <- melt(as.data.frame(table(extpre$year,extpre$race)))
extprem <- extprem[,c(1,2,4)]
colnames(extprem) <- c("year","race","extpre_overall")
##first-week
extpre <- subset(extpre,extpre$mortfirstwk==1)
extprem <- cbind(extprem,as.data.frame(table(extpre$year,extpre$race)))
extprem <- extprem[,c(1,2,3,6)]
colnames(extprem) <- c("year","race","extpre_overall","extpre_firstwk")
##neonatal
extpre <- subset(d,d$preemie=="extremely preterm" & d$mortneonat==1)
extprem <- cbind(extprem,as.data.frame(table(extpre$year,extpre$race)))
extprem <- extprem[,c(1:4,7)]
colnames(extprem)[colnames(extprem)=="Freq"] <- "extpre_neonatal"
##post-neonatal
extprem$extpre_post <- extprem$extpre_overall-extprem$extpre_neonatal


#very preterm
##overall
verypre <- subset(d,d$preemie=="very preterm")
veryprem <- melt(as.data.frame(table(verypre$year,verypre$race)))
veryprem <- veryprem[,c(1,2,4)]
colnames(veryprem) <- c("year","race","verypredeaths")
##first-week
verypre <- subset(verypre,verypre$mortfirstwk==1)
veryprem <- cbind(veryprem,as.data.frame(table(verypre$year,verypre$race)))
veryprem <- veryprem[,c(1,2,3,6)]
colnames(veryprem) <- c("year","race","verypre_overall","verypre_firstwk")
##neonatal
verypre <- subset(d,d$preemie=="very preterm" & d$mortneonat==1)
veryprem <- cbind(veryprem,as.data.frame(table(verypre$year,verypre$race)))
veryprem <- veryprem[,c(1:4,7)]
colnames(veryprem)[colnames(veryprem)=="Freq"] <- "verypre_neonatal"
##post-neonatal
veryprem$verypre_post <- veryprem$verypre_overall-veryprem$verypre_neonatal

#late preterm
##overall
latepre <- subset(d,d$preemie=="later preterm")
lateprem <- melt(as.data.frame(table(latepre$year,latepre$race)))
lateprem <- lateprem[,c(1,2,4)]
colnames(lateprem) <- c("year","race","latepredeaths")
##first-week
latepre <- subset(latepre,latepre$mortfirstwk==1)
lateprem <- cbind(lateprem,as.data.frame(table(latepre$year,latepre$race)))
lateprem <- lateprem[,c(1,2,3,6)]
colnames(lateprem) <- c("year","race","latepre_overall","latepre_firstwk")
##neonatal
latepre <- subset(d,d$preemie=="later preterm" & d$mortneonat==1)
lateprem <- cbind(lateprem,as.data.frame(table(latepre$year,latepre$race)))
lateprem <- lateprem[,c(1:4,7)]
colnames(lateprem)[colnames(lateprem)=="Freq"] <- "latepre_neonatal"
##post-neonatal
lateprem$latepre_post <- lateprem$latepre_overall-lateprem$latepre_neonatal



#full term
##overall
fullt <- subset(d,d$preemie=="full-term")
fulltm <- melt(as.data.frame(table(fullt$year,fullt$race)))
fulltm <- fulltm[,c(1,2,4)]
colnames(fulltm) <- c("year","race","fulltermdeaths")
##first-week
fullt <- subset(fullt,fullt$mortfirstwk==1)
fulltm <- cbind(fulltm,as.data.frame(table(fullt$year,fullt$race)))
fulltm <- fulltm[,c(1,2,3,6)]
colnames(fulltm) <- c("year","race","ft_overall","ft_firstwk")
##neonatal
fullt <- subset(d,d$preemie=="full-term" & d$mortneonat==1)
fulltm <- cbind(fulltm,as.data.frame(table(fullt$year,fullt$race)))
fulltm <- fulltm[,c(1:4,7)]
colnames(fulltm)[colnames(fulltm)=="Freq"] <- "ft_neonatal"
##post-neonatal
fulltm$ft_post <- fulltm$ft_overall-fulltm$ft_neonatal

#add births by gest status
#fortunately we have these already in the df called share

share <- merge(x=share,y=extprem,by=c("year","race"))
share <- merge(x=share,y=veryprem,by=c("year","race"))
share <- merge(x=share,y=lateprem,by=c("year","race"))
share <- merge(x=share,y=fulltm,by=c("year","race"))

#overall mortality by gestational status
share$`extremely preterm overall mortality` <- share$extpre_overall*1000/share$`extremely preterm`
share$`very preterm overall mortality` <- share$verypre_overall*1000/share$`very preterm`
share$`late preterm overall mortality` <- share$latepre_overall*1000/share$`later preterm`
share$`full term overall mortality` <- share$ft_overall*1000/share$`full-term`

#first-week mortality by gestational status
share$`extremely preterm first-week mortality` <- share$extpre_firstwk*1000/share$`extremely preterm`
share$`very preterm first-week mortality` <- share$verypre_firstwk*1000/share$`very preterm`
share$`late preterm first-week mortality` <- share$latepre_firstwk*1000/share$`later preterm`
share$`full term first-week mortality` <- share$ft_firstwk*1000/share$`full-term`

#neonatal mortality by gestational status
share$`extremely preterm neonatal mortality` <- share$extpre_neonatal*1000/share$`extremely preterm`
share$`very preterm neonatal mortality` <- share$verypre_neonatal*1000/share$`very preterm`
share$`late preterm neonatal mortality` <- share$latepre_neonatal*1000/share$`later preterm`
share$`full term neonatal mortality` <- share$ft_neonatal*1000/share$`full-term`

#post-neonatal mortality by gestational status
share$`extremely preterm post-neonatal mortality` <- share$extpre_post*1000/share$`extremely preterm`
share$`very preterm post-neonatal mortality` <- share$verypre_post*1000/share$`very preterm`
share$`late preterm post-neonatal mortality` <- share$latepre_post*1000/share$`later preterm`
share$`full term post-neonatal mortality` <- share$ft_post*1000/share$`full-term`

#for comparison's sake -
#the denominators here are a bit different than they were in
#the mortality df because we've discarded those with no information on gestational age
share$`overall first-week mortality` <- (share$extpre_firstwk+share$verypre_firstwk+share$latepre_firstwk+share$ft_firstwk)*1000/share$total
share$`overall neonatal mortality` <- (share$extpre_neonatal+share$verypre_neonatal+share$latepre_neonatal+share$ft_neonatal)*1000/share$total
share$`overall post-neonatal mortality` <- (share$extpre_post+share$verypre_post+share$latepre_post+share$ft_post)*1000/share$total
share$`overall mortality` <- (share$extpre_overall+share$verypre_overall+share$latepre_overall+share$ft_overall)*1000/share$total

#nice - quite similar to the calculations from mortality df

#clarify all column names
colnames(share) <- c("year","race","extremely preterm births","very preterm births","later preterm births",
                     "full-term births","total births","total extremely preterm deaths","extremely preterm deaths, first week",
                     "extremely preterm deaths, neonatal","extremely preterm deaths, post-neonatal","total very preterm deaths",
                     "very preterm deaths, first week","very preterm deaths, neonatal","very preterm deaths, post-neonatal",
                     "total late preterm deaths","late preterm deaths, first week","late preterm deaths, neonatal",
                     "late preterm deaths, post-neonatal","total full-term deaths","full-term deaths, first week",
                     "full-term deaths, neonatal","full-term deaths, post-neonatal","extremely preterm overall mortality",
                     "very preterm overall mortality","late preterm overall mortality","full-term overall mortality",
                     "extremely preterm first-week mortality","very preterm first-week mortality","late preterm first-week mortality",
                     "full-term first-week mortality","extremely preterm neonatal mortality","very preterm neonatal mortality",
                     "late preterm neonatal mortality","full-term neonatal mortality","extremely preterm post-neonatal mortality",
                     "very preterm post-neonatal mortality","late preterm post-neonatal mortality","full-term post-neonatal mortality",
                     "overall first-week mortality","overall neonatal mortality","overall post-neonatal mortality","overall mortality")

write_csv(share,path = "./output/mortality_descriptives.csv")
