## Download linked death records from NBER
## Authors: MA and LR


library(tidyverse)
source("./code/functions/download_raw_data.R")
download_dir = "./raw_data"

##data we need: 
##cohort-linked death files 1983-2013 (1992-1994 not available)
##cohort-linked birth files 1983-2013 (1992-1994 not available)
###note that for current purposes, the birth microdata are not strictly necessary
###but at least for 1983-1991, the aggregate data aren't readily available (e.g. from WONDER)
###so we download the microdata and generate our aggregate counts of births by race and 
###gestational age at birth from that.
###Be extremely careful with downloading birth files (code to download them is commented out
###by default) as they are very large files!

for (i in 1983:1991){
  download_linked_deaths(i)
  #download_births(i)
  # 
  fname <- sprintf("%s/linkco%sus_num.csv.zip", download_dir, i)
  assign(paste("dfm",i,sep="_"),readr::read_csv(fname)) 
  
  #fname <- sprintf("%s/linkco%sus_den.csv.zip", download_dir, i)
  #assign(paste("dfb",i,sep="_"),readr::read_csv(fname))
  
}

dfb1 <- rbind(dfb_1983,dfb_1984,dfb_1985,dfb_1986)
dfb2 <- rbind(dfb_1987,dfb_1988)
dfb3 <- rbind(dfb_1989,dfb_1990,dfb_1991)

##doing these by group because of differences between different years

births_1 <- dfb1 %>%
  rename(year = datayear) %>% 
  filter(mrace==1|mrace==2) %>%
  mutate(race = ifelse(mrace==1, "W", "B")) %>%
  group_by(year, race,gestat10) %>% 
  summarize(births = n())

births_2 <- dfb2 %>%
  rename(year = datayear) %>% 
  filter(mrace==1|mrace==2) %>%
  mutate(race = ifelse(mrace==1, "W", "B")) %>%
  group_by(year, race,gestat10) %>% 
  summarize(births = n())

births_3 <- dfb3 %>%
  rename(year = biryr) %>% 
  filter(orracem==6|orracem==7) %>%
  mutate(race = ifelse(orracem==6, "NHW", "NHB")) %>%
  group_by(year, race,gestat10) %>% 
  summarize(births = n())

births <- rbind(births_1,births_2,births_3)
colnames(births) <- c("year","race","gestation","births")
births$preemie <-  cut(x = births$gestation, breaks = c(0,2,3, 5, 9,10))
levels(births$preemie) <- c("extremely preterm","very preterm","later preterm","full-term","unknown")
saveRDS(births,"./data/births.RDS")

###Only death data is downloaded 1995-2013; we use CDC WONDER for summary birth data.
###Modify to download birth microdata as well (see previous for-loop for example) if desired.
###I tested the year 2003 to see if generating from microdata yields different birth counts than
###CDC WONDER data. Microdata has more births across all race/gestational age combinations - largest difference is 0.015% 
###in extremely preterm NHW infants. Difference for NHB extremely preterm is 0.011%; all other combinations
###have less than .01% difference. File available in output folder, called birth_count_validation.RDS.

for (i in 1995:2013){
  download_linked_deaths(i)
  
  fname <- sprintf("%s/linkco%sus_num.csv.zip", download_dir, i)
  assign(paste("dfm",i,sep="_"),readr::read_csv(fname)) 
  
}


###reducing size of death files by discarding columns we won't need, reordering variables, 
### and renaming variables be the same across years

#variables included:

#year of birth
#mother's age
#mother's race/Hispanicity (coding varies by year)
#live birth order
#gestational age (coding varies by year)
#sex
#age at death (coding varies by year)
#year of death
#cause of death
#cause recode (coding varies by year)

names <- c("datayear","dmage","mrace","dlivord","gestat10","csex",
           "ager5","ucod","ucodr61","dthyr")

m <- list(dfm_1983,dfm_1984,dfm_1985,dfm_1986,dfm_1987,dfm_1988)

for (i in 1:length(m)){
  m[[i]] <- m[[i]][,colnames(m[[i]]) %in% names]
  m[[i]] <- m[[i]][c("datayear","dmage","mrace","dlivord","gestat10","csex",
                    "ager5","ucod","ucodr61","dthyr")]
  colnames(m[[i]]) <- c("birth_year","mother_age","mother_race","birth_order","gest_age","sex",
                        "death_age","ucod","ucodr","death_year")
  m[[i]]$birth_order <- ifelse(m[[i]]$birth_order > 8 & m[[i]]$birth_order < 99,8,m[[i]]$birth_order)
  m[[i]]$sex <- ifelse(m[[i]]$sex == 1, "M","F")
}

#variable change in 1989 

names <- c("biryr","dmage","orracem","dlivord","gestat","csex",
           "aged","ucod","ucodr61","dthyr")

n <- list(dfm_1989,dfm_1990,dfm_1991)
for (i in 1:length(n)){
  n[[i]] <- n[[i]][,colnames(n[[i]]) %in% names]
  n[[i]] <- n[[i]][c("biryr","dmage","orracem","dlivord","gestat","csex",
                     "aged","ucod","ucodr61","dthyr")]
  colnames(n[[i]]) <- c("birth_year","mother_age","mother_race","birth_order","gest_age","sex",
                        "death_age","ucod","ucodr","death_year")
  n[[i]]$birth_order <- ifelse(n[[i]]$birth_order > 8 & n[[i]]$birth_order < 99,8,n[[i]]$birth_order)
  n[[i]]$sex <- ifelse(n[[i]]$sex == 1, "M","F")
}

#variable change in 1995

names <- c("biryr","dmage","orracem","dlivord","gestat","csex",
           "aged","ucod","ucodr61","dthyr")

o <- list(dfm_1995,dfm_1996,dfm_1997,dfm_1998)
for (i in 1:length(o)){
  o[[i]] <- o[[i]][,colnames(o[[i]]) %in% names]
  o[[i]] <- o[[i]][c("biryr","dmage","orracem","dlivord","gestat","csex",
                     "aged","ucod","ucodr61","dthyr")]
  colnames(o[[i]]) <- c("birth_year","mother_age","mother_race","birth_order","gest_age","sex",
                        "death_age","ucod","ucodr","death_year")
  o[[i]]$birth_order <- ifelse(o[[i]]$birth_order > 8 & o[[i]]$birth_order < 99,8,o[[i]]$birth_order)
  o[[i]]$sex <- ifelse(o[[i]]$sex == 1, "M","F")
}

#variable change in 1999

names <- c("biryr","dmage","orracem","dlivord","gestat","csex",
           "aged","ucod","ucodr130","dthyr")

p <- list(dfm_1999,dfm_2000,dfm_2001,dfm_2002)
for (i in 1:length(p)){
  p[[i]] <- p[[i]][,colnames(p[[i]]) %in% names]
  p[[i]] <- p[[i]][c("biryr","dmage","orracem","dlivord","gestat","csex",
                     "aged","ucod","ucodr130","dthyr")]
  colnames(p[[i]]) <- c("birth_year","mother_age","mother_race","birth_order","gest_age","sex",
                        "death_age","ucod","ucodr","death_year")
  p[[i]]$birth_order <- ifelse(p[[i]]$birth_order > 8 & p[[i]]$birth_order < 99,8,p[[i]]$birth_order)
  p[[i]]$sex <- ifelse(p[[i]]$sex == 1, "M","F")
}

#variable change in 2003

names <- c("dob_yy","mager41","mracehisp","lbo","combgest","sex",
           "aged","ucod","ucodr130","dthyr")

dfm_2003 <- dfm_2003[,colnames(dfm_2003) %in% names]
dfm_2003 <- dfm_2003[c("dob_yy","mager41","mracehisp","lbo","combgest","sex",
                       "aged","ucod","ucodr130","dthyr")]
colnames(dfm_2003) <- c("birth_year","mother_age","mother_race","birth_order","gest_age","sex",
                      "death_age","ucod","ucodr","death_year")
dfm_2003$mother_age <- dfm_2003$mother_age + 13
dfm_2003$birth_order <- ifelse(dfm_2003$birth_order > 8 & dfm_2003$birth_order < 99,8,dfm_2003$birth_order)
p[[(length(p)+1)]] <- dfm_2003

#in 2004 cause of death recode variable name changes, from there on out everything is the same

names <- c("dob_yy","mager41","mracehisp","lbo","combgest","sex",
           "aged","ucod","ucod130","dthyr")

q <- list(dfm_2004,dfm_2005,dfm_2006,dfm_2007,dfm_2008,dfm_2009,dfm_2010,dfm_2011,dfm_2012,dfm_2013)

for (i in 1:length(q)){
  q[[i]] <- q[[i]][,colnames(q[[i]]) %in% names]
  q[[i]] <- q[[i]][c("dob_yy","mager41","mracehisp","lbo","combgest","sex",
                     "aged","ucod","ucod130","dthyr")]
  colnames(q[[i]]) <- c("birth_year","mother_age","mother_race","birth_order","gest_age","sex",
                        "death_age","ucod","ucodr","death_year")
}
q[[1]]$birth_order <- ifelse(q[[1]]$birth_order > 8 & q[[1]]$birth_order < 99,8,q[[i]]$birth_order)

#are variable values the same throughout?
#year of birth - yes
#mother's age - 1983-98 has 11-50, 1999-2002 use single years age 10-54, 2003 recode has 1= 14-, 2=15, 3=16 etc. 2004-2013 has single years
    #starting at 12- and going to 50+
#mother's race/Hispanicity - no. through 1988, 1=white and 2=black, hispanicity data is unknown.
#for all later years:
      # 6 ... Non-Hispanic White
      # 7 ... Non-Hispanic Black
#live birth order - in 2005, switches to 8+ with 9=unknown; previously went higher and 99 was unknown
#gestational age - no
#sex - 1=M 2=F until F=F and M=M
#age at death - 
#age recode used 1983-1988 is as follows:
#1 = under one hour
#2 = 1-23 hours (1+2 = first-day mortality)
#3 = more than 1 day, less than 7 (1+2+3 = first-week mortality) 
#4 = more than 1 week, less than 28 days (1+2+3+4 = neonatal mortality)
#5 = post-neonatal

#starting in 1989, 0-364

#year of death - yes
#cause of death - unsure
#recode - no, switch to 130-cause recode in 1999


dfm1 <- bind_rows(m)
dfm2 <- bind_rows(n)
dfm3 <- bind_rows(o)
dfm4 <- bind_rows(p)
dfm5 <- bind_rows(q)

dfm_all <- rbind(dfm1,dfm2,dfm3,dfm4,dfm5)

#fix age
dfm_all$mother_age <- ifelse(dfm_all$mother_age < 15,14,dfm_all$mother_age)
dfm_all$mother_age <- ifelse(dfm_all$mother_age > 50,50,dfm_all$mother_age)

saveRDS(dfm_all,"./data/dfm_all.RDS")


## make births file (for simulation)

denom <- read.csv(here("data","births03-06.csv"))
denom2 <- read.csv(here("data","births07-13.csv"))
denom <- bind_rows(denom, denom2)


denom$preemie <-  cut(x = denom$gest, breaks = c(0,27,31, 36, 48))
levels(denom$preemie) <- c("extremely preterm","very preterm","later preterm","full-term")
denom <- subset(denom,denom$gest <99)

births <- denom %>% filter(year==2012) %>% group_by(race, preemie) %>% summarise(sum(births))
saveRDS(births, "./data/births.RDS")

