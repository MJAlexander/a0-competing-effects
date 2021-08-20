## Process HMD data
## You need to download data from https://www.mortality.org/cgi-bin/hmd/hmd_download.php (all hmd countries)
## Author: MA

countries <- list.files("data/hmd_countries/") # not on repo
countries <- countries[countries!="NZL_NM"]

deaths <- tibble()
births <- tibble()

for(i in 1:length(countries)){
  file_name <- paste0("data/hmd_countries/", countries[i], "/InputDB/",countries[i],"death.txt" )
  d <- read_delim(file_name, delim = ",", col_types = "ccdddccccdcdddcd")
  these_deaths <- d %>% 
    mutate(Age = as.numeric(Age), 
           AgeInterval = as.numeric(AgeInterval),
           Year = as.numeric(Year),
           YearInterval = as.numeric(YearInterval),
           Deaths = as.numeric(Deaths),
           #sex = ifelse(is.logical(Sex), ifelse(Sex==TRUE, "m", "f"), Sex),
           PopName = ifelse("Popname" %in% colnames(d), Popname, PopName)) %>% 
    filter(Age<6, LDB==1) %>% 
    group_by(Year, Sex, Lexis) %>% 
    filter(RefCode==max(RefCode)) %>% 
    select(PopName, Year, YearInterval, Sex, Age, AgeInterval, Lexis, Deaths)
  file_name_b <- paste0("data/hmd_countries/", countries[i], "/InputDB/",countries[i],"birth.txt" )
  b <- read_delim(file_name_b, delim = ",")
  these_births <- b %>% 
    mutate(Year = as.numeric(Year), 
           Births = as.numeric(Births), 
           #sex = ifelse(is.logical(Sex), ifelse(Sex==TRUE, "m", "f"), Sex),
           PopName = ifelse("Popname" %in% colnames(b), Popname, PopName)) %>% 
    group_by(Year, Sex) %>% 
    filter(RefCode==max(RefCode)) %>% 
    filter(Year>=min(these_deaths$Year)) %>% 
    select(PopName, Year, Sex, Births)
  
  deaths <- bind_rows(deaths, these_deaths)
  births <- bind_rows(births, these_births)
}


# a0 ----------------------------------------------------------------------


a0 <- deaths %>% 
  filter(Age==0, Lexis %in% c("TU", "TL")) %>% 
  mutate(cohort = ifelse(Lexis=="TL", Year, Year-1)) %>% 
  arrange(PopName, cohort, Sex) %>% 
  group_by(PopName, cohort, Sex) %>% 
  mutate(n = n()) %>% 
  filter(n==2) %>% 
  group_by(PopName, cohort, Sex) %>% 
  summarise(a0 = Deaths[Lexis=="TU"]/sum(Deaths))

# there's some outliers. the later numbers in CAN look wrong

a0 %>% filter(PopName=="CAN") 

## anyway, let's remove these. also removing zero values and I'm not sure about Poland

a0 <- a0 %>% 
  filter(a0!=0, PopName!="POL") %>% 
  mutate(remove = PopName=="CAN"&cohort>2009) %>% 
  filter(!remove) %>% 
  select(-remove)

# imr ---------------------------------------------------------------------

imr <- deaths %>% 
  filter(Age==0, Lexis %in% c("TU", "TL")) %>% 
  group_by(PopName, Year, Sex) %>% 
  mutate(n = n()) %>% 
  filter(n==2) %>% 
  summarise(Deaths=sum(Deaths)) %>% 
  left_join(births) %>% 
  mutate(imr = Deaths/Births) %>% 
  select(PopName, Year, Sex, imr) %>% 
  rename(cohort = Year)

# U5 ----------------------------------------------------------------------

# prep

deaths <- deaths %>% 
  group_by(PopName, Year, Sex, Age) %>% 
  mutate(n = n()) %>% 
  group_by(PopName, Year, Sex) %>% 
  mutate(n_ages = length(unique(Age)))

deaths_red <- deaths %>% 
  mutate(remove=Lexis=="VH"|(Lexis =="RR")&n==3) %>% 
  filter(!remove, n_ages==6) %>% 
  group_by(PopName, Year, Sex, Age) %>% 
  mutate(n = n()) 

#

u5mr <- deaths_red %>% 
  group_by(PopName, Year, Sex) %>% 
  summarise(Deaths=sum(Deaths)) %>% 
  left_join(births) %>% 
  mutate(u5mr = Deaths/Births) %>% 
  select(PopName, Year, Sex, u5mr) %>% 
  rename(cohort = Year)

df <- a0 %>% 
  left_join(imr) %>% 
  left_join(u5mr) %>% 
  mutate(ratio = imr/u5mr) 

write_rds(df %>% filter(ratio<1|ratio>0.4, imr<1) %>% select(-low_imr), path = "data/hmd_data.rds")

