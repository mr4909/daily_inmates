##############################
# Author: Mari Roberts
# Date: 8/03/2020
# Import and Clean
##############################

# load necessary packages
requiredPackages = c('dplyr', # data manipulation
                     'readr', # read files
                     'stringr', # wrapper functions
                     'lubridate') # date format
# only downloads packages if needed
for(p in requiredPackages){
  if(!require(p,character.only = TRUE)) install.packages(p)
  library(p,character.only = TRUE)
}

###############
# import data
###############

inmates <- read_csv("Daily_Inmates_In_Custody_8_26_2020.csv")

###############
# clean data
###############

# summarise data
lapply(inmates, table)

# rename variables
inmates <- inmates %>% select(inmate_ID = INMATEID,
                              date_admitted = ADMITTED_DT,
                              date_discharged = DISCHARGED_DT,
                              custody_level = CUSTODY_LEVEL,
                              in_mental_observation = BRADH,           
                              race = RACE,
                              gender = GENDER,
                              age = AGE,
                              in_gang = SRG_FLG,
                              top_charge = TOP_CHARGE,
                              infraction = INFRACTION)
inmates <- inmates %>% mutate(gender = ifelse(gender == "M",0,1),
                              in_mental_observation = ifelse(in_mental_observation == "N",0,1),
                              in_gang = ifelse(in_gang == "N",0,1),
                              infraction = ifelse(infraction == "N",0,1))

#factor variables
inmates$inmate_ID <-factor(inmates$inmate_ID)
inmates$in_mental_observation <-factor(inmates$in_mental_observation)
inmates$race <-factor(inmates$race)
inmates$gender <-factor(inmates$gender)
inmates$in_gang <-factor(inmates$in_gang)
inmates$infraction <- factor(inmates$infraction)
inmates$custody_level <- factor(inmates$custody_level, ordered = TRUE,
                                levels = c("MIN","MED","MAX"))
levels(inmates$race)
levels(inmates$race) <- c("Asian", "Black", "Native American",
                          "Other", "Unknown", "White")
levels(inmates$gender)
levels(inmates$gender) <- c("Male", "Female")

# fix date format
inmates$date_admitted <- mdy_hms(inmates$date_admitted)
# inmates$date_discharged <- mdy_hms(inmates$date_discharged)

# remove NAs
inmates <- inmates %>% filter(race != "NA" &
                              #gender != "NA"
                              age != "NA"
                              #custody_level != "NA"
                              )

# extract month, year, and day
inmates$year <- format(inmates$date_admitted, "%Y")
inmates$month <- format(inmates$date_admitted, "%b")
inmates$day <- format(inmates$date_admitted, "%d")
inmates$month_day <- format(inmates$date_admitted, "%d-%b")
inmates$common_date <- as.Date(paste0(inmates$year,"-",format(inmates$date_admitted, "%j")), "%Y-%j")

# create age groups
inmates <- inmates %>% 
  mutate(agegroup = 
           case_when(age <= 20 ~ "18-20",
                     age >=21 & age<= 25 ~ "21-25",
                     age >=26 & age<= 30   ~ "26-30",
                     age >=31 & age<= 35   ~ "31-35",
                     age >=36 & age<= 40   ~ "36-40",
                     age >=41 & age<= 45   ~ "41-45",
                     age >=46 & age<= 50   ~ "46-50",
                     age >=51 & age<= 55   ~ "51-55",
                     age >= 56   ~ "56+",
                     ))
