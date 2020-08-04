##############################
# Author: Mari Roberts
# Date: 8/04/2020
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

inmates <- read_csv("Daily_Inmates_In_Custody.csv",na = c("", "NA", "*", "**"))

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

# fix date format
inmates$date_admitted <- mdy_hms(inmates$date_admitted)
# inmates$date_discharged <- mdy_hms(inmates$date_discharged)

# check for NA's
# map(latrine_all, ~sum(is.na(.)))

