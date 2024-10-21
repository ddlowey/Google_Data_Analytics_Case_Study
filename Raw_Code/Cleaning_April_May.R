#### Load packages ####
#=====================#

library("tidyverse")

#### Load in all data ####
#========================#

Activity_d_AprMay <- read.csv("/Users/../mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")
Calories_d_AprMay <- read.csv("/Users/../mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/dailyCalories_merged.csv")
Intensities_d_AprMay <- read.csv("/Users/../mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/dailyIntensities_merged.csv")
Sleep_d_AprMay <- read.csv("/Users/../mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")
Steps_d_AprMay <- read.csv("/Users/../mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/dailySteps_merged.csv")
Calories_h_AprMay <- read.csv("/Users/../mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/hourlyCalories_merged.csv")
Intensities_h_AprMay <- read.csv("/Users/../mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/hourlyIntensities_merged.csv")
Steps_h_AprMay <- read.csv("/Users/../mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/hourlySteps_merged.csv")
Calories_m_AprMay <- read.csv("/Users/../mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/minuteCaloriesNarrow_merged.csv")
Intensities_m_AprMay <- read.csv("/Users/../mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/minuteIntensitiesNarrow_merged.csv")
MET_m_AprMay <- read.csv("/Users/../mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/minuteMETsNarrow_merged.csv")
Sleep_m_AprMay <- read.csv("/Users/../mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/minuteSleep_merged.csv")
Steps_m_AprMay <- read.csv("/Users/../mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/minuteStepsNarrow_merged.csv")
Weight_AprMay <- read.csv("/Users/../mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/weightLogInfo_merged.csv")
HeartRate_s_AprMay <- read.csv("/Users/../mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/heartrate_seconds_merged.csv")

#'*Variable names consist of category + time measurement + period indicator*
#'*Files with wide format excluded* 

#### Preview the data ####
#========================#

glimpse(Activity_d_AprMay)
glimpse(Calories_d_AprMay)
glimpse(Intensities_d_AprMay)
glimpse(Sleep_d_AprMay)
glimpse(Steps_d_AprMay)
glimpse(Calories_h_AprMay)
glimpse(Intensities_h_AprMay)
glimpse(Steps_h_AprMay)
glimpse(Calories_m_AprMay)
glimpse(Intensities_m_AprMay)
glimpse(MET_m_AprMay)
glimpse(Sleep_m_AprMay)
glimpse(Steps_m_AprMay)
glimpse(Weight_AprMay)
glimpse(HeartRate_s_AprMay)

#### Check timezones of data and computer and adjust if necessary ####
#====================================================================#

tz(Activity_d_AprMay$ActivityDate)
tz(Calories_d_AprMay$ActivityDay)
tz(Intensities_d_AprMay$ActivityDay)
tz(Sleep_d_AprMay$SleepDay)
tz(Steps_d_AprMay$ActivityDay)
tz(Calories_h_AprMay$ActivityHour)
tz(Intensities_h_AprMay$ActivityHour)
tz(Steps_h_AprMay$ActivityHour)
tz(Calories_m_AprMay$ActivityMinute)
tz(Intensities_m_AprMay$ActivityMinute)
tz(MET_m_AprMay$ActivityMinute)
tz(Sleep_m_AprMay$date)
tz(Steps_m_AprMay$ActivityMinute)
tz(Weight_AprMay$Date)
tz(HeartRate_s_AprMay$Time)

Sys.time()
Sys.setenv(TZ = "UTC")

#### Make column naming more consistent ####
#==========================================#

# Change column names to uppercase where necessary #

Sleep_m_AprMay <- rename_with(Sleep_m_AprMay, str_to_title)

Sleep_m_AprMay <- rename(Sleep_m_AprMay, "LogId" = Logid)

# Rename date column from each dataset #

Activity_d_AprMay <- rename(Activity_d_AprMay, "ActivityTime" = ActivityDate)
Calories_d_AprMay <- rename(Calories_d_AprMay, "ActivityTime" = ActivityDay)
Intensities_d_AprMay <- rename(Intensities_d_AprMay, "ActivityTime" = ActivityDay)
Sleep_d_AprMay <- rename(Sleep_d_AprMay, "ActivityTime" = SleepDay)
Steps_d_AprMay <- rename(Steps_d_AprMay, "ActivityTime" = ActivityDay)
Calories_h_AprMay <- rename(Calories_h_AprMay, "ActivityTime" = ActivityHour)
Intensities_h_AprMay <- rename(Intensities_h_AprMay, "ActivityTime" = ActivityHour)
Steps_h_AprMay <- rename(Steps_h_AprMay, "ActivityTime" = ActivityHour)
Calories_m_AprMay <- rename(Calories_m_AprMay, "ActivityTime" = ActivityMinute)
Intensities_m_AprMay <- rename(Intensities_m_AprMay, "ActivityTime" = ActivityMinute)
MET_m_AprMay <- rename(MET_m_AprMay, "ActivityTime" = ActivityMinute)
Sleep_m_AprMay <- rename(Sleep_m_AprMay, "ActivityTime" = Date)
Steps_m_AprMay <- rename(Steps_m_AprMay, "ActivityTime" = ActivityMinute)
Weight_AprMay <- rename(Weight_AprMay, "ActivityTime" = Date)
HeartRate_s_AprMay <- rename(HeartRate_s_AprMay, "ActivityTime" = Time)

# Specify vague column names where necessary #

Steps_d_AprMay <- rename(Steps_d_AprMay, "TotalSteps" = StepTotal)

Steps_h_AprMay <- rename(Steps_h_AprMay, "TotalSteps" = StepTotal) 

Sleep_m_AprMay <- rename(Sleep_m_AprMay, "SleepStage" = Value) 

HeartRate_s_AprMay <- rename(HeartRate_s_AprMay, "HeartRate" = Value) 

#### Change data types ####
#=========================#

Activity_d_AprMay$Id <- as.character(Activity_d_AprMay$Id)
Activity_d_AprMay$ActivityTime <- mdy(Activity_d_AprMay$ActivityTime)

Calories_d_AprMay$Id <- as.character(Calories_d_AprMay$Id)
Calories_d_AprMay$ActivityTime <- mdy(Calories_d_AprMay$ActivityTime)

Intensities_d_AprMay$Id <- as.character(Intensities_d_AprMay$Id)
Intensities_d_AprMay$ActivityTime <- mdy(Intensities_d_AprMay$ActivityTime)

Sleep_d_AprMay$Id <- as.character(Sleep_d_AprMay$Id)
Sleep_d_AprMay$ActivityTime <- mdy_hms(Sleep_d_AprMay$ActivityTime)

Steps_d_AprMay$Id <- as.character(Steps_d_AprMay$Id)
Steps_d_AprMay$ActivityTime <- mdy(Steps_d_AprMay$ActivityTime)

Calories_h_AprMay$Id <- as.character(Calories_h_AprMay$Id)
Calories_h_AprMay$ActivityTime <- mdy_hms(Calories_h_AprMay$ActivityTime)

Intensities_h_AprMay$Id <- as.character(Intensities_h_AprMay$Id)
Intensities_h_AprMay$ActivityTime <- mdy_hms(Intensities_h_AprMay$ActivityTime)

Steps_h_AprMay$Id <- as.character(Steps_h_AprMay$Id)
Steps_h_AprMay$ActivityTime <- mdy_hms(Steps_h_AprMay$ActivityTime)

Calories_m_AprMay$Id <- as.character(Calories_m_AprMay$Id)
Calories_m_AprMay$ActivityTime <- mdy_hms(Calories_m_AprMay$ActivityTime)

Intensities_m_AprMay$Id <- as.character(Intensities_m_AprMay$Id)
Intensities_m_AprMay$ActivityTime <- mdy_hms(Intensities_m_AprMay$ActivityTime)

MET_m_AprMay$Id <- as.character(MET_m_AprMay$Id)
MET_m_AprMay$ActivityTime <- mdy_hms(MET_m_AprMay$ActivityTime)

Sleep_m_AprMay$Id <- as.character(Sleep_m_AprMay$Id)
Sleep_m_AprMay$ActivityTime <- mdy_hms(Sleep_m_AprMay$ActivityTime)

Steps_m_AprMay$Id <- as.character(Steps_m_AprMay$Id)
Steps_m_AprMay$ActivityTime <- mdy_hms(Steps_m_AprMay$ActivityTime)

Weight_AprMay$Id <- as.character(Weight_AprMay$Id)
Weight_AprMay$ActivityTime <- mdy_hms(Weight_AprMay$ActivityTime)

HeartRate_s_AprMay$Id <- as.character(HeartRate_s_AprMay$Id)
HeartRate_s_AprMay$ActivityTime <- mdy_hms(HeartRate_s_AprMay$ActivityTime)

#### Check unique 'Id'-values for each dataset ####
#=================================================#

n_distinct(Activity_d_AprMay$Id)
n_distinct(Calories_d_AprMay$Id)
n_distinct(Intensities_d_AprMay$Id)
n_distinct(Sleep_d_AprMay$Id)
n_distinct(Steps_d_AprMay$Id)
n_distinct(Calories_h_AprMay$Id)
n_distinct(Intensities_h_AprMay$Id)
n_distinct(Steps_h_AprMay$Id)
n_distinct(Calories_m_AprMay$Id)
n_distinct(Intensities_m_AprMay$Id)
n_distinct(MET_m_AprMay$Id)
n_distinct(Sleep_m_AprMay$Id)
n_distinct(Steps_m_AprMay$Id)
n_distinct(Weight_AprMay$Id)
n_distinct(HeartRate_s_AprMay$Id)

#'*Sleep (24), Weight (8), HeartR(14), rest (33)*
#'*3 more participants than expected*

# Compare IDs to make sure they are consistent throughout each dataset #

actd_id_AprMay <- print(unique(Activity_d_AprMay$Id))
cald_id_AprMay <- print(unique(Calories_d_AprMay$Id))
intd_id_AprMay <- print(unique(Intensities_d_AprMay$Id))
sled_id_AprMay <- print(unique(Sleep_d_AprMay$Id))
sted_id_AprMay <- print(unique(Steps_d_AprMay$Id))
calh_id_AprMay <- print(unique(Calories_h_AprMay$Id))
inth_id_AprMay <- print(unique(Intensities_h_AprMay$Id))
steh_id_AprMay <- print(unique(Steps_h_AprMay$Id))
calm_id_AprMay <- print(unique(Calories_m_AprMay$Id))
intm_id_AprMay <- print(unique(Intensities_m_AprMay$Id))
metm_id_AprMay <- print(unique(MET_m_AprMay$Id))
slem_id_AprMay <- print(unique(Sleep_m_AprMay$Id))
stem_id_AprMay <- print(unique(Steps_m_AprMay$Id))
weig_id_AprMay <- print(unique(Weight_AprMay$Id))
hearts_id_AprMay <- print(unique(HeartRate_s_AprMay$Id))

match(actd_id_AprMay, cald_id_AprMay)
match(actd_id_AprMay, intd_id_AprMay)
match(actd_id_AprMay, sled_id_AprMay) 
match(actd_id_AprMay, sted_id_AprMay)
match(actd_id_AprMay, calh_id_AprMay)
match(actd_id_AprMay, inth_id_AprMay)
match(actd_id_AprMay, steh_id_AprMay)
match(actd_id_AprMay, calm_id_AprMay)
match(actd_id_AprMay, intm_id_AprMay)
match(actd_id_AprMay, metm_id_AprMay)
match(actd_id_AprMay, slem_id_AprMay) 
match(actd_id_AprMay, stem_id_AprMay)
match(actd_id_AprMay, weig_id_AprMay) 
match(actd_id_AprMay, hearts_id_AprMay) 

#'*IDs are consistent throughout each dataset minus differences in participants*

#### Check time frame for each dataset ####
#=========================================#

summarise(Activity_d_AprMay, min(ActivityTime), max(ActivityTime))
summarise(Calories_d_AprMay, min(ActivityTime), max(ActivityTime)) 
summarise(Intensities_d_AprMay, min(ActivityTime), max(ActivityTime)) 
summarise(Sleep_d_AprMay, min(ActivityTime), max(ActivityTime)) 
summarise(Steps_d_AprMay, min(ActivityTime), max(ActivityTime)) 
summarise(Calories_h_AprMay, min(ActivityTime), max(ActivityTime))
summarise(Intensities_h_AprMay, min(ActivityTime), max(ActivityTime))
summarise(Steps_h_AprMay, min(ActivityTime), max(ActivityTime))
summarise(Calories_m_AprMay, min(ActivityTime), max(ActivityTime))
summarise(Intensities_m_AprMay, min(ActivityTime), max(ActivityTime))
summarise(MET_m_AprMay, min(ActivityTime), max(ActivityTime))
summarise(Sleep_m_AprMay, min(ActivityTime), max(ActivityTime)) 
summarise(Steps_m_AprMay, min(ActivityTime), max(ActivityTime)) 
summarise(Weight_AprMay, min(ActivityTime), max(ActivityTime)) 
summarise(HeartRate_s_AprMay, min(ActivityTime), max(ActivityTime)) 

#'*Found one date that should not be included according to data description* 
#'*(= 2016-04-11)*

# Check and remove anomalies #

Sleep_m_AprMay %>% 
  filter(grepl("2016-04-11", ActivityTime)) 

Sleep_m_AprMay <- 
  Sleep_m_AprMay[!grepl("2016-04-11", Sleep_m_AprMay$ActivityTime),]

#### Check for NULLs or missing values in each dataset ####
#=========================================================#

Activity_d_AprMay %>% 
  subset(!complete.cases(Activity_d_AprMay))

Calories_d_AprMay %>% 
  subset(!complete.cases(Calories_d_AprMay))

Intensities_d_AprMay %>% 
  subset(!complete.cases(Intensities_d_AprMay))

Sleep_d_AprMay %>% 
  subset(!complete.cases(Sleep_d_AprMay))

Steps_d_AprMay %>% 
  subset(!complete.cases(Steps_d_AprMay))

Calories_h_AprMay %>% 
  subset(!complete.cases(Calories_h_AprMay))

Intensities_h_AprMay %>% 
  subset(!complete.cases(Intensities_h_AprMay))

Steps_h_AprMay %>% 
  subset(!complete.cases(Steps_h_AprMay))

Calories_m_AprMay %>% 
  subset(!complete.cases(Calories_m_AprMay))

Intensities_m_AprMay %>% 
  subset(!complete.cases(Intensities_m_AprMay))

MET_m_AprMay %>% 
  subset(!complete.cases(MET_m_AprMay))

Sleep_m_AprMay %>% 
  subset(!complete.cases(Sleep_m_AprMay))

Steps_m_AprMay %>% 
  subset(!complete.cases(Steps_m_AprMay))

Weight_AprMay %>% 
  subset(!complete.cases(Weight_AprMay)) 

HeartRate_s_AprMay %>% 
  subset(!complete.cases(HeartRate_s_AprMay))

# Check missing values #

count(Weight_AprMay, !complete.cases(Weight_AprMay)) 

#'*Weight includes 65 rows with NULLs; all for column 'Fat'* 
#'*Will be kept for analysis*

#### Check for duplicates in each dataset ####
#============================================#

sum(duplicated(Activity_d_AprMay)) 
sum(duplicated(Calories_d_AprMay))
sum(duplicated(Intensities_d_AprMay))
sum(duplicated(Sleep_d_AprMay))  
sum(duplicated(Steps_d_AprMay))
sum(duplicated(Calories_h_AprMay)) 
sum(duplicated(Intensities_h_AprMay)) 
sum(duplicated(Steps_h_AprMay)) 
sum(duplicated(Calories_m_AprMay)) 
sum(duplicated(Intensities_m_AprMay)) 
sum(duplicated(MET_m_AprMay)) 
sum(duplicated(Sleep_m_AprMay)) 
sum(duplicated(Steps_m_AprMay)) 
sum(duplicated(Weight_AprMay)) 
sum(duplicated(HeartRate_s_AprMay)) 

#'*3 duplicates for Sleep_d_AprMay / 543 duplicates for Sleep_m_MarApr*

# Check and remove duplicates #

Sleep_d_AprMay %>% 
  filter(duplicated(Sleep_d_AprMay)) 

Sleep_d_AprMay <- Sleep_d_AprMay[!duplicated(Sleep_d_AprMay),]

#-----------------------------------------------------------------------------#

Sleep_m_AprMay %>% 
  filter(duplicated(Sleep_m_AprMay)) 

Sleep_m_AprMay <- Sleep_m_AprMay[!duplicated(Sleep_m_AprMay),]

#### Check for 0s in each dataframe ####
#======================================#

filter_all(Activity_d_AprMay, any_vars(. ==0)) 
filter_all(Calories_d_AprMay, any_vars(. ==0))
filter_all(Intensities_d_AprMay, any_vars(. ==0))
filter_all(Sleep_d_AprMay, any_vars(. ==0))
filter_all(Steps_d_AprMay, any_vars(. ==0))
filter_all(Calories_h_AprMay, any_vars(. ==0))
filter_all(Intensities_h_AprMay, any_vars(. ==0))
filter_all(Steps_h_AprMay, any_vars(. ==0))
filter_all(Calories_m_AprMay, any_vars(. ==0))
filter_all(Intensities_m_AprMay, any_vars(. ==0))
filter_all(MET_m_AprMay, any_vars(. ==0))
filter_all(Sleep_m_AprMay, any_vars(. ==0))
filter_all(Steps_m_AprMay, any_vars(. ==0))
filter_all(Weight_AprMay, any_vars(. ==0))
filter_all(HeartRate_s_AprMay, any_vars(. ==0))

#'*Activity_d, Calories_d, Intensities_d, Steps_d, Intensities_h, Steps_h,* 
#'*Calories_m, Intensities_m, MET_m, Steps_m contain 0s*

#### Drop/add columns and replace values where necessary #### 
#===========================================================#

Intensities_d_AprMay <- 
  select(Intensities_d_AprMay, -SedentaryActiveDistance, -LightActiveDistance,
         -ModeratelyActiveDistance, -VeryActiveDistance)

#-----------------------------------------------------------------------------#

Sleep_d_AprMay <- select(Sleep_d_AprMay, -TotalSleepRecords) 

#-----------------------------------------------------------------------------#

Intensities_m_AprMay <- mutate(Intensities_m_AprMay, "Intensity_chr" = Intensity)

Intensities_m_AprMay$Intensity_chr[Intensities_m_AprMay$Intensity_chr==0] <- "Sedentary"
Intensities_m_AprMay$Intensity_chr[Intensities_m_AprMay$Intensity_chr==1] <- "Light"
Intensities_m_AprMay$Intensity_chr[Intensities_m_AprMay$Intensity_chr==2] <- "Moderate"
Intensities_m_AprMay$Intensity_chr[Intensities_m_AprMay$Intensity_chr==3] <- "VeryActive"

#-----------------------------------------------------------------------------#

MET_m_AprMay <- 
  mutate(MET_m_AprMay, METs/10) %>% 
  select(-METs) %>% 
  rename("METs" = `METs/10`)

MET_m_AprMay <- 
  MET_m_AprMay %>% 
  filter(METs >= 0.9) # filter because lowest value possible (= sleep)

#-----------------------------------------------------------------------------#

Sleep_m_AprMay <- select(Sleep_m_AprMay, -LogId)
Sleep_m_AprMay <- mutate(Sleep_m_AprMay, "SleepStage_chr" = SleepStage)

second(Sleep_m_AprMay$ActivityTime) <- 0 # to make minute dfs more consistent

Sleep_m_AprMay$SleepStage_chr[Sleep_m_AprMay$SleepStage_chr==1] <- "asleep"
Sleep_m_AprMay$SleepStage_chr[Sleep_m_AprMay$SleepStage_chr==2] <- "restless"
Sleep_m_AprMay$SleepStage_chr[Sleep_m_AprMay$SleepStage_chr==3] <- "awake"

#-----------------------------------------------------------------------------#

Weight_AprMay <- 
  Weight_AprMay %>% 
  separate(ActivityTime, into = c("ActivityTime", "Time"), sep = 10) %>% 
  select(-WeightPounds, -LogId, -Time)

Weight_AprMay$ActivityTime <- ymd(Weight_AprMay$ActivityTime)

#### Create new tables #### 
#=========================#

# Daily dataframe for MET #

MET_d_AprMay <-  
  MET_m_AprMay %>% 
  group_by(Id, "ActivityTime" = as.Date(ActivityTime)) %>%
  summarise("METs_d" = round(sum(METs))) %>% 
  mutate("Mean_METs_d" = round(METs_d/(60*24), digits = 2)) 

# Create a table for Heart Rate per minute #

HeartRate_m_AprMay <- HeartRate_s_AprMay

second(HeartRate_m_AprMay$ActivityTime) <- 0

HeartRate_m_AprMay <-
  HeartRate_m_AprMay %>% 
  group_by(Id, ActivityTime) %>% 
  summarise("Mean_HeartR" = round(mean(HeartRate), digits = 2), 
            "Max_HeartR" = max(HeartRate), 
            "Min_HeartR" = min(HeartRate)) 

## Check and preview new dataframe ##

glimpse(MET_d_AprMay)
glimpse(HeartRate_m_AprMay)

#### Limit decimal places in each table ####
#==========================================#

Activity_d_AprMay$LoggedActivitiesDistance <- 
  round(Activity_d_AprMay$LoggedActivitiesDistance, digits = 2)

Intensities_h_AprMay$AverageIntensity <- 
  round(Intensities_h_AprMay$AverageIntensity, digits = 2)

Calories_m_AprMay$Calories <- 
  round(Calories_m_AprMay$Calories, digits = 2)

#### Double check cleaned and newly created dataframes ####
#=========================================================#

glimpse(Activity_d_AprMay)
glimpse(Calories_d_AprMay)
glimpse(Intensities_d_AprMay)
glimpse(MET_d_AprMay)
glimpse(Sleep_d_AprMay)
glimpse(Steps_d_AprMay)

glimpse(Calories_h_AprMay)
glimpse(Intensities_h_AprMay)
glimpse(Steps_h_AprMay)

glimpse(Calories_m_AprMay)
glimpse(Intensities_m_AprMay)
glimpse(MET_m_AprMay)
glimpse(Sleep_m_AprMay)
glimpse(Steps_m_AprMay)

glimpse(Weight_AprMay)
glimpse(HeartRate_s_AprMay)
glimpse(HeartRate_m_AprMay)
