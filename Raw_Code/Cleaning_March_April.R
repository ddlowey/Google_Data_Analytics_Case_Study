#### Load packages ####
#=====================#

library("tidyverse")

#### Load in all data ####
#========================#

Activity_d_MarApr <- read.csv("/Users/../mturkfitbit_export_3.12.16-4.11.16/Fitabase Data 3.12.16-4.11.16/dailyActivity_merged.csv")
Calories_h_MarApr <- read.csv("/Users/../mturkfitbit_export_3.12.16-4.11.16/Fitabase Data 3.12.16-4.11.16/hourlyCalories_merged.csv")
Intensities_h_MarApr <- read.csv("/Users/../mturkfitbit_export_3.12.16-4.11.16/Fitabase Data 3.12.16-4.11.16/hourlyIntensities_merged.csv")
Steps_h_MarApr <- read.csv("/Users/../mturkfitbit_export_3.12.16-4.11.16/Fitabase Data 3.12.16-4.11.16/hourlySteps_merged.csv")
Calories_m_MarApr <- read.csv("/Users/../mturkfitbit_export_3.12.16-4.11.16/Fitabase Data 3.12.16-4.11.16/minuteCaloriesNarrow_merged.csv")
Intensities_m_MarApr <- read.csv("/Users/../mturkfitbit_export_3.12.16-4.11.16/Fitabase Data 3.12.16-4.11.16/minuteIntensitiesNarrow_merged.csv")
MET_m_MarApr <- read.csv("/Users/../mturkfitbit_export_3.12.16-4.11.16/Fitabase Data 3.12.16-4.11.16/minuteMETsNarrow_merged.csv")
Sleep_m_MarApr <- read.csv("/Users/../mturkfitbit_export_3.12.16-4.11.16/Fitabase Data 3.12.16-4.11.16/minuteSleep_merged.csv")
Steps_m_MarApr <- read.csv("/Users/../mturkfitbit_export_3.12.16-4.11.16/Fitabase Data 3.12.16-4.11.16/minuteStepsNarrow_merged.csv")
Weight_MarApr <- read.csv("/Users/../mturkfitbit_export_3.12.16-4.11.16/Fitabase Data 3.12.16-4.11.16/weightLogInfo_merged.csv")
HeartRate_s_MarApr <- read.csv("/Users/../mturkfitbit_export_3.12.16-4.11.16/Fitabase Data 3.12.16-4.11.16/heartrate_seconds_merged.csv")

#'*Variable names consist of category + time measurement + period indicator*

#### Preview the data ####
#========================#

glimpse(Activity_d_MarApr)
glimpse(Calories_h_MarApr)
glimpse(Intensities_h_MarApr)
glimpse(Steps_h_MarApr)
glimpse(Calories_m_MarApr)
glimpse(Intensities_m_MarApr)
glimpse(MET_m_MarApr)
glimpse(Sleep_m_MarApr)
glimpse(Steps_m_MarApr)
glimpse(Weight_MarApr)
glimpse(HeartRate_s_MarApr)

#### Check timezones of data and computer and adjust if necessary ####
#====================================================================#

tz(Activity_d_MarApr$ActivityDate)
tz(Calories_h_MarApr$ActivityHour)
tz(Intensities_h_MarApr$ActivityHour)
tz(Steps_h_MarApr$ActivityHour)
tz(Calories_m_MarApr$ActivityMinute)
tz(Intensities_m_MarApr$ActivityMinute)
tz(MET_m_MarApr$ActivityMinute)
tz(Sleep_m_MarApr$date)
tz(Steps_m_MarApr$ActivityMinute)
tz(Weight_MarApr$Date)
tz(HeartRate_s_MarApr$Time)

Sys.time()
Sys.setenv(TZ = "UTC")

#### Make column naming more consistent ####
#==========================================#

# Change column names to uppercase where necessary #

Sleep_m_MarApr <- rename_with(Sleep_m_MarApr, str_to_title)
Sleep_m_MarApr <- rename(Sleep_m_MarApr, "LogId" = Logid)

# Rename date column from each dataset #

Activity_d_MarApr <- rename(Activity_d_MarApr, "ActivityTime" = ActivityDate)
Calories_h_MarApr <- rename(Calories_h_MarApr, "ActivityTime" = ActivityHour)
Intensities_h_MarApr <-  rename(Intensities_h_MarApr, "ActivityTime" = ActivityHour)
Steps_h_MarApr <- rename(Steps_h_MarApr, "ActivityTime" = ActivityHour)
Calories_m_MarApr <- rename(Calories_m_MarApr, "ActivityTime" = ActivityMinute)
Intensities_m_MarApr <- rename(Intensities_m_MarApr, "ActivityTime" = ActivityMinute)
MET_m_MarApr <- rename(MET_m_MarApr, "ActivityTime" = ActivityMinute)
Sleep_m_MarApr <- rename(Sleep_m_MarApr, "ActivityTime" = Date)
Steps_m_MarApr <- rename(Steps_m_MarApr, "ActivityTime" = ActivityMinute)
Weight_MarApr <- rename(Weight_MarApr, "ActivityTime" = Date)
HeartRate_s_MarApr <- rename(HeartRate_s_MarApr, "ActivityTime" = Time)

# Specify vague column names where necessary #

Steps_h_MarApr <- rename(Steps_h_MarApr, "TotalSteps" = StepTotal)

Sleep_m_MarApr <- rename(Sleep_m_MarApr, "SleepStage" = Value)

HeartRate_s_MarApr <- rename(HeartRate_s_MarApr, "HeartRate" =  Value)

#### Change data types ####
#=========================#

Activity_d_MarApr$Id <- as.character(Activity_d_MarApr$Id)
Activity_d_MarApr$ActivityTime <- mdy(Activity_d_MarApr$ActivityTime)

Calories_h_MarApr$Id <- as.character(Calories_h_MarApr$Id)
Calories_h_MarApr$ActivityTime <- mdy_hms(Calories_h_MarApr$ActivityTime)

Intensities_h_MarApr$Id <- as.character(Intensities_h_MarApr$Id)
Intensities_h_MarApr$ActivityTime <- mdy_hms(Intensities_h_MarApr$ActivityTime)

Steps_h_MarApr$Id <- as.character(Steps_h_MarApr$Id)
Steps_h_MarApr$ActivityTime <- mdy_hms(Steps_h_MarApr$ActivityTime)

Calories_m_MarApr$Id <- as.character(Calories_m_MarApr$Id)
Calories_m_MarApr$ActivityTime <- mdy_hms(Calories_m_MarApr$ActivityTime)

Intensities_m_MarApr$Id <- as.character(Intensities_m_MarApr$Id)
Intensities_m_MarApr$ActivityTime <- mdy_hms(Intensities_m_MarApr$ActivityTime)

MET_m_MarApr$Id <- as.character(MET_m_MarApr$Id)
MET_m_MarApr$ActivityTime <- mdy_hms(MET_m_MarApr$ActivityTime)

Sleep_m_MarApr$Id <- as.character(Sleep_m_MarApr$Id)
Sleep_m_MarApr$ActivityTime <- mdy_hms(Sleep_m_MarApr$ActivityTime)
Sleep_m_MarApr$LogId <- as.character(Sleep_m_MarApr$LogId)

Steps_m_MarApr$Id <- as.character(Steps_m_MarApr$Id)
Steps_m_MarApr$ActivityTime <- mdy_hms(Steps_m_MarApr$ActivityTime)

Weight_MarApr$Id <- as.character(Weight_MarApr$Id)
Weight_MarApr$ActivityTime <- mdy_hms(Weight_MarApr$ActivityTime)
Weight_MarApr$LogId <- as.character(Weight_MarApr$LogId)

HeartRate_s_MarApr$Id <- as.character(HeartRate_s_MarApr$Id)
HeartRate_s_MarApr$ActivityTime <- mdy_hms(HeartRate_s_MarApr$ActivityTime)

#### Check unique 'Id'-values for each dataset ####
#=================================================#

n_distinct(Activity_d_MarApr$Id)
n_distinct(Calories_h_MarApr$Id)
n_distinct(Intensities_h_MarApr$Id)
n_distinct(Steps_h_MarApr$Id)
n_distinct(Calories_m_MarApr$Id)
n_distinct(Intensities_m_MarApr$Id)
n_distinct(MET_m_MarApr$Id)
n_distinct(Sleep_m_MarApr$Id)
n_distinct(Steps_m_MarApr$Id)
n_distinct(Weight_MarApr$Id)
n_distinct(HeartRate_s_MarApr$Id)

#'*Activity (35), Sleep (23), HeartR (14), Weight (11), rest (34)*
#'*4-5 more participants than expected*

# Compare IDs to make sure they are consistent throughout each dataset #

actd_id <- print(unique(Activity_d_MarApr$Id))
calh_id <- print(unique(Calories_h_MarApr$Id))
inth_id <- print(unique(Intensities_h_MarApr$Id))
steh_id <- print(unique(Steps_h_MarApr$Id))
calm_id <- print(unique(Calories_m_MarApr$Id))
intm_id <- print(unique(Intensities_m_MarApr$Id))
metm_id <- print(unique(MET_m_MarApr$Id))
slem_id <- print(unique(Sleep_m_MarApr$Id))
stem_id <- print(unique(Steps_m_MarApr$Id))
weig_id <- print(unique(Weight_MarApr$Id))
hearts_id <- print(unique(HeartRate_s_MarApr$Id))

match(actd_id, calh_id)
match(actd_id, inth_id)
match(actd_id, steh_id)
match(actd_id, calm_id)
match(actd_id, intm_id)
match(actd_id, metm_id)
match(actd_id, slem_id) 
match(actd_id, stem_id)
match(actd_id, weig_id) 
match(actd_id, hearts_id) 

actd_id[17] # ID unique to 'Activity' table (= 4388161847)

#'*IDs are consistent throughout each dataset minus differences in participants*

#### Check time frame for each dataset ####
#=========================================#

summarise(Activity_d_MarApr, min(ActivityTime), max(ActivityTime))
summarise(Calories_h_MarApr, min(ActivityTime), max(ActivityTime))
summarise(Intensities_h_MarApr, min(ActivityTime), max(ActivityTime))
summarise(Steps_h_MarApr, min(ActivityTime), max(ActivityTime))
summarise(Calories_m_MarApr, min(ActivityTime), max(ActivityTime))
summarise(Intensities_m_MarApr, min(ActivityTime), max(ActivityTime))
summarise(MET_m_MarApr, min(ActivityTime), max(ActivityTime))
summarise(Sleep_m_MarApr, min(ActivityTime), max(ActivityTime))
summarise(Steps_m_MarApr, min(ActivityTime), max(ActivityTime)) 
summarise(Weight_MarApr, min(ActivityTime), max(ActivityTime)) 
summarise(HeartRate_s_MarApr, min(ActivityTime), max(ActivityTime))

#'*Found two dates that should not be included according to data description*
#'*'2016-03-11' and '2016-04-12'*

# Check anomalies #

Sleep_m_MarApr %>% 
  filter(grepl("2016-03-11", ActivityTime)) 

Activity_d_MarApr %>% 
  filter(grepl("2016-04-12", ActivityTime)) 

Calories_h_MarApr %>% 
  filter(grepl("2016-04-12", ActivityTime))

Intensities_h_MarApr %>% 
  filter(grepl("2016-04-12", ActivityTime))

Steps_h_MarApr %>% 
  filter(grepl("2016-04-12", ActivityTime))

Calories_m_MarApr %>% 
  filter(grepl("2016-04-12", ActivityTime))

Intensities_m_MarApr %>%
  filter(grepl("2016-04-12", ActivityTime))

Sleep_m_MarApr %>% 
  filter(grepl("2016-04-12", ActivityTime))

Steps_m_MarApr %>% 
  filter(grepl("2016-04-12", ActivityTime))

MET_m_MarApr %>% 
  filter(grepl("2016-04-12", ActivityTime))

Weight_MarApr %>% 
  filter(grepl("2016-04-12", ActivityTime))

HeartRate_s_MarApr %>% 
  filter(grepl("2016-04-12", ActivityTime))

#'*'2016-03-11' because some participants went to bed before the 12th*

# Exclude corresponding rows #

Sleep_m_MarApr <- 
  Sleep_m_MarApr[!grepl("2016-03-11", Sleep_m_MarApr$ActivityTime),]

Activity_d_MarApr <- 
  Activity_d_MarApr[!grepl("2016-04-12", Activity_d_MarApr$ActivityTime),]

Calories_h_MarApr <- 
  Calories_h_MarApr[!grepl("2016-04-12", Calories_h_MarApr$ActivityTime),]

Intensities_h_MarApr <- 
  Intensities_h_MarApr[!grepl("2016-04-12", Intensities_h_MarApr$ActivityTime),]

Steps_h_MarApr <- 
  Steps_h_MarApr[!grepl("2016-04-12", Steps_h_MarApr$ActivityTime),]

Calories_m_MarApr <- 
  Calories_m_MarApr[!grepl("2016-04-12", Calories_m_MarApr$ActivityTime),]

Intensities_m_MarApr <- 
  Intensities_m_MarApr[!grepl("2016-04-12", Intensities_m_MarApr$ActivityTime),]

MET_m_MarApr <- 
  MET_m_MarApr[!grepl("2016-04-12", MET_m_MarApr$ActivityTime),]

Sleep_m_MarApr <- 
  Sleep_m_MarApr[!grepl("2016-04-12", Sleep_m_MarApr$ActivityTime),]

Steps_m_MarApr <- 
  Steps_m_MarApr[!grepl("2016-04-12", Steps_m_MarApr$ActivityTime),]

Weight_MarApr <- 
  Weight_MarApr[!grepl("2016-04-12", Weight_MarApr$ActivityTime),]

HeartRate_s_MarApr <- 
  HeartRate_s_MarApr[!grepl("2016-04-12", HeartRate_s_MarApr$ActivityTime),]

#### Check for NULLs or missing values in each dataset ####
#=========================================================#

Activity_d_MarApr %>% 
  subset(!complete.cases(Activity_d_MarApr))

Calories_h_MarApr %>% 
  subset(!complete.cases(Calories_h_MarApr))

Intensities_h_MarApr %>% 
  subset(!complete.cases(Intensities_h_MarApr))

Steps_h_MarApr %>% 
  subset(!complete.cases(Steps_h_MarApr))

Calories_m_MarApr %>% 
  subset(!complete.cases(Calories_m_MarApr))

Intensities_m_MarApr %>% 
  subset(!complete.cases(Intensities_m_MarApr))

MET_m_MarApr %>% 
  subset(!complete.cases(MET_m_MarApr))

Sleep_m_MarApr %>% 
  subset(!complete.cases(Sleep_m_MarApr))

Steps_m_MarApr %>% 
  subset(!complete.cases(Steps_m_MarApr))

Weight_MarApr %>% 
  subset(!complete.cases(Weight_MarApr))  

HeartRate_s_MarApr %>% 
  subset(!complete.cases(HeartRate_s_MarApr))

# Check missing values #

count(Weight_MarApr, !complete.cases(Weight_MarApr)) 

#'*Weight includes 29 NULLs; all for column 'Fat'* 
#'*Will be kept for analysis*

#### Check for duplicates in each dataset ####
#============================================#

sum(duplicated(Activity_d_MarApr)) 
sum(duplicated(Calories_h_MarApr)) 
sum(duplicated(Intensities_h_MarApr)) 
sum(duplicated(Steps_h_MarApr)) 
sum(duplicated(Calories_m_MarApr)) 
sum(duplicated(Intensities_m_MarApr)) 
sum(duplicated(MET_m_MarApr)) 
sum(duplicated(Sleep_m_MarApr)) 
sum(duplicated(Steps_m_MarApr)) 
sum(duplicated(Weight_MarApr)) 
sum(duplicated(HeartRate_s_MarApr)) 

#'*525 duplicates for Sleep_m_MarApr*

# Check and remove duplicates #

Sleep_m_MarApr %>% 
  filter(duplicated(Sleep_m_MarApr)) 

Sleep_m_MarApr <- Sleep_m_MarApr[!duplicated(Sleep_m_MarApr),]

#### Check for 0s in each dataframe ####
#======================================#

filter_all(Activity_d_MarApr, any_vars(. ==0)) 
filter_all(Calories_h_MarApr, any_vars(. ==0))
filter_all(Intensities_h_MarApr, any_vars(. ==0))
filter_all(Steps_h_MarApr, any_vars(. ==0))
filter_all(Calories_m_MarApr, any_vars(. ==0))
filter_all(Intensities_m_MarApr, any_vars(. ==0))
filter_all(MET_m_MarApr, any_vars(. ==0))
filter_all(Sleep_m_MarApr, any_vars(. ==0))
filter_all(Steps_m_MarApr, any_vars(. ==0))
filter_all(Weight_MarApr, any_vars(. ==0))
filter_all(HeartRate_s_MarApr, any_vars(. ==0))

#'*Activity_d_MarApr, Intensities_h, Steps_h, Calories_m_nar,* 
#'*'Intensities_m_nar, MET_m_nar, Steps_m_nar contain 0s*

#### Drop/add columns and replace values where necessary ####
#===========================================================#

Intensities_m_MarApr <- mutate(Intensities_m_MarApr, "Intensity_chr" = Intensity)

Intensities_m_MarApr$Intensity_chr[Intensities_m_MarApr$Intensity_chr==0] <- "Sedentary"
Intensities_m_MarApr$Intensity_chr[Intensities_m_MarApr$Intensity_chr==1] <- "Light"
Intensities_m_MarApr$Intensity_chr[Intensities_m_MarApr$Intensity_chr==2] <- "Moderate"
Intensities_m_MarApr$Intensity_chr[Intensities_m_MarApr$Intensity_chr==3] <- "VeryActive"

#-----------------------------------------------------------------------------#

MET_m_MarApr <- 
  mutate(MET_m_MarApr, METs/10) %>% 
  select(-METs) %>% 
  rename("METs" = `METs/10`)

MET_m_MarApr <- 
  MET_m_MarApr %>% 
  filter(METs >= 0.9) # filter because lowest value possible (= sleep)

#-----------------------------------------------------------------------------#

Sleep_m_MarApr <- select(Sleep_m_MarApr, -LogId)
Sleep_m_MarApr <- mutate(Sleep_m_MarApr, "SleepStage_chr" = SleepStage)

second(Sleep_m_MarApr$ActivityTime) <- 0 # to make minute dfs more consistent

Sleep_m_MarApr$SleepStage_chr[Sleep_m_MarApr$SleepStage_chr==1] <- "asleep"
Sleep_m_MarApr$SleepStage_chr[Sleep_m_MarApr$SleepStage_chr==2] <- "restless"
Sleep_m_MarApr$SleepStage_chr[Sleep_m_MarApr$SleepStage_chr==3] <- "awake"

#-----------------------------------------------------------------------------#

Weight_MarApr <- 
  Weight_MarApr %>%
  separate(ActivityTime, into = c("ActivityTime", "Time"), sep = 10) %>% 
  select(-Time, -WeightPounds, -LogId)  

Weight_MarApr$ActivityTime <- ymd(Weight_MarApr$ActivityTime)

#### Create new tables ####
#==========================#

# Daily dataframes #

Calories_d_MarApr <- 
  Calories_m_MarApr %>% 
  group_by(Id, "ActivityTime" = as.Date(ActivityTime)) %>% 
  summarise("Calories" = round(sum(Calories)))

#-----------------------------------------------------------------------------#

Intensities_d_MarApr <- 
  Intensities_m_MarApr %>% 
  group_by(Id, "ActivityTime" = as.Date(ActivityTime), Intensity_chr) %>% 
  count() %>% 
  pivot_wider(names_from = Intensity_chr , values_from = n) %>% 
  rename("SedentaryMinutes" = Sedentary, "LightlyActiveMinutes" = Light, 
         "FairlyActiveMinutes" = Moderate, "VeryActiveMinutes" = VeryActive) 

Intensities_d_MarApr[is.na(Intensities_d_MarApr)] <- 0

Intensities_d_MarApr <- Intensities_d_MarApr[,c(1,2,5,3,4,6)]

#-----------------------------------------------------------------------------#

MET_d_MarApr <-  
  MET_m_MarApr %>% 
  group_by(Id, "ActivityTime" = as.Date(ActivityTime)) %>%
  summarise("METs_d" = round(sum(METs))) %>% 
  mutate("Mean_METs_d" = round(METs_d/(60*24), digits = 2)) 

#-----------------------------------------------------------------------------#

Sleep_d_MarApr <- 
  Sleep_m_MarApr %>% 
  group_by(Id, "ActivityTime" = as.Date(ActivityTime)) %>% 
  select(-SleepStage)

Sleep_d_MarApr_asleep <- 
  Sleep_d_MarApr %>% 
  filter(SleepStage_chr=="asleep") %>% 
  group_by(Id, ActivityTime) %>% 
  count(SleepStage_chr)

Sleep_d_MarApr_inBed <- 
  Sleep_d_MarApr %>% 
  group_by(Id, ActivityTime) %>% 
  count(SleepStage_chr) %>% 
  summarise(sum(n)) %>% 
  print()

Sleep_d_MarApr <- 
  left_join(Sleep_d_MarApr_asleep, Sleep_d_MarApr_inBed, 
            by = c("Id", "ActivityTime")) %>% 
  rename("TotalMinutesAsleep" = n, "TotalTimeInBed" = `sum(n)`) %>% 
  select(-SleepStage_chr)

#-----------------------------------------------------------------------------#

Steps_d_MarApr <- 
  Steps_m_MarApr %>% 
  group_by(Id, "ActivityTime" = as.Date(ActivityTime)) %>% 
  summarise("TotalSteps" = sum(Steps))

# Create a table for Heart Rate per minute #

HeartRate_m_MarApr <- HeartRate_s_MarApr

second(HeartRate_m_MarApr$ActivityTime) <- 0

HeartRate_m_MarApr <-
  HeartRate_m_MarApr %>% 
  group_by(Id, ActivityTime) %>% 
  summarise("Mean_HeartR" = round(mean(HeartRate), digits = 2), 
            "Max_HeartR" = max(HeartRate), 
            "Min_HeartR" = min(HeartRate)) 

## Check and preview new dataframes ##

glimpse(Calories_d_MarApr)
glimpse(Intensities_d_MarApr)
glimpse(MET_d_MarApr) 
glimpse(Sleep_d_MarApr)
glimpse(Steps_d_MarApr)
glimpse(HeartRate_m_MarApr)

#### Limit decimal places in each table ####
#==========================================#

Activity_d_MarApr$LoggedActivitiesDistance <- 
  round(Activity_d_MarApr$LoggedActivitiesDistance, digits = 2)

Intensities_h_MarApr$AverageIntensity <- 
  round(Intensities_h_MarApr$AverageIntensity, digits = 2)

Calories_m_MarApr$Calories <- 
  round(Calories_m_MarApr$Calories, digits = 2)

#### Double check cleaned and newly created dataframes ####
#=========================================================#

glimpse(Activity_d_MarApr)
glimpse(Calories_d_MarApr)
glimpse(Intensities_d_MarApr)
glimpse(MET_d_MarApr) 
glimpse(Sleep_d_MarApr)
glimpse(Steps_d_MarApr)

glimpse(Calories_h_MarApr)
glimpse(Intensities_h_MarApr)
glimpse(Steps_h_MarApr)

glimpse(Calories_m_MarApr)
glimpse(Intensities_m_MarApr)
glimpse(MET_m_MarApr)
glimpse(Sleep_m_MarApr)
glimpse(Steps_m_MarApr)

glimpse(Weight_MarApr)
glimpse(HeartRate_s_MarApr)
glimpse(HeartRate_m_MarApr)
