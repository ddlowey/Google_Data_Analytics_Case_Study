#### Bind respective dataframes from both months together ####
#============================================================#

# Daily Dataframes #

Activity_d <- 
  bind_rows(Activity_d_MarApr, Activity_d_AprMay) %>% 
  arrange(Id, ActivityTime)

Calories_d <- 
  bind_rows(Calories_d_MarApr, Calories_d_AprMay) %>% 
  arrange(Id, ActivityTime)

Intensities_d <- 
  bind_rows(Intensities_d_MarApr, Intensities_d_AprMay) %>% 
  arrange(Id, ActivityTime)

MET_d <- 
  bind_rows(MET_d_MarApr, MET_d_AprMay) %>% 
  arrange(Id, ActivityTime)

Sleep_d <- 
  bind_rows(Sleep_d_MarApr, Sleep_d_AprMay) %>% 
  arrange(Id, ActivityTime)

Steps_d <- 
  bind_rows(Steps_d_MarApr, Steps_d_AprMay) %>% 
  arrange(Id, ActivityTime)

Weight <- 
  bind_rows(Weight_MarApr, Weight_AprMay) %>% 
  arrange(Id, ActivityTime)

# Hourly Dataframes #

Calories_h <- 
  bind_rows(Calories_h_MarApr, Calories_h_AprMay) %>% 
  arrange(Id, ActivityTime)

Intensities_h <- 
  bind_rows(Intensities_h_MarApr, Intensities_h_AprMay) %>% 
  arrange(Id, ActivityTime)

Steps_h <- 
  bind_rows(Steps_h_MarApr, Steps_h_AprMay) %>% 
  arrange(Id, ActivityTime)

# Minute Dataframes #

Calories_m <- 
  bind_rows(Calories_m_MarApr, Calories_m_AprMay) %>% 
  arrange(Id, ActivityTime)

Intensities_m <- 
  bind_rows(Intensities_m_MarApr, Intensities_m_AprMay) %>% 
  arrange(Id, ActivityTime)

MET_m <- 
  bind_rows(MET_m_MarApr, MET_m_AprMay) %>% 
  arrange(Id, ActivityTime)

Sleep_m <- 
  bind_rows(Sleep_m_MarApr, Sleep_m_AprMay) %>% 
  arrange(Id, ActivityTime)

Steps_m <- 
  bind_rows(Steps_m_MarApr, Steps_m_AprMay) %>% 
  arrange(Id, ActivityTime)

HeartRate_m <- 
  bind_rows(HeartRate_m_MarApr, HeartRate_m_AprMay) %>% 
  arrange(Id, ActivityTime)

#### Preview bound dataframes ####
#================================#

glimpse(Activity_d)
glimpse(Calories_d)
glimpse(Intensities_d)
glimpse(MET_d)
glimpse(Sleep_d)
glimpse(Steps_d)
glimpse(Weight)

glimpse(Calories_h)
glimpse(Intensities_h)
glimpse(Steps_h)

glimpse(Calories_m)
glimpse(Intensities_m)
glimpse(MET_m)
glimpse(Sleep_m)
glimpse(Steps_m)
glimpse(HeartRate_m)

#### Check each dataframe for problematic values ####
#===================================================#

f_max_min <- function(x){c(max(x), min(x))}

apply(Activity_d, 2, f_max_min) 
apply(Calories_d, 2, f_max_min) 
apply(Intensities_d, 2, f_max_min) 
apply(MET_d, 2, f_max_min) 
apply(Sleep_d, 2, f_max_min) 
apply(Steps_d, 2, f_max_min) 
apply(Weight, 2, f_max_min) 

apply(Calories_h, 2, f_max_min) 
apply(Intensities_h, 2, f_max_min) 
apply(Steps_h, 2, f_max_min) 

apply(Calories_m, 2, f_max_min) 
apply(Intensities_m, 2, f_max_min) 
apply(MET_m, 2, f_max_min) 
apply(Sleep_m, 2, f_max_min) 
apply(Steps_m, 2, f_max_min) 
apply(HeartRate_m, 2, f_max_min) 

#'*Problematic values for Sedentary Minutes, Total Steps, Calories,* 
#'*Total Intensity, Time in Bed, METs*

# Check and remove problematic cases where necessary #

## Daily dataframes ##

Activity_d %>% 
  filter(TotalSteps==0)

Activity_d <- filter(Activity_d, TotalSteps!=0)

Activity_d %>% 
  filter(SedentaryMinutes==1440)

Activity_d <- filter(Activity_d, SedentaryMinutes!=1440)

Activity_d %>% 
  filter(Calories==0)

Activity_d %>%    
  mutate("SumMinutes" = VeryActiveMinutes+FairlyActiveMinutes+
           LightlyActiveMinutes+SedentaryMinutes) %>% 
  filter(SumMinutes!=1440)

#-----------------------------------------------------------------------------#

Calories_d %>% 
  filter(Calories==0)

Calories_d <- filter(Calories_d, Calories!=0)

#-----------------------------------------------------------------------------#

Intensities_d %>% 
  filter(SedentaryMinutes==1440)

Intensities_d <- filter(Intensities_d, SedentaryMinutes!=1440)

Intensities_d %>%    
  mutate("SumMinutes" = VeryActiveMinutes+FairlyActiveMinutes+
           LightlyActiveMinutes+SedentaryMinutes) %>% 
  filter(SumMinutes!=1440)

#-----------------------------------------------------------------------------#

MET_d %>% 
  filter(Mean_METs_d < 0.9)

MET_d <-
  MET_d %>% 
  filter(Mean_METs_d >= 0.9)

#-----------------------------------------------------------------------------#

Steps_d %>% 
  filter(TotalSteps==0)

Steps_d <- filter(Steps_d, TotalSteps!=0)

## Hourly dataframes ##

Intensities_h %>% 
  group_by(Id, "Day" =  as.Date(ActivityTime)) %>% 
  filter(sum(TotalIntensity)==0)

Intensities_h <- 
  Intensities_h %>%
  group_by(Id, "Day" = as.Date(ActivityTime)) %>% 
  filter(sum(TotalIntensity)!=0) %>% 
  group_by(Id, ActivityTime) %>% 
  select(-Day)

#-----------------------------------------------------------------------------#

Steps_h %>% 
  group_by(Id, "Day" = as.Date(ActivityTime)) %>% 
  filter(sum(TotalSteps)==0)

Steps_h <- 
  Steps_h %>%
  group_by(Id, "Day" = as.Date(ActivityTime)) %>% 
  filter(sum(TotalSteps)!=0) %>%
  group_by(Id, ActivityTime) %>% 
  select(-Day)

## Minute dataframes ##

Calories_m %>% 
  filter(Calories==0)

Calories_m <- filter(Calories_m, Calories!=0)

#-----------------------------------------------------------------------------#

Intensities_m %>% 
  group_by(Id, "Day" =  as.Date(ActivityTime)) %>% 
  filter(sum(Intensity)==0)

Intensities_m <- 
  Intensities_m %>%
  group_by(Id, "Day" = as.Date(ActivityTime)) %>% 
  filter(sum(Intensity)!=0) %>% 
  group_by(Id, ActivityTime) %>% 
  select(-Day)

#-----------------------------------------------------------------------------#

MET_m %>% 
  group_by(Id, "ActivityTime" = as.Date(ActivityTime)) %>%
  mutate(MeanMET = sum(METs)/(60*24)) %>% 
  filter(MeanMET < 0.9)

MET_m <-
  MET_m %>% 
  group_by(Id, "ActivityTime" = as.Date(ActivityTime)) %>%
  mutate(MeanMET = sum(METs)/(60*24)) %>% 
  filter(MeanMET >= 0.9)

#-----------------------------------------------------------------------------#

Steps_m %>% 
  group_by(Id, "Day" = as.Date(ActivityTime)) %>% 
  filter(sum(Steps)==0)

Steps_m <- 
  Steps_m %>%
  group_by(Id, "Day" = as.Date(ActivityTime)) %>% 
  filter(sum(Steps)!=0) %>% 
  group_by(Id, ActivityTime) %>% 
  select(-Day)
