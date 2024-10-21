#### Load packages ####
#=====================#

library("tidyverse")
library("plotrix")
library("RColorBrewer")
library("treemapify")
library("pals")
library("plotly")
library("gridExtra")

#### Change timezone to UTC ####
#==============================#

Sys.setenv(TZ = "UTC")

#### Examine Daily Activity and Weight ####
#=========================================#

summary(Activity_d)
summary(Weight)

#### Examine Calories ####
#========================#

# Look at statistics #

summary(Calories_d)
summary(Calories_h)
summary(Calories_m)

# Average Calories burned per day #

## Create dataframe to group Calories into ranges ##

Grouped_Calories_d <-
  setNames(
    data.frame(
      table(
        cut(
          Calories_d$Calories, 
          breaks = c(-Inf, 1200, 1500, 2000, 2500, 3000, Inf),
          labels = c("< 1200", "1200-1500", "1500 - 2000", "2000 - 2500", 
                     "2500 - 3000", "> 3000")
        )
      )
    ), c("Range", "Frequency")
  )

## Add percentages ##

Grouped_Calories_d <-
  Grouped_Calories_d %>% 
  mutate("Percentage" = round_percent(Frequency, 2))

## Preview new dataframe ##

print(Grouped_Calories_d)

## Create plot ##

{
  par(mar=c(0,0,0,0))  
  plotrix::pie3D(Grouped_Calories_d$Frequency, 
                 border = "white", 
                 theta = 1, 
                 radius = 0.8,
                 col = hcl.colors(length(Grouped_Calories_d$Range), "PuBu"), 
                 explode = 0.15,
                 labels = paste0(Grouped_Calories_d$Percentage, " %"), 
                 labelcex = 1
  ) 
  
  title(main = "Average Calories Burned per Day", cex.main = 1.7, line = -2)
  
  legend("bottomright", 
         inset = c(-0.8,-0.25), 
         legend = paste0(Grouped_Calories_d$Range, " Cal"), 
         fill = hcl.colors(length(Grouped_Calories_d$Range), "PuBu"), 
         xpd = TRUE, 
         text.width = 1.1,
         y.intersp = 1.5
  )
}

dev.off()

# Calories burnt throughout the week #

## Prepare dataframe ##

Calories_over_Time <-
  Calories_h %>% 
  mutate("Weekday" = weekdays(ActivityTime), .after = ActivityTime)

Calories_over_Time$ActivityTime <- 
  format(Calories_over_Time$ActivityTime, format = "%H:%M:%S")

Calories_over_Time$Weekday <- 
  factor(Calories_over_Time$Weekday, 
         levels = c("Sunday", "Saturday", "Friday", "Thursday", "Wednesday", 
                    "Tuesday", "Monday"))

## Create plot ##

ggplot(Calories_over_Time, aes(x = ActivityTime, y = Weekday, fill = Calories)) +
  geom_tile(lwd = .1, colour="grey30") +
  scale_fill_viridis(option = "inferno") +
  labs(title = "Calories Throughout the Week") +
  theme(plot.title = element_text(size = 15),
        axis.text.x = element_text(angle = 90)) 

#### Examine Intensities ####
#===========================#

# Look at statistics #

summary(Intensities_d)
summary(Intensities_h)
summary(Intensities_m)

# Average Sedentary Minutes #

## Create dataframe to group SedentaryMinutes into ranges ##

Grouped_Intensities_d <-
  Intensities_d %>% 
  mutate("SedentaryActiveMinutes" = SedentaryMinutes-(8*60), 
         .after = SedentaryMinutes) %>% 
  select(1, 2, 4)

Grouped_Intensities_d <-
  setNames(
    data.frame(
      table(
        cut(
          Grouped_Intensities_d$SedentaryActiveMinutes, 
          breaks = c(-Inf, 240, 480, 660, Inf),
          labels = c("< 240", "240-480", "480-660", "> 660")
        )
      )
    ), c("Range", "Frequency")
  )  

## Add percentages ##

Grouped_Intensities_d <-
  Grouped_Intensities_d %>% 
  mutate("Percentage" = round_percent(Frequency, 2))

## Create plot ##

{
  par(mar=c(0,0,0,0))  
  plotrix::pie3D(Grouped_Intensities_d$Frequency, 
                 border = "white", 
                 theta = 1, 
                 radius = 0.8,
                 col = hcl.colors(length(Grouped_Intensities_d$Range), "PuBu"), 
                 explode = 0.15,
                 labels = paste0(Grouped_Intensities_d$Percentage, " %"), 
                 labelcex = 1
  ) 
  
  title(main = "Average Sedentary Minutes per Day", cex.main = 1.7, line = -2)
  
  legend("bottomright", 
         inset = c(-0.8,-0.275), 
         legend = paste0(Grouped_Intensities_d$Range, " Sedentary Minutes"), 
         fill = hcl.colors(length(Grouped_Intensities_d$Range), "PuBu"), 
         xpd = TRUE, 
         text.width = 1.8,
         y.intersp = 1.5
  )
  }

dev.off()

# Average proportion of intensity levels #

## Prepare dataframe ##

Comparison_Intensities <-
  Intensities_d %>% 
  pivot_longer(cols = c(3:6), names_to = "ActivityType", values_to = "Minutes") %>% 
  group_by(ActivityType) %>% 
  summarise("Minutes" = sum(Minutes)) %>% 
  mutate("Percentage" = round_percent(Minutes, 2),  
         "ActivityLevel" = "ActivityLevel")

Comparison_Intensities <- Comparison_Intensities[c(3,2,1,4),]

## Create plot ##

{
  par(mar=c(1,0,1,0))
  pie(Comparison_Intensities$Minutes,
      labels = paste0(Comparison_Intensities$Percentage, " %"),
      col = hcl.colors(length(Grouped_Intensities_d$Range), "PuBu"),
      border = "white", 
      radius = 0.7, cex = 0.75)
  
  legend("bottom",
         legend = Comparison_Intensities$ActivityType,
         fill = hcl.colors(length(Grouped_Intensities_d$Range), "PuBu"),
         ncol = 4, 
         cex = 0.75)
  
  title("Average Ratio of Intensity Levels", line = -2, cex.main = 1.75)
}

dev.off()

# Intensities throughout the week #

## Prepare dataframe ##

Intensities_over_Time <-
  Intensities_h %>% 
  mutate("Weekday" = weekdays(ActivityTime), .after = ActivityTime)

Intensities_over_Time$ActivityTime <- 
  format(Intensities_over_Time$ActivityTime, format = "%H:%M:%S")

Intensities_over_Time$Weekday <- 
  factor(Intensities_over_Time$Weekday, 
         levels = c("Sunday", "Saturday", "Friday", "Thursday", "Wednesday", 
                    "Tuesday", "Monday"))
 
## Create plot ##

ggplot(Intensities_over_Time, aes(x = ActivityTime, 
                                  y = Weekday, 
                                  fill = TotalIntensity)) +
  geom_tile(lwd = .1, colour="grey30") +
  scale_fill_viridis(option = "inferno") +
  labs(title = "Intensity Levels Throughout the Week") +
  theme(plot.title = element_text(size = 15),
        axis.text.x = element_text(angle = 90)) 

#### Examine METs ####
#====================#

# Look at statistics #

summary(MET_d)
summary(MET_m)

# METs over time #

ggplot(MET_d, aes(x = ActivityTime, y = Id, fill = METs_d)) +
  geom_tile() +
  scale_fill_viridis(option = "inferno") +
  theme_minimal() +
  labs(title = "METs over Time") +
  theme(plot.title = element_text(hjust = .5, size = 15))

# Average METs per day #
 
## Create dataframe and determine mean values for in-chart reference ##

MET_d_Averages <-
  MET_d %>% 
  group_by(Id) %>% 
  summarise("AVG_METs_d" = mean(METs_d), "AVG_METs_m" = mean(Mean_METs_d)) 

round(mean(MET_d_Averages$AVG_METs_d), digits = 2)
round(mean(MET_d_Averages$AVG_METs_m), digits = 2)

## Create plot ##

ggplot(MET_d_Averages, aes(x = Id, y = AVG_METs_d)) + 
  geom_col(fill = "turquoise3", colour="black", width = 1) +
  theme_minimal() +
  labs(title = "Average METs per day") +
  ylab("METs") +
  theme(axis.text.x = element_text(angle = 90),
        plot.margin = unit(c(.5,2,.5,.5), "cm"),
        plot.title = element_text(hjust = 0.5, size = 17.5)) +
  scale_y_continuous(breaks = c(500, 1000, 1500, 2000, 2500)) +
  geom_hline(yintercept = round(mean(MET_d_Averages$AVG_METs_d), digits = 2), 
             linewidth = 1, linetype = 2, 
             colour = "darkorange2") +
  annotate("text", 
           x = 38.1, 
           y = round(mean(MET_d_Averages$AVG_METs_d), digits = 2)+100,
           label = paste(round(mean(MET_d_Averages$AVG_METs_d), 
                               digits = 2), "/ d"), 
           colour = "darkorange2") +
  annotate("text", 
           x = 38.1, 
           y = round(mean(MET_d_Averages$AVG_METs_d), digits = 2)-100,
           label = paste(round(mean(MET_d_Averages$AVG_METs_m), 
                               digits = 2), "/ min"), 
           colour = "darkorange2") +
  annotate("rect", 
           xmin = -.5, xmax = 35.5, 
           ymin = 450, ymax = 900, 
           alpha = .5, 
           fill = "orange", colour = "darkorange3") +
  annotate("text", x = 38.1, y = 675, label = "450 - 900", colour = "darkorange2") +
  coord_cartesian(clip = "off")

# Average distribution of MET levels #

## Prepare dataframe to examine average MET proportions ##

Grouped_MET_Levels <-
  MET_m %>% 
  group_by(METs) %>% 
  count(name = "Total_Minutes") %>% 
  summarise("Total_Minutes" = sum(Total_Minutes)) %>% 
  separate(METs, 2, into=c("MET_Level", "Decimal")) %>% 
  select(-Decimal) 

Grouped_MET_Levels$MET_Level <- gsub("\\.", "", Grouped_MET_Levels$MET_Level)    # to remove the dot

Grouped_MET_Levels <-
  Grouped_MET_Levels %>% 
  group_by(MET_Level) %>% 
  summarise("Total_Minutes" = sum(Total_Minutes)) %>% 
  arrange(as.numeric(MET_Level))

Grouped_MET_Levels$MET_Level <-
  factor(Grouped_MET_Levels$MET_Level, 
         levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11",
                    "12", "13", "14", "15", "18"))

## Create dataframe for percentages ##

Grouped_MET_Levels_relperc <-
  Grouped_MET_Levels %>% 
  mutate("Percentage" = round_percent(Total_Minutes, 2))

## Create plot ##

ggplot(Grouped_MET_Levels, 
       mapping = aes(area = Total_Minutes, 
                     fill = MET_Level, 
                     label = MET_Level)) +
  geom_treemap() +
  scale_fill_manual(values = as.vector(stepped3(18))) +
  labs(title = "Average MET-Levels",
       tag = "Rest: 0.16%")+
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        plot.margin = unit(c(.5,3.2,.5,.5), "cm"),
        plot.tag.position = c(1.11, 0.88),           
        plot.tag = element_text(colour = "grey", size = 14),
        legend.position = c(1.11, 0.32)) +
  geom_treemap_text(label = paste0(Grouped_MET_Levels_relperc$Percentage, "%"),
                    colour = c(rep("white", 3), rep("black", 13))) +
  guides(fill = guide_legend(ncol = 2)) 

#### Examine Sleep ####
#=====================#

# Look at statistics #

summary(Sleep_d)
summary(Sleep_m)

# Average sleep per day #

## Create dataframes to group sleep into ranges ##

Grouped_Sleep_d_Asleep <-
  setNames(
    data.frame(
      table(
        cut(
          Sleep_d$TotalMinutesAsleep, 
          breaks = c(-Inf, (7*60), (9*60), Inf),
          labels = c("< 7", "7-9", "> 9")
        )
      )
    ), c("Range", "Frequency")
  ) %>% 
  mutate("Percentage" = round_percent(Frequency, 2), "Sleep" = "Time Asleep")  

Grouped_Sleep_d_Total <-
  setNames(
    data.frame(
      table(
        cut(
          Sleep_d$TotalTimeInBed, 
          breaks = c(-Inf, (7*60), (9*60), Inf),
          labels = c("< 7", "7-9", "> 9")
        )
      )
    ), c("Range", "Frequency")
  ) %>% 
  mutate("Percentage" = round_percent(Frequency, 2), "Sleep" = "Time in Bed")  

## Bind dataframes ##

Grouped_Sleep_d <- bind_rows(Grouped_Sleep_d_Asleep, Grouped_Sleep_d_Total)

## Create plot ##

ggplot(Grouped_Sleep_d, aes(x = Sleep, y = Percentage, fill = Range)) +
  geom_bar(stat = "identity", width = 0.6, colour = "black") +
  theme_minimal(base_size = 15) +
  scale_fill_brewer(labels = paste0(Grouped_Sleep_d$Range, " h"), 
                    palette = "PuBu", 
                    direction = -1) +
  labs(title = "Average Sleep per Day") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  geom_text(aes(label = paste0(Percentage, " %")), 
            colour = "black",
            position = position_stack(vjust = .5), 
            size = 3.2) +
  geom_segment(aes(x = 1.3, y = Percentage[3]/2 , 
                   xend = 1.7, yend = Percentage[6]/2), 
               linetype=2, colour = "grey") +
  
  geom_segment(aes(x = 1.3, y = (Percentage[2]/2)+Percentage[3], 
                   xend = 1.7, yend = (Percentage[5]/2)+Percentage[6]), 
               linetype=2, colour = "grey") +
  
  geom_segment(aes(x = 1.3, y = (Percentage[1]/2)+Percentage[2]+Percentage[3], 
                   xend = 1.7, yend = (Percentage[4]/2)+Percentage[5]+Percentage[6]), 
               linetype=2, colour = "grey") 

# Average sleeping times #

## Create dataframes ##
 
Sleep_Time <- Sleep_m  

minute(Sleep_Time$ActivityTime) <- 0

Sleep_Time$ActivityTime <- format(Sleep_Time$ActivityTime, format = "%H:%M:%S")

Sleep_Time_Individual <-
  Sleep_Time %>% 
  group_by(Id, ActivityTime) %>% 
  count(SleepStage_chr, name = "SleepSUM") 

Sleep_Time_All <-
  Sleep_Time %>% 
  group_by(ActivityTime) %>% 
  count(SleepStage_chr, name = "SleepSUM")  

## Create plots ##

Sleep_Time_Individual %>% 
  filter(SleepStage_chr=="asleep") %>% 
  ggplot(aes(x = ActivityTime, y = Id, fill = SleepSUM)) +
  geom_tile(colour = "white", linetype = 1, show.legend = FALSE) +
  scale_fill_viridis(option = "viridis") +
  labs(title = "Average Individual Sleeping Times") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, colour = "grey70"),
        axis.title = element_blank(),
        axis.text = element_text(colour = "grey70"),
        axis.text.x = element_text(angle = 90),
        plot.background = element_rect(fill = "black"),
        panel.grid = element_blank(), 
        panel.background = element_rect(fill = "black"))

Sleep_Time_All %>% 
  filter(SleepStage_chr=="asleep") %>% 
  ggplot(aes(x = ActivityTime, y = SleepStage_chr, fill = SleepSUM)) +
  geom_tile(colour = "white", lwd= .3, linetype = 1, show.legend = FALSE) +
  scale_fill_viridis(option = "viridis") +
  theme_minimal() +
  labs(title = "Average Sleeping Time") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, colour = "grey70"),
        axis.title = element_blank(),
        axis.text = element_text(colour = "grey70"),
        axis.text.x = element_text(angle = 90),
        plot.background = element_rect(fill = "black"),
        panel.grid = element_blank(), 
        panel.background = element_rect(fill = "black"))

# Sleep Phase Ratio # 

## Create dataframes ##

Sleep_Time_Individual_perc <- 
  Sleep_Time_Individual %>% 
  group_by(SleepStage_chr) %>% 
  summarise("SleepSUM" = sum(SleepSUM))

Sleep_Time_Individual_perc <- 
  round_percent(Sleep_Time_Individual_perc$SleepSUM,2)

## Create plot ##  

Sleep_Time_Individual %>% 
  ggplot(aes(x = Id, y = SleepSUM, fill = SleepStage_chr)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_brewer(palette = "Pastel1", direction = -1) +
  theme_minimal() +
  labs(title = "Sleep Phase Ratio", fill = "Sleep Stage") +
  ylab("Total Sleep") +
  theme(axis.text.x = element_text(angle = 90), 
        plot.title = element_text(hjust = 0.5, size = 17.5)) +
  geom_hline(yintercept = Sleep_Time_Individual_perc/100, 
             colour = c("darkgreen", "darkblue", "darkred"),
             linewidth = .4) +
  annotate("text", 
           x = 27, y = Sleep_Time_Individual_perc/100+0.03, 
           label = paste0(c(Sleep_Time_Individual_perc[1], 
                            Sleep_Time_Individual_perc[2],
                            Sleep_Time_Individual_perc[3]), "%"), 
           colour = c("#85CF75", "#699CC8", "#F65649")) +
  coord_cartesian(clip = "off")

#### Examine Steps ####
#=====================#

# Look at statistics #

summary(Steps_d)
summary(Steps_h)
summary(Steps_m)

# Average Steps taken per day #

## Create dataframe to group Steps into ranges ##

Grouped_Steps_d <-
  setNames(
    data.frame(
      table(
        cut(
          Steps_d$TotalSteps, 
          breaks = c(-Inf, 1000, 2500, 5000, 10000, Inf),
          labels = c("< 1000", "1000-2500", "2500 - 5000", "5000 - 10000", 
                     "> 10000")
        )
      )
    ), c("Range", "Frequency")
  ) %>% 
  mutate("Percentage" = round_percent(Frequency, 2), "Steps" = "Steps")          # dummy value

Grouped_Steps_d <- Grouped_Steps_d[c(5,4,3,2,1),]

## Create plot ##

Grouped_Steps_d %>% 
  ggplot(mapping = aes(x = Steps, y = Percentage, fill = Range)) +
  geom_bar(stat = "identity", width = 0.5, colour = "black") +
  theme_minimal(base_size = 15) +
  scale_fill_brewer(labels = paste0(c("< 1000", "1000-2500", "2500 - 5000", 
                                      "5000 - 10000", "> 10000"), " Steps"), 
                    palette = "PuBu", 
                    direction = -1) +
  labs(title = "Average Steps taken per Day") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_blank(),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  geom_text(aes(label = paste0(Percentage, " %")), 
            colour = c(rep("black", 4), "white"),
            position = position_stack(vjust = .5), 
            size = 3.2) 

# Steps throughout the week #

## Prepare dataframe ##

Steps_over_Time <-
  Steps_h %>% 
  mutate("Weekday" = weekdays(ActivityTime), .after = ActivityTime)

Steps_over_Time$ActivityTime <- 
  format(Steps_over_Time$ActivityTime, format = "%H:%M:%S")

Steps_over_Time$Weekday <- 
  factor(Steps_over_Time$Weekday, 
         levels = c("Sunday", "Saturday", "Friday", "Thursday", "Wednesday", 
                    "Tuesday", "Monday"))

## Create plot ##

ggplot(Steps_over_Time, aes(x = ActivityTime, y = Weekday, fill = TotalSteps)) +
  geom_tile(lwd = .1, colour="grey30") +
  scale_fill_viridis(option = "inferno") +
  labs(title = "Steps Throughout the Week") +
  theme(plot.title = element_text(size = 15),
        axis.text.x = element_text(angle = 90)) 

#### Examine Heart Rate ####
#==========================#

# Look at statistics # 

summary(HeartRate_d)
summary(HeartRate_m)

# Average, maximum and minimum heart rates #

## Create dataframes ##

Max_Min_HR_wide <-
  HeartRate_d %>% 
  group_by(Id) %>% 
  summarise("Max_HR" = max(Max_HeartRate_d), "Min_HR" = min(Min_HeartRate_d))

Max_Min_HR_long <-
  bind_rows(
    
    HeartRate_d %>% 
      group_by(Id) %>% 
      summarise("Max_Min_HR" = max(Max_HeartRate_d)),
    
    HeartRate_d %>% 
      group_by(Id) %>% 
      summarise("Max_Min_HR" = min(Min_HeartRate_d))
    
  )

## Create plot ##

p_HeartRate_interactive <-
  ggplot() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_line(data = Max_Min_HR_long, 
            aes(x = Id, y = Max_Min_HR), 
            colour = "red", 
            linewidth = 1.5, 
            alpha = .25) +
  geom_line(data = HeartRate_d, 
            aes(x = Id, y = Mean_HeartRate_d), 
            colour = "red", 
            linewidth = 2.5) +
  geom_point(data = Max_Min_HR_wide, 
             aes(x = Id, y = Max_HR), 
             colour = "red", 
             size = 2.5) +
  geom_point(data = Max_Min_HR_wide, 
             aes(x = Id, y = Min_HR), 
             colour = "red", 
             size = 2.5) +
  labs(title = "Heart Rates by Users") +
  ylab("Heart Rate") +
  theme(plot.title = element_text(hjust = .5, size = 20))  

ggplotly(p_HeartRate_interactive)

# Heart rates during sedentary time #

## Create dataframe ##

Intensities_Sedentary <- 
  Intensities_m %>% 
  filter(Intensity_chr=="Sedentary")

Sedentary_HeartRates <- inner_join(Intensities_Sedentary, HeartRate_m, 
                                   by = c("Id", "ActivityTime"))

Sedentary_HeartRates <-
  Sedentary_HeartRates %>% 
  group_by(Id) %>% 
  summarise("MeanHR" = mean(Mean_HeartR), 
            "MinHR" = min(Mean_HeartR), 
            "MaxHR" = max(Mean_HeartR))

## Create plot ##

Plot_Sedentary_HeartRates <-
  ggplot(Sedentary_HeartRates) +
  geom_point(aes(x = Id, y = MeanHR), fill = "darkseagreen2", size = 3.5, shape = 23) +
  geom_point(aes(x = Id, y = MinHR), fill = "skyblue1", size = 3.5, shape = 25) +
  geom_point(aes(x = Id, y = MaxHR), fill = "salmon", size = 3.5, shape = 24) +
  labs(title = "Minimum, Mean and Maximum Heart Rates During Sedentary Time") +
  ylab("Heart Rate") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 11, face = "bold"),
        axis.text.x = element_text(angle = 90))

ggplotly(Plot_Sedentary_HeartRates)

# Average daily heart rates over the entire period by user #

HeartRate_d %>% 
  group_by(Id, ActivityTime) %>% 
  summarise("Mean_HR" = mean(Mean_HeartRate_d)) %>% 
  ggplot(aes(x = ActivityTime, y = Mean_HR)) +
  geom_line(colour = "red") +
  facet_wrap(~Id) +
  theme_minimal() +
  theme(axis.title = element_blank())

# Time spent at different heart rates #

## Create dataframe ##

Grouped_HeartRate_m <-
  setNames(
    data.frame(
      table(
        cut(
          HeartRate_m$Mean_HeartR, 
          breaks = c(-Inf, 50, 80, 100, 150, 180, Inf),
          labels = c("< 50", "50-80", "80-100", "100-150", "150-180", "> 180")
        )
      )
    ), c("Range", "Minutes")
  ) %>% 
  mutate("Percentage" = round_percent(Minutes, 2), "HeartRate" = "HeartRate")    # dummy value

## Create plot ##

ggplot(Grouped_HeartRate_m, aes(x = Minutes, y = Range, fill = Range)) +
  geom_col(colour=c("#F0027F", "#386CB0", "#FFFF99", 
                    "#FDC086", "#BEAED4", "#7FC97F")) +
  labs(title = "Total Time Spent at Different Heart Rates") +
  xlab("Total Minutes") +
  ylab("Heart Rate") +
  scale_x_continuous(breaks = c(50000, 100000, 150000, 200000, 250000, 300000), 
                     labels = c("50000", "100000", "150000", "200000", "250000", 
                                "300000")) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 15)) +
  scale_fill_brewer(palette = "Accent", direction = -1) +
  geom_text(label = paste(Grouped_HeartRate_m$Percentage, "%"), 
            nudge_x = 25000) +
  coord_cartesian(clip = "off")

#### Examine caloric relations for Intensity, METs and Steps #### 
#===============================================================#

# Join and preview dataframe #

Calories_Intensities_MET_Steps_d <- 
  inner_join(Calories_d, Intensities_d, 
             by = c("Id", "ActivityTime"))

Calories_Intensities_MET_Steps_d <- 
  inner_join(Calories_Intensities_MET_Steps_d, Steps_d, 
             by = c("Id", "ActivityTime"))

Calories_Intensities_MET_Steps_d <- 
  inner_join(Calories_Intensities_MET_Steps_d, MET_d, 
             by = c("Id", "ActivityTime"))

Calories_Intensities_MET_Steps_d <- 
  Calories_Intensities_MET_Steps_d[,c(1,2,8,7,6,5,4,9,10,3)]

glimpse(Calories_Intensities_MET_Steps_d)

# Create plots #

## Intensities - Calories ##

### Prepare plots ###

Plot_Intensities_Calories1 <-
  ggplot(Calories_Intensities_MET_Steps_d, 
         aes(x = SedentaryMinutes, y = Calories)) +
  geom_point(colour = "plum2") +
  geom_smooth() +
  theme_minimal() 

Plot_Intensities_Calories2 <-
  ggplot(Calories_Intensities_MET_Steps_d, 
         aes(x = LightlyActiveMinutes, y = Calories)) +
  geom_point(colour = "plum2") +
  geom_smooth() +
  theme_minimal() 

Plot_Intensities_Calories3 <-
  ggplot(Calories_Intensities_MET_Steps_d, 
         aes(x = FairlyActiveMinutes, y = Calories)) +
  geom_point(colour = "plum2") +
  geom_smooth() +
  theme_minimal() 

Plot_Intensities_Calories4 <-
  ggplot(Calories_Intensities_MET_Steps_d, 
         aes(x = VeryActiveMinutes, y = Calories)) +
  geom_point(colour = "plum2") +
  geom_smooth() +
  theme_minimal() 

### Arrange and display plots ###

grid.arrange(Plot_Intensities_Calories1,Plot_Intensities_Calories2,
             Plot_Intensities_Calories3,Plot_Intensities_Calories4, 
             top = "Correlation between Intensity Levels and Calories")

## TotalSteps - Calories / METs - Calories ##

### Prepare plots ###

Plot_Steps_Calories <-
  ggplot(Calories_Intensities_MET_Steps_d, 
         aes(x = TotalSteps, y = Calories)) +
  geom_point(colour = "plum2") +
  geom_smooth() +
  theme_minimal() +
  labs(title = "Correlation between Calories and Steps") +
  xlab("Total Steps") +
  theme(plot.title = element_text(size = 11))

Plot_METs_Calories <-
  ggplot(Calories_Intensities_MET_Steps_d, 
         aes(x = Mean_METs_d, y = Calories)) +
  geom_point(colour = "plum2") +
  geom_smooth() +
  theme_minimal() +
  labs(title = "Correlation between Calories and METs") +
  xlab("Mean METs / d") +
  theme(plot.title = element_text(size = 11))

### Arrange and display plots ###

grid.arrange(Plot_Steps_Calories, Plot_METs_Calories, nrow = 1)


#### Examine Sleep relations ####
#===============================#

# Join and preview dataframe #

Calories_Intensities_MET_Sleep_Steps_d <- 
  inner_join(Calories_Intensities_MET_Steps_d, Sleep_d, 
             by = c("Id", "ActivityTime"))

glimpse(Calories_Intensities_MET_Sleep_Steps_d)

# Create plots #

## Sleep - Calories / Sleep - METs / Sleep - Steps ##

### Prepare plots ###

Plot_Calories_Sleep <-
  ggplot(Calories_Intensities_MET_Sleep_Steps_d, 
         aes(x = Calories, y = TotalMinutesAsleep)) +
  geom_point(colour = "plum2") +
  geom_smooth() +
  theme_minimal() +
  labs(title = "Calories and Minutes Asleep") +
  theme(plot.title = element_text(size = 10))

Plot_METs_Sleep <-
  ggplot(Calories_Intensities_MET_Sleep_Steps_d, 
         aes(x = METs_d, y = TotalMinutesAsleep)) +
  geom_point(colour = "plum2") +
  geom_smooth() +
  theme_minimal() +
  labs(title = "METs and Minutes Asleep") +
  xlab("METs / d") +
  theme(plot.title = element_text(size = 10))

Plot_Steps_Sleep <-
  ggplot(Calories_Intensities_MET_Sleep_Steps_d, 
         aes(x = TotalSteps, y = TotalMinutesAsleep)) +
  geom_point(colour = "plum2") +
  geom_smooth() +
  theme_minimal() +
  labs(title = "Steps and Minutes Asleep") +
  xlab("Total Steps") +
  theme(plot.title = element_text(size = 10))

### Arrange and display plots ###

grid.arrange(Plot_Calories_Sleep, Plot_METs_Sleep, Plot_Steps_Sleep, nrow = 1)

## Sleep - Intensity ##

### Prepare plots ###

Plot_SedentaryMinutes_Sleep <-
  ggplot(Calories_Intensities_MET_Sleep_Steps_d, 
         aes(x = SedentaryMinutes, y = TotalMinutesAsleep)) +
  geom_point(colour = "plum2") +
  geom_smooth() +
  theme_minimal() +
  labs(title = "Sedentary Minutes and Minutes Asleep") +
  theme(plot.title = element_text(size = 11))

Plot_VeryActiveMinutes_Sleep <-
  ggplot(Calories_Intensities_MET_Sleep_Steps_d, 
         aes(x = VeryActiveMinutes, y = TotalMinutesAsleep)) +
  geom_point(colour = "plum2") +
  geom_smooth() +
  theme_minimal() +
  labs(title = "Very Active Minutes and Minutes Asleep") +
  theme(plot.title = element_text(size = 11))

### Arrange and display plots ###

grid.arrange(Plot_SedentaryMinutes_Sleep, 
             Plot_VeryActiveMinutes_Sleep, nrow = 1)

#### Examine Heart Rate Relations ####
#====================================#

# Join and preview dataframe #

Calories_Intensities_MET_Sleep_Steps_HeartRate_d <-
  inner_join(Calories_Intensities_MET_Sleep_Steps_d, HeartRate_d, 
             by = c("Id", "ActivityTime"))

glimpse(Calories_Intensities_MET_Sleep_Steps_HeartRate_d)

## Heart Rate - Calories / Heart Rate - Sleep ##
 
### Prepare plots ###

Plot_Calories_HeartRate <-
  ggplot(Calories_Intensities_MET_Sleep_Steps_HeartRate_d, 
         aes(x = Calories, y = Mean_HeartRate_d)) +
  geom_point(colour = "plum2") +
  geom_smooth() +
  theme_minimal() +
  labs(title = "Correlation between Heart Rate and Calories") +
  ylab("Heart Rate") +
  theme(plot.title = element_text(size = 10))

Plot_Sleep_HeartRate <-
  ggplot(Calories_Intensities_MET_Sleep_Steps_HeartRate_d, 
         aes(x = TotalMinutesAsleep, y = Mean_HeartRate_d)) +
  geom_point(colour = "plum2") +
  geom_smooth() +
  theme_minimal() +
  labs(title = "Correlation between Heart Rate and Sleep") +
  ylab("Heart Rate") +
  theme(plot.title = element_text(size = 10))

### Arrange and display plots ###

grid.arrange(Plot_Calories_HeartRate, Plot_Sleep_HeartRate, nrow = 1)
