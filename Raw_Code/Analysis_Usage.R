#### Load packages ####
#=====================#

library("tidyverse")
library("plotrix")
library("viridis")
library("MESS")

#### Change timezone to UTC ####
#==============================#

Sys.setenv(TZ = "UTC")

#### Create dataframe for feature usage ####
#==========================================#

# Create daily dataframe for HeartRate #

HeartRate_d <-  
  HeartRate_m %>% 
  group_by(Id, "ActivityTime" = as.Date(ActivityTime)) %>%
  summarise("Mean_HeartRate_d" = round(mean(Mean_HeartR), digits = 2),
            "Max_HeartRate_d" = max(Max_HeartR),
            "Min_HeartRate_d" = min(Max_HeartR)) 

# Create and prepare dataframes for binding #

Usage_Distance <-
  Activity_d %>% 
  filter(TrackerDistance!=0) %>% 
  select(1,2)  %>% 
  mutate("Feature" = "Distance", .before = ActivityTime) 

Usage_LogActivity <-
  Activity_d %>% 
  filter(LoggedActivitiesDistance!=0) %>% 
  select(1,2)  %>% 
  mutate("Feature" = "LogActivity", .before = ActivityTime) 

Usage_Calories <-
  Calories_d %>% 
  select(1,2) %>% 
  mutate("Feature" = "Calories", .before = ActivityTime)

Usage_Intensities <-  
  Intensities_d %>%
  select(1,2) %>% 
  mutate("Feature" = "Intensities", .before = ActivityTime)

Usage_MET <-
  MET_d %>% 
  select(1,2) %>% 
  mutate("Feature" = "MET", .before = ActivityTime)

Usage_Sleep <-
  Sleep_d %>% 
  select(1,2) %>% 
  mutate("Feature" = "Sleep", .before = ActivityTime)

Usage_Steps <-
  Steps_d %>% 
  select(1,2) %>% 
  mutate("Feature" = "Steps", .before = ActivityTime)

Usage_Weight <-
  Weight %>% 
  select(1,2) %>% 
  mutate("Feature" = "Weight", .before = ActivityTime)

Usage_Fat <-
  Weight %>% 
  filter(!is.na(Fat)) %>% 
  select(1,2) %>% 
  mutate("Feature" = "Fat", .before = ActivityTime)

Usage_BMI <- 
  Weight %>% 
  select(1,2) %>% 
  mutate("Feature" = "BMI", .before = ActivityTime)

Usage_HeartRate <- 
  HeartRate_d %>% 
  select(1,2) %>% 
  mutate("Feature" = "HeartRate", .before = ActivityTime)

# Bind dataframes together #

Usage_All <-
  bind_rows(Usage_Distance, Usage_LogActivity, Usage_Calories, Usage_Intensities,
            Usage_MET, Usage_Sleep, Usage_Steps, Usage_Weight, Usage_Fat, 
            Usage_BMI, Usage_HeartRate)

#### Create dataframes for days tracked per feature ####
#======================================================#

# Long data #

DaysTracked_All_long <-
  Usage_All %>% 
  group_by(Id, Feature) %>% 
  count(ActivityTime) %>% 
  summarise("DaysTracked" = sum(n)) 

DaysTracked_All_long$Feature <-
  factor(DaysTracked_All_long$Feature, 
         levels = c("Fat", "LogActivity", "BMI", "Weight", "HeartRate", "Sleep",
                    "Distance", "Steps", "Intensities", "MET", "Calories"))

# Wide data #

DaysTracked_All <- 
  DaysTracked_All_long %>% 
  pivot_wider(names_from = Feature, values_from = DaysTracked)

DaysTracked_All[is.na(DaysTracked_All)] <- 0

DaysTracked_All <- DaysTracked_All[,c(1,4,12,3,6,7,8,9,10,5,2,11)]

#### Preview new dataframes ####
#==============================#

glimpse(Usage_All)
glimpse(DaysTracked_All_long)
glimpse(DaysTracked_All)

#### Create plots for feature usage ####
#======================================#

# Create single plot that is applied to each feature #

DaysTracked_plots <- 
  lapply(
    names(DaysTracked_All)[-13], \(var)
    
    ggplot(DaysTracked_All, aes(x = Id,
                                y = .data[[var]],
                                fill = .data[[var]])) + 
      geom_col(width = 0.8) +
      ylim(0, 65) +
      theme_minimal() +
      labs(title = "Days Tracked per User") +
      theme(axis.text.x = element_text(angle = 90)) +
      geom_hline(yintercept = mean(DaysTracked_All[[var]]),
                 colour = "plum2",
                 linewidth = 1) +
      
      annotate("text",                                                           # add mean value to trend line
               x = 37,                                                      
               y = mean(DaysTracked_All[[var]])+2,
               label =  round(mean(DaysTracked_All[[var]]), digits = 1),
               colour = "plum2", fontface = "bold", size = 4) +
      coord_cartesian(clip = "off") +
      
      labs(tag = sprintf("nUsers: %i",                                           # add tag for number of users
                         DaysTracked_All %>%                                     # who used feature
                           select(Id, .data[[var]]) %>%
                           filter(.data[[var]] != 0) %>%
                           nrow()
      )
      ) + 
      
      theme(plot.tag.position = "topright",                                      # adjust parameters of tag
            plot.tag = element_text(colour = "plum2", 
                                    size = 14))
  )

# View plot for each feature #

print(DaysTracked_plots[[2]])  # Distance
print(DaysTracked_plots[[3]])  # LoggedActivity
print(DaysTracked_plots[[4]])  # Calories
print(DaysTracked_plots[[5]])  # Intensities
print(DaysTracked_plots[[6]])  # MET
print(DaysTracked_plots[[7]])  # Sleep
print(DaysTracked_plots[[8]])  # Steps
print(DaysTracked_plots[[9]])  # Weight
print(DaysTracked_plots[[10]]) # Fat
print(DaysTracked_plots[[11]]) # BMI
print(DaysTracked_plots[[12]]) # HeartRate

# Combine and compare results #

## Set colour palette for "features" ##

clr_features <-
  c("red", "coral2", "orange", "yellow", "green3","aquamarine2", "turquoise3",
    "dodgerblue1", "mediumpurple", "hotpink", "plum1")

## Stacked bar graph ##

ggplot(DaysTracked_All_long, aes(Id, DaysTracked, fill = Feature)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = clr_features) +
  labs(title = "Total Days Tracked for Each Feature") +
  theme_classic() +
  theme(plot.title = element_text(size = 15),
        axis.text.x = element_text(angle = 90)) 

## Circular stacked bar graph ##

ggplot(DaysTracked_All_long, aes(x = Id, y = DaysTracked, fill = Feature)) +
  scale_fill_manual(values = clr_features) +
  geom_bar(stat = "identity", size = 5) +
  ylim(-50, 650) +
  theme_minimal() +
  coord_radial(start = 0) +
  guides(theta = guide_axis_theta(angle = 90)) +
  labs(title = "Total Days Tracked for Each Feature") +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title = element_blank(),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_blank(),
    legend.position = "right"
  ) +
  annotate("text", x = rep(11, 3), y = c(200, 400, 600), 
           label = c("200", "400", "600"), colour = "grey", size = 3)

#### Examine user engagement rate ####
#====================================#

# Calculate percentages of user engagement for each feature #

nUsers <- colSums(DaysTracked_All!=0)

Distance_perc <- as.numeric(round((100/35)*nUsers[2], digits = 2)) 
LogActivity_perc <- as.numeric(round((100/35)*nUsers[3], digits = 2))
Calories_perc <- as.numeric(round((100/35)*nUsers[4], digits = 2))
Intensities_perc <- as.numeric(round((100/35)*nUsers[5], digits = 2))
MET_perc <- as.numeric(round((100/35)*nUsers[6], digits = 2))
Sleep_perc <- as.numeric(round((100/35)*nUsers[7], digits = 2))
Steps_perc <- as.numeric(round((100/35)*nUsers[8], digits = 2))
Weight_perc <- as.numeric(round((100/35)*nUsers[9], digits = 2))
Fat_perc <- as.numeric(round((100/35)*nUsers[10], digits = 2))
BMI_perc <- as.numeric(round((100/35)*nUsers[11], digits = 2))
HeartRate_perc <- as.numeric(round((100/35)*nUsers[12], digits = 2))

# Create a dataframe consisting of features' names and their corresponding #
# engagement percentages #

UserEngagement_Features <- 
  c("Distance", "LoggedActivity", "Calories", "Intensities", "MET",
    "Sleep", "Steps", "Weight", "Fat", "BMI", "HeartRate") 

UserEngagement_Percentages <- 
  c(Distance_perc, LogActivity_perc, Calories_perc, Intensities_perc,
    MET_perc, Sleep_perc, Steps_perc, Weight_perc, Fat_perc, BMI_perc,
    HeartRate_perc)

UserEngagement <- data.frame(UserEngagement_Features, UserEngagement_Percentages)

UserEngagement <- arrange(UserEngagement, UserEngagement_Percentages)

# Create plot for user engagement per feature #

## Check and alter margin values ##

par("mar") 
default_par <- par(mar = c(5.1, 4.1, 4.1, 2.1)) # save as default

par(mar = c(0,0,0,0))

## Create circular plot ##

Plot_UserEngagement <- 
  function(x, labels, 
           colors = c("red", "coral2", "yellow", "orange", "green3","aquamarine2",
                      "turquoise3", "hotpink", "plum1", "mediumpurple", 
                      "dodgerblue1"), 
           cex.lab=0.65) {
    require(plotrix)
    plot(0, 
         xlim = c(-1.75, 1.75), 
         ylim = c(-1.75, 1.75), 
         type = "n", 
         axes = F, 
         xlab = NA, 
         ylab = NA)
    
    radii <- seq(1.025, 0.2, length.out = length(x))
    draw.circle(0, 0, radii, border = "lightgrey")
    angles <- (1/4 - x)*2*pi
    draw.arc(0, 0, radii, angles, pi/2, 
             col = colors, 
             lwd = 130/length(x), 
             lend = 2, 
             n = 100)
    ymult <- (par("usr")[4]-par("usr")[3])/
      (par("usr")[2]-par("usr")[1])*par("pin")[1]/par("pin")[2]
    text(x = -0.02, 
         y = radii*ymult, 
         labels = paste(labels," - ", x*100, "%", sep = ""), 
         pos = 2, 
         cex = cex.lab)
  }

Plot_UserEngagement(UserEngagement$UserEngagement_Percentages/100, 
                    UserEngagement$UserEngagement_Features) +
  text(0, 0.05, "User", cex = 0.57, col = "grey") +
  text(0, -0.05, "Engagement", cex = 0.57, col = "grey")

#### Compare total number of trackings for each feature ####
#==========================================================#

# Prepare dataframe #

Total_nTrackings <- 
  DaysTracked_All_long %>% 
  group_by(Feature) %>% 
  summarise("nTrackings" = sum(DaysTracked)) %>% 
  arrange(nTrackings)

# Create plot #

Total_nTrackings %>% 
  ggplot() +
  geom_col(mapping = aes(x = Feature, y = nTrackings, fill = Feature)) +
  scale_fill_manual(values = clr_features) +
  ylim(0, (35*62)) +
  labs(title = "Total Number of Trackings for Each Feature") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        legend.position = "none") +
  geom_text(aes(x = Feature, 
                y = nTrackings+100, 
                label = nTrackings))

#### Examine the percentage distribution ####
#===========================================#

# Create dataframe with percentages where smaller fractions are grouped together # 

Summarised_nTrackings <-
  Total_nTrackings %>% 
  group_by(Feature = fct_lump_min(Feature, 100, nTrackings)) %>% 
  summarise("nTrackings" = sum(nTrackings)) 

Summarised_nTrackings <-
  Summarised_nTrackings %>% 
  mutate("Percentage" = ((100/sum(Summarised_nTrackings$nTrackings))
                         *Summarised_nTrackings$nTrackings))

Summarised_nTrackings$Percentage <- 
  round(Summarised_nTrackings$Percentage, digits = 2)

Summarised_nTrackings <- arrange(Summarised_nTrackings, -nTrackings)

# Create own dataframe for the smaller fractions #

## Look up necessary values ##

Total_nTrackings %>%                              
  mutate("Percentage" = ((100/sum(Total_nTrackings$nTrackings))
                         *Total_nTrackings$nTrackings)) 

## Create dataframe and add dummy values for visualisation ##

Other_nTrackings1 <- c("Fat", "LogActivity", "BMI", "Weight", "")
Other_nTrackings2 <- c("0.04 %", "0.53 %", "0.98 %", "0.98 %", "")
Other_nTrackings3 <- c(4, 53, 98, 98, 900)

Other_nTrackings <- 
  data.frame("Feature" = Other_nTrackings1, 
             "Percentage" = Other_nTrackings2, 
             "Count" = Other_nTrackings3)

## Set colour palettes for pie charts ##

Clr_nTrackingsPie_all <-
  c("plum1", "hotpink", "mediumpurple", "dodgerblue1", "turquoise3", 
    "aquamarine2", "green3", "bisque1")

Clr_nTrackingsPie_other <- 
  c("red", "coral2", "orange", "yellow", "white")

## Create pie charts showing the total tracking distribution ##

{
  # For side-by-side view of both charts
  
  par(mfrow=c(1,2), mar = c(0,2,0,5)) 
  
  # Pie chart for total tracking distribution
  
  pie(Summarised_nTrackings$nTrackings, 
      labels = paste0(Summarised_nTrackings$Percentage, " %"), 
      col = Clr_nTrackingsPie_all, 
      border = "white", radius = 1) 
  
  legend("bottomleft", 
         legend = Summarised_nTrackings$Feature, 
         ncol = 2, 
         bty = "n", 
         fill = Clr_nTrackingsPie_all, 
         border = "white")
  
  title("Total Trackings Distribution", line = -3)
  
  # Pie chart for slice 'Other'
  
  pie(Other_nTrackings$Count,                  
      labels = Other_nTrackings$Percentage, 
      col = Clr_nTrackingsPie_other, 
      border = "white", 
      radius = 1)
  
  legend("bottomleft", 
         legend = Other_nTrackings$Feature, 
         ncol = 2, 
         bty = "n", 
         fill = Clr_nTrackingsPie_other, 
         border = "white")
  
  title("Composition of group 'Other'", line = -3)
  }

dev.off()

#### Examine how much time each feature was used over the entire period ####
#==========================================================================#

# Create and bind dataframes #

Usage_Time_Calories <-
  Calories_m %>% 
  mutate("Feature" = "Calories") %>% 
  select(-Calories)

Usage_Time_Intensities <-
  Intensities_m %>% 
  mutate("Feature" = "Intensities", .after = ActivityTime) %>% 
  select(1:3)

Usage_Time_MET <-
  MET_m %>% 
  mutate("Feature" = "MET", .after = ActivityTime) %>% 
  select(1:3)

Usage_Time_Sleep <-
  Sleep_m %>% 
  mutate("Feature" = "Sleep", .after = ActivityTime) %>% 
  select(1:3)

Usage_Time_Steps <-
  Steps_m %>% 
  mutate("Feature" = "Steps") %>% 
  select(-Steps)

Usage_Time_HeartRate <-
  HeartRate_m %>% 
  mutate("Feature" = "HeartRate", .after = ActivityTime) %>% 
  select(1:3)

#-----------------------------------------------------------------------------#

Usage_Time_All <-
  bind_rows(
    Usage_Time_Calories,
    Usage_Time_Intensities,
    Usage_Time_MET,
    Usage_Time_Sleep,
    Usage_Time_Steps,
    Usage_Time_HeartRate
  )

# Prepare dataframe for plotting #

Usage_Time_All <-
  Usage_Time_All %>% 
  group_by(Feature) %>% 
  count(ActivityTime, name = "Minutes_Used") %>% 
  summarise("Minutes_Used" = sum(Minutes_Used)) %>% 
  mutate("Total_Minutes_Observed" = 62*24*60*35) %>% 
  mutate("UsagePercentage" = round((100/Total_Minutes_Observed)*Minutes_Used, 
                                   digits = 2))

Usage_Time_All$Feature <- factor(Usage_Time_All$Feature, 
                                 levels = c("HeartRate", "Sleep", "MET", 
                                            "Calories", "Intensities", "Steps"))

Usage_Time_All <- arrange(Usage_Time_All, Feature)

# Create plot #

par(mar = c(0,0,0,0))

Plot_MinutesUsed <- 
  function(x, labels, 
           colors = c("green3", "aquamarine2", "hotpink", "plum1", 
                      "mediumpurple","dodgerblue1"), 
           cex.lab=0.65) {
    require(plotrix)
    plot(0, 
         xlim = c(-1.75, 1.75), 
         ylim = c(-1.75, 1.75), 
         type = "n", 
         axes = F, 
         xlab = NA, 
         ylab = NA)
    
    radii <- seq(.75, 0.175, length.out = length(x))
    draw.circle(0, 0, radii, border = "lightgrey")
    angles <- (1/4 - x)*2*pi
    draw.arc(0, 0, radii, angles, pi/2, 
             col = colors, 
             lwd = 100/length(x), 
             lend = 2, 
             n = 100)
    ymult <- (par("usr")[4]-par("usr")[3])/
      (par("usr")[2]-par("usr")[1])*par("pin")[1]/par("pin")[2]
    text(x = -0.02, 
         y = radii*ymult, 
         labels = paste(
           labels," - ", x*100, 
           "% -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -", sep = ""), 
         pos = 2, 
         cex = cex.lab)
  }

Plot_MinutesUsed(Usage_Time_All$UsagePercentage/100, 
                 Usage_Time_All$Feature) +
  text(0, 0.05, "Time", cex = 0.8, col = "grey") +
  text(0, -0.05, "Used", cex = 0.8, col = "grey")

title(main = "Amount of Time Each Feature was Used", line = -2)
legend("bottomright", 
       ncol = 3, 
       cex= 0.65, 
       box.lty = 3, 
       text.width = c(0.25,0.25,0.275),
       legend = c(
         "", c("", substitute(paste(italic("HeartRate"))),      # Col 1
               substitute(paste(italic("Sleep"))), 
               substitute(paste(italic("MET"))),
               substitute(paste(italic("Calories"))),
               substitute(paste(italic("Intensities"))),
               substitute(paste(italic("Steps")))),
         substitute(paste(bold("Minutes"))),                    # Col 2
         substitute(paste(bold("Used"))), 
         Usage_Time_All$Minutes_Used[1:6],
         substitute(paste(bold("Minutes"))),                    # Col 3
         substitute(paste(bold("Observed"))), 
         Usage_Time_All$Total_Minutes_Observed[1:6]
       )
)

dev.off()

#### Examine relation between tracking activity and time ####
#===========================================================#

# Tracking Activity over the entire period of time #

Usage_All %>% 
  group_by(Id, ActivityTime) %>% 
  count(ActivityTime, name = "ActivityCount") %>% 
  ggplot() +
  geom_tile(mapping = aes(x = Id, y = ActivityTime, fill = ActivityCount)) +
  labs(title = "Tracking Activity Over the Whole Period") +
  scale_fill_viridis(option = "magma") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 90))

# Activity per weekday #

Usage_All %>% 
  mutate("Weekday" = weekdays(ActivityTime), .after = ActivityTime) %>% 
  group_by(Id, Weekday) %>% 
  count(Weekday, name = "ActivityCount") %>% 
  ggplot() +
  geom_tile(mapping = aes(x = Weekday, y = Id, fill = ActivityCount)) +
  scale_fill_viridis(option = "viridis") +
  scale_x_discrete(limits = c("Monday", "Tuesday", "Wednesday", "Thursday", 
                              "Friday", "Saturday", "Sunday")) +
  labs(title = "Tracking Activity per Weekday") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_blank())

# Comparison of activity levels between weekdays #

Usage_All %>% 
  mutate("Weekday" = weekdays(ActivityTime), .after = ActivityTime) %>% 
  group_by(Weekday) %>% 
  count(name = "ActivityCount")  %>% 
  ggplot(aes(Weekday, ActivityCount)) +
  geom_bar(stat = "identity", fill = "steelblue2", width = 0.8, colour = "black") +
  geom_text(aes(label = signif(ActivityCount)), nudge_y = 50) +
  scale_x_discrete(limits = c("Monday", "Tuesday", "Wednesday", "Thursday", 
                              "Friday", "Saturday", "Sunday")) +
  labs(title = "Total Tracking Activity per Weekday") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank())

#### Examine how long the device was worn on average ####
#=======================================================#

# Prepare dataframes #

## Dataframe for time grouped ##

Grouped_TrackedMinutes <-
  Intensities_d %>% 
  mutate("TotalTrackingMinutes" = 
           SedentaryMinutes+LightlyActiveMinutes+
           FairlyActiveMinutes+VeryActiveMinutes, .before = SedentaryMinutes) %>% 
  select(1:3)

Grouped_TrackedMinutes <-
  setNames(
    data.frame(
      table(
        cut(
          Grouped_TrackedMinutes$TotalTrackingMinutes,
          breaks = c(-Inf, 300, 600, 900, 1439, 1440),
          labels = c("< 5h", "5-10h", "10-15h", "15 < 24h", "24h")
        )
      )
    ) , c("Range", "Frequency")
  ) %>% 
  mutate("Percentage" = round_percent(Frequency, 2)) 

Grouped_TrackedMinutes <- arrange(Grouped_TrackedMinutes, -Percentage) 

Grouped_TrackedMinutes <- 
  Grouped_TrackedMinutes %>% 
  mutate("Cumulative_Percentage" = cumsum(Percentage), 
         .after = Percentage) 

Grouped_TrackedMinutes$Range <- 
  factor(Grouped_TrackedMinutes$Range, levels = Grouped_TrackedMinutes$Range)

print(Grouped_TrackedMinutes)

## Dataframe for worn all day or not ##

TrackedMinutes_all_day <-
  with(Grouped_TrackedMinutes, sum(Percentage[Range!="24h"]))

TrackedMinutes_all_day <-
  data.frame("Range" = c("Not_All_Day", "All_Day"),
             "Frequency" = 
               c(
                 with(Grouped_TrackedMinutes, sum(Frequency[Range!="24h"])),
                 Grouped_TrackedMinutes$Frequency[1]
               ),
             "Percentage" = 
               c(
                 with(Grouped_TrackedMinutes, sum(Percentage[Range!="24h"])),
                 Grouped_TrackedMinutes$Percentage[1]
               )
  )

print(TrackedMinutes_all_day)

# Create plots #

ggplot(Grouped_TrackedMinutes, aes(x = Grouped_TrackedMinutes$Range)) +
  geom_bar(aes(y = Grouped_TrackedMinutes$Percentage),
           fill = "lightskyblue1", 
           stat = "identity", 
           colour = "black") +
  geom_path(aes(y = Grouped_TrackedMinutes$Cumulative_Percentage, group = 1), 
            colour = "plum1", 
            lty = 3, size = 1.2) +
  geom_point(aes(y = Grouped_TrackedMinutes$Cumulative_Percentage), 
             colour = "hotpink", 
             pch=16, size=2) +
  theme_classic() +
  labs(title = "Relative Usage of Device") +
  xlab("Hours Used") +
  ylab("Percentage") +
  theme(plot.title = element_text(hjust = 0.5, size = 17.5),
        axis.title = element_text(size = 12.5),
        axis.text.x = element_text(vjust = 0.6)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  geom_text(aes(y = Grouped_TrackedMinutes$Percentage+10, 
                label = paste0(Grouped_TrackedMinutes$Percentage, " %")))

par(default_par)
pie(TrackedMinutes_all_day$Frequency,
    labels = paste(c("Not Used the Whole Day -", "Used the Whole Day -"), 
                   paste0(TrackedMinutes_all_day$Percentage, "%")),
    col = c("lightskyblue4", "lightskyblue1"),
    border = "white",
    main = "Relative Usage of Device", 
    cex.main = 1.65, cex = 0.9,
    radius = 1)

dev.off()
