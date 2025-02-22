setwd("C:\\Users\\hilla\\OneDrive\\Documents\\418 Sports Analytics Project")
pacman::p_load(tidyverse, magrittr, lubridate, fastDummies, DataExplorer)
df <- read.csv("STAcleanedData.csv")
df %<>% na.omit() 

punterFinal <- df %>% select(possessionTeam, specialTeamsResult, playResult, season, collegeName, displayName, snapTime, operationTime, hangTime, kickContactType) 
punterFinal$season <- as.character(punterFinal$season)

# Hang time, operation time, and snap time across all three season --------
seasonTimes <- punterFinal %>% 
  select(season, snapTime, operationTime, hangTime) %>% 
  group_by(season) %>% 
  summarize(avgSnapTime = mean(snapTime), 
            avgOperationTime = mean(operationTime),
            avgHangTime = mean(hangTime))

seasonTimes %<>% pivot_longer(cols = c(2,3,4), names_to = "Action", values_to = "Time") #to make grouped bar chart

ggplot(data = seasonTimes, aes(x=season, y=Time, fill=Action)) + 
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label=round(Time,3)), position=position_dodge(width=0.9), vjust=-0.25) +
  labs(title="Average Punting Statistics in the NFL", x="Season", y="Time",
       subtitle = "Average hang, operation, and snap time from the 2018-2020 NFL Seasons",
       fill = "Statistic") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 15),
        axis.title = element_text(color = "Black", size = 12, face = "italic"))
  


# Most efficient punting units from that year -----------------------------
# 2020 is the best year 
eff2020 <- punterFinal %>% 
  select(season, possessionTeam, snapTime, operationTime, hangTime) %>%
  filter(season == "2020") %>% 
  group_by(possessionTeam) %>% 
  summarize(avgSnapTime = mean(snapTime), 
            avgOperationTime = mean(operationTime),
            avgHangTime = mean(hangTime))

library(plotly)

eff2020 %>% 
  ggplot(aes(avgOperationTime, avgSnapTime)) +
  geom_point()+
  geom_label(label = eff2020$possessionTeam, fill="#9dd9f3")+
  labs(title="Most Efficient Punting Units in 2020", x="Average Operation Time", y="Average Snap Time") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        axis.title = element_text(color = "Black", size = 12, face = "italic")) + 
  guides(size = "none") + theme(legend.position="none") +
  theme(legend.text = element_text(colour="black", size = 8, face = "bold")) 


# Washington Punting Trends 2020 ------------------------------------------
washPuntTrends <- df %>% select(season, week, displayName, hangTime) %>%
  filter(season == "2020") %>% 
  group_by(week) %>% 
  summarize(avgHangTime = mean(hangTime))

washPuntTrends %>% 
  mutate(NFLaverage = mean(df$hangTime)) %>% 
  ggplot(aes(week,avgHangTime)) +
  geom_line() +
  geom_label(label = round(washPuntTrends$avgHangTime,3), fill="#9dd9f3")+
  labs(title="Washington Hang Time Trend", x="Week", y="Average Hang Time", 
       subtitle = "Tress Way (#5) weekly average hang time in the 2020 NFL season") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 15),
        axis.title = element_text(color = "Black", size = 12, face = "italic")) +
  geom_hline(yintercept = mean(df$hangTime), color="red") +
  geom_text(aes(0, mean(df$hangTime), label = round(mean(df$hangTime),3), vjust = -1, hjust = 1, color = "red")) +
  guides(colour=guide_legend(title="NFL Average")) + scale_colour_discrete(labels="4.341")


  






