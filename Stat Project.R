install.packages("ggplot2")
install.packages("tidyr")
install.packages("dplyr")
install.packages("RColorBrewer")
library(ggplot2)
library(tidyr)
library(dplyr)
library("RColorBrewer")

#Created variable for dataset to be included in R
nba_stats <- read.csv(file = "nba_stats.csv", stringsAsFactors = FALSE)


#FIGURE 2 Boxplot for graphing Points vs Conference
ggplot(data = nba_stats) +
  geom_boxplot(mapping =aes(x = CONFERENCE, y = PTS)) +
  labs(x = "Conference", y = "Points", title = "Points vs Conference")


#FIGURE 4 Scatter plot for graphing Points vs Minutes based on Conference
ggplot(data = nba_stats) +
  geom_point(mapping =aes(x = MIN, y = PTS, color = CONFERENCE)) +
  geom_smooth(mapping = aes(x = MIN, y = PTS), method = "lm") +
  labs(x = "Minutes", y = "Points", title = "Points vs Minutes based on Conference")


#FIGURE 3 Stacked Plot of Different Field Goals Made
nba <- read.csv(file="nba_stats.csv",
                header = TRUE, sep = ",")
dim(nba)
head(nba)
str(nba)

hist(nba$PTS, breaks = 30)

nba$PTS <- round(nba$PTS, 1)

mean(nba$PTS, na.rm = TRUE)
min(nba$PTS, na.rm = TRUE)
max(nba$PTS, na.rm = TRUE)
summary(nba$PTS, na.rm = TRUE)

Q1 <- 0.600
Q3 <- 3.975
IQR <- Q3 - Q1
IQR

# 9.0375 - this is the upper limit for outliers in the dataset
Q3 + (1.5 * IQR) 
# -4.4625 - can't have negative points so zero is the lowest
Q1 - (1.5 * IQR) 

top20 <- nba %>%
  arrange(desc(PTS))%>%
  head(., 20)
top20

# Each player is in the dataset once
table(nba$PLAYER_NAME)	

levels(top20$PLAYER_NAME)
top20$PLAYER_NAME <- factor(top20$PLAYER_NAME, 
                            levels = unique(top20$PLAYER_NAME[order(-top20$PTS)]))
levels(top20$PLAYER_NAME)


# STACKED BAR CHART#
# Each player - (top20 bottom20 for clarity)
# How many shots made?
# Each type of shots?
# Field goals FGM
# Free throws made FTM
# 3 point field goals made (FG3M)
# Number of field goals made total minus number 3 point field goals made
top20$FG2M <- top20$FGM - top20$FG3M

top20$totgoals <- top20$FTM + top20$FG2M + top20$FG3M
calcpts <- (top20$FTM*1) + (top20$FG2M*2) + (top20$FG3M*3)

# Checking that the points were calculated correctly
head(cbind(top20$PTS, calcpts))
names(top20)
top20 <- as_tibble(top20)
str(top20)
subset <- top20 %>% 
  select(PLAYER_NAME, PTS, FTM, FG2M, FG3M) %>% 
  gather(key = goaltype, value = numgoals, FTM, FG2M, FG3M) %>% 
  arrange(PLAYER_NAME)
subset

#FINAL STACKED BARPLOT
library("RColorBrewer")
colourCount <- length(unique(top20$PLAYER_NAME))
getPalette <- colorRampPalette(brewer.pal(9, "Set2"))

playerpts <- subset %>%
  group_by(PLAYER_NAME) %>%
  summarize(pts = min(PTS)) %>%
  ungroup()

stackedplot <- ggplot(data=subset, aes(x=PLAYER_NAME, y=numgoals, fill=goaltype)) +
  geom_col() +
  scale_fill_manual(values = getPalette(colourCount)) +
  ggtitle("Most Goals Scored by Goal Type") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.3, size = 16, family = "Palatino")) + 
  coord_flip()+
  scale_x_discrete(limits = rev(levels(top20$PLAYER_NAME))) +
  labs(y = "Player Goals Scored", x = NULL) +
  theme(axis.text = element_text(family ="GillSansMT", face = "bold", size = 10, color = "grey35"),
        axis.title=element_text(family ="GillSansMT", size = 12, face = "bold")) +
  theme(legend.position = "bottom")
stackedplot


#FIGURE 5 Comparing Other Statistics of Top Players
#Select the file and organize the observations as false strings
#Run the file to check
nba_stats <- read.csv(file = "nba_stats.csv", stringsAsFactors = FALSE)
str(nba_stats)
#Filter through the data set with the players above 20 points and facet their stats
ggplot(data = nba_stats) + geom_bar(mapping = aes(x = nba_stats[which(nba_stats$PTS >= "20")], 
                                                fill = categories), position = "dodge") + 
  labs(x = "Players", y = "Count", title = "Players Points and Other Statistics")

