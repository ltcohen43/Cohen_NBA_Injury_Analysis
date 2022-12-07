library(readxl)
library(dplyr)
library(ggplot2)
library(outliers)
library(devtools)
library(cowplot)
library(dataRetrieval)
library(kableExtra)
library(knitr)
injury <- read_excel("NBA_Injuries_Since_2010.xlsx")
NBA_Stats <- read_excel("NBA_Stats.xlsx")

injury <- setNames(injury, c("player","team","injured_on", "returned", "days_missed", "injury_type"))
colnames(injury)


df <- injury %>% 
  na.omit() %>%
  mutate(division = case_when(team == 'Celtics' | team == 'Nets'| team == '76ers'| team == 'Raptors' ~ 'Atlantic',
                          team == 'Bulls' | team == 'Cavaliers' | team == 'Pistons' | team == 'Pacers' | team == 'Bucks' ~ 'Central',
                          team == 'Hawks' | team == 'Hornets' | team == 'Heat' | team == 'Magic' | team == 'Wizards' ~ 'Southeast',
                          team == 'Nuggets' | team == 'Timberwolves' | team == 'Thunder' | team == 'Blazers' | team == 'Jazz' ~ 'Northwest',
                          team == 'Warriors' | team == 'Clippers' | team == 'Lakers' | team == 'Suns' | team == 'Kings' ~ 'Pacific',
                          team == 'Mavericks' | team == 'Rockets' | team == 'Grizzlies' | team == 'Pelicans' | team == 'Spurs' ~ 'Southwest'
                          )) %>% 
  mutate(conference = case_when(division == 'Atlantic' | division == 'Central' | division == 'Southeast' ~ 'Eastern',
                                division == 'Northwest' | division == 'Pacific' | division == 'Southwest' ~ 'Western')) %>% 
  mutate(injury = case_when(injury_type == 'Protocol' | injury_type == 'Illness' ~ 'Covid',
                            injury_type == 'Sprained left ankle' | injury_type == 'Sprained right ankle'| injury_type == 'Sore left ankle' | injury_type == 'Sore right ankle' ~ 'Ankle' ,
                            injury_type == 'Sore left knee' | injury_type == 'Sore right knee' ~ 'Knee', 
                            injury_type == 'Concussion' ~ 'Head', 
                            injury_type == 'Sore lower back' ~ 'Back'))
df2 <- df %>% 
  select(conference, days_missed) 

# Descriptive Stats 
summary(df)
summary(NBA_Stats)

# Checking most common injuries
prop.table(table(injury$injury_type))
# Protocol injury is NBA health and safety protocols which relates to Covid protocols. Data set started in December 2020 which Covid started in 2020.
z <- injury %>% ggplot(aes(injury_type)) + geom_bar() + theme_bw() +
  labs(title= "Figure 2: Bar Graph of Injury Type", x="Type of Injury", 
       y= "Count") 


# Checking without Covid 

y <- df %>% 
  filter(injury != 'Covid') %>% 
  ggplot(aes(injury)) + geom_bar() + theme_bw() +
  labs(title= "Figure 3: Bar Graph of Injury without Illneses", x="Location of Injury", 
       y= "Count") 
y
# ankle injury is the most common injury


x <- age_df %>% 
  ggplot(aes(age_group)) + geom_bar() 
x

# Hypothesis 1: Are the number of days missed in the Eastern Conference more than the number of days missed in the Western Conference 
# H0: EC > WC # HA: EC = WC
t.test(data=df2, days_missed ~ conference) 
# average days missed in the Eastern conference (7.28 days) > Western (7.14 days)
# p value = 0.8334 fail to reject H0, the average number of days missed for injury for the Eastern conference is not significantly greater than the Western conference
# Since also 0 is in the CI

df3 <- df %>% 
  select(injury, days_missed)


#finding player with the most injuries listen: "Giannis Antetokounmpo" "Derrick Rose"  
find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]}
find_mode(injury$player)

colnames(NBA_Stats)[which(colnames(NBA_Stats) == 'PLAYER')] <- 'player'
main <- df %>% left_join(NBA_Stats) %>% 
  select(-'TEAM')

names(main) <- tolower(names(main))
main

missing_main <- main %>% 
  filter(is.na(age)) %>% 
  select(player) 
missing_main
missing_main <- missing_main[!duplicated(missing_main), ]

main <- na.omit(main) 

# Hypothesis 2: The older players are more likely to miss more days while being injured
# for this data set needed to combine data found from https://www.nba.com/stats/players/bio, removed missing rows due to time (2180 obs -> 1353obs)
results <-lm(main_stats$days_missed~main_stats$age)
summary(results)
anova(results)
# Results: As the age of the players increases by 1yr, the average number of days missed decreases by 0.09508 days, the p-value is 0.1874 meaning not significant enough to predict the days missed for a player
# Result after adding more players:
#As the age of the players increases by 1yr, the average number of days missed decreases by 0.10285 days
# the p-value is 0.0998 meaning at the 0.10 alpha level, age is significant and I can predict days missed from age.

boxplot(main_stats$days_missed~main_stats$age)

summary(main_stats) # min age: 19, max age: 42

# THE PROBLEM: RESIDUALS ARE EXPONTEIAL 
par(mfrow = c(2, 2))
plot(results)

# filtering out age over 35 because of typical retirement age and possible outliers from using grubbs test
main_stats1 <- main_stats %>% 
  filter(age < 35)

test <- grubbs.test(main_stats1$age)
test

# rerunning the regression, the p-value was higher (0.2712) meaning less significant
results_age1 <-lm(main_stats1$days_missed~main_stats1$age)
summary(results_age1)
anova(results_age1)
par(mfrow = c(2, 2))
plot(results_age1)

# Comparing Age Group and Injury Type
hist(main_stats$age, xlab= 'Age', main = 'Histogram of NBA Players Age') 
# make age more discrete into bins of 5
age_df <- main_stats %>% 
  mutate(
    age_group = dplyr::case_when(
      age <= 22           ~ "< 22", # age when people graduate college
      age > 22 & age <= 25 ~ "22-25",
      age > 25 & age <= 30 ~ "26-30",
      age > 30 & age <= 35 ~ "31-35",
      age > 35             ~ "> 35"
    ),
    # Convert to factor
    age_group = factor(
      age_group,
      level = c("< 22","22-25","26-30", "31-35", "> 35")
    )
  )

prop.table(table(age_df$age_group))
a <- age_df %>% ggplot(aes(age_group)) + geom_bar() + ggtitle("Distubtion of Age Group") + ylab("Count") + xlab("Age Group")
a # most common age group is 26-30 

age_df1 <- filter(age_df, injury_type != "Protocol")
age_df1 <- filter(age_df1, injury_type != "Illness")

# for each of the age groups finding the most common injuries 
age_df22 <- age_df1 %>% filter(age_group == '< 22') 
prop.table(table(age_df22$injury_type)) 
a <- age_df22 %>% ggplot(aes(injury_type)) + geom_bar()
a # most common injury for the players under 22 is sprained left ankle

age_df22_25 <- age_df1 %>% filter(age_group == '22-25') 
prop.table(table(age_df1$injury_type)) 
b <- age_df22_25 %>% ggplot(aes(injury_type)) + geom_bar()
b # most common injury for the players 22-25 is sprained right ankle

age_df26_30 <- age_df1 %>% filter(age_group == '26-30') 
prop.table(table(age_df26_30$injury_type)) 
c <- age_df26_30 %>% ggplot(aes(injury_type)) + geom_bar()
c # most common injury for the players 26-30 is sprained left ankle

age_df31_35 <- age_df1 %>% filter(age_group == '31-35') 
prop.table(table(age_df31_35$injury_type)) 
d <- age_df31_35 %>% ggplot(aes(injury_type)) + geom_bar()
d # most common injury for the players 31-35 is sprained right ankle

age_df35 <- age_df1 %>% filter(age_group == '< 35') 
prop.table(table(age_df35$injury_type)) 
e <- age_df35 %>% ggplot(aes(injury_type)) + geom_bar()
e # most common injury for the players over is sprained right ankle

# Hypothesis 3: There is a difference in the age groups with how many games the players missed
results7 <-aov(days_missed~age_group, data=age_df)
summary(results7)
TukeyHSD(results7)

boxplot(days_missed~age_group, data=age_df, ylim= c(0,60), xlab= "Age Group (in years)", 
        ylab= "Number of Days Missed from injury", main = "Box plot of Age Group vs. Days Missed")

age_df %>% 
  ggplot(aes(x=age_group, y=days_missed)) + 
  geom_boxplot(width=0.5,lwd=1.0) +
  theme_bw() +
  geom_jitter(width=0.15,size=0.8, aes(color=age_group)) +
  scale_y_continuous(limits = c(0,60)) +
  labs(title= "Figure 1: Box plot of Age Group vs. Days Missed", x="Age Group (in years)", 
       y= "Number of Days Missed from injury", colour = 'Age Group') 

# Hypothesis 4: Taller players are more likely to miss more days while being injured
results2 <-lm(main_stats$days_missed~main_stats$height_inches)
summary(results2)
anova(results2)
# Results: As the height of the players increases by 1cm, the average number of days missed decreases by 0.01105 days, the p-value is 0.907 meaning not significant enough to predict the days missed for a player


boxplot(main_stats$days_missed~main_stats$height_inches, ylim= c(0,80))

# 90 inches (7'5'') is an outlier, remove it
        
main_stats2 <- main %>% 
          filter(height_inches < 85)

#rerun regression: makes the p value higher
results2_height <-lm(main_stats2$days_missed~main_stats2$height_inches)
summary(results2_height)
anova(results2_height)
  
boxplot(main_stats2$days_missed~main_stats2$height)

# Hypothesis 5: The more games the player played, the more likely they are to get hurt
results4 <-lm(main_stats$days_missed~main_stats$gp)
summary(results4)
anova(results4)
# Results: As the number of games the player played increases by 1 game, the average number of days missed decreases by 0.008577 days
# the p-value is 0.61 meaning not significant enough to predict the days missed for a player

# Hypothesis 6: which injury has greatest difference???
results <-aov(data=df3, days_missed ~ injury)
summary(results)
TukeyHSD(results)

main %>% 
  filter(injury != 'Covid') %>% 
  ggplot(aes(x=injury, y=days_missed)) + 
  geom_boxplot(width=0.5,lwd=1.0) +
  theme_bw() +
  geom_jitter(width=0.15,size=0.8, aes(color=injury)) +
  scale_y_continuous(limits = c(0,60)) +
  labs(title= "Figure 2: Box plot of Injury vs. Days Missed", x="Injury", 
       y= "Number of Days Missed from Injury", colour = 'Injury') 

# MLR for age, games played and injury
results_both <- lm(days_missed ~ age + gp , data=main)
summary(results_both)

main2 <- main %>% 
  filter(injury != 'Covid') 
results_all <- lm(days_missed ~ age + gp + injury, data=main2)
summary(results_all)

# Hypothesis #IDK : The more days missed, the less number of games played

results_games <- lm(data=main, gp ~ days_missed)
summary(results_games)

# chi square test
# without covid table 
table<- table(age_df1$age_group, age_df1$injury)
table %>%  
  kbl(caption = "Table 1: Table of Injury vs. Age Group") %>%
  kable_paper("striped", full_width = F) %>%
  kable_styling() 

chisq.test(age_df1$age_group, age_df1$injury, correct=FALSE)

#scatter plots for days_missed with age, height and gp

main %>% 
  ggplot(aes(height_inches,days_missed)) +
  geom_point() +
  scale_y_continuous(limits = c(0,60)) + 
  geom_smooth() +
  theme_bw ()

hist(main$gp)
