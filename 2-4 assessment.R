# question 2
pa_per_game <- Batting %>% 
  filter(yearID == 2002) %>% 
  group_by(teamID) %>%
  summarize(pa_per_game = sum(AB+BB)/max(G)) %>% 
  .$pa_per_game %>% 
  mean
pa_per_game

# question 3
teamA <- c(2,4,1,0,1)
teamB <- c(1,6,2,1,0)
sum(teamA)
sum(teamB)

# question 9A
library(Lahman)
library(broom)
library(tidyverse)
str(Teams)
head(Teams)

BB_HR_1971 <- Teams %>%
  filter(yearID == 1971) %>%
  group_by(teamID) %>%
  lm(R ~ BB + HR, data = .) %>%
  tidy()
BB_HR_1971

# question 10 
Teams %>%
  filter(yearID %in% 1961:2018) %>%
  group_by(yearID) %>%
  do(tidy(lm(R ~ BB + HR, data = .), conf.int = TRUE)) %>%
  filter(term == "BB") %>%
  ggplot(aes(yearID, y = estimate, ymin = conf.low, ymax = conf.high)) + 
  geom_point() + 
  geom_smooth(method = "lm")

# question 11
res <- Teams %>%
  filter(yearID %in% 1961:2018) %>%
  group_by(yearID) %>%
  do(tidy(lm(R ~ BB + HR, data = .))) %>%
  ungroup()

head(res)

res %>% 
  filter(term == "BB") %>%
  do(tidy(lm(estimate ~ yearID, data = .), conf.int = TRUE))
  
  
  