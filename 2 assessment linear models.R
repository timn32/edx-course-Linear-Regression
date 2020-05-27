library(tidyverse)
library(broom)
library(Lahman)
Teams_small <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(avg_attendance = attendance/G)

head(Teams_small)

# question 1a
Teams_small %>%
  mutate(runs_per_game = R/G) %>%
  lm(avg_attendance ~ runs_per_game, data = .)

Teams_small %>%
  mutate(HR_per_game = HR/G) %>%
  lm(avg_attendance ~ HR_per_game, data = .)

# question 1b
Teams_small %>%
  lm(avg_attendance ~ W, data = .) %>%
  .$coef %>%
  .[2]

Teams_small %>%
  lm(avg_attendance ~ W, data = .) %>%
  .$coef %>%
  .[1]

# question 1c
Teams_small %>%
  lm(avg_attendance ~ yearID, data = .) %>%
  .$coef %>%
  .[2]

# question 2
Teams_small %>%
  mutate(r_per_game = R/G) %>%
  summarise(r = cor(W, r_per_game)) 


Teams_small %>%
  mutate(HR_per_game = HR/G) %>%
  summarise(r = cor(W, HR_per_game))

# question 3a
Teams_small %>%
  mutate(team_strata = factor(round(W/10))) %>%
  filter(team_strata == 8) %>%
  nrow

dat <- Teams_small %>%
  mutate(W_strata = round(W/10)) %>%
  filter(W_strata >= 5 & W_strata <= 10)

dat %>%
  mutate(r_per_game = R/G, HR_per_game = HR/G) %>%
  group_by(W_strata) %>%
  summarise(slope = cor(r_per_game, avg_attendance)*sd(avg_attendance)/sd(r_per_game)) %>%
  arrange(slope)

dat %>%
  mutate(r_per_game = R/G, HR_per_game = HR/G) %>%
  group_by(W_strata) %>%
  summarise(slope = cor(HR_per_game, avg_attendance)*sd(avg_attendance)/sd(HR_per_game)) %>%
  arrange(slope)

#quetion 3C
dat %>%
  mutate(r_per_game = R/G, HR_per_game = HR/G) %>%
  group_by(W_strata) %>%
  summarise(slope = cor(r_per_game, avg_attendance)*sd(avg_attendance)/sd(r_per_game))

dat %>%
  mutate(r_per_game = R/G, HR_per_game = HR/G) %>%
  group_by(W_strata) %>%
  summarise(slope = cor(HR_per_game, avg_attendance)*sd(avg_attendance)/sd(HR_per_game))

# question 4
dat4 <- Teams_small %>% 
  mutate(r_per_game = R/G, HR_per_game = HR/G)

dat4 %>%
  lm(avg_attendance ~ r_per_game + HR_per_game + W + yearID , data = .) %>%
  .$coef


fit <- Teams_small %>% 
  mutate(R_per_game = R/G,
         HR_per_game = HR/G) %>%
  lm(avg_attendance ~ R_per_game + HR_per_game + W + yearID, data = .)


#question 5
head(fit)
intercept <- tidy(fit) %>%
  filter(term == "(Intercept)") %>%
  pull(estimate)

rrg <- tidy(fit) %>%
  filter(term == "R_per_game") %>%
  pull(estimate)

HRg <- tidy(fit) %>%
  filter(term == "HR_per_game") %>%
  pull(estimate)

Wins <- tidy(fit) %>%
  filter(term == "W") %>%
  pull(estimate)

year_2002 <- tidy(fit) %>%
  filter(term == "yearID") %>%
  pull(estimate)

intercept + 5*rrg + 1.2*HRg + 80*Wins + 2002 * year_2002

intercept + 5*rrg + 1.2*HRg + 80*Wins + 1960 * year_2002

# question 6
predict_2002 <- Teams %>%
  mutate(R_per_game = R/G, HR_per_game = HR/G, avg_attendance = attendance/G) %>%
  filter(yearID == 2002) %>%
  select(yearID, R_per_game, HR_per_game, W, avg_attendance) %>%
  predict(fit, .)
predict_2002

Teams_2002 <- Teams %>% filter(yearID == 2002)
head(Teams_2002)
cor(Teams_2002$attendance, predict_2002)
predict