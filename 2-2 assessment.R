# question 1
beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 36))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss), col=2)

# question 3
library(Lahman)
Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(R_per_game = R/G, BB_per_game = BB/G, HR_per_game = HR/G) %>%
  lm(R_per_game ~ BB_per_game + HR_per_game, data = .) %>%
  .$coef

# question 4
B <- 1000
N <- 100
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>% 
    lm(son ~ father, data = .) %>% .$coef 
})

lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,]) 
lse

# question 6
#option 2
galton_heights %>% ggplot(aes(father, son)) +
  geom_point() +
  geom_smooth(method = "lm")

#option 3
model <- lm(son ~ father, data = galton_heights)
predictions <- predict(model, interval = c("confidence"), level = 0.95)
data <- as_tibble(predictions) %>% bind_cols(father = galton_heights$father)

ggplot(data, aes(x = father, y = fit)) +
  geom_line(color = "blue", size = 1) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + 
  geom_point(data = galton_heights, aes(x = father, y = son))

# option 4
model <- lm(son ~ father, data = galton_heights)
predictions <- predict(model)
data <- as_tibble(predictions) %>% bind_cols(father = galton_heights$father)

ggplot(data, aes(x = father, y = fit)) +
  geom_line(color = "blue", size = 1) + 
  geom_point(data = galton_heights, aes(x = father, y = son))

# question 7 and 8
set.seed(1989) #if you are using R 3.5 or earlier
library(HistData)
data("GaltonFamilies")
options(digits = 3)    # report 3 significant digits

female_heights <- GaltonFamilies %>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

coef <- female_heights %>%
  lm(mother ~ daughter, data = .) %>%
  .$coef
coef[1]

predict_mother <- coef[1] + coef[2]*female_heights$daughter[1]
predict_mother
female_heights$mother[1]

# question 9
library(Lahman)
bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)
 
bat_99_01 <- Batting %>%
  filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB + BB, 
         singles = (H - X2B - X3B - HR)/pa, 
         bb = BB/pa) %>%
  filter(pa >= 100) %>%
  group_by(playerID) %>%
  summarise(mean_singles = mean(singles), mean_bb = mean(bb))

head(bat_99_01)

bat_99_01 %>% filter(mean_singles > 0.2) %>% nrow
bat_99_01 %>% filter(mean_bb > 0.2) %>% nrow

# question 10
bat_99_01_and_02 <- inner_join(bat_02, bat_99_01)

head(bat_99_01_and_02)

bat_99_01_and_02 %>% summarise(r = cor(singles, mean_singles)) %>% pull(r)
cor(bat_99_01_and_02$singles, bat_99_01_and_02$mean_singles)

bat_99_01_and_02 %>% summarise(r = cor(bb, mean_bb)) %>% pull(r)
cor(bat_99_01_and_02$bb, bat_99_01_and_02$mean_bb)

# question 11
library(gridExtra)
p1 <- bat_99_01_and_02 %>%
  ggplot(aes(mean_singles, singles)) +
  geom_point(alpha = 0.5)

p2 <- bat_99_01_and_02 %>%
  ggplot(aes(mean_bb, bb)) +
  geom_point(alpha = 0.5)

grid.arrange(p1, p2, ncol = 2)

# question 12
bat_99_01_and_02 %>%
  lm(singles ~ mean_singles, data = .) %>%
  .$coef

bat_99_01_and_02 %>%
  lm(bb ~ mean_bb, data = .) %>%
  .$coef
