# number of fathers with height 72 or 72.5 inches
sum(galton_heights$father == 72)
sum(galton_heights$father == 72.5)

# predicted height of a son with a 72 inch tall father
conditional_avg <- galton_heights %>%
  filter(round(father) == 75) %>%
  summarise(avg = mean(son)) %>%
  pull(avg)
conditional_avg

# statify fathers heights to make a boxplot of son heights
galton_heights %>% 
  mutate(father_strata = factor(round(father))) %>%
  ggplot(aes(father_strata, son)) + 
  geom_boxplot() +
  geom_point()

# center of each boxplot
galton_heights %>%
  mutate(father = round(father)) %>%
  group_by(father) %>%
  summarise(son_conditional_avg = mean(son)) %>%
  ggplot(aes(father, son_conditional_avg)) + 
  geom_point()

# calculate valies to plot regression line on original data
mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father, galton_heights$son)
m <- r*s_y/s_x
b <- mu_y - m*mu_x
mu_x
mu_y
s_x
s_y
m
b

# add regression line to plot
galton_heights %>%
  ggplot(aes(father, son)) + 
  geom_point(alpha = 0.5) + 
  geom_abline(intercept = b, slope = m)
