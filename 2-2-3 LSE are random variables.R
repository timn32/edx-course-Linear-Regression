# Monte Carlo simulation
B <- 1000
N <- 50
lse <- replicate(B, {
  sample_n(galton_heights, N, replace= TRUE) %>%
  lm(son ~ father, data = .) %>%
    .$coef
})

lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,])

# Plot the distribution of beta_0 and beta_1
library(gridExtra)
p1 <- lse %>% 
  ggplot(aes(beta_0)) +
  geom_histogram(binwidth = 5, color = "black")

p2 <- lse %>%
  ggplot(aes(beta_1)) + 
  geom_histogram(binwidth = 0.1, color = "black")

grid.arrange(p1, p2, ncol = 2)

# summary statistics
sample_n(galton_heights, N, replace = TRUE) %>%
  lm(son ~ father, data = .) %>%
  summary %>%
  .$coef

lse %>% summarise(se_0 = sd(beta_0), se_1 = sd(beta_1))