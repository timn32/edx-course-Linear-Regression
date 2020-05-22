lse %>% summarise(cor(beta_0, beta_1))

# compare cor when fathers heights are standardised to the mean
B <- 1000
N <- 50
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>%
    mutate(father = father - mean(father)) %>%
    lm(son ~ father, data = .) %>%
    .$coef
})

cor(lse[1,], lse[2,])