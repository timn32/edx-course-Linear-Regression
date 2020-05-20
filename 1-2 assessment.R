#question 7
Teams %>% 
  filter(yearID %in% 1961:2001) %>%
  mutate(AB_per_game = AB / G, R_per_game = R/G) %>%
  summarize(cor = cor(AB_per_game, R_per_game)) %>%
  pull(cor)

# question 8
Teams %>% 
  filter(yearID %in% 1961:2001) %>%
  mutate(W_per_game = W / G, E_per_game = E/G) %>%
  summarize(cor = cor(W_per_game, E_per_game)) %>%
  pull(cor)

# question 9
Teams %>% 
  filter(yearID %in% 1961:2001) %>%
  mutate(X2B_per_game = X2B / G, X3B_per_game = X3B/G) %>%
  summarize(cor = cor(X2B_per_game, X3B_per_game)) %>%
  pull(cor)