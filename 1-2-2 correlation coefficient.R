rho <- mean(scale(x) * scale(y))

galton_heights %>% summarize(r = cor(father, son)) %>% pull(r)