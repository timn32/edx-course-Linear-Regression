# Define female_heights, a set of mother and daughter heights sampled from GaltonFamilies, as follows:
set.seed(1989) #if you are using R 3.5 or earlier
library(HistData)
data("GaltonFamilies")

female_heights <- GaltonFamilies%>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

# question 8
m_mother <- mean(female_heights$mother)
sd_mother <- sd(female_heights$mother)
m_daughter <- mean(female_heights$daughter)
sd_daughter <- sd(female_heights$daughter)
r <- cor(female_heights$mother, female_heights$daughter) 

m_mother
sd_mother
m_daughter
sd_daughter
r

# question 9
m <- r * sd_daughter / sd_mother
b <- m_daughter - m * m_mother
m
b
delta_daughter <- m * 1
delta_daughter

# question 10
r^2*100

# question 11
E_daughter <- m_daughter + r * (60 - m_mother)/sd_mother*sd_daughter
E_daughter

