library(dslabs)
data(admissions)
admissions

head(admissions)
str(admissions)

# confounding assessment verified learners only
library(dslabs)
data("research_funding_rates")
research_funding_rates

awarded <- c("awarded", "not")
male <- c(sum(research_funding_rates$awards_men), 
         sum(research_funding_rates$applications_men)
         -sum(research_funding_rates$awards_men))
female <- c(sum(research_funding_rates$awards_women), 
           sum(research_funding_rates$applications_women)
           -sum(research_funding_rates$awards_women))

dat <- data_frame(awarded, male, female)

dat

# code from answer
two_by_two <- research_funding_rates %>% 
  select(-discipline) %>% 
  summarize_all(funs(sum)) %>%
  summarize(yes_men = awards_men, 
            no_men = applications_men - awards_men, 
            yes_women = awards_women, 
            no_women = applications_women - awards_women) %>%
  gather %>%
  separate(key, c("awarded", "gender")) %>%
  spread(gender, value)
two_by_two

# question 2
two_by_two %>%
  summarise(percent_men = .$men[2]/sum(.$men)*100, 
            percent_women = .$women[2]/sum(.$women)*100)
  
# answer codes
two_by_two %>% 
  mutate(men = round(men/sum(men)*100, 1), women = round(women/sum(women)*100, 1)) %>%
  filter(awarded == "yes") %>%
  pull(men)

two_by_two %>% 
  mutate(men = round(men/sum(men)*100, 1), women = round(women/sum(women)*100, 1)) %>%
  filter(awarded == "yes") %>%
  pull(women)

# question 3
two_by_two %>%
  select(-awarded) %>%
  do(tidy(chisq.test(.)))

# answer code
two_by_two %>% select(-awarded) %>% chisq.test() %>% tidy() %>% pull(p.value)

# question 4
dat <- research_funding_rates %>% 
  mutate(discipline = reorder(discipline, success_rates_total)) %>%
  rename(success_total = success_rates_total,
         success_men = success_rates_men,
         success_women = success_rates_women) %>%
  gather(key, value, -discipline) %>%
  separate(key, c("type", "gender")) %>%
  spread(type, value) %>%
  filter(gender != "total")
dat

dat %>%
  ggplot(aes(discipline, success, col = gender, size = applications)) + 
  geom_point()

research_funding_rates %>% 
  mutate(discipline = reorder(discipline, success_rates_total)) %>%
  rename(success_total = success_rates_total,
         success_men = success_rates_men,
         success_women = success_rates_women) %>%
  gather(key, value, -discipline) %>%
  separate(key, c("type", "gender")) %>%
  spread(type, value) %>%
  filter(gender == "total") %>%
  arrange(success)

# answer code
dat %>% 
  ggplot(aes(discipline, success, size = applications, color = gender)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_point()
  