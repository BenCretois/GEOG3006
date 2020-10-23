library(tidyverse)

# Open the ESS dataset
ess <- read_csv('C:/Users/benjamcr/Rproj/GEOG3006/data/ess_round9.csv')

# Internet use, how much in a typical day, in minutes
ess$netustm

# gender - 1 male, 2 female 
ess$gndr

# age
ess$agea

# social meeting with friends, relative, colleagues
ess$sclmeet


unique(ess$cntry)

data <- ess %>% 
  dplyr::filter(cntry == "NO") %>% # I filter only the observatins for Norway
  dplyr::select(internet = netustm, 
                age = agea, 
                gender = gndr, 
                social = sclmeet)  %>% # I select the variables I am interested in
  mutate(gender_recode = ifelse(gender == 1, 0, 1)) %>% 
  drop_na() # I get rid of all the missing values

ggplot(data, aes(x = internet, y = age)) + 
  geom_point() + 
  geom_smooth() + 
  geom_jitter() +
  theme_classic() +
  xlab('internet use (in minutes)')

ggplot(data, aes(x = social, y = internet)) + 
  geom_point() + 
  geom_smooth() + 
  geom_jitter() +
  theme_classic() +
  ylab('internet use (in minutes)')

ggplot(data, aes(x = gender, y = internet)) + 
  geom_point() + 
  geom_smooth() + 
  geom_jitter() +
  theme_classic() +
  ylab('internet use (in minutes)')

data %>% 
  dplyr::select(age, gender, social) %>% 
  cor(.) %>% 
  ggcorrplot(., type = "lower", lab = TRUE)
library(ggcorrplot)

data %>% 
  dplyr::select(age, gender, social) %>% 
  cor(.)


model <- lm(internet ~ age + gender_recode + social, data = data)
summary(model)

broom::tidy(model)

?predict
model2 <- lm(internet ~ age + gender_recode + social + age:gender_recode, data = data)
summary(model2)


data <- data %>% mutate(pred = predict(model))
p <- predict(model, se.fit = TRUE)
p$se.fit

cor(data$pred, data$internet)

data %>% ggplot(., aes(x = age)) +
  geom_line(aes(y = pred), col = 'darkblue', size = 1.5) +
  geom_point(aes(y = internet), alpha = .3) +
  theme_classic()

data$se <- p$se.fit

data %>% ggplot(., aes(x = age)) +
  geom_line(aes(y = pred), col = 'darkblue', size = 1) +
  geom_ribbon(aes(ymin = pred - se, ymax = pred + se)) +
  geom_point(aes(y = internet), alpha = .3) +
  theme_classic()
  