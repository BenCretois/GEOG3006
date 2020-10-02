library(tidyverse)

openintro::evals
data <- openintro::evals

ggplot(data, aes(x=score)) + geom_histogram() + theme_bw()
ggplot(data, aes(y=score, x= bty_avg)) + geom_point() + geom_smooth(method = 'lm')
ggplot(data, aes(y=score, x= pic_outfit)) + geom_boxplot() + theme_bw ()

data %>% 
  group_by(pic_outfit) %>% 
  summarise(score_mean = mean(score))


# Q2:

ggplot(data, aes(y=score, x= bty_avg)) + geom_point() + geom_jitter()


# Q3

lm1 <- lm(score ~ bty_avg, data = data)
summary(lm1)

broom::tidy(lm1)
seq_btyavg <- seq(from = min(data$bty_avg), to = max(data$bty_avg), length.out = 463)

data$score_pred <- predict(lm1, data) 

ggplot(data, aes(x= bty_avg, y = score)) + 
  geom_point() + 
  geom_jitter() +
  geom_line(aes(y = score_pred), color = 'red')
