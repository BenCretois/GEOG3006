library(tidyverse)

openintro::evals
data <- openintro::evals

ggplot(data, aes(x=score)) + geom_histogram()
ggplot(data, aes(y=score, x= bty_avg)) + geom_point() + geom_smooth(method = 'lm')
ggplot(data, aes(y=score, x= pic_outfit)) + geom_boxplot() 

data %>% 
  group_by(pic_outfit) %>% 
  summarise(score_mean = mean(score))


# Q2:

ggplot(data, aes(y=score, x= bty_avg)) + geom_point() + geom_jitter()


# Q3

lm1 <- lm(score ~ bty_avg, data = data)
summary(lm1)
broom::tidy(lm1)

pred_df <- data.frame(score_pred = predict(lm1, data), data$bty_avg)

ggplot(data, aes(y=score, x= bty_avg)) + 
  geom_point() +
  geom_line(color = 'red', data = pred_df, aes(x=data.bty_avg, y = score_pred)) + geom_smooth(method = 'lm', se = FALSE)
