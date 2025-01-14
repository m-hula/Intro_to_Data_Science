##position regression. can position (attack, midfield, defender, goalkeeper) predict log MV
##first start with the training test split of 80/20
position_train <- Prem_recent[1:426,]
position_test <- Prem_recent[427:533,]

##sanity check
ggplot(position_train,
       aes(position, log_market_value_eur)) +
  geom_boxplot(varwidth = TRUE, fill="plum") +
  labs(x="Position", y="Log MV", title="Market value by position")

##checking for frequency of players in each position
ggplot(position_train, aes(position)) +
  geom_bar() +
  labs(x="Position", y="Number of players", title="Frequency of players by position")

##as frequency of players by position isn't normally distributed I use Kruskal Wallis test
kruskal.test(log_market_value_eur ~ position, data = position_train) ##P < 0.05, Test statistic of 57

##fitting model using position to predict log MV
mod_position <- lm(formula = log_market_value_eur ~ position,
                   data=position_train)
summary(mod_position) ##p < 0.05, adjusted R square 0.19

##looking at residuals
plot(mod_position, which = 1)
plot(mod_position, which=2)

##predicting test data
Prem_pos_test <- position_test
Prem_pos_test$predicted <- predict(mod_position, newdata=Prem_pos_test)
Prem_pos_test$residuals <- Prem_pos_test$predicted - Prem_pos_test$log_market_value_eur

##getting an SSE value
sse_positions <- sum(Prem_pos_test$residuals**2)
sse_positions ##SSE of 136. 







