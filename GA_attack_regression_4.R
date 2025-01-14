##sanity check. Goals and assists total for attackers
ggplot(prem_training[prem_training$position == "Attack",],
       aes(x=goals_assists, y=log_market_value_eur)) +
  geom_point() +
  labs(x="Goals and assists", y="Log Market value (Euros)", 
       title="PL market value for\nattackers by goals and assists", caption="As of 2022/12/08")

##creating data frame and training and test data with only attackers
Prem_Attack <- Prem_recent[Prem_recent$position == "Attack",] ##135 rows
Attack_GA_Train <- Prem_Attack[1:108,]
Attack_GA_Test <- Prem_Attack[109:135,]

##check distribution of goals
hist(Prem_Attack$goals_assists)

##correlation using spearmans as goals_assists in attackers is not normally distributed
cor.test(Prem_Attack$goals_assists, Prem_Attack$log_market_value_eur, method = "spearman", exact = FALSE) 
##P < 0.05, rho 0.32

##fitting model using goals_assists in attackers to predict log market value
mod_attack_GA <- lm(formula = log_market_value_eur ~ goals_assists,
                    data = Attack_GA_Train)
summary(mod_attack_GA)##p < 0.05, coef of goals_assists 0.005, R squared of 0.11.

##add regression line
coefs_GA <- coef(mod_attack_GA)
ggplot(data=mod_attack_GA,
       aes(x=goals_assists, y=log_market_value_eur)) +
  geom_point() +
  geom_abline(mapping=aes(
    slope=coefs_GA["goals_assists"],
    intercept=coefs_GA["(Intercept)"]), colour="red")

##calculate residuals
GA_resid <- Attack_GA_Train
GA_resid$predicted <- predict(mod_attack_GA)
GA_resid$residuals <- residuals(mod_attack_GA)

##plotting these residuals
ggplot(data=GA_resid,
       aes(x=goals_assists, y=log_market_value_eur)) +
  geom_point(size=3) +
  geom_point(size=2, aes(y=predicted), shape=1) +
  geom_segment(aes(xend=goals_assists, yend=predicted), alpha=0.9, colour="red") +
  geom_abline(mapping=aes(
    slope=coefs_GA["goals_assists"],
    intercept=coefs_GA["(Intercept)"]), colour="gray")
##or plot residuals directly
plot(mod_attack_GA, which = 1)
plot(mod_attack_GA, which = 2)

##making predictions with the model
predict(mod_attack_GA,
        newdata = Attack_GA_Test, interval = "confidence")

##calculating the residuals for the test dataset as we know the true values
Attack_GA_Test_2 <- Attack_GA_Test
Attack_GA_Test_2$predicted <- predict(mod_attack_GA, newdata = Attack_GA_Test_2)
Attack_GA_Test_2$residuals <- Attack_GA_Test_2$predicted - Attack_GA_Test_2$log_market_value_eur

##graph to show how well the model can predict log MV based on goals and assists for attackers
ggplot(data=Attack_GA_Test_2,
       aes(x=goals_assists, y=log_market_value_eur)) +
  geom_point(size=3) +
  geom_point(size=2, aes(y=predicted), shape=1) +
  geom_segment(aes(xend=goals_assists, yend=predicted), alpha=0.9, colour="red") +
  geom_abline(mapping=aes(
    slope=coefs_GA["goals_assists"],
    intercept=coefs_GA["(Intercept)"]), colour="gray")

##getting a sum of squares errors
sse_GA <- sum(Attack_GA_Test_2$residuals**2)
sse_GA ##33. This model (goals and assists for attackers) can predict log MV way better for 
       ##attackers than age could predict log MV for all positions.





