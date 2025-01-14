##creating training and test splits of 80/20 and then removing NA goals_assists
GA_all_train <- Prem_recent[1:426,]
GA_all_test <- Prem_recent[427:533,]
GA_all_train <- GA_all_train[!is.na(GA_all_train$goals_assists),]
GA_all_test <- GA_all_test[!is.na(GA_all_test$goals_assists),]

##sanity check
ggplot(GA_all_train,
       aes(x=log_market_value_eur, y=goals_assists)) +
  geom_point() +
  labs(x="Log market value", y="Goals & assists", title="Prem market value\nby goals/assists", 
       caption = "As of 2022/12/08")

##distribution of goals and assists for all positions is not normally distributed
ggplot(GA_all_train,
       aes(x=goals_assists)) +
  geom_histogram(binwidth = 5, colour="black", fill="lightblue")

##so use spearmans correlation
cor.test(GA_all_train$goals_assists,
         GA_all_train$log_market_value_eur, method = "spearman", exact = FALSE) ##P < 0.05, rho 0.438

##fitting a model for the regression
mod_GA <- lm(formula = log_market_value_eur~goals_assists,
             data = GA_all_train)
summary(mod_GA) ##P < 0.05, coef 0.009, R square 0.10

##adding a linear regression line
coefs_all_GA <- coef(mod_GA)
ggplot(data=GA_all_train,
       aes(x=goals_assists, y=log_market_value_eur)) +
  geom_point() +
  geom_abline(mapping=aes(
    slope=coefs_all_GA["goals_assists"],
    intercept=coefs_all_GA["(Intercept)"]), colour='red')

##create residuals
all_GA_resid <- GA_all_train
all_GA_resid$predicted <- predict(mod_GA)
all_GA_resid$residuals <- residuals(mod_GA)

##plotting residuals
ggplot(data=all_GA_resid,
       aes(x=goals_assists, y=log_market_value_eur)) +
  geom_point(size=3) +
  geom_point(size=2, aes(y=predicted), shape=1) +
  geom_segment(aes(xend=goals_assists, yend=predicted), alpha=0.9, colour='red') +
  geom_abline(mapping=aes(
    slope=coefs_all_GA["goals_assists"],
    intercept=coefs_all_GA["(Intercept)"]), colour='gray')

plot(mod_GA, which=1)  
plot(mod_GA, which=2)  
  
##making predictions with the model
predict(mod_GA,
        newdata=GA_all_test, interval="confidence")

##calculating residuals on test data to see how well model is actually doing
GA_resid_test <- GA_all_test
GA_resid_test$predicted <- predict(mod_GA, newdata = GA_resid_test)  
GA_resid_test$residuals <- GA_resid_test$predicted - GA_resid_test$log_market_value_eur  
GA_resid_test

##plot how well the model could predict
ggplot(data=GA_resid_test,
       aes(x=goals_assists, y=log_market_value_eur)) +
  geom_point(size=3) +
  geom_point(size=2, aes(y=predicted), shape=1) +
  geom_segment(aes(xend=goals_assists, yend=predicted), alpha=0.9, colour='red') +
  geom_abline(mapping=aes(
    slope=coefs_all_GA["goals_assists"],
    intercept=coefs_all_GA["(Intercept)"]), colour='gray')

##getting a sum of squared errors value
sse_GA_all <- sum(GA_resid_test$residuals**2)
sse_GA_all ##108.goals and assists can't predict log MV better when including all positions than
           ##only just attackers. Unsurprisingly this suggests that goals and assists are more
           ##important in determining a players market value if they are an attacker. 






