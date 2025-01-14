##regression model to predict market value (MV)
nrow(Prem_recent) ##533 rows/players that played in the PL in the last recorded season (2022)

##reordering/shuffling the rows to make sure data is randomised
Prem_recent <- Prem_recent[sample(1:nrow(Prem_recent)),]

##create a training to test split of 80/20%
prem_training <- Prem_recent[1:426,]
prem_test <- Prem_recent[427:533,]

##AGE TO PREDICT LOG MV BUT ACTUALLY USING LOG MARKET VALUE COLUMN
ggplot(prem_training,
       aes(x=age, y=log_market_value_eur)) +
  geom_point() +
  labs(x="age", y="Log market value (Euros)", title= "PL log market value by age", 
       caption="As of 2022/12/08") +
  scale_y_continuous(breaks = c(12, 14, 16, 18),
                     labels = c("100K", "1M", "10M", "100M"))

##check normallity of distribution
ggplot(prem_training,
       aes(x=age)) +
  geom_histogram(binwidth = 2, colour="black", fill="lightblue")

##testing for correlation
cor.test(prem_training$age,
         prem_training$log_market_value_eur, method="spearman", exact=FALSE) ##p value < 0.05, rho of -0.30

##fitting model using age to predict log market value
model_age <- lm(formula = log_market_value_eur ~ age,
                data = prem_training)
summary(model_age) ##p < 0.05. -0.10 coef, R squared of 0.12

##drawing a regression line
coefs_age2 <- coef(model_age)
ggplot(prem_training,
       aes(x=age, y=log_market_value_eur)) +
  geom_point() +
  geom_abline(mapping=aes(
    slope=coefs_age2["age"],
    intercept=coefs_age2["(Intercept)"]),
    colour="red")

##add residuals to the graph. First calculate predicted and residual values
prem_age2_resid <- prem_training
prem_age2_resid$predicted <- predict(model_age)
prem_age2_resid$residuals <- residuals(model_age)

##plot the residuals
ggplot(prem_age2_resid,
       aes(x=age, y=log_market_value_eur)) +
  geom_point(size=3) +
  geom_point(size=2, aes(y=predicted), shape=1) +
  geom_segment(aes(xend=age, yend=predicted), alpha=0.9, colour='red') +
  geom_abline(mapping=aes(
    slope=coefs_age2["age"],
    intercept=coefs_age2["(Intercept)"]),
    colour='red')

##also plot residuals directly
plot(model_age, which = 1)
plot(model_age, which = 2)

##making predictions with our model
predict(model_age,
        newdata = prem_test)

##calculate true values for these test values
prem_age2_test <- prem_test
prem_age2_test$predicted <- predict(model_age, newdata = prem_age2_test)
prem_age2_test$residuals <- prem_age2_test$predicted - prem_age2_test$log_market_value_eur

##graph residuals like we did with the training data
ggplot(data=prem_age2_test,
       aes(x=age, y=log_market_value_eur)) +
  geom_point(size=3) +
  geom_point(size=2, aes(y=predicted), shape=1) +
  geom_segment(aes(xend=age, yend=predicted), alpha=0.9, colour='red') +
  geom_abline(mapping=aes(
    slope=coefs_age2["age"],
    intercept=coefs_age2["(Intercept)"]),
    colour='red')

##get a value
sse_age2 <- sum(prem_age2_test$residuals**2)
sse_age2 ##SSE of 134





  





