##correlation matrix to visualise the strength of relationships between independent variables
install.packages("corrplot")
library(corrplot)
Prem_vars_subset <- Prem_recent[, c("age", "goals_assists", "club_category", "position")]

##converting character variables to numeric for the correlation matrix
Prem_vars_subset$club_category <- as.numeric(factor(Prem_vars_subset$club_category))
Prem_vars_subset$position <- as.numeric(factor(Prem_vars_subset$position))

##computing and visualising the correlation matrix
cor_matrix <- cor(Prem_vars_subset, use = "complete.obs", method = "spearman")
corrplot(cor_matrix, method = "circle", col = colorRampPalette(c("blue", "white", "red"))(200))

##############################################################################
###multi-variate linear regression using age and position to predict log MV###
##############################################################################

library(rgl)

##creating 80/20 training to test split
Multi_train <- Prem_recent[1:426,]
Multi_test <- Prem_recent[427:533,]

##checking for col linearity. Age and position
ggplot(Multi_train, aes(x = factor(age), fill = position)) +
  geom_bar() +
  labs(title = "Stacked Bar Chart of Age by Position",
    x = "Age", y = "Frequency", fill = "Position")
##testing for median age by each position
kruskal.test(age ~ position, data = Multi_train) ##P < 0.05, Chi squared = 16

##plot
ggplot(Multi_train,
       aes(x=age, y=log_market_value_eur, colour=position)) +
  geom_point()

##creating model
mod_age_pos <- lm(formula = log_market_value_eur ~ age+position, data=Multi_train)
summary(mod_age_pos) ##P < 0.05, adjusted R squared 0.22

##using model to predict
predict(mod_age_pos, newdata = Multi_test)

##making a copy of the test data
multi_test_copy <- Multi_test
multi_test_copy$predicted <- predict(mod_age_pos, newdata = multi_test_copy)
multi_test_copy$residuals <- multi_test_copy$predicted - multi_test_copy$log_market_value_eur

##getting an SSE value
sse_multi_age_pos <- sum(multi_test_copy$residuals**2)
sse_multi_age_pos ##SSE 136. Better than age alone, but worse than position alone.

##################################################################################################
#####another multi-variate linear regression but with goals/assists (all positions) and age#######
##################################################################################################
##creating training to test split of 80/20
GA_age_train <- Prem_recent[1:426,]
GA_age_train <- na.omit(GA_age_train)
GA_age_test <- Prem_recent[427:533,]
GA_age_test <- na.omit(GA_age_test)

##plotting goals and assists with age. checking for col linearity
ggplot(GA_age_train,
       aes(age, goals_assists)) +
  geom_point()
##correlation test, using spearmans as goals_assists is not normally distributed
cor.test(GA_age_train$age, GA_age_train$goals_assists, method = "spearman", exact = FALSE)#P < 0.05
                                                                                          #rho = 0.27
##plotting age, goals_Assists and log MV
plot3d(x=GA_age_train$age,
       y=GA_age_train$goals_assists,
       z=GA_age_train$log_market_value_eur)

##creating model
mod_GA_age <- lm(formula = log_market_value_eur ~ goals_assists+age,
                 data = GA_age_train)
summary(mod_GA_age) ##P < 0.05, adjusted R square 0.31

##adding regression line to 3D graph
plot3d(x=GA_age_train$age,
       y=GA_age_train$goals_assists,
       z=GA_age_train$log_market_value_eur, 
       type="s", size=2, col="blue")
GA_age_coefs <- coef(mod_GA_age)
planes3d(a=GA_age_coefs["age"],b=GA_age_coefs["goals_assists"],
         c=-1,d=GA_age_coefs["(Intercept)"], col='gray')

##using the model to predict log MV
predict(mod_GA_age, newdata = GA_age_test)

GA_age_test_copy <- GA_age_test
GA_age_test_copy$predicted <- predict(mod_GA_age, newdata = GA_age_test_copy)
GA_age_test_copy$residuals <- GA_age_test_copy$predicted - GA_age_test_copy$log_market_value_eur

##getting an SSE value
sse_GA_age <- sum(GA_age_test_copy$residuals**2)
sse_GA_age ##SSE of 89. 

###################################################################################################
#####multi_variate linear regression of position and club category (big 6 vs other) as these ######
################################two have zero col linearity#########################################
###################################################################################################
pos_club_train <- Prem_recent[1:426,]
pos_club_test <- Prem_recent[427:533,]

##plotting position and club category
install.packages("ggmosaic")
library(ggmosaic)
ggplot(pos_club_train) +
  geom_mosaic(aes(x=product(position), fill=club_category)) +
  theme_minimal()

##chi square test of independence to double check position and club_category aren't related
chisq.test(pos_club_train$position, pos_club_train$club_category)#P > 0.05 which in Chi square
                                                                 #means position is not related to
                                                                 #club category

##creating model using position and club_category to predict log MV
mod_pos_club <- lm(formula=log_market_value_eur ~ position+club_category,
                   data = pos_club_train)
summary(mod_pos_club)##P < 0.05, adjusted R square 0.27

##predicting with the model
predict(mod_pos_club, newdata = pos_club_test)

pos_club_test_copy <- pos_club_test
pos_club_test_copy$predicted <- predict(mod_pos_club, newdata = pos_club_test_copy)
pos_club_test_copy$residuals <- pos_club_test_copy$predicted - pos_club_test_copy$log_market_value_eur

##getting an SSE value
sse_pos_club <- sum(pos_club_test_copy$residuals**2)
sse_pos_club ##SSE 101


###################################################################################
############## multi-variate regression using club_category and age################
###################################################################################
club_age_train <- Prem_recent[1:426,]
club_age_test <- Prem_recent[427:533,]

##creating model
mod_club_age <- lm(formula=log_market_value_eur ~ club_category+age,
                   data = club_age_train)
summary(mod_club_age) # P < 0.05, adjusted r square = 0.24

##predciting with the model
predict(mod_club_age, newdata = club_age_test)

club_age_test_copy <- club_age_test
club_age_test_copy$predicted <- predict(mod_club_age, newdata = club_age_test_copy)
club_age_test_copy$residuals <- club_age_test_copy$predicted - club_age_test_copy$log_market_value_eur

sse_club_age <- sum(club_age_test_copy$residuals**2)
sse_club_age ##124

#########################################################################################
########### Multi-variate regression with all 4 variables################################
#########################################################################################

##creating training/test splits of 80/20
four_vars_train <- Prem_recent[1:426,]
four_vars_test <- Prem_recent[427:533,]
four_vars_test <- na.omit(four_vars_test)

##creating the model
mod_four_vars <- lm(
  formula = log_market_value_eur ~ age+position+goals_assists+club_category,
  data = four_vars_train)
summary(mod_four_vars) # p < 0.05, Adjusted R square = 0.37

##predicting with model
predict(mod_four_vars, newdata = four_vars_test)

four_vars_test_copy <- four_vars_test
four_vars_test_copy$predicted <- predict(mod_four_vars, newdata = four_vars_test_copy)
four_vars_test_copy$residuals <- four_vars_test_copy$predicted - four_vars_test_copy$log_market_value_eur

##getting an SSE value
SSE_EPL <- sum(four_vars_test_copy$residuals**2)
SSE_EPL ##SSE of 76. 



