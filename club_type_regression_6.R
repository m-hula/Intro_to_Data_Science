##creating training test splits of 80/20
club_train <- Prem_recent[1:426,]
club_test <- Prem_recent[427:533,]

##sanity check
ggplot(data = club_train,
       aes(club_category, log_market_value_eur)) +
  geom_boxplot(varwidth=TRUE, fill="plum") +
  labs(x="Log market value (Euros)", title="Big 6 VS\nEPL clubs market value", colour="Club type")

##man whitney test
wilcox.test(log_market_value_eur ~ club_category, data=club_train, exact = FALSE) ##p < 0.05, w=30000

##fitting a model for club category to predict log MV
mod_club <- lm(formula = log_market_value_eur ~ club_category,
               data=club_train)
summary(mod_club) ##P <0.05, coef -0.9, R square 0.11

##residuals
plot(mod_club, which=1)

##predicting the test data
Prem_club_test <- club_test
Prem_club_test$predicted <- predict(mod_club, newdata = Prem_club_test)
Prem_club_test$residuals <- Prem_club_test$predicted - Prem_club_test$log_market_value_eur

##getting a SSE value
sse_club <- sum(Prem_club_test$residuals**2)
sse_club ##142. 


###########################################################################################
#############using all clubs, not just club category to predict log MV#####################
###########################################################################################

##club regression, but not big 6 vs other. starting with the 80/20 training to test split
all_clubs_train <- Prem_recent[1:426,]
all_clubs_test <- Prem_recent[427:533,]

##sanity check
ggplot(all_clubs_train,
       aes(current_club_name,log_market_value_eur)) +
  geom_boxplot(varwidth = TRUE, fill="plum") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x="club", y="log MV", title="Market value by club")

##checking for the frequency of players for each club
ggplot(all_clubs_train, aes(current_club_name)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle=90, hjust=1)) +
  labs(x="", y="Number of players", title="Number of players per club")

##Kruskal Wallis test to see how log MV changes based on club. Use this test as club name is categorical
##and the number of players isn't even for each club
kruskal.test(log_market_value_eur ~ current_club_name, data=all_clubs_train) ##p < 0.05, test statistic
                                                                             ## 93.7

##fitting model using club to predict log MV
mod_all_clubs <- lm(formula = log_market_value_eur~current_club_name, data = all_clubs_train)
summary(mod_all_clubs) ##P < 0.05, adjusted R square 0.13.

##no regression lines as this analysis is using categorical data

##looking at residuals
plot(mod_all_clubs, which = 1)

##Predicting the test data
Prem_all_club_test <- all_clubs_test
Prem_all_club_test$predicted <- predict(mod_all_clubs, newdata=Prem_all_club_test)
Prem_all_club_test$residuals <- Prem_all_club_test$predicted - Prem_all_club_test$log_market_value_eur

##getting an SSE score
sse_all_clubs <- sum(Prem_all_club_test$residuals**2)
sse_all_clubs ##SSE of 135. 

