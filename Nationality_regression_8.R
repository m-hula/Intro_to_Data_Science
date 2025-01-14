##predicting log MV using nationality 

##creating training and test data for nationalities
Nation_training <- Prem_recent[1:426,]
Nation_test <- Prem_recent[427:533,]

##sanity check. 
##checking for normality/frequency of players by citizenship
ggplot(Nation_training, aes(country_of_citizenship)) +
  geom_bar()

##correlation test. frequency of players by citizenship isn't normally distributed and is categorical
kruskal.test(log_market_value_eur ~ country_of_citizenship, data=Nation_training)
##p > 0.05, test statistic 71. No significant correlation between citizenship and log MV


####################################################################################################################
#############Now do regression comparing English nationality vs all other nationalities to predict log MV########### 
####################################################################################################################

##creating a column that is english citizenship vs non-english citizenship
Prem_recent$nationality_category <- ifelse(Prem_recent$country_of_citizenship
                                           %in% ("England"), "English", "Not English")
##making training and test splits
English_train <- Prem_recent[1:426,]
English_test <- Prem_recent[427:533,]

##sanity check
ggplot(English_train,
       aes(nationality_category, log_market_value_eur)) +
  geom_boxplot(varwidth = TRUE, fill='plum') +
  labs(x="English vs non-English", y="Log MV")

##checking frequency of players for english vs non-english
ggplot(English_train, 
       aes(nationality_category)) +
  geom_bar()

##Mann whitney U test
wilcox.test(log_market_value_eur ~ nationality_category, data=English_train, exact=FALSE)
##P > 0.05. W=17987.Not significant






