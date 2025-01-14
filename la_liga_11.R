##creating a la liga players only data set
la_liga <- player_df[player_df$current_club_domestic_competition_id=="ES1",]

##remove players who's position=missing
la_liga <-la_liga %>% filter(position != "Missing")

##separating Barcelona, Real and athletico Madrid for the club category variable
la_liga$club_category <- ifelse(la_liga$current_club_name %in% c("Real Madrid", "Atlético de Madrid", 
                                                                 "FC Barcelona"), "Big club", "Other")

##only including players with a last season of 2022
la_liga_recent <- la_liga[la_liga$last_season == 2022,]

##adding age column
la_liga_recent$age <- as.numeric(difftime(specific_date, la_liga_recent$date_of_birth,
                                          units="days")) / 365.25
la_liga_recent$age <- floor(la_liga_recent$age)

##adding goals and assists column
la_liga_recent <- merge(la_liga_recent, apps_subset, by = "player_id", all.x = TRUE)
la_liga_recent <- transform(la_liga_recent, goals_assists = la_liga_recent$goals + la_liga_recent$assists)

##adding a log market value
la_liga_recent$log_market_value_eur <- log(la_liga_recent$market_value_in_eur)

la_liga_recent <- la_liga_recent[la_liga_recent$market_value_in_eur!=0,]
la_liga_recent <- la_liga_recent %>%
  filter(!is.na(goals_assists) & !is.na(age) & !is.na(position))

############################################################################################################
##########Multi-variate model using age, club category, goals + assists (all positions), position###########
############################################################################################################
la_liga_recent <- la_liga_recent[sample(1:nrow(la_liga_recent)),]

la_liga_train <- la_liga_recent[1:391,]
la_liga_test <- la_liga_recent[392:489,]

##creating the model
mod_la_liga <- lm(
  formula = log_market_value_eur ~ age+goals_assists+club_category+position,
  data = la_liga_train)
summary(mod_la_liga) # P < 0.05, Adjusted R square= 0.39

##predicting with the model
predict(mod_la_liga, newdata = la_liga_test)

la_liga_test_copy <- la_liga_test
la_liga_test_copy$predicted <- predict(mod_la_liga, newdata = la_liga_test_copy)
la_liga_test_copy$residuals <- la_liga_test_copy$predicted - la_liga_test_copy$log_market_value_eur

##getting an SSE score
SSE_la_liga <- sum(la_liga_test_copy$residuals**2)
SSE_la_liga ##SEE of 93




