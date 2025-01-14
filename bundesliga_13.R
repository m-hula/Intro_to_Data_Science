##creating a bundesliga players only data set
bundesliga <- player_df[player_df$current_club_domestic_competition_id=="L1",]

##remove players who's position=missing
bundesliga <- bundesliga %>% filter(position != "Missing")

##separating Bayern Munich and Bourssia Dortmund for the club category variable
bundesliga$club_category <- ifelse(bundesliga$current_club_name %in% c("Bayern Munich", "Borussia Dortmund")
                                   , "Big club", "Other")

##only including players with a last season of 2022
bundesliga_recent <- bundesliga[bundesliga$last_season == 2022,]

##adding age column
bundesliga_recent$age <- as.numeric(difftime(specific_date, bundesliga_recent$date_of_birth,
                                          units="days")) / 365.25
bundesliga_recent$age <- floor(bundesliga_recent$age)

##adding goals and assists column
bundesliga_recent <- merge(bundesliga_recent, apps_subset, by = "player_id", all.x = TRUE)
bundesliga_recent <- transform(bundesliga_recent, goals_assists = bundesliga_recent$goals + bundesliga_recent$assists)

##adding a log market value
bundesliga_recent$log_market_value_eur <- log(bundesliga_recent$market_value_in_eur)

bundesliga_recent <- bundesliga_recent[bundesliga_recent$market_value_in_eur!=0,]
bundesliga_recent <- bundesliga_recent %>%
  filter(!is.na(goals_assists) & !is.na(age) & !is.na(position) & !is.na(log_market_value_eur))

############################################################################################################
##########Multi-variate model using age, club category, goals + assists (all positions), position###########
############################################################################################################
bundesliga_recent <- bundesliga_recent[sample(1:nrow(bundesliga_recent)),]

bundesliga_train <- bundesliga_recent[1:375,]
bundesliga_test <- bundesliga_recent[376:469,]

##creating the model
mod_bundesliga <- lm(
  formula = log_market_value_eur ~ age+goals_assists+club_category+position,
  data = bundesliga_train)
summary(mod_bundesliga) # P < 0.05, adjusted R square = 0.39

##predicting with the model
predict(mod_bundesliga, newdata = bundesliga_test)

bundesliga_test_copy <- bundesliga_test
bundesliga_test_copy$predicted <- predict(mod_bundesliga, newdata = bundesliga_test_copy)
bundesliga_test_copy$residuals <- bundesliga_test_copy$predicted - bundesliga_test_copy$log_market_value_eur

##getting an SSE score
SSE_bundesliga <- sum(bundesliga_test_copy$residuals**2)
SSE_bundesliga ##SEE of 100




