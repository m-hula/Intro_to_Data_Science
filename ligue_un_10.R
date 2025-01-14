##Creating a ligue 1/french league data set
ligue_1 <- player_df[player_df$current_club_domestic_competition_id=="FR1",]

##filtering the ligue 1 players to remove players who's position=missing
ligue_1 <-ligue_1 %>% filter(position != "Missing")

##separating PSG for the club category variable
ligue_1$club_category <- ifelse(ligue_1$current_club_name %in% ("Paris Saint-Germain"), "Big club", "Other")

##only including players with a last season of 2022
ligue_1_recent <- ligue_1[ligue_1$last_season == 2022,]

##adding age column
ligue_1_recent$age <- as.numeric(difftime(specific_date, ligue_1_recent$date_of_birth,
                                          units="days")) / 365.25
ligue_1_recent$age <- floor(ligue_1_recent$age)

##adding goals and assists column
ligue_1_recent <- merge(ligue_1_recent, apps_subset, by = "player_id", all.x = TRUE)
ligue_1_recent <- transform(ligue_1_recent, goals_assists = ligue_1_recent$goals + ligue_1_recent$assists)

##adding a log market value
ligue_1_recent$log_market_value_eur <- log(ligue_1_recent$market_value_in_eur)

ligue_1_recent <- ligue_1_recent[ligue_1_recent$market_value_in_eur!=0,]
ligue_1_recent <- ligue_1_recent %>%
  filter(!is.na(goals_assists) & !is.na(age) & !is.na(position))

############################################################################################################
##########Multi-variate model using age, club category, goals + assists (all positions), position###########
############################################################################################################
ligue_1_recent <- ligue_1_recent[sample(1:nrow(ligue_1_recent)),]

ligue_1_training <- ligue_1_recent[1:396,]
ligue_1_test <- ligue_1_recent[397:495,]

##creating the model
mod_ligue_1 <- lm(
  formula = log_market_value_eur ~ age+goals_assists+club_category+position,
  data = ligue_1_training)
summary(mod_ligue_1)  #P < 0.05, Adjusted R square = 0.25

##predicting with the model
predict(mod_ligue_1, newdata = ligue_1_test)

ligue_1_test_copy <- ligue_1_test
ligue_1_test_copy$predicted <- predict(mod_ligue_1, newdata = ligue_1_test_copy)
ligue_1_test_copy$residuals <- ligue_1_test_copy$predicted - ligue_1_test_copy$log_market_value_eur

##getting an SSE score
SSE_ligue_1 <- sum(ligue_1_test_copy$residuals**2)
SSE_ligue_1 ##SEE of 127








