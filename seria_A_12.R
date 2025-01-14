##creating a seria A players only data set
seria_A <- player_df[player_df$current_club_domestic_competition_id=="IT1",]

##remove players who's position=missing
seria_A <- seria_A %>% filter(position != "Missing")

##separating Juventus, AC milan, and Inter Milan for the club category variable
seria_A$club_category <- ifelse(seria_A$current_club_name %in% c("Juventus FC", "Inter Milan", 
                                                                 "AC Milan"), "Big club", "Other")

##only including players with a last season of 2022
seria_A_recent <- seria_A[seria_A$last_season == 2022,]

##adding age column
seria_A_recent$age <- as.numeric(difftime(specific_date, seria_A_recent$date_of_birth,
                                          units="days")) / 365.25
seria_A_recent$age <- floor(seria_A_recent$age)

##adding goals and assists column
seria_A_recent <- merge(seria_A_recent, apps_subset, by = "player_id", all.x = TRUE)
seria_A_recent <- transform(seria_A_recent, goals_assists = seria_A_recent$goals + seria_A_recent$assists)

##adding a log market value
seria_A_recent$log_market_value_eur <- log(seria_A_recent$market_value_in_eur)

seria_A_recent <- seria_A_recent[seria_A_recent$market_value_in_eur!=0,]
seria_A_recent <- seria_A_recent %>%
  filter(!is.na(goals_assists) & !is.na(age) & !is.na(position) & !is.na(log_market_value_eur))

############################################################################################################
##########Multi-variate model using age, club category, goals + assists (all positions), position###########
############################################################################################################
seria_A_recent <- seria_A_recent[sample(1:nrow(seria_A_recent)),]

seria_A_train <- seria_A_recent[1:427,]
seria_A_test <- seria_A_recent[428:534,]

##creating the model
mod_seria_A <- lm(
  formula = log_market_value_eur ~ age+goals_assists+club_category+position,
  data = seria_A_train)
summary(mod_seria_A) #P < 0.05, adjusted R square = 0.26

##predicting with the model
predict(mod_seria_A, newdata = seria_A_test)

seria_A_test_copy <- seria_A_test
seria_A_test_copy$predicted <- predict(mod_seria_A, newdata = seria_A_test_copy)
seria_A_test_copy$residuals <- seria_A_test_copy$predicted - seria_A_test_copy$log_market_value_eur

##getting an SSE score
SSE_seria_A <- sum(seria_A_test_copy$residuals**2)
SSE_seria_A ##SEE of 171




