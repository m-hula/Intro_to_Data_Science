##premier league players only
premier_league <- player_df[player_df$current_club_domestic_competition_id=="GB1",]

##filtering the PL players to remove players who's position=missing
premier_league<-premier_league %>% filter(position != "Missing")

##adding an extra column to premier_league which separates clubs to the big 6 and 'other'.
premier_league$club_category <- ifelse(premier_league$current_club_name
                                       %in% c("Chelsea FC", "Manchester United",
                                              "Liverpool FC", "Tottenham Hotspur",
                                              "Arsenal FC", "Manchester City"), "Big 6", "Other")

##Data exploration:plotting PL players market value, comparing big 6 clubs to other
ggplot(premier_league, 
       aes(last_season, highest_market_value_in_eur)) +
  geom_point(aes(colour=club_category)) +
  labs(x="Last season played", y="Highest market value (Euros)", 
       title="Big 6 vs other\nplayer market value") +
  scale_x_continuous(breaks=c(2012,2017,2022))

##Data exploration: violin plot of market value in big 6 vs other clubs.
ggplot(premier_league, 
       aes(club_category, highest_market_value_in_eur, fill=club_category)) +
  geom_violin(trim = FALSE) +
  labs(x="Big 6 or other", y="Highest market value of each player (Euros)",
       title="Player market value distribution in big vs small clubs", fill="club type") +
  scale_fill_manual(values = c("Big 6" = "red", "Other" = "green"))

##Data exploration: scatter plot with regression lines in big 6 vs other clubs market value.
ggplot(premier_league,
       aes(last_season, highest_market_value_in_eur, colour=club_category)) +
  geom_point() +
  geom_smooth(method="lm") +
  labs(x="Last season played", y="Highest Market Value (Euros)", 
       title="Trends in market value\nby club", colour="Club type") +
  scale_x_continuous(breaks=c(2012, 2017, 2022))

##making PL data set only include players with current MV
current_prem <- premier_league[!is.na(premier_league$market_value_in_eur),]

##making a PL data set that only includes players with last_season of 2022
Prem_recent <- current_prem[current_prem$last_season == 2022,]

##adding an age column to Prem_recent. Dec 8th 2022 was when 'players' data frame was last synced
##so use this to create age
specific_date <- as.Date("2022-12-08")
Prem_recent$age <- as.numeric(difftime(specific_date, Prem_recent$date_of_birth,
                                       units="days")) / 365.25
##converting the age to integers and rounding down. so 31.8=31 years old.
Prem_recent$age <- floor(Prem_recent$age)

##adding goals, assists, minutes played column to Prem_recent and current_prem
##first I subset apps data frame to make a new data frame that only contains player Id, goals, 
##assists, minutes
apps_subset <- apps_df[, c("player_id", "goals", "assists", "minutes_played")]
##now I add up duplicates for player_id. for example the same player comes up multiple times
##as they are playing in multiple competitions. This means apps_subset shows a players total goals
##assists and minutes played 
apps_subset <- apps_subset %>%
  group_by(player_id) %>%
  summarize(goals = sum(goals, na.rm = TRUE),
            assists = sum(assists, na.rm = TRUE),
            minutes_played = sum(minutes_played, na.rm = TRUE))
##now we need to merge these columns to Prem_recent and current_prem
Prem_recent <- merge(Prem_recent, apps_subset, by = "player_id", all.x = TRUE)
current_prem <- merge(current_prem, apps_subset, by = "player_id", all.x = TRUE)

##adding a goals and assists total column
Prem_recent <- transform(Prem_recent, goals_assists = Prem_recent$goals + Prem_recent$assists)
current_prem <- transform(current_prem, goals_assists = current_prem$goals + current_prem$assists)

##filtering out any players who have a market value of 0
Prem_recent <- Prem_recent[Prem_recent$market_value_in_eur > 0,]
current_prem <- current_prem[current_prem$market_value_in_eur > 0,]

##adding a log market value
Prem_recent$log_market_value_eur <- log(Prem_recent$market_value_in_eur)
current_prem$log_market_value_eur <- log(current_prem$market_value_in_eur)

##plotting regular and log market value
reg_bp <- ggplot(Prem_recent,
       aes(y=market_value_in_eur)) +
  geom_boxplot(fill = "lightblue", colour = "darkblue", outlier.colour = "red", outlier.size = 3) +
  labs(title = "Outliers of market value", y = "Market value (Euros)")

log_bp <- ggplot(Prem_recent,
aes(y=log_market_value_eur)) +
  geom_boxplot(fill = "lightblue", colour = "darkblue", outlier.colour = "red", outlier.size = 3) +
  labs(title = "Outliers of log market value", y = "Log Market value (Euros)", caption = "EPL")

ggplot(Prem_recent,
       aes(x=market_value_in_eur)) +
  geom_bar(fill = "lightblue", colour = "black") +
  labs(title = "Bar chart of market value distribution", x = "Marekt value (Euros)", caption = "EPL")

ggplot(Prem_recent,
       aes(x=log_market_value_eur)) +
  geom_bar(fill="lightblue", colour ="black") +
  labs(title ="Bar chart of log market value distribution", x = "Log market value (Euros)", caption = "EPL")

grid.arrange(reg_bp, log_bp, ncol=2)







