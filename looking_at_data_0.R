library(tidyverse)

##reading in appearances, players, and player valuations as a dataframe
apps_df <- read.csv("https://query.data.world/s/fincrjtmkavuqydroaq7wzh7z33rdn?dws=00000", 
               header=TRUE, stringsAsFactors=FALSE);
player_df <- read.csv("https://query.data.world/s/2khztgxr6xqnnj5mfocnezof66niub?dws=00000", 
               header=TRUE, stringsAsFactors=FALSE);
value_df <- read.csv("https://query.data.world/s/36fe5ifgihk34oxlyjg5bc2b4yic3o?dws=00000", 
               header=TRUE, stringsAsFactors=FALSE);
clubs_df <- read.csv("https://query.data.world/s/wblxa7rufgqdtv5w5hdrgefnkac4iq?dws=00000", 
               header=TRUE, stringsAsFactors=FALSE);

##players of very high market value
expensive_players <- value_df[value_df$market_value_in_eur>100000000,]

##pretty usless graph
ggplot(expensive_players,
       aes(last_season, market_value_in_eur)) +
  geom_point(aes(colour = player_club_domestic_competition_id)) +
  labs(x="Market value (Euros)", y="League", colour="league")

##players who are english citizenship and play in the prem
english_prem <- player_df[player_df$country_of_citizenship=="England" & 
            player_df$current_club_domestic_competition_id=="GB1",]

##shows how market value has increased for enlgish prem players, based on a players last season
ggplot(english_prem, 
       aes(last_season, highest_market_value_in_eur)) +
  geom_point() +
  labs(x="Last season of player", y="Highest market value (Euros)", title="English prem") 

##some euorpean nations in the prem
Test_Euro_nations_prem <- player_df[player_df$country_of_citizenship %in% c("England", "France", 
                                                                   "Germany", "Austria",
                                                                   "Spain", "Poland") &
                                 player_df$current_club_domestic_competition_id=="GB1",]

##plotting market value with time/last season playes, and sorting by citizenship (some euro countries)
ggplot(Test_Euro_nations_prem, 
       aes(last_season, highest_market_value_in_eur)) +
  geom_point(aes(colour=country_of_citizenship)) +
  labs(x="Last season of player", y="Highest market value (Euros)", 
  title="Log Prem market value\nby nation", colour="Nationality") 

##plot market value in fpl by some euro nations and facet by nationality
ggplot(Euro_nations_prem,
       aes(last_season, highest_market_value_in_eur)) +
  geom_line() +
  facet_grid("country_of_citizenship") +
  labs(x="Year", y="Market value (Euros)", title="Market value in FPL\n by nationality")

##number of players for each country of citizenship in the fpl
prem_nationalities <- player_df %>%
  filter(current_club_domestic_competition_id == "GB1") %>%
  group_by(country_of_citizenship) %>%
  summarise(count = n())

##premier league players only
premier_league <- player_df[player_df$current_club_domestic_competition_id=="GB1",]

##filtering the PL players to remove players who's position=missing
premier_league<-premier_league %>% filter(position != "Missing")

##plot PL players values by club, facet by position
ggplot(premier_league,
       aes(last_season, highest_market_value_in_eur)) +
  geom_point(aes(colour=current_club_name)) +
  facet_grid("position") +
  labs(x="Last season played", y="Highest market value (Euros)", 
       title="Prem market value\nby club and postion", colour="Club") +
    scale_x_continuous(breaks=c(2012, 2017, 2022))

##adding an extra column to premier_league which separates clubs to the big 6 and 'other'.
premier_league$club_category <- ifelse(premier_league$current_club_name
                                       %in% c("Chelsea FC", "Manchester United",
                                              "Liverpool FC", "Tottenham Hotspur",
                                              "Arsenal FC", "Manchester City"), "Big 6", "Other")

##now plot prem players market value, comparing big 6 clubs to other
ggplot(premier_league, 
       aes(last_season, highest_market_value_in_eur)) +
  geom_point(aes(colour=club_category)) +
  labs(x="Last season played", y="Highest market value (Euros)", 
       title="Big 6 vs other\nplayer market value") +
  scale_x_continuous(breaks=c(2012,2017,2022))

##violin plot of market value in big 6 vs other
ggplot(premier_league, 
       aes(club_category, highest_market_value_in_eur, fill=club_category)) +
  geom_violin(trim = FALSE) +
  labs(x="Big 6 or other", y="Highest market value of each player (Euros)",
       title="Player market value distribution in big vs small clubs", fill="club type") +
  scale_fill_manual(values = c("Big 6" = "red", "Other" = "green"))

##scatter plot with regression lines in big 6 vs other market value
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

##adding an age column to prem_recent. Dec 8th 2022 was when 'players' data frame was last synced
##so use this to create age
specific_date <- as.Date("2022-12-08")
Prem_recent$age <- as.numeric(difftime(specific_date, Prem_recent$date_of_birth,
                                       units="days")) / 365.25
##converting the age to integers and rounding down. so 31.8=31 years old
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

  



 









