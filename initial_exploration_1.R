##load packages 
library(tidyverse)

##reading in dataframes. 
apps_df <- read.csv("https://query.data.world/s/fincrjtmkavuqydroaq7wzh7z33rdn?dws=00000", 
                    header=TRUE, stringsAsFactors=FALSE);
player_df <- read.csv("https://query.data.world/s/2khztgxr6xqnnj5mfocnezof66niub?dws=00000", 
                      header=TRUE, stringsAsFactors=FALSE);
value_df <- read.csv("https://query.data.world/s/36fe5ifgihk34oxlyjg5bc2b4yic3o?dws=00000", 
                     header=TRUE, stringsAsFactors=FALSE);
clubs_df <- read.csv("https://query.data.world/s/wblxa7rufgqdtv5w5hdrgefnkac4iq?dws=00000", 
                     header=TRUE, stringsAsFactors=FALSE);

##players who are English citizenship and play in the premier league (PL)
english_prem <- player_df[player_df$country_of_citizenship=="England" & 
                            player_df$current_club_domestic_competition_id=="GB1",]

##shows how market value has increased for English PL players, based on a players last season
ggplot(english_prem, 
       aes(last_season, highest_market_value_in_eur)) +
  geom_point() +
  labs(x="Last season of player", y="Highest market value (Euros)", title="English prem") 

##some European nations in the PL.
Test_Euro_nations_prem <- player_df[player_df$country_of_citizenship %in% c("England", "France", 
                                                                            "Germany", "Austria",
                                                                            "Spain", "Poland") &
                                      player_df$current_club_domestic_competition_id=="GB1",]

##plotting market value with time/last season players, and sorting by citizenship (some euro countries)
ggplot(Test_Euro_nations_prem, 
       aes(last_season, highest_market_value_in_eur)) +
  geom_point(aes(colour=country_of_citizenship)) +
  labs(x="Last season of player", y="Highest market value (Euros)", 
       title="Log Prem market value\nby nation", colour="Nationality")

##plot market value in PL by some euro nations and facet by nationality
ggplot(Euro_nations_prem,
       aes(last_season, highest_market_value_in_eur)) +
  geom_line() +
  facet_grid("country_of_citizenship") +
  labs(x="Year", y="Market value (Euros)", title="Market value in FPL\n by nationality")

##number of players for each country of citizenship in the PL
prem_nationalities <- player_df %>%
  filter(current_club_domestic_competition_id == "GB1") %>%
  group_by(country_of_citizenship) %>%
  summarise(count = n())


