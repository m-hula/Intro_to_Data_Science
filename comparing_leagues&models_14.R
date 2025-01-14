##creating a data frame of the SSE score for each league 
SSE_data <- data.frame(
  Metric = c("English", "French", "Italian", "Spanish", "German"),
  Value = c(SSE_EPL, SSE_ligue_1, SSE_seria_A, SSE_la_liga, SSE_bundesliga))


##visualising this data on a bar chart
ggplot(SSE_data,
       aes(x=Metric, y=Value, fill=Metric)) +
  geom_bar(stat = "identity") +
  labs(x="Domestic League", y="SSE value", title="Multi-variate linear prediction model for Market value across
       major European Leagues") +
  guides(fill="none") +
  theme_minimal() +
  coord_cartesian(ylim = c(50, NA))


##########################################################################################
##############################comparing only premier league models########################
##########################################################################################

##creating a data frame of the simple/single linear regression models and their SSE values
SSE_simple <- data.frame(
  model_names = c("Age", "Goals and assists*\n(Attackers only)", "Goals and assists", "Club category",
             "Club", "Position"),
  model_values = c(sse_age2, sse_GA, sse_GA_all, sse_club, sse_all_clubs, sse_positions))

##creating another data frame that doesn't include goals + assists for attackers only
SSE_all_pos <- data.frame(
  variables_used = c("Age", "Goals and assists", "Club category", "Club", "Position"),
  sse_results = c(sse_age2, sse_GA_all, sse_club, sse_all_clubs, sse_positions))

##visualising this data as a bar chart (not including GA for attackers only)
ggplot(SSE_all_pos,
       aes(x=variables_used, y=sse_results, fill=variables_used)) +
  geom_bar(stat="identity") +
  labs(x="Predictor variable", y="SSE value", title="Comparison of variables used to predict EPL player
       Market value") +
  guides(fill="none") +
  theme_minimal() +
  coord_cartesian(ylim = c(100, NA))

##visualising this data as a line graph, but including goals/assists for attackers only
ggplot(SSE_simple,
       aes(x=model_names, y=model_values, group = 1)) +
  geom_line(colour = "blue", linewidth = 1) +
  geom_point(size = 3, colour = "red") +
  labs(x="Predictor variable", y="SSE vale", title="Comparison of variables used to predict EPL player
       Market value") +
  theme_minimal()


##creating a data frame of the multi-variate linear regression models.
SSE_multi <- data.frame(
  combination = c("Age & Position", "Goals + Assists\n & Age", "Position & \nClub category", 
                  "Age & Club category", "All 4 variables"),
  SSE_results = c(sse_multi_age_pos, sse_GA_age, sse_pos_club, sse_club_age, SSE_EPL))

##visualising these on a graph
ggplot(SSE_multi,
       aes(x=combination, y=SSE_results, group = 1)) +
  geom_line(colour = "blue", linewidth = 1) +
  geom_point(size = 3, colour = "red") +
  labs(x="Predictor variables", y="SSE vale", title="Comparison of multi-variate models used 
  to predict EPL player Market value") +
  theme_minimal()




