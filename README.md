## README

<p xmlns:cc="http://creativecommons.org/ns#" >This work is licensed under <a href="https://creativecommons.org/licenses/by/4.0/?ref=chooser-v1" target="_blank" rel="license noopener noreferrer" style="display:inline-block;">CC BY 4.0<img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/cc.svg?ref=chooser-v1" alt=""><img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/by.svg?ref=chooser-v1" alt=""></a></p>

To run the code seen in this project the following R packages are required: Tidyverse, corrplot.

Research questions: Primary- What are the best predictors of a footballers market value in the English Premier League.
                    Secondary- How does a multi-variate model of the best performing variables perform across the other major football leagues.

Key findings: Performance is the best predictor of a players market value, while nationality shows no relationship with market value. Using goals + assists, age, position, and club category (are they a big club in their league) is best at predicting a players market value in the English Premier League, and is the worse in the Italian Serie A.                    

This code helps analyse the best predictors of a footballers market value in the English Premier League via simple linear regression. In addition, a multi-variate linear regression model of the best predictors was used across the other major football leagues to compare it's performance. The data sets used are "players" and "appearances" which come from https://data.world/dcereijo/player-scores.

To run the code I recommend following the scripts that are numbered in the correct order. "looking_at_data_0" can be skipped. The scripts labelled 1 and 2 are inital exploration of the data and creating tidy data sets. The scripts 3-8 are simple linear regression models using the variables seen in their titles. Scripts 9-13 are for running the multi-variate linear regression model across the major football leagues. The final script, 14, is code for creating graphs that compare the different models.
