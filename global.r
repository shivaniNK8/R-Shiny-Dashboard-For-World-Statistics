library(plyr)
library(shiny)
library(tidyverse)
library(gapminder)
library(plotly)

source('plots.r')

# geo_df <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv")
# df <- left_join(gapminder,geo_df, by = c("country"="COUNTRY"))



plot_title <- "Life Expectancy"
legend_title <- "Life Expectancy"
attr <- "lifeExp"
