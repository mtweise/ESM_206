install.packages("gapminder")
library("gapminder")

#lab 2 Tuesday
#pivot and data viz
############################################

#clear enviro
rm(list=ls())

library(tidyverse)
library(here)
library(janitor)
library(gapminder)


#get data
data("gapminder")
gapminder

#clean data
n_am <- gapminder |>
  clean_names() |>
  filter(country %in% c("United States", "Canada", "Mexico")) |>
  select(country, year, pop)
 
#note this is already clean, we're about to mess it up

#now it's untidy data
n_am_wider_year <- n_am |>
  pivot_wider(names_from = year, 
              values_from = pop)

n_am_wider_countries <- n_am |>
  pivot_wider(names_from = country,
              values_from=pop)

#pivot longer into tidy data
n_am_longer <- n_am_wider_year |>
  pivot_longer(cols= "1952": "2007",
               names_to = "year",
               values_to= "population")

#change year from character to numeric
n_am_longer$year <- as.numeric(n_am_longer$year)

write_csv(n_am_wider_countries, here("data", "north_american_pop.csv"))

##################################################

#pull out all the gapminder data from 2007
gap_2007 <- gapminder |>
  clean_names() |>
  filter(year== 2007)

#boxplot
bp_gap_2007_gdp <- ggplot(data= gap_2007, aes(x=continent, y=gdp_percap) ) +
  geom_boxplot() +
  labs(x="Continent", y="GDP oer capita (USD)", 
       title="GDP per capita per continent in 2007") +
  theme_bw()

bp_gap_2007_gdp


#violin plot
viol_gap_2007_gdp <- ggplot(data= gap_2007, aes(x=continent, y=gdp_percap) ) +
  geom_violin() +
  labs(x="Continent", y="GDP oer capita (USD)", 
       title="GDP per capita per continent in 2007") +
  theme_bw()
viol_gap_2007_gdp

#save figures
ggsave(here("figures", "2007_gep_density_low.jpg"), viol_gap_2007_gdp,
       width=6, height=3, units="in", dpi=600)

#life expectancy on your own
#pull out all the gapminder data from 2007
gap_2007 <- gapminder |>
  clean_names() |>
  filter(year== 2007)

#boxplot
bp_gap_2007_life_exp <- ggplot(data= gap_2007, aes(x=continent, y=life_exp) ) +
  geom_boxplot() +
  labs(x="Continent", y="Life Expectancy (yrs)", 
       title="Life Expectancy per continent in 2007") +
  theme_bw()

bp_gap_2007_life_exp
ggsave(here("figures", "2007_life_exp_low.jpg"), bp_gap_2007_life_exp,
       width=6, height=3, units="in", dpi=600)

install.packages("here")
library(here)
install.packages("usethis")
library(usethis)

#edit
