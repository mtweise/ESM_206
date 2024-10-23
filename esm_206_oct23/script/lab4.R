#Lab 4

###################################
library(patchwork)
library(here)
library(janitor)
library(tidyverse)

#Import data
trawl <- read_csv(here("esm_206_oct23/data", "trawl_data.csv"))
trawl_clean <- trawl |> clean_names()

trawl_pl_clean <- trawl_clean|>
  select(fish_id, plasma_lactate) |>
  drop_na()

#data visualization using histograms

ggplot(trawl_pl_clean, aes(x=plasma_lactate))+
  geom_histogram(binwidth = 5)+
  theme_bw()

#my turn
trawl_cortisol_clean <- trawl_clean|>
  select(fish_id, plasma_cortisol) |>
  drop_na()

ggplot(trawl_cortisol_clean, aes(x=plasma_cortisol))+
  geom_histogram(bins=20)+
  theme_bw()

#plot with mean and median
pl_mean <- mean(trawl_pl_clean$plasma_lactate, na.rm=TRUE)
pl_median <- median(trawl_pl_clean$plasma_lactate, na.rm=TRUE)

ggplot(trawl_pl_clean, aes(x=plasma_lactate))+
  geom_histogram(bins = 15)+
  geom_vline(xintercept = pl_mean, color="red", size=2)+
  geom_vline(xintercept = pl_median, color="blue", size=2)+
  theme_bw()


#calculate other stats

summary(trawl_pl_clean$plasma_lactate)
quantile(trawl_pl_clean$plasma_lactate)
quantile(trawl_pl_clean$plasma_lactate, probs=c(0.1, 0.2, 0.3))
sd(trawl_pl_clean$plasma_lactate)
var(trawl_pl_clean$plasma_lactate)
range(trawl_pl_clean$plasma_lactate)
min(trawl_pl_clean$plasma_lactate)
max(trawl_pl_clean$plasma_lactate)


#deomnstration of patchwork
den_pl <- ggplot(trawl_pl_clean, aes(x=plasma_lactate))+
  geom_density(fill="gray")+
  theme_bw()

box_pl <- ggplot(trawl_pl_clean, aes(x=plasma_lactate))+
  geom_boxplot()+
  theme_bw()

hist_pl <- ggplot(trawl_pl_clean, aes(x=plasma_lactate))+
  geom_histogram()+
  theme_bw()



stack_pl <- (den_pl / box_pl / hist_pl) +
  plot_annotation(tag_levels= 'A')

stack_pl

