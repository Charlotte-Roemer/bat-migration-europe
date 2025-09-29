library(ggeffects)
library(furrr)
library(tidyverse)
library(glmmTMB)
library(broom.mixed)
library(DHARMa)
library(performance)
library(ggsignif)
library(emmeans)
library(multcomp)
library(lmtest)

#setting the wd and import data set
setwd("/Users/alexandregrave/Desktop/STAGE_CEFE/Data/Time_metric_df")
df_5sec<-read.csv("df_5sec.csv", row.names = 1)
df_1min<-read.csv("df_1min.csv", row.names = 1)
df_5min<-read.csv("df_5min.csv",row.names = 1)
df_10min<-read.csv("df_10min.csv",row.names = 1)
df_1hour<-read.csv("df_1hour.csv",row.names = 1)
df_night<-read.csv("df_night.csv",row.names = 1)

hist(df_5sec$julian_day)
hist(df_5sec$mean_windspeed.ms)
hist(df_5sec$cumulated_rain.mm)

hist(df_5sec$Pippip, xlim = c(0,100), ylim = c(0,300),breaks = 100)


# DATA PROCESSING --------------------------------------------------------------
df_list <- list(df_5sec,df_1min, df_5min, df_10min, df_1hour, df_night)

#function 1
process_data <- function(df) {
  df <- df %>%
    filter(!is.na(mean_temperature.dcelsius) & !is.na(mean_windspeed.ms)) %>%
    mutate(
      julian_day_scaled = as.vector(scale(julian_day, center = TRUE, scale = TRUE)),
      windspeed_scaled = as.vector(scale(mean_windspeed.ms, center = TRUE, scale = TRUE)),
      rain_scaled = as.vector(scale(cumulated_rain.mm, center = TRUE, scale = TRUE)),
      id_parc = as.factor(id_parc),
      id_eol = as.factor(id_eol),
      recorder = as.factor(recorder)
    )
  return(df)
}


# Appliquer la fonction à chaque dataframe de la liste
df_list <- lapply(df_list, process_data)
df_5sec <- df_list [[1]]
df_1min <- df_list[[2]]
df_5min <- df_list[[3]]
df_10min <- df_list[[4]]
df_1hour <- df_list[[5]]
df_night <- df_list[[6]]


#NON-LINEAR EFFECT OF COVARIATES

# Fonction pour ajouter les colonnes sinusoïdales
add_sin_cos_columns <- function(df) {
  df <- df %>%
    mutate(
      s1 = sin(2 * pi * julian_day / 366),
      c1 = cos(2 * pi * julian_day / 366),
      s2 = sin(4 * pi * julian_day / 366),
      c2 = cos(4 * pi * julian_day / 366)
    )
  return(df)
}

# Fonctions pour les coefficients temporels
temporal_coeff_jday <- function(julian_day, coefficients) {
  coefficients['julian_day'] +
    coefficients['julian_day:s1'] * sin(2 * pi * julian_day / 366) +
    coefficients['julian_day:c1'] * cos(2 * pi * julian_day / 366) +
    coefficients['julian_day:s2'] * sin(4 * pi * julian_day / 366) +
    coefficients['julian_day:c2'] * cos(4 * pi * julian_day / 366)
}

temporal_coeff_temp <- function(julian_day, coefficients) {
  coefficients['mean_temperature.dcelsius'] +
    coefficients['mean_temperature.dcelsius:s1'] * sin(2 * pi * julian_day / 366) +
    coefficients['mean_temperature.dcelsius:c1'] * cos(2 * pi * julian_day / 366) +
    coefficients['mean_temperature.dcelsius:s2'] * sin(4 * pi * julian_day / 366) +
    coefficients['mean_temperature.dcelsius:c2'] * cos(4 * pi * julian_day / 366)
}

temporal_coeff_wind <- function(julian_day, coefficients) {
  coefficients['mean_windspeed.ms'] +
    coefficients['mean_windspeed.ms:s1'] * sin(2 * pi * julian_day / 366) +
    coefficients['mean_windspeed.ms:c1'] * cos(2 * pi * julian_day / 366) +
    coefficients['mean_windspeed.ms:s2'] * sin(4 * pi * julian_day / 366) +
    coefficients['mean_windspeed.ms:c2'] * cos(4 * pi * julian_day / 366)
}

temporal_coeff_rain <- function(julian_day, coefficients) {
  coefficients['cumulated_rain.mm'] +
    coefficients['cumulated_rain.mm:s1'] * sin(2 * pi * julian_day / 366) +
    coefficients['cumulated_rain.mm:c1'] * cos(2 * pi * julian_day / 366) +
    coefficients['cumulated_rain.mm:s2'] * sin(4 * pi * julian_day / 366) +
    coefficients['cumulated_rain.mm:c2'] * cos(4 * pi * julian_day / 366)
}

# Fonction pour ajuster les modèles et calculer les coefficients temporels
process_dataframe <- function(df, response_variable) {
  df <- add_sin_cos_columns(df)
  
  # Ajuster le modèle jour julien
  formula_jday <- as.formula(paste(response_variable, "~ julian_day + julian_day:s1 + julian_day:c1 + julian_day:s2 + julian_day:c2"))
  model_jday <- lm(formula_jday, data = df)
  coefficients_jday <- coef(model_jday)
  
  # Ajuster le modèle température
  formula_temp <- as.formula(paste(response_variable, "~ mean_temperature.dcelsius + mean_temperature.dcelsius * s1 + mean_temperature.dcelsius * c1 + mean_temperature.dcelsius * s2 + mean_temperature.dcelsius * c2"))
  model_temp <- lm(formula_temp, data = df)
  coefficients_temp <- coef(model_temp)
  
  # Ajuster le modèle vent
  formula_wind <- as.formula(paste(response_variable, "~ mean_windspeed.ms + mean_windspeed.ms * s1 + mean_windspeed.ms * c1 + mean_windspeed.ms * s2 + mean_windspeed.ms * c2"))
  model_wind <- lm(formula_wind, data = df)
  coefficients_wind <- coef(model_wind)
  
  # Ajuster le modèle pluie
  formula_rain <- as.formula(paste(response_variable, "~ cumulated_rain.mm + cumulated_rain.mm * s1 + cumulated_rain.mm * c1 + cumulated_rain.mm * s2 + cumulated_rain.mm * c2"))
  model_rain <- lm(formula_rain, data = df)
  coefficients_rain <- coef(model_rain)
  
  # Calculer les coefficients temporels
  df <- df %>%
    mutate(
      temporal_coef_jd = temporal_coeff_jday(julian_day, coefficients_jday),
      temporal_coef_temps = temporal_coeff_temp(julian_day, coefficients_temp),
      temporal_coef_winds = temporal_coeff_wind(julian_day, coefficients_wind),
      temporal_coef_rain = temporal_coeff_rain(julian_day, coefficients_rain)
    )
  return(df)
}

df_list <- list(df_5sec = df_5sec,df_1min = df_1min, df_5min = df_5min, df_10min = df_10min, df_1hour = df_1hour, df_night = df_night)

# Appliquer le traitement pour 'Pippip' et 'Nyclei'
processed_df_list_pippip <- lapply(df_list, process_dataframe, response_variable = "Pippip")
processed_df_list_nyclei <- lapply(df_list, process_dataframe, response_variable = "Nyclei")

df_5sec_pippip <- processed_df_list_pippip$df_5sec
df_1min_pippip <- processed_df_list_pippip$df_1min
df_5min_pippip <- processed_df_list_pippip$df_5min
df_10min_pippip <- processed_df_list_pippip$df_10min
df_1hour_pippip <- processed_df_list_pippip$df_1hour
df_night_pippip <- processed_df_list_pippip$df_night


df_5sec_nyclei <- processed_df_list_nyclei$df_5sec
df_1min_nyclei <- processed_df_list_nyclei$df_1min
df_5min_nyclei <- processed_df_list_nyclei$df_5min
df_10min_nyclei <- processed_df_list_nyclei$df_10min
df_1hour_nyclei <- processed_df_list_nyclei$df_1hour
df_night_nyclei <- processed_df_list_nyclei$df_night



# CHECKING NON-LINEAR EFFECTS ---------------------------------------------

#look at the Y ~ jday
#PIPPIP
#5SEC
#sinusoidal
ggplot(df_5sec_pippip, aes(x = julian_day, y = temporal_coef_jd)) +
  geom_line() +
  labs(title = "Variation de l'activité en fonction du jour julien (1min)",
       x = "Jour Julien",
       y = "Coefficient Temporel")
#loess
ggplot(df_5sec_pippip, aes(julian_day, Pippip)) +
  geom_point() + 
  geom_smooth(method = "loess") +
  scale_y_continuous(trans='log10')

#sinus
ggplot(df_5sec_pippip, aes(x = julian_day, y = temporal_coef_temps)) +
  geom_line() +
  labs(title = "Variation de l'activité en fonction de la température (5sec)",
       x = "Jour Julien",
       y = "Coefficient Temporel")
ggplot(df_5sec, aes(mean_temperature.dcelsius, Pippip)) +
  geom_point() + 
  geom_smooth(method = "loess")+
  scale_y_continuous(trans='log10')

ggplot(df_5sec_pippip, aes(x = julian_day, y = temporal_coef_winds)) +
  geom_line() +
  labs(title = "Variation de l'activité en fonction du vent (1min)",
       x = "Jour Julien",
       y = "Coefficient Temporel")
ggplot(df_5sec_pippip, aes(mean_windspeed.ms, Pippip)) +
  geom_point() + 
  geom_smooth(method = "loess") +
  scale_y_continuous(trans='log10')

ggplot(df_5sec_pippip, aes(x = julian_day, y = temporal_coef_rain)) +
  geom_line() +
  labs(title = "Variation de l'activité en fonction de la pluie (1min)",
       x = "Jour Julien",
       y = "Coefficient Temporel")
ggplot(df_5sec_pippip, aes(cumulated_rain.mm, Pippip)) +
  geom_point() + 
  geom_smooth(method = "loess")+
  scale_y_continuous(trans='log10')


#1MIN
#sinusoïdal
ggplot(df_1min_pippip, aes(x = julian_day, y = temporal_coef_jd)) +
  geom_line() +
  labs(title = "Variation de l'activité en fonction du jour julien (1min)",
       x = "Jour Julien",
       y = "Coefficient Temporel")
#loess
ggplot(df_1min_pippip, aes(julian_day, Pippip)) +
  geom_point() + 
  geom_smooth(method = "loess") +
  scale_y_continuous(trans='log10')

#sinus
ggplot(df_1min_pippip, aes(x = julian_day, y = temporal_coef_temps)) +
  geom_line() +
  labs(title = "Variation de l'activité en fonction de la température (1min)",
       x = "Jour Julien",
       y = "Coefficient Temporel")
#
ggplot(df_1min_pippip, aes(mean_temperature.dcelsius, Pippip)) +
  geom_point() + 
  geom_smooth(method = "loess")+
  scale_y_continuous(trans='log10')

ggplot(df_1min_pippip, aes(x = julian_day, y = temporal_coef_winds)) +
  geom_line() +
  labs(title = "Variation de l'activité en fonction du vent (1min)",
       x = "Jour Julien",
       y = "Coefficient Temporel")
ggplot(df_1min_pippip, aes(mean_windspeed.ms, Pippip)) +
  geom_point() + 
  geom_smooth(method = "loess")+
  scale_y_continuous(trans='log10')

ggplot(df_1min_pippip, aes(x = julian_day, y = temporal_coef_rain)) +
  geom_line() +
  labs(title = "Variation de l'activité en fonction de la pluie (1min)",
       x = "Jour Julien",
       y = "Coefficient Temporel")


#5MIN
ggplot(df_5min_pippip, aes(x = julian_day, y = temporal_coef_jd)) +
  geom_line() +
  labs(title = "Variation de l'activité en fonction du jour julien (5min)",
       x = "Jour Julien",
       y = "Coefficient Temporel")
ggplot(df_5min, aes(julian_day_scaled, Pippip)) +
  geom_point() + 
  geom_smooth(method = "loess")+
  scale_y_continuous(trans='log10')

ggplot(df_5min_pippip, aes(x = julian_day, y = temporal_coef_temps)) +
  geom_line() +
  labs(title = "Variation de l'activité en fonction de la température (5min)",
       x = "Jour Julien",
       y = "Coefficient Temporel")
ggplot(df_5min_pippip, aes(mean_temperature.dcelsius, Pippip)) +
  geom_point() + 
  geom_smooth(method = "loess")+
  scale_y_continuous(trans='log10')

ggplot(df_5min_pippip, aes(x = julian_day, y = temporal_coef_winds)) +
  geom_line() +
  labs(title = "Variation de l'activité en fonction du vent (5min)",
       x = "Jour Julien",
       y = "Coefficient Temporel")

ggplot(df_5min_pippip, aes(x = julian_day, y = temporal_coef_rain)) +
  geom_line() +
  labs(title = "Variation de l'activité en fonction de la pluie (5min)",
       x = "Jour Julien",
       y = "Coefficient Temporel")

#10MIN
ggplot(df_10min_pippip, aes(x = julian_day, y = temporal_coef_jd)) +
  geom_line() +
  labs(title = "Variation de l'activité en fonction du jour julien (10min)",
       x = "Jour Julien",
       y = "Coefficient Temporel")
ggplot(df_10min_pippip, aes(julian_day, Pippip)) +
  geom_point() + 
  geom_smooth(method = "loess")+
  scale_y_continuous(trans='log10')

ggplot(df_10min_pippip, aes(x = julian_day, y = temporal_coef_temps)) +
  geom_line() +
  labs(title = "Variation de l'activité en fonction de la température (10min)",
       x = "Jour Julien",
       y = "Coefficient Temporel")

ggplot(df_10min_pippip, aes(x = julian_day, y = temporal_coef_winds)) +
  geom_line() +
  labs(title = "Variation de l'activité en fonction du vent (10min)",
       x = "Jour Julien",
       y = "Coefficient Temporel")
ggplot(df_10min_pippip, aes(mean_windspeed.ms, Pippip)) +
  geom_point() + 
  geom_smooth(method = "loess")+
  scale_y_continuous(trans='log10')

ggplot(df_10min_pippip, aes(x = julian_day, y = temporal_coef_rain)) +
  geom_line() +
  labs(title = "Variation de l'activité en fonction de la pluie (10min)",
       x = "Jour Julien",
       y = "Coefficient Temporel")

#1HOUR
ggplot(df_1hour_pippip, aes(x = julian_day, y = temporal_coef_jd)) +
  geom_line() +
  labs(title = "Variation de l'activité en fonction du jour julien (1hour)",
       x = "Jour Julien",
       y = "Coefficient Temporel")
ggplot(df_1hour_pippip, aes(julian_day, Pippip)) +
  geom_point() + 
  geom_smooth(method = "loess")+
  scale_y_continuous(trans='log10')

ggplot(df_1hour_pippip, aes(x = julian_day, y = temporal_coef_temps)) +
  geom_line() +
  labs(title = "Variation de l'activité en fonction de la température (1hour)",
       x = "Jour Julien",
       y = "Coefficient Temporel")
ggplot(df_1hour_pippip, aes(mean_temperature.dcelsius, Pippip)) +
  geom_point() + 
  geom_smooth(method = "loess")+
  scale_y_continuous(trans='log10')

ggplot(df_1hour_pippip, aes(x = julian_day, y = temporal_coef_winds)) +
  geom_line() +
  labs(title = "Variation de l'activité en fonction du vent (1hour)",
       x = "Jour Julien",
       y = "Coefficient Temporel")
ggplot(df_1hour_pippip, aes(mean_windspeed.ms, Pippip)) +
  geom_point() + 
  geom_smooth(method = "loess")+
  scale_y_continuous(trans='log10')

ggplot(df_1hour_pippip, aes(x = julian_day, y = temporal_coef_rain)) +
  geom_line() +
  labs(title = "Variation de l'activité en fonction de la pluie (1hour)",
       x = "Jour Julien",
       y = "Coefficient Temporel")

#NIGHT
ggplot(df_night_pippip, aes(x = julian_day, y = temporal_coef_jd)) +
  geom_line() +
  labs(title = "Variation de l'activité en fonction du jour julien (nuit)",
       x = "Jour Julien",
       y = "Coefficient Temporel")
ggplot(df_night_pippip, aes(julian_day, Pippip)) +
  geom_point() + 
  geom_smooth(method = "loess")

ggplot(df_night_pippip, aes(x = julian_day, y = temporal_coef_temps)) +
  geom_line() +
  labs(title = "Variation de l'activité en fonction de la température (nuit)",
       x = "Jour Julien",
       y = "Coefficient Temporel")
ggplot(df_night_pippip, aes(mean_temperature.dcelsius, Pippip)) +
  geom_point() + 
  geom_smooth(method = "loess")

ggplot(df_night_pippip, aes(x = julian_day, y = temporal_coef_winds)) +
  geom_line() +
  labs(title = "Variation de l'activité en fonction du vent (nuit)",
       x = "Jour Julien",
       y = "Coefficient Temporel")

ggplot(df_night_pippip, aes(x = julian_day, y = temporal_coef_rain)) +
  geom_line() +
  labs(title = "Variation de l'activité en fonction de la pluie (nuit)",
       x = "Jour Julien",
       y = "Coefficient Temporel")

#NYCLEI
#5sec
ggplot(df_5sec_nyclei, aes(x = julian_day, y = temporal_coef_jd)) +
  geom_line() +
  labs(title = "Variation de l'activité en fonction du jour julien (5sec)",
       x = "Jour Julien",
       y = "Coefficient Temporel")
ggplot(df_5sec_nyclei, aes(julian_day, Nyclei)) +
  geom_point() + 
  geom_smooth(method = "loess") +
  scale_y_continuous(trans='log10')


ggplot(df_5sec_nyclei, aes(x = julian_day, y = temporal_coef_temps)) +
  geom_line() +
  labs(title = "Variation de l'activité en fonction de la température (5sec)",
       x = "Jour Julien",
       y = "Coefficient Temporel")
ggplot(df_5sec_nyclei, aes(mean_temperature.dcelsius, Nyclei)) +
  geom_point() + 
  geom_smooth(method = "loess") +
  scale_y_continuous(trans='log10')

ggplot(df_5sec_nyclei, aes(x = julian_day, y = temporal_coef_winds)) +
  geom_line() +
  labs(title = "Variation de l'activité en fonction du vent (5sec)",
       x = "Jour Julien",
       y = "Coefficient Temporel")
ggplot(df_5sec_nyclei, aes(mean_windspeed.ms, Nyclei)) +
  geom_point() + 
  geom_smooth(method = "loess") +
  scale_y_continuous(trans='log10')

ggplot(df_5sec_nyclei, aes(x = julian_day, y = temporal_coef_rain)) +
  geom_line() +
  labs(title = "Variation de l'activité en fonction de la pluie (5sec)",
       x = "Jour Julien",
       y = "Coefficient Temporel")
ggplot(df_5sec_nyclei, aes(cumulated_rain.mm, Nyclei)) +
  geom_point() + 
  geom_smooth(method = "loess") +
  scale_y_continuous(trans='log10')

#1MIN
ggplot(df_1min_nyclei, aes(x = julian_day, y = temporal_coef_jd)) +
  geom_line() +
  labs(title = "Variation de l'activité en fonction du jour julien (1min)",
       x = "Jour Julien",
       y = "Coefficient Temporel")
ggplot(df_1min_nyclei, aes(julian_day, Nyclei)) +
  geom_point() + 
  geom_smooth(method = "loess") +
  scale_y_continuous(trans='log10')

ggplot(df_1min_nyclei, aes(x = julian_day, y = temporal_coef_temps)) +
  geom_line() +
  labs(title = "Variation de l'activité en fonction de la température (1min)",
       x = "Jour Julien",
       y = "Coefficient Temporel")
ggplot(df_1min_nyclei, aes(mean_temperature.dcelsius, Nyclei)) +
  geom_point() + 
  geom_smooth(method = "loess") +
  scale_y_continuous(trans='log10')

ggplot(df_1min_nyclei, aes(x = julian_day, y = temporal_coef_winds)) +
  geom_line() +
  labs(title = "Variation de l'activité en fonction du vent (1min)",
       x = "Jour Julien",
       y = "Coefficient Temporel")
ggplot(df_1min_nyclei, aes(mean_windspeed.ms, Nyclei)) +
  geom_point() + 
  geom_smooth(method = "loess") +
  scale_y_continuous(trans='log10')

ggplot(df_1min_nyclei, aes(x = julian_day, y = temporal_coef_rain)) +
  geom_line() +
  labs(title = "Variation de l'activité en fonction de la pluie (1min)",
       x = "Jour Julien",
       y = "Coefficient Temporel")

#5MIN
ggplot(df_5min_nyclei, aes(x = julian_day, y = temporal_coef_jd)) +
  geom_line() +
  labs(title = "Variation de l'activité en fonction du jour julien (5min)",
       x = "Jour Julien",
       y = "Coefficient Temporel")
ggplot(df_5min_nyclei, aes(julian_day, Nyclei)) +
  geom_point() + 
  geom_smooth(method = "loess") +
  scale_y_continuous(trans='log10')

ggplot(df_5min_nyclei, aes(x = julian_day, y = temporal_coef_temps)) +
  geom_line() +
  labs(title = "Variation de l'activité en fonction de la température (5min)",
       x = "Jour Julien",
       y = "Coefficient Temporel")
ggplot(df_5min_nyclei, aes(mean_temperature.dcelsius, Nyclei)) +
  geom_point() + 
  geom_smooth(method = "loess") +
  scale_y_continuous(trans='log10')

ggplot(df_5min_nyclei, aes(x = julian_day, y = temporal_coef_winds)) +
  geom_line() +
  labs(title = "Variation de l'activité en fonction du vent (5min)",
       x = "Jour Julien",
       y = "Coefficient Temporel")
ggplot(df_5min_nyclei, aes(mean_windspeed.ms, Nyclei)) +
  geom_point() + 
  geom_smooth(method = "loess") +
  scale_y_continuous(trans='log10')

ggplot(df_5min_nyclei, aes(x = julian_day, y = temporal_coef_rain)) +
  geom_line() +
  labs(title = "Variation de l'activité en fonction de la pluie (5min)",
       x = "Jour Julien",
       y = "Coefficient Temporel")
ggplot(df_5min_nyclei, aes(cumulated_rain.mm, Nyclei)) +
  geom_point() + 
  geom_smooth(method = "loess") +
  scale_y_continuous(trans='log10')

#10MIN
ggplot(df_10min_nyclei, aes(x = julian_day, y = temporal_coef_jd)) +
  geom_line() +
  labs(title = "Variation de l'activité en fonction du jour julien (10min)",
       x = "Jour Julien",
       y = "Coefficient Temporel")
ggplot(df_10min_nyclei, aes(julian_day, Nyclei)) +
  geom_point() + 
  geom_smooth(method = "loess") +
  scale_y_continuous(trans='log10')

ggplot(df_10min_nyclei, aes(x = julian_day, y = temporal_coef_temps)) +
  geom_line() +
  labs(title = "Variation de l'activité en fonction de la température (10min)",
       x = "Jour Julien",
       y = "Coefficient Temporel")
ggplot(df_10min_nyclei, aes(mean_temperature.dcelsius, Nyclei)) +
  geom_point() + 
  geom_smooth(method = "loess") +
  scale_y_continuous(trans='log10')

ggplot(df_10min_nyclei, aes(x = julian_day, y = temporal_coef_winds)) +
  geom_line() +
  labs(title = "Variation de l'activité en fonction du vent (10min)",
       x = "Jour Julien",
       y = "Coefficient Temporel")
ggplot(df_10min_nyclei, aes(mean_windspeed.ms, Nyclei)) +
  geom_point() + 
  geom_smooth(method = "loess") +
  scale_y_continuous(trans='log10')

ggplot(df_10min_nyclei, aes(x = julian_day, y = temporal_coef_rain)) +
  geom_line() +
  labs(title = "Variation de l'activité en fonction de la pluie (10min)",
       x = "Jour Julien",
       y = "Coefficient Temporel")
ggplot(df_10min_nyclei, aes(cumulated_rain.mm, Nyclei)) +
  geom_point() + 
  geom_smooth(method = "loess") +
  scale_y_continuous(trans='log10')

#1HOUR
ggplot(df_1hour_nyclei, aes(x = julian_day, y = temporal_coef_jd)) +
  geom_line() +
  labs(title = "Variation de l'activité en fonction du jour julien (1hour)",
       x = "Jour Julien",
       y = "Coefficient Temporel")
ggplot(df_1hour_nyclei, aes(julian_day, Nyclei)) +
  geom_point() + 
  geom_smooth(method = "loess") +
  scale_y_continuous(trans='log10')

ggplot(df_1hour_nyclei, aes(x = julian_day, y = temporal_coef_temps)) +
  geom_line() +
  labs(title = "Variation de l'activité en fonction de la température (1hour)",
       x = "Jour Julien",
       y = "Coefficient Temporel")
ggplot(df_1hour_nyclei, aes(mean_temperature.dcelsius, Nyclei)) +
  geom_point() + 
  geom_smooth(method = "loess") +
  scale_y_continuous(trans='log10')

ggplot(df_1hour_nyclei, aes(x = julian_day, y = temporal_coef_winds)) +
  geom_line() +
  labs(title = "Variation de l'activité en fonction du vent (1hour)",
       x = "Jour Julien",
       y = "Coefficient Temporel")
ggplot(df_1hour_nyclei, aes(mean_windspeed.ms, Nyclei)) +
  geom_point() + 
  geom_smooth(method = "loess") +
  scale_y_continuous(trans='log10')

ggplot(df_1hour_nyclei, aes(x = julian_day, y = temporal_coef_rain)) +
  geom_line() +
  labs(title = "Variation de l'activité en fonction de la pluie (1hour)",
       x = "Jour Julien",
       y = "Coefficient Temporel")
ggplot(df_1hour_nyclei, aes(cumulated_rain.mm, Nyclei)) +
  geom_point() + 
  geom_smooth(method = "loess") +
  scale_y_continuous(trans='log10')


#NIGHT
ggplot(df_night_nyclei, aes(x = julian_day, y = temporal_coef_jd)) +
  geom_line() +
  labs(title = "Variation de l'activité en fonction du jour julien (nuit)",
       x = "Jour Julien",
       y = "Coefficient Temporel")
ggplot(df_night_nyclei, aes(julian_day, Nyclei)) +
  geom_point() + 
  geom_smooth(method = "loess") #+
#scale_y_continuous(trans='log10')

ggplot(df_night_nyclei, aes(x = julian_day, y = temporal_coef_temps)) +
  geom_line() +
  labs(title = "Variation de l'activité en fonction de la température (nuit)",
       x = "Jour Julien",
       y = "Coefficient Temporel")
ggplot(df_night_nyclei, aes(mean_temperature.dcelsius, Nyclei)) +
  geom_point() + 
  geom_smooth(method = "loess") 

ggplot(df_night_nyclei, aes(x = julian_day, y = temporal_coef_winds)) +
  geom_line() +
  labs(title = "Variation de l'activité en fonction du vent (nuit)",
       x = "Jour Julien",
       y = "Coefficient Temporel")
ggplot(df_night_nyclei, aes(mean_windspeed.ms, Nyclei)) +
  geom_point() + 
  geom_smooth(method = "loess") 

ggplot(df_night_nyclei, aes(x = julian_day, y = temporal_coef_rain)) +
  geom_line() +
  labs(title = "Variation de l'activité en fonction de la pluie (nuit)",
       x = "Jour Julien",
       y = "Coefficient Temporel")
ggplot(df_night_nyclei, aes(cumulated_rain.mm, Nyclei)) +
  geom_point() + 
  geom_smooth(method = "loess") 


# checking distribution of 'continuous' variables
lapply(df_list, function(df) hist(df$mean_temperature.dcelsius))
lapply(df_list, function(df) hist(df$mean_windspeed.ms))
lapply(df_list, function(df) hist(df$julian_day))

#checking colinearity
cor_matrices <- lapply(df_list, function(df) {
  cor(df[, c("mean_temperature.dcelsius", "mean_windspeed.ms", "julian_day")], use = "pairwise.complete.obs")
})

library(gridExtra)

p1 <- ggplot(df_night_pippip, aes(x = "Pippip")) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "blue", alpha = 0.7) +
  geom_density(color = "red") +
  labs(title = "Histogram and Density Plot",
       x = "Your Variable",
       y = "Density")

p2 <- ggplot(df_night_pippip, aes(y = "Pippip")) +
  geom_boxplot(fill = "blue", alpha = 0.7) +
  labs(title = "Box Plot",
       y = "Your Variable")

p3 <- ggplot(df_night_pippip, aes(sample = "Pippip")) +
  stat_qq() +
  stat_qq_line(col = "red") +
  labs(title = "QQ Plot")

grid.arrange(p1, p2, p3, nrow = 2)

summary(mod_p_cf_night)
#'boxplot avec effet du matériel en fonction des métriques boxplot ~ jday + pvalue
#'pour les autres graphs, simplifiez juste 1 courbe par pas de temps avec astérix (1 plot par prédicteurs)
#'métrique en lignes 
#'M&M batcorder # -> présentez les pratiques les plus courantes, au vu des écarts entre SM4 & Batcorde (Adams,2012)
#'utilisation du dataset de KB pour élaborer la méthode

# GLMM SELECTION -------------------------------------------------------
#STEP 1
#PIPPIP
#NULL 
mod_p_b_5sec<- glmmTMB(Pippip ~ poly(julian_day_scaled,2) + (1 | id_parc/id_eol), data = df_5sec, family = nbinom2)
mod_p_b_1min<- glmmTMB(Pippip ~ poly(julian_day_scaled,2) + (1 | id_parc/id_eol), data = df_1min, family = nbinom2)
mod_p_b_5min<- glmmTMB(Pippip ~ poly(julian_day_scaled,2) + (1 | id_parc/id_eol), data = df_5min, family = nbinom2)
mod_p_b_10min<-glmmTMB(Pippip ~ poly(julian_day_scaled,2) + (1 | id_parc/id_eol), data = df_10min, family = nbinom2)
mod_p_b_1hour<-glmmTMB(Pippip ~ poly(julian_day_scaled,2) + (1 | id_parc/id_eol), data = df_1hour, family = nbinom2)
mod_p_b_night<-glmmTMB(Pippip ~ poly(julian_day_scaled,2) + (1 | id_parc/id_eol), data = df_night, family = binomial())

#PIPPIP COMPLEX ADD
#wind
mod_p_cw_5sec <-  glmmTMB(Pippip ~ recorder + windspeed_scaled + poly(julian_day_scaled,2) + (1 | id_parc/id_eol), data = df_5sec, family = nbinom2)
mod_p_cw_1min <-  glmmTMB(Pippip ~ recorder + windspeed_scaled + poly(julian_day_scaled,2) + (1 | id_parc/id_eol), data = df_1min, family = nbinom2)
mod_p_cw_5min <-  glmmTMB(Pippip ~ recorder + windspeed_scaled + poly(julian_day_scaled,2) + (1 | id_parc/id_eol), data = df_5min, family = nbinom2)
mod_p_cw_10min <- glmmTMB(Pippip ~ recorder + windspeed_scaled + poly(julian_day_scaled,2) + (1 | id_parc/id_eol), data = df_10min, family = nbinom2)
mod_p_cw_1hour <- glmmTMB(Pippip ~ recorder + windspeed_scaled + poly(julian_day_scaled,2) + (1 | id_parc/id_eol), data = df_1hour, family = nbinom2)
mod_p_cw_night <- glmmTMB(Pippip ~ recorder + windspeed_scaled + poly(julian_day_scaled,2) + (1 | id_parc/id_eol), data = df_night, family = binomial())

#temp
#mod_p_ct_5sec <-  glmmTMB(Pippip ~ recorder + poly(temperature_scaled,2) + poly(julian_day_scaled,2) + (1 | id_parc/id_eol), data = df_5sec,  family = nbinom2, control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))
#mod_p_ct_1min <-  glmmTMB(Pippip ~ recorder + poly(temperature_scaled,2) + poly(julian_day_scaled,2) + (1 | id_parc/id_eol), data = df_1min,  family = nbinom2, control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))
#mod_p_ct_5min <-  glmmTMB(Pippip ~ recorder + poly(temperature_scaled,2) + poly(julian_day_scaled,2) + (1 | id_parc/id_eol), data = df_5min,  family = nbinom2, control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))
#mod_p_ct_10min <- glmmTMB(Pippip ~ recorder + poly(temperature_scaled,2) + poly(julian_day_scaled,2) + (1 | id_parc/id_eol), data = df_10min, family = nbinom2, control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))
#mod_p_ct_1hour <- glmmTMB(Pippip ~ recorder + poly(temperature_scaled,2) + poly(julian_day_scaled,2) + (1 | id_parc/id_eol), data = df_1hour, family = nbinom2, control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))
#mod_p_ct_night <- glmmTMB(Pippip ~ recorder + poly(temperature_scaled,2) + poly(julian_day_scaled,2) + (1 | id_parc/id_eol), data = df_night, family = binomial())

#FULL
mod_p_cf_5sec  <- glmmTMB(Pippip ~ recorder + windspeed_scaled + rain_scaled + poly(julian_day_scaled,2) + (1 | id_parc/id_eol), data = df_5sec,  family = nbinom2)
mod_p_cf_1min  <- glmmTMB(Pippip ~ recorder + windspeed_scaled + rain_scaled + poly(julian_day_scaled,2) + (1 | id_parc/id_eol), data = df_1min,  family = nbinom2)
mod_p_cf_5min  <- glmmTMB(Pippip ~ recorder + windspeed_scaled + rain_scaled + poly(julian_day_scaled,2) + (1 | id_parc/id_eol), data = df_5min,  family = nbinom2)
mod_p_cf_10min <- glmmTMB(Pippip ~ recorder + windspeed_scaled + rain_scaled + poly(julian_day_scaled,2) + (1 | id_parc/id_eol), data = df_10min, family = nbinom2, control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))
mod_p_cf_1hour <- glmmTMB(Pippip ~ recorder + windspeed_scaled + rain_scaled + poly(julian_day_scaled,2) + (1 | id_parc/id_eol), data = df_1hour, family = nbinom2)
mod_p_cf_night <- glmmTMB(Pippip ~ recorder + windspeed_scaled + rain_scaled + poly(julian_day_scaled,2) + (1 | id_parc/id_eol), data = df_night, family = binomial())

#fonction Charlotte pour checker la colinéarité entre chaque prédicteurs (à creuser)
vif.mer <- function (fit) {
  ## adapted from rms::vif
  
  v <- vcov(fit)$cond
  nam <- names(fixef(fit)$cond)
  
  ## exclude intercepts
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)]
  }
  
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  v
}

vif.mer(mod_p_cf_5sec)
vif.mer(mod_p_cf_1min)
vif.mer(mod_p_cf_5min)
vif.mer(mod_p_cf_10min)
vif.mer(mod_p_cf_1hour)
vif.mer(mod_p_cf_night)

#AIC 5sec
AICtab_p_5sec<-AIC(mod_p_b_5sec,mod_p_cw_5sec,mod_p_ct_5sec,mod_p_cf_5sec)
AICtab_p_5sec$Delta_AIC <- AICtab_p_5sec$AIC - min(AICtab_p_5sec$AIC)
AICtab_p_5sec <- AICtab_p_5sec[order(AICtab_p_5sec$AIC), ]
print(AICtab_p_5sec)


#AIC 1MIN
AICtab_p_1min<-AIC(mod_p_b_1min,mod_p_cw_1min,mod_p_ct_1min,mod_p_cf_1min)
AICtab_p_1min$Delta_AIC <- AICtab_p_1min$AIC - min(AICtab_p_1min$AIC)
AICtab_p_1min <- AICtab_p_1min[order(AICtab_p_1min$AIC), ]
print(AICtab_p_1min)

#AIC 5MIN
AICtab_p_5min<-AIC(mod_p_b_5min,mod_p_cw_5min,mod_p_ct_5min,mod_p_cf_5min)
AICtab_p_5min$Delta_AIC <- AICtab_p_5min$AIC - min(AICtab_p_5min$AIC)
AICtab_p_5min <- AICtab_p_5min[order(AICtab_p_5min$AIC), ]
print(AICtab_p_5min)

#AIC 10MIN
AICtab_p_10min<-AIC(mod_p_b_10min,mod_p_cw_10min,mod_p_ct_10min, mod_p_cf_10min)
AICtab_p_10min$Delta_AIC <- AICtab_p_10min$AIC - min(AICtab_p_10min$AIC)
AICtab_p_10min <- AICtab_p_10min[order(AICtab_p_10min$AIC), ]
print(AICtab_p_10min)

#AIC 1HOUR
AICtab_p_1hour<-AIC(mod_p_b_1hour,mod_p_cw_1hour,mod_p_ct_1hour,mod_p_cf_1hour)
AICtab_p_1hour$Delta_AIC <- AICtab_p_1hour$AIC - min(AICtab_p_1hour$AIC)
AICtab_p_1hour <- AICtab_p_1hour[order(AICtab_p_1hour$AIC), ]
print(AICtab_p_1hour)

#AIC NIGHT
AICtab_p_night<-AIC(mod_p_b_night,mod_p_cw_night,mod_p_ct_night,mod_p_cf_night)
AICtab_p_night$Delta_AIC <- AICtab_p_night$AIC - min(AICtab_p_night$AIC)
AICtab_p_night <- AICtab_p_night[order(AICtab_p_night$AIC), ]
print(AICtab_p_night)

#NYCLEI
#NULL 
mod_n_b_5sec<- glmmTMB(Nyclei ~ recorder + poly(julian_day_scaled,2) + (1 | id_parc/id_eol), data = df_5sec, family = nbinom2)
mod_n_b_1min<- glmmTMB(Nyclei ~ recorder + poly(julian_day_scaled,2) + (1 | id_parc/id_eol), data = df_1min, family = nbinom2)
mod_n_b_5min<- glmmTMB(Nyclei ~ recorder + poly(julian_day_scaled,2) + (1 | id_parc/id_eol), data = df_5min, family = nbinom2)
mod_n_b_10min<-glmmTMB(Nyclei ~ recorder + poly(julian_day_scaled,2) + (1 | id_parc/id_eol), data = df_10min, family = nbinom2)
mod_n_b_1hour<-glmmTMB(Nyclei ~ recorder + poly(julian_day_scaled,2) + (1 | id_parc/id_eol), data = df_1hour, family = nbinom2)
mod_n_b_night<-glmmTMB(Nyclei ~ recorder + poly(julian_day_scaled,2) + (1 | id_parc/id_eol), data = df_night, family = binomial())

#NYCLEI COMPLEX ADD
#wind
mod_n_cw_5sec <-  glmmTMB(Nyclei ~ recorder + windspeed_scaled + poly(julian_day_scaled,2) + (1 | id_parc/id_eol), data = df_5sec, family = nbinom2)
mod_n_cw_1min <-  glmmTMB(Nyclei ~ recorder + windspeed_scaled + poly(julian_day_scaled,2) + (1 | id_parc/id_eol), data = df_1min, family = nbinom2)
mod_n_cw_5min <-  glmmTMB(Nyclei ~ recorder + windspeed_scaled + poly(julian_day_scaled,2) + (1 | id_parc/id_eol), data = df_5min, family = nbinom2)
mod_n_cw_10min <- glmmTMB(Nyclei ~ recorder + windspeed_scaled + poly(julian_day_scaled,2) + (1 | id_parc/id_eol), data = df_10min, family = nbinom2)
mod_n_cw_1hour <- glmmTMB(Nyclei ~ recorder + windspeed_scaled + poly(julian_day_scaled,2) + (1 | id_parc/id_eol), data = df_1hour, family = nbinom2)
mod_n_cw_night <- glmmTMB(Nyclei ~ recorder + windspeed_scaled + poly(julian_day_scaled,2) + (1 | id_parc/id_eol), data = df_night, family = binomial())

#temp
mod_n_ct_5sec <-  glmmTMB(Nyclei ~ recorder + poly(temperature_scaled,2) + poly(julian_day_scaled,2) + (1 | id_parc/id_eol), data = df_5sec, family = nbinom2)
mod_n_ct_1min <-  glmmTMB(Nyclei ~ recorder + poly(temperature_scaled,2) + poly(julian_day_scaled,2) + (1 | id_parc/id_eol), data = df_1min, family = nbinom2)
mod_n_ct_5min <-  glmmTMB(Nyclei ~ recorder + poly(temperature_scaled,2) + poly(julian_day_scaled,2) + (1 | id_parc/id_eol), data = df_5min, family = nbinom2)
mod_n_ct_10min <- glmmTMB(Nyclei ~ recorder + poly(temperature_scaled,2) + poly(julian_day_scaled,2) + (1 | id_parc/id_eol), data = df_10min, family = nbinom2)
mod_n_ct_1hour <- glmmTMB(Nyclei ~ recorder + poly(temperature_scaled,2) + poly(julian_day_scaled,2) + (1 | id_parc/id_eol), data = df_1hour, family = nbinom2)#,control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))
mod_n_ct_night <- glmmTMB(Nyclei ~ recorder + poly(temperature_scaled,2) + poly(julian_day_scaled,2) + (1 | id_parc/id_eol), data = df_night, family = binomial()) #

#FULL
mod_n_cf_5sec  <- glmmTMB(Nyclei ~ recorder + windspeed_scaled + rain_scaled + poly(julian_day_scaled,2) + (1 | id_parc/id_eol), data = df_5sec, family = nbinom2)
mod_n_cf_1min  <- glmmTMB(Nyclei ~ recorder + windspeed_scaled + rain_scaled + poly(julian_day_scaled,2) + (1 | id_parc/id_eol), data = df_1min, family = nbinom2)
mod_n_cf_5min  <- glmmTMB(Nyclei ~ recorder + windspeed_scaled + rain_scaled + poly(julian_day_scaled,2) + (1 | id_parc/id_eol), data = df_5min, family = nbinom2)
mod_n_cf_10min <- glmmTMB(Nyclei ~ recorder + windspeed_scaled + rain_scaled + poly(julian_day_scaled,2) + (1 | id_parc/id_eol), data = df_10min, family = nbinom2)
mod_n_cf_1hour <- glmmTMB(Nyclei ~ recorder + windspeed_scaled + rain_scaled + poly(julian_day_scaled,2) + (1 | id_parc/id_eol), data = df_1hour, family = nbinom2,control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))
mod_n_cf_night <- glmmTMB(Nyclei ~ recorder + windspeed_scaled + rain_scaled + poly(julian_day_scaled,2) + (1 | id_parc/id_eol), data = df_night, family = binomial())

summary(mod_n_cf_night)
#AIC 5sec
AICtab_n_5sec<-AIC(mod_n_b_5sec,mod_n_cw_5sec,mod_n_ct_5sec,mod_n_cf_5sec)
AICtab_n_5sec$Delta_AIC <- AICtab_n_5sec$AIC - min(AICtab_n_5sec$AIC)
AICtab_n_5sec <- AICtab_n_5sec[order(AICtab_n_5sec$AIC), ]
print(AICtab_n_5sec)


#AIC 1MIN
AICtab_n_1min<-AIC(mod_n_b_1min,mod_n_cw_1min,mod_n_ct_1min,mod_n_cf_1min)
AICtab_n_1min$Delta_AIC <- AICtab_n_1min$AIC - min(AICtab_n_1min$AIC)
AICtab_n_1min <- AICtab_n_1min[order(AICtab_n_1min$AIC), ]
print(AICtab_n_1min)

#AIC 5MIN
AICtab_n_5min<-AIC(mod_n_b_5min,mod_n_cw_5min,mod_n_ct_5min,mod_n_cf_5min)
AICtab_n_5min$Delta_AIC <- AICtab_n_5min$AIC - min(AICtab_n_5min$AIC)
AICtab_n_5min <- AICtab_n_5min[order(AICtab_n_5min$AIC), ]
print(AICtab_n_5min)

#AIC 10MIN
AICtab_n_10min<-AIC(mod_n_b_10min,mod_n_cw_10min,mod_n_ct_10min,mod_n_cf_10min)
AICtab_n_10min$Delta_AIC <- AICtab_n_10min$AIC - min(AICtab_n_10min$AIC)
AICtab_n_10min <- AICtab_n_10min[order(AICtab_n_10min$AIC), ]
print(AICtab_n_10min)

#AIC 1HOUR
AICtab_n_1hour<-AIC(mod_n_b_1hour,mod_n_cw_1hour,mod_n_ct_1hour,mod_n_cf_1hour)
AICtab_n_1hour$Delta_AIC <- AICtab_n_1hour$AIC - min(AICtab_n_1hour$AIC)
AICtab_n_1hour <- AICtab_n_1hour[order(AICtab_n_1hour$AIC), ]
print(AICtab_n_1hour)

#AIC NIGHT
AICtab_n_night<-AIC(mod_n_b_night,mod_n_cw_night,mod_n_ct_night,mod_n_cf_night)
AICtab_n_night$Delta_AIC <- AICtab_n_night$AIC - min(AICtab_n_night$AIC)
AICtab_n_night <- AICtab_n_night[order(AICtab_n_night$AIC), ]
print(AICtab_n_night)




#PIPPIP COMPLEX #comparer l'ajout du vent et de la température 1 à la fois et comparer AIC (pour Charlotte soit vent soit temperature)
#graph 1 boxplot par recorder 

#graph pluie et vent 
#ggpredict noter le predicteur qu'on veut pour chaque graph et du coup un ggpredict par x (graph)
# recorder + vent (fort) + pluie (effet faible) + jday (fort) (justifier par la littérature)
# une couleur par pas de temps et un facet par recorder 


#STEP 2

#TEST 0-INFLATED #faire tourner un modèle avant
# Stocker le modèle dans simulation
simulation <- simulate(mod_p_cw_1hour, nsim = 1000)

# Calculer la proportion de zéros dans chaque simulation
proportions_simulées <- sapply(simulation, function(x) mean(x == 0))

# Calculer la proportion observée de zéros dans les données
proportion_observée <- mean(df_night_pippip$Pippip == 0)

# Comparer les proportions observées et simulées
hist(proportions_simulées, main = "Distribution simulée des proportions de zéros",
     xlab = "Proportion de zéros")
abline(v = proportion_observée, col = "red", lwd = 2)

###
df_list_p<-list(df_5sec,df_1min,df_5min, df_10min, df_1hour, df_night)
df_list_n<-list(df_5sec,df_1min,df_5min, df_10min, df_1hour, df_night)

model_list_p<-list(mod_p_cf_1min ,mod_p_cf_5min ,mod_p_cf_10min,mod_p_cf_1hour,mod_p_cf_night)
model_list_n<-list(mod_n_cf_1min ,mod_n_cf_5min ,mod_n_cf_10min,mod_n_cf_1hour,mod_n_cf_night)

#0 inflation test by hand ???
#pippip
simulate_proportions_p <- function(model, df) {
  # Simuler les données
  simulation_p <- simulate(model, nsim = 500)
  
  # Calculer la proportion de zéros dans chaque simulation
  proportions_simulées_p <- sapply(simulation_p, function(x) mean(x == 0))
  
  # Calculer la proportion observée de zéros dans les données
  proportion_observée_p <- mean(df$Pippip == 0)
  
  # Comparer les proportions observées et simulées
  hist(proportions_simulées_p, main = "Distribution simulée des proportions de zéros",
       xlab = "Proportion de zéros")
  abline(v = proportion_observée_p, col = "red", lwd = 2)
}

#Nyclei
simulate_proportions_n <- function(model, df) {
  # Simuler les données
  simulation_n <- simulate(model, nsim = 500)
  
  # Calculer la proportion de zéros dans chaque simulation
  proportions_simulées_n <- sapply(simulation_n, function(x) mean(x == 0))
  
  # Calculer la proportion observée de zéros dans les données
  proportion_observée_n <- mean(df$Nyclei == 0)
  
  # Comparer les proportions observées et simulées
  hist(proportions_simulées_n, main = "Distribution simulée des proportions de zéros",
       xlab = "Proportion de zéros")
  abline(v = proportion_observée_n, col = "red", lwd = 2)
}
# Appliquer la fonction à chaque modèle dans la liste
mapply(simulate_proportions_p, model_list_p, df_list_p)
mapply(simulate_proportions_n, model_list_n, df_list_n)

#AUTOMATIC 0 inf check 
# Liste des modèles
model_list <- list(
  mod_p_cf_5sec, mod_p_cf_1min, mod_p_cf_5min, mod_p_cf_10min, mod_p_cf_1hour,
  mod_n_cf_5sec,mod_n_cf_1min, mod_n_cf_5min, mod_n_cf_10min, mod_n_cf_1hour
)

# Noms des modèles
model_names <- c("mod_p_cf_5sec","mod_p_cf_1min", "mod_p_cf_5min", "mod_p_cf_10min", "mod_p_cf_1hour", "mod_p_cf_night",
                 "mod_n_cf_5sec","mod_n_cf_1min", "mod_n_cf_5min", "mod_n_cf_10min", "mod_n_cf_1hour", "mod_n_cf_night")

# Fonction pour vérifier la surabondance de zéros et la surdispersion
check_models <- function(model_list, model_names) {
  for (i in seq_along(model_list)) {
    cat("Checking model:", model_names[i], "\n")
    print(check_zeroinflation(model_list[[i]])) #modif pour 
  }
}

# Exécuter
check_models(model_list, model_names)


#RESIDUALS
#test résidus (pas fait)
#Pippip
res_mod_p_cf_5sec <- resid(mod_p_cf_5sec)
res_mod_p_cf_1min <-  resid(mod_p_cf_1min)
res_mod_p_cf_5min <-  resid(mod_p_cf_5min)
res_mod_p_cf_10min <- resid(mod_p_cf_10min)   
res_mod_p_cf_1hour <- resid(mod_p_cf_1hour) 
res_mod_p_cf_night <- resid(mod_p_cf_night) 

hist(res_mod_p_cf_5sec, breaks = 500, main = "Histogramme des résidus", xlim = c(-10,10))
hist(res_mod_p_cf_1min, breaks = 500, main = "Histogramme des résidus", xlim = c(-10,10))
hist(res_mod_p_cf_5min, breaks = 100, main = "Histogramme des résidus", xlim = c(-10,10))
hist(res_mod_p_cf_10min, breaks = 100, main = "Histogramme des résidus", xlim = c(-10,10))
hist(res_mod_p_cf_1hour, breaks = 100, main = "Histogramme des résidus", xlim = c(-10,10))
hist(res_mod_p_cf_night, breaks = 100, main = "Histogramme des résidus",  xlim = c(-2,2))

res_mod_p_cf_5sec  <- simulateResiduals(fittedModel = mod_p_cf_5sec)
res_mod_p_cf_1min  <- simulateResiduals(fittedModel = mod_p_cf_1min)
res_mod_p_cf_5min  <- simulateResiduals(fittedModel = mod_p_cf_5min)
res_mod_p_cf_10min <- simulateResiduals(fittedModel = mod_p_cf_10min)
res_mod_p_cf_1hour <- simulateResiduals(fittedModel = mod_p_cf_1hour)
res_mod_p_cf_night <- simulateResiduals(fittedModel = mod_p_cf_night)

plot(res_mod_p_cf_5sec)
plot(res_mod_p_cf_1min)
plot(res_mod_p_cf_5min)
plot(res_mod_p_cf_10min)
plot(res_mod_p_cf_1hour)
plot(res_mod_p_cf_night)

testOverdispersion(res_mod_p_cf_5sec)
testOverdispersion(res_mod_p_cf_1min)
testOverdispersion(res_mod_p_cf_5min)
testOverdispersion(res_mod_p_cf_10min)
testOverdispersion(res_mod_p_cf_1hour)
testOverdispersion(res_mod_p_cf_night)

testZeroInflation(res_mod_p_cf_5sec) 
testZeroInflation(res_mod_p_cf_1min) 
testZeroInflation(res_mod_p_cf_5min) 
testZeroInflation(res_mod_p_cf_10min)
testZeroInflation(res_mod_p_cf_1hour)
testZeroInflation(res_mod_p_cf_night)


#Nyclei
res_mod_n_cf_5sec <-  resid(mod_n_cf_5sec)
res_mod_n_cf_1min <-  resid(mod_n_cf_1min)
res_mod_n_cf_5min <-  resid(mod_n_cf_5min)
res_mod_n_cf_10min <- resid(mod_n_cf_10min)   
res_mod_n_cf_1hour <- resid(mod_n_cf_1hour) 
res_mod_n_cf_night <- resid(mod_n_cf_night) 

hist(res_mod_n_cf_5sec, breaks = 300, main = "Histogramme des résidus", xlim = c(-10,10))
hist(res_mod_n_cf_1min, breaks = 300, main = "Histogramme des résidus", xlim = c(-10,10))
hist(res_mod_n_cf_5min, breaks = 300, main = "Histogramme des résidus", xlim = c(-10,10))
hist(res_mod_n_cf_10min, breaks = 300, main = "Histogramme des résidus", xlim = c(-10,10))
hist(res_mod_n_cf_1hour, breaks = 100, main = "Histogramme des résidus", xlim = c(-5,5))
hist(res_mod_n_cf_night, breaks = 100, main = "Histogramme des résidus", xlim = c(-2,2))

res_mod_n_cf_5sec  <- simulateResiduals(fittedModel = mod_n_cf_5sec)
res_mod_n_cf_1min  <- simulateResiduals(fittedModel = mod_n_cf_1min)
res_mod_n_cf_5min  <- simulateResiduals(fittedModel = mod_n_cf_5min)
res_mod_n_cf_10min <- simulateResiduals(fittedModel = mod_n_cf_10min)
res_mod_n_cf_1hour <- simulateResiduals(fittedModel = mod_n_cf_1hour)
res_mod_n_cf_night <- simulateResiduals(fittedModel = mod_n_cf_night)

plot(res_mod_n_cf_5sec)
plot(res_mod_n_cf_1min)
plot(res_mod_n_cf_5min)
plot(res_mod_n_cf_10min)
plot(res_mod_n_cf_1hour)
plot(res_mod_n_cf_night)

testOverdispersion(res_mod_n_cf_1min)
testOverdispersion(res_mod_n_cf_5min)
testOverdispersion(res_mod_n_cf_10min)
testOverdispersion(res_mod_n_cf_1hour)
testOverdispersion(res_mod_n_cf_night)

testZeroInflation(res_mod_n_cf_1min) 
testZeroInflation(res_mod_n_cf_5min) 
testZeroInflation(res_mod_n_cf_10min)
testZeroInflation(res_mod_n_cf_1hour)
testZeroInflation(res_mod_n_cf_night)


#couleur dégradé 
#asterix avec couleur pour chaque courbe
#tableau summary avec *** à la place des p-value

# RESULTS PIPPIP -----------------------------------------------------------------
#get significative diff (grosse fonction qui prend du temps)
fit_model_with_ref <- function(data, ref_level, family, metric) {
  data$recorder <- relevel(factor(data$recorder), ref = ref_level)
  model <- glmmTMB(Pippip ~ recorder + windspeed_scaled + rain_scaled + poly(julian_day_scaled, 2) + (1 | id_parc/id_eol), 
                   data = data, family = family)
  
  # Extraire les coefficients et les p-values
  model_summary <- tidy(model)
  
  # Filtrer pour conserver uniquement les termes liés à "recorder"
  recorder_terms <- grep("^recorder", model_summary$term)
  model_summary <- model_summary[recorder_terms, ]
  
  # Ajouter les informations sur le niveau de référence et la métrique
  model_summary$ref_level <- ref_level
  model_summary$metric <- metric
  
  return(model_summary)
}

# Niveaux de 'recorder'
recorder_levels <- c("Batmode", "SM3", "SM4", "Batcorder")

# Liste des datasets et leurs familles associées
datasets <- list(df_5sec = df_5sec, df_1min = df_1min, df_5min = df_5min, df_10min = df_10min, df_1hour = df_1hour, df_night = df_night)
families <- list(nbinom2(), nbinom2(), nbinom2(), nbinom2(), nbinom2(), binomial())
metrics <- c("5sec", "1min", "5min", "10min", "1hour", "night")

# Stocker les résultats
results <- list()

# Ajuster les modèles pour chaque niveau de référence et chaque dataset
for (i in seq_along(datasets)) {
  df <- datasets[[i]]
  family <- families[[i]]
  metric <- metrics[i]
  
  for (ref in recorder_levels) {
    model_summary <- fit_model_with_ref(df, ref, family, metric)
    results[[paste0(ref, "_", metric)]] <- model_summary
  }
}

# Combiner tous les résultats
combined_results <- do.call(rbind, results)
# Réorganiser la table pour avoir les métriques de temps en première colonne
combined_results <- combined_results %>% 
  select(metric, everything())

# Filtrer pour conserver uniquement les paires significatives (par exemple, p-value < 0.05)
significant_comparisons <- combined_results %>%
  filter(p.value < 0.05 & term != "(Intercept)")

significant_comparisons <- significant_comparisons %>% 
  select(metric,term,p.value,ref_level)

significance_stars <- function(p_value) {
  if (length(p_value) == 0 || is.null(p_value)) {
    return("")
  } else if (is.na(p_value)) {
    return("")
  } else if (p_value < 0.001) {
    return("***")
  } else if (p_value < 0.01) {
    return("**")
  } else if (p_value < 0.05) {
    return("*")
  } else if (p_value < 0.1) {
    return(".")
  } else {
    return("")
  }
}

# Appliquer la fonction pour remplacer les p-values par des étoiles
significant_comparisons <- significant_comparisons %>%
  mutate(p.value = sapply(p.value, significance_stars))

#BOXPLOT PREDIC
pred_bp_mod0 <- ggpredict(mod_p_cf_5sec, terms = c("recorder[all]"))
pred_bp_mod1 <- ggpredict(mod_p_cf_1min, terms = c("recorder[all]"))
pred_bp_mod2 <- ggpredict(mod_p_cf_5min, terms = c("recorder[all]"))
pred_bp_mod3 <- ggpredict(mod_p_cf_10min, terms =c("recorder[all]"))
pred_bp_mod4 <- ggpredict(mod_p_cf_1hour, terms =c("recorder[all]"))
pred_bp_mod5 <- ggpredict(mod_p_cf_night, terms =c("recorder[all]"))

# Combine les résultats de ggpredict si nécessaire
combined_predictions <- bind_rows(
  mutate(pred_bp_mod0, Metric = "5sec"),
  mutate(pred_bp_mod1, Metric = "1min"),
  mutate(pred_bp_mod2, Metric = "5min"),
  mutate(pred_bp_mod3, Metric = "10min"),
  mutate(pred_bp_mod4, Metric = "1hour"),
  mutate(pred_bp_mod5, Metric = "night")
)

# Transformer la variable Metric en facteur avec les niveaux dans l'ordre souhaité
combined_predictions$Metric <- factor(combined_predictions$Metric, 
                                      levels = c("5sec","1min", "5min", "10min", "1hour", "night"))
combined_predictions <- combined_predictions %>%
  drop_na()

combined_predictions <- combined_predictions %>%
  rename(recorder = x)


# Créer les boxplots avec ggplot2 
ggplot(combined_predictions, aes(x = recorder, y = predicted, color = recorder)) +
  geom_point(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, position = position_dodge(width = 1)) +
  labs(x = "", y = bquote("Nombre de détections ("~italic("P. pipistrellus")~")"), color = "Enregistreur") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ Metric, scales = "free_x", nrow = 1) +
  scale_y_continuous(trans='log10') +
  geom_signif(data = subset(combined_predictions, Metric == "5sec"),
    comparisons = list(c("Batcorder", "Batmode")),
    annotations = c("***"),
    y_position = -1.93,
    tip_length = -0.003,
    vjust = 1.8,
    colour = "black",
    size = 0.4
  ) +
  geom_signif(
    data = subset(combined_predictions, Metric == "5sec"),
    comparisons = list(c("Batmode", "SM4")),
    annotations = c("**"),
    y_position = -2,
    tip_length = -0.003,
    vjust = 1.8,
    colour = "black",
    size = 0.4
  ) +
  geom_signif(
    data = subset(combined_predictions, Metric == "1min"),
    comparisons = list(c("Batcorder", "Batmode")),
    annotations = c("***"),
    y_position = -1.93,
    tip_length = -0.003,
    vjust = 1.8,
    colour = "black",
    size = 0.4
  ) +
  geom_signif(
    data = subset(combined_predictions, Metric == "1min"),
    comparisons = list(c("Batmode", "SM4")),
    annotations = c("**"),
    y_position = -2,
    tip_length = -0.003,
    vjust = 1.8,
    colour = "black",
    size = 0.4
  ) +
  geom_signif(
    data = subset(combined_predictions, Metric == "5min"),
    comparisons = list(c("Batmode", "Batcorder")),
    annotations = c("**"),
    y_position = -1.93,
    tip_length = -0.003,
    vjust = 1.8,
    colour = "black",
    size = 0.4
  ) +
  geom_signif(
    data = subset(combined_predictions, Metric == "5min"),
    comparisons = list(c("Batmode", "SM4")),
    annotations = c("***"),
    y_position = -2,
    tip_length = -0.003,
    vjust = 1.8,
    colour = "black",
    size = 0.4
  ) +
  geom_signif(
    data = subset(combined_predictions, Metric == "10min"),
    comparisons = list(c("Batcorder", "Batmode")),
    annotations = c("**"),
    y_position = -1.93,
    tip_length = -0.003,
    vjust = 1.8,
    colour = "black",
    size = 0.4
  ) +
  geom_signif(
    data = subset(combined_predictions, Metric == "10min"),
    comparisons = list(c("Batmode", "SM4")),
    annotations = c("**"),
    y_position = -2,
    tip_length = -0.003,
    vjust = 1.8,
    colour = "black",
    size = 0.4
  ) +
  geom_signif(
    data = subset(combined_predictions, Metric == "1hour"),
    comparisons = list(c("Batcorder", "Batmode")),
    annotations = c("**"),
    y_position = -1.93,
    tip_length = -0.003,
    vjust = 1.8,
    colour = "black",
    size = 0.4
  ) +
  geom_signif(
    data = subset(combined_predictions, Metric == "1hour"),
    comparisons = list(c("Batmode", "SM4")),
    annotations = c("**"),
    y_position = -2,
    tip_length = -0.003,
    vjust = 1.8,
    colour = "black",
    size = 0.4
  ) +
  geom_signif(
    data = subset(combined_predictions, Metric == "night"),
    comparisons = list(c("Batcorder", "Batmode")),
    annotations = c("*"),
    y_position = -2,
    tip_length = -0.003,
    vjust = 1.8,
    colour = "black",
    size = 0.4
  ) 

#fonction to get back to original scale
inverse_scale <- function(x, mean, std_dev) {
  return ((x * std_dev) + mean)
}

# Y ~ jday ----------------------------------------------------------------
#geom_line jday
pred_p_jday_mod0 <- ggpredict(mod_p_cf_5sec,  terms = c("julian_day_scaled[all]"))
pred_p_jday_mod1 <- ggpredict(mod_p_cf_1min,  terms = c("julian_day_scaled[all]"))
pred_p_jday_mod2 <- ggpredict(mod_p_cf_5min,  terms = c("julian_day_scaled[all]"))
pred_p_jday_mod3 <- ggpredict(mod_p_cf_10min, terms = c("julian_day_scaled[all]"))
pred_p_jday_mod4 <- ggpredict(mod_p_cf_1hour, terms = c("julian_day_scaled[all]"))
pred_p_jday_mod5 <- ggpredict(mod_p_cf_night, terms = c("julian_day_scaled[all]"))

# Ajouter la colonne Metric à chaque prédiction
pred_p_jday_mod0$Metric <- "5sec"
pred_p_jday_mod1$Metric <- "1min"
pred_p_jday_mod2$Metric <- "5min"
pred_p_jday_mod3$Metric <- "10min"
pred_p_jday_mod4$Metric <- "1hour"
pred_p_jday_mod5$Metric <- "night"


# Combiner les prédictions dans un seul dataframe
comb_pred_jday <- bind_rows(pred_p_jday_mod0,pred_p_jday_mod1, pred_p_jday_mod2, pred_p_jday_mod3, pred_p_jday_mod4, pred_p_jday_mod5)

comb_pred_jday$Metric <- factor(comb_pred_jday$Metric, 
                                levels = c("5sec","1min", "5min", "10min", "1hour", "night"))
# Vérifiez et renommez les colonnes si nécessaire
comb_pred_jday <- comb_pred_jday %>%
  rename(
    julian_day = x, 
    recorder = group)

mean_julian_day <- mean(df_5sec$julian_day, na.rm = TRUE)  # Remplacez df_5sec par votre dataframe original
std_dev_julian_day <- sd(df_5sec$julian_day, na.rm = TRUE)  # Remplacez df_5sec par votre dataframe original
comb_pred_jday$julian_day <- inverse_scale(comb_pred_jday$julian_day, mean_julian_day, std_dev_julian_day)

#plot !!
ggplot(comb_pred_jday, aes(x = julian_day, y = predicted, color = Metric)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1, outline.type = "both", size = 0.1) +
  #scale_y_continuous(trans = "log10") +
  labs(x = "Jour julien", y = bquote("Nombre de contacts ("~italic("P. pipistrellus")~")"), color = "Intervalle temporel") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_color_brewer(palette = "Paired") + 
  scale_color_viridis_d(labels = c("5sec ***", "1min ***", "5min ***", "10min ***", "1heure ***", "nuit ***"))



# Y ~ vent  -----------------------------------------------------
#geom_line vent
pred_p_wind_mod0 <- ggpredict(mod_p_cf_5sec,  terms = c("windspeed_scaled[all]"))#, "recorder"))
pred_p_wind_mod1 <- ggpredict(mod_p_cf_1min,  terms = c("windspeed_scaled[all]"))#, "recorder"))
pred_p_wind_mod2 <- ggpredict(mod_p_cf_5min,  terms = c("windspeed_scaled[all]"))#, "recorder"))
pred_p_wind_mod3 <- ggpredict(mod_p_cf_10min, terms = c("windspeed_scaled[all]"))#, "recorder"))
pred_p_wind_mod4 <- ggpredict(mod_p_cf_1hour, terms = c("windspeed_scaled[all]"))#, "recorder"))
pred_p_wind_mod5 <- ggpredict(mod_p_cf_night, terms = c("windspeed_scaled[all]"))#, "recorder"))

# Ajouter la colonne Metric à chaque prédiction
pred_p_wind_mod0$Metric <- "5sec"
pred_p_wind_mod1$Metric <- "1min"
pred_p_wind_mod2$Metric <- "5min"
pred_p_wind_mod3$Metric <- "10min"
pred_p_wind_mod4$Metric <- "1hour"
pred_p_wind_mod5$Metric <- "night"

# Combiner les prédictions dans un seul dataframe
comb_pred_wind <- bind_rows(pred_p_wind_mod0,
                            pred_p_wind_mod1, 
                            pred_p_wind_mod2, 
                            pred_p_wind_mod3, 
                            pred_p_wind_mod4, 
                            pred_p_wind_mod5)


# Vérifiez et renommez les colonnes si nécessaire
comb_pred_wind <- comb_pred_wind %>%
  rename(
    mean_windspeed.ms = x, 
    recorder = group,
    predicted = predicted, 
    conf.low = conf.low, 
    conf.high = conf.high
  )


comb_pred_wind$Metric <- factor(comb_pred_wind$Metric, 
                                levels = c("5sec","1min", "5min", "10min", "1hour", "night"))

mean_windspeed <- mean(df_5sec$mean_windspeed.ms, na.rm = TRUE)  # Remplacez df_5sec par votre dataframe original
std_dev_windspeed <- sd(df_5sec$mean_windspeed.ms, na.rm = TRUE)  # Remplacez df_5sec par votre dataframe original
comb_pred_wind$mean_windspeed.ms <- inverse_scale(comb_pred_wind$mean_windspeed.ms, mean_windspeed, std_dev_windspeed)

#plot !!
ggplot(comb_pred_wind, aes(x = mean_windspeed.ms, y = predicted, color = Metric)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1, outline.type = "both", size = 0.1) +
  #scale_y_continuous(trans = "log10") +
  labs(x = "Vitesse du vent (m/s)", y = bquote("Nombre de contacts ("~italic("P. pipistrellus")~")"), color = "Intervalle temporel") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_viridis_d(labels = c("5sec ***", "1min ***", "5min ***", "10min ***", "1heure ***", "nuit ***"))



# Y ~ pluie -----------------------------------------------------

#geom_line pluie
pred_p_rain_mod0 <- ggpredict(mod_p_cf_5sec,  terms = c("rain_scaled[all]"))#, "recorder"))
pred_p_rain_mod1 <- ggpredict(mod_p_cf_1min,  terms = c("rain_scaled[all]"))#, "recorder"))
pred_p_rain_mod2 <- ggpredict(mod_p_cf_5min,  terms = c("rain_scaled[all]"))#, "recorder"))
pred_p_rain_mod3 <- ggpredict(mod_p_cf_10min, terms = c("rain_scaled[all]"))#, "recorder"))
pred_p_rain_mod4 <- ggpredict(mod_p_cf_1hour, terms = c("rain_scaled[all]"))#, "recorder"))
pred_p_rain_mod5 <- ggpredict(mod_p_cf_night, terms = c("rain_scaled[all]"))#, "recorder"))

# Ajouter la colonne Metric à chaque prédiction
pred_p_rain_mod0$Metric <- "5sec"
pred_p_rain_mod1$Metric <- "1min"
pred_p_rain_mod2$Metric <- "5min"
pred_p_rain_mod3$Metric <- "10min"
pred_p_rain_mod4$Metric <- "1hour"
pred_p_rain_mod5$Metric <- "night"

# Combiner les prédictions dans un seul dataframe
comb_pred_rain <- bind_rows(pred_p_rain_mod0,
                            pred_p_rain_mod1, 
                            pred_p_rain_mod2, 
                            pred_p_rain_mod3, 
                            pred_p_rain_mod4, 
                            pred_p_rain_mod5)

# Vérifiez et renommez les colonnes si nécessaire
comb_pred_rain <- comb_pred_rain %>%
  rename(cumulated_rain.mm = x, 
    recorder = group)

comb_pred_rain$Metric <- factor(comb_pred_rain$Metric, 
                                levels = c("5sec","1min", "5min", "10min", "1hour", "night"))

mean_rain <- mean(df_5sec$cumulated_rain.mm, na.rm = TRUE)  # Remplacez df_5sec par votre dataframe original
std_dev_rain <- sd(df_5sec$cumulated_rain.mm, na.rm = TRUE)  # Remplacez df_5sec par votre dataframe original
comb_pred_rain$cumulated_rain.mm <- inverse_scale(comb_pred_rain$cumulated_rain.mm, mean_rain, std_dev_rain)

#plot !!
ggplot(comb_pred_rain, aes(x = cumulated_rain.mm, y = predicted, color = Metric)) +
  geom_line() +
  #scale_y_continuous(trans = 'log10')+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1, outline.type = "both", size = 0.1) +
  labs(x = "Accumulation de pluie (mm)", y = bquote("Nombre de contacts ("~italic("P. pipistrellus")~")"), color = "Intervalle temporel") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_color_viridis_d(labels = c("5sec **", "1min **", "5min ***", "10min ***", "1heure ***", "nuit ***"))




# # # RESULTS NYCLEI -----------------------------------------------------------------
#get significative diff
fit_model_with_ref_n <- function(data, ref_level, family, metric) {
  data$recorder <- relevel(factor(data$recorder), ref = ref_level)
  model_n <- glmmTMB(Nyclei ~ recorder + windspeed_scaled + rain_scaled + poly(julian_day_scaled, 2) + (1 | id_parc/id_eol), 
                   data = data, family = family)
  
  # Extraire les coefficients et les p-values
  model_summary_n <- tidy(model_n)
  
  # Filtrer pour conserver uniquement les termes liés à "recorder"
  recorder_terms_n <- grep("^recorder", model_summary_n$term)
  model_summary_n <- model_summary_n[recorder_terms_n, ]
  
  # Ajouter les informations sur le niveau de référence et la métrique
  model_summary_n$ref_level <- ref_level
  model_summary_n$metric <- metric
  
  return(model_summary_n)
}

# Niveaux de 'recorder'
recorder_levels_n <- c("Batmode", "SM3", "SM4", "Batcorder")

# Liste des datasets et leurs familles associées
datasets_n <- list(df_5sec = df_5sec, df_1min = df_1min, df_5min = df_5min, df_10min = df_10min, df_1hour = df_1hour, df_night = df_night)
families_n <- list(nbinom2(), nbinom2(), nbinom2(), nbinom2(), nbinom2(), binomial())
metrics_n <- c("5sec", "1min", "5min", "10min", "1hour", "night")

# Stocker les résultats
results_n <- list()

# Ajuster les modèles pour chaque niveau de référence et chaque dataset
for (i in seq_along(datasets)) {
  df <- datasets_n[[i]]
  family <- families_n[[i]]
  metric <- metrics_n[i]
  
  for (ref in recorder_levels) {
    model_summary_n <- fit_model_with_ref_n(df, ref, family, metric)
    results_n[[paste0(ref, "_", metric)]] <- model_summary_n
  }
}

# Combiner tous les résultats
combined_results_n <- do.call(rbind, results_n)

# Réorganiser la table pour avoir les métriques de temps en première colonne
combined_results_n <- combined_results_n %>%
  select(metric, everything())

# Filtrer pour conserver uniquement les paires significatives (par exemple, p-value < 0.05)
significant_comparisons_n <- combined_results_n %>%
  filter(p.value < 0.05 & term != "(Intercept)")

# Afficher les résultats significatifs
print(significant_comparisons_n)

significant_comparisons_n <- significant_comparisons_n %>% 
  select(metric,term,p.value,ref_level)

# Ajouter les étoiles de significativité
significance_stars_n <- function(p_value) {
  if (is.na(p_value)) {
    return("")
  } else if (p_value < 0.001) {
    return("***")
  } else if (p_value < 0.01) {
    return("**")
  } else if (p_value < 0.05) {
    return("*")
  } else if (p_value < 0.1) {
    return(".")
  } else {
    return("")
  }
}

# Appliquer la fonction pour remplacer les p-values par des étoiles
significant_comparisons_n <- significant_comparisons_n %>%
  mutate(p.value = sapply(p.value, significance_stars_n))

significant_comparisons_n <- significant_comparisons_n %>%
  filter(ref_level != "Batcorder" & term != "recorderBatcorder")

# Exemple de prédictions pour une seule période de temps (par exemple, 1min)
#BOXPLOT predict
pred_bp_n_mod0 <- ggpredict(mod_n_cf_5sec,  terms = c("recorder[all]"))
pred_bp_n_mod1 <- ggpredict(mod_n_cf_1min,  terms = c("recorder[all]"))
pred_bp_n_mod2 <- ggpredict(mod_n_cf_5min,  terms = c("recorder[all]"))
pred_bp_n_mod3 <- ggpredict(mod_n_cf_10min, terms = c("recorder[all]"))
pred_bp_n_mod4 <- ggpredict(mod_n_cf_1hour, terms = c("recorder[all]"))
pred_bp_n_mod5 <- ggpredict(mod_n_cf_night, terms = c("recorder[all]"))

# Combine les résultats de ggpredict si nécessaire
combined_n_predictions <- bind_rows(
  mutate(pred_bp_n_mod0, Metric = "5sec"),
  mutate(pred_bp_n_mod1, Metric = "1min"),
  mutate(pred_bp_n_mod2, Metric = "5min"),
  mutate(pred_bp_n_mod3, Metric = "10min"),
  mutate(pred_bp_n_mod4, Metric = "1hour"),
  mutate(pred_bp_n_mod5, Metric = "night")
)

combined_n_predictions$Metric <- factor(combined_n_predictions$Metric, 
                                        levels = c("5sec","1min", "5min", "10min", "1hour", "night"))

combined_n_predictions <- combined_n_predictions %>%
  rename(recorder = x)

combined_n_predictions<- combined_n_predictions %>%
  drop_na()

# Créer les boxplots avec ggplot2
ggplot(combined_n_predictions, aes(x = recorder, y = predicted, color = recorder)) +
  geom_point(stat = "identity", position = "dodge") +
  #scale_y_continuous(trans = 'log10') +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, position = position_dodge(width = 0.9)) +
  labs(x = "", y = bquote("Nombre de détections ("~italic("N. leisleri")~")"), color = "Enregistreur") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ Metric, scales = "free_x", nrow = 1) +
  geom_signif(
    data = subset(combined_n_predictions, Metric == "5sec"),
    comparisons = list(c("Batmode", "SM4")),
    annotations = c("*"),
    y_position = -0.3,
    tip_length = -0.003,
    vjust = 1.8,
    colour = "black",
    size = 0.4
  )  +
  geom_signif(
    data = subset(combined_n_predictions, Metric == "1min"),
    comparisons = list(c("Batmode", "SM4")),
    annotations = c("."),
    y_position = -0.3,
    tip_length = -0.003,
    vjust = 1.8,
    colour = "black",
    size = 0.4
  ) +
  geom_signif(
    data = subset(combined_n_predictions, Metric == "5min"),
    comparisons = list(c("Batmode", "SM4")),
    annotations = c("*"),
    y_position = -0.3,
    tip_length = -0.003,
    vjust = 1.8,
    colour = "black",
    size = 0.4
  ) +
  geom_signif(
    data = subset(combined_n_predictions, Metric == "10min"),
    comparisons = list(c("Batmode", "SM4")),
    annotations = c("*"),
    y_position = -0.3,
    tip_length = -0.003,
    vjust = 1.8,
    colour = "black",
    size = 0.4
  ) +
  geom_signif(
    data = subset(combined_n_predictions, Metric == "1hour"),
    comparisons = list(c("Batmode", "SM4")),
    annotations = c("**"),
    y_position = -0.3,
    tip_length = -0.003,
    vjust = 1.8,
    colour = "black",
    size = 0.4
  ) 


summary(pred_bp_n_mod1)
summary(pred_bp_n_mod2)
summary(pred_bp_n_mod3)
summary(pred_bp_n_mod4)
summary(pred_bp_n_mod5)

# Y ~ jday  -----------------------------------------------------
#geom_line jday
pred_n_jday_mod0 <- ggpredict(mod_n_cf_5sec,  terms = c("julian_day_scaled[all]"))
pred_n_jday_mod1 <- ggpredict(mod_n_cf_1min,  terms = c("julian_day_scaled[all]"))
pred_n_jday_mod2 <- ggpredict(mod_n_cf_5min,  terms = c("julian_day_scaled[all]"))
pred_n_jday_mod3 <- ggpredict(mod_n_cf_10min, terms = c("julian_day_scaled[all]"))
pred_n_jday_mod4 <- ggpredict(mod_n_cf_1hour, terms = c("julian_day_scaled[all]"))
pred_n_jday_mod5 <- ggpredict(mod_n_cf_night, terms = c("julian_day_scaled[all]"))


# Ajouter la colonne Metric à chaque prédiction
pred_n_jday_mod0$Metric <- "5sec"
pred_n_jday_mod1$Metric <- "1min"
pred_n_jday_mod2$Metric <- "5min"
pred_n_jday_mod3$Metric <- "10min"
pred_n_jday_mod4$Metric <- "1hour"
pred_n_jday_mod5$Metric <- "night"



# Combiner les prédictions dans un seul dataframe
comb_pred_n_jday <- bind_rows(pred_n_jday_mod0,
                              pred_n_jday_mod1, 
                              pred_n_jday_mod2, 
                              pred_n_jday_mod3, 
                              pred_n_jday_mod4, 
                              pred_n_jday_mod5)

comb_pred_n_jday$Metric <- factor(comb_pred_n_jday$Metric, 
                                levels = c("5sec","1min", "5min", "10min", "1hour", "night"))



# Vérifiez et renommez les colonnes si nécessaire
comb_pred_n_jday <- comb_pred_n_jday %>%
  rename(
    julian_day = x, 
    #recorder = group,
    predicted = predicted, 
    conf.low = conf.low, 
    conf.high = conf.high
  )

mean_jday_n <- mean(df_5sec$julian_day, na.rm = TRUE)  # Remplacez df_5sec par votre dataframe original
std_dev_jday_n <- sd(df_5sec$julian_day, na.rm = TRUE)  # Remplacez df_5sec par votre dataframe original
comb_pred_n_jday$julian_day <- inverse_scale(comb_pred_n_jday$julian_day, mean_jday_n, std_dev_jday_n)

#plot !!
ggplot(comb_pred_n_jday, aes(x = julian_day, y = predicted, color = Metric)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1, outline.type = "both", size = 0.1) +
  #facet_wrap(~ recorder, scales = "free_y") +
  labs(x = "Jour julien", y = bquote("Nombre de contacts ("~italic("N. leisleri")~")"), color = "Intervalle temporel") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_viridis_d(labels = c("5sec ***", "1min ***", "5min **", "10min **", "1heure *", "nuit"))


# Y ~ vent  -----------------------------------------------------
#geom_line vent
pred_n_wind_mod0 <- ggpredict(mod_n_cf_5sec,  terms = c("windspeed_scaled[all]"))
pred_n_wind_mod1 <- ggpredict(mod_n_cf_1min,  terms = c("windspeed_scaled[all]"))
pred_n_wind_mod2 <- ggpredict(mod_n_cf_5min,  terms = c("windspeed_scaled[all]"))
pred_n_wind_mod3 <- ggpredict(mod_n_cf_10min, terms = c("windspeed_scaled[all]"))
pred_n_wind_mod4 <- ggpredict(mod_n_cf_1hour, terms = c("windspeed_scaled[all]"))
pred_n_wind_mod5 <- ggpredict(mod_n_cf_night, terms = c("windspeed_scaled[all]"))

# Ajouter la colonne Metric à chaque prédiction
pred_n_wind_mod0$Metric <- "5sec"
pred_n_wind_mod1$Metric <- "1min"
pred_n_wind_mod2$Metric <- "5min"
pred_n_wind_mod3$Metric <- "10min"
pred_n_wind_mod4$Metric <- "1hour"
pred_n_wind_mod5$Metric <- "night"

# Combiner les prédictions dans un seul dataframe
comb_pred_n_wind <- bind_rows(pred_n_wind_mod0,
                              pred_n_wind_mod1, 
                              pred_n_wind_mod2, 
                              pred_n_wind_mod3, 
                              pred_n_wind_mod4, 
                              pred_n_wind_mod5)

comb_pred_n_wind$Metric <- factor(comb_pred_n_wind$Metric, 
                                        levels = c("5sec","1min", "5min", "10min", "1hour", "night"))

# Vérifiez et renommez les colonnes si nécessaire
comb_pred_n_wind <- comb_pred_n_wind %>%
  rename(
    mean_windspeed.ms = x, 
    recorder = group,
    predicted = predicted, 
    conf.low = conf.low, 
    conf.high = conf.high
  )

mean_windspeed <- mean(df_5sec$mean_windspeed.ms, na.rm = TRUE)  # Remplacez df_5sec par votre dataframe original
std_dev_windspeed <- sd(df_5sec$mean_windspeed.ms, na.rm = TRUE)  # Remplacez df_5sec par votre dataframe original
comb_pred_n_wind$mean_windspeed.ms <- inverse_scale(comb_pred_n_wind$mean_windspeed.ms, mean_windspeed, std_dev_windspeed)

#plot !!
ggplot(comb_pred_n_wind, aes(x = mean_windspeed.ms, y = predicted, color = Metric)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1, outline.type = "both", size = 0.1) +
  #facet_wrap(~ recorder, scales = "free_y") +
  labs(x = "Vitesse du vent (m/s)", y = bquote("Nombre de contacts ("~italic("N. leisleri")~")"), color = "Intervalle temporel") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_viridis_d(labels = c("5sec ***", "1min ***", "5min ***", "10min ***", "1heure ***", "nuit ***"))

# Y ~ pluie -----------------------------------------------------

#geom_line pluie
pred_n_rain_mod0 <- ggpredict(mod_n_cf_5sec,  terms = c("rain_scaled[all]"))#, "recorder"))
pred_n_rain_mod1 <- ggpredict(mod_n_cf_1min,  terms = c("rain_scaled[all]"))#, "recorder"))
pred_n_rain_mod2 <- ggpredict(mod_n_cf_5min,  terms = c("rain_scaled[all]"))#, "recorder"))
pred_n_rain_mod3 <- ggpredict(mod_n_cf_10min, terms = c("rain_scaled[all]"))#, "recorder"))
pred_n_rain_mod4 <- ggpredict(mod_n_cf_1hour, terms = c("rain_scaled[all]"))#, "recorder"))
pred_n_rain_mod5 <- ggpredict(mod_n_cf_night, terms = c("rain_scaled[all]"))#, "recorder"))

# Ajouter la colonne Metric à chaque prédiction
pred_n_rain_mod0$Metric <- "5sec"
pred_n_rain_mod1$Metric <- "1min"
pred_n_rain_mod2$Metric <- "5min"
pred_n_rain_mod3$Metric <- "10min"
pred_n_rain_mod4$Metric <- "1hour"
pred_n_rain_mod5$Metric <- "night"

# Combiner les prédictions dans un seul dataframe
comb_pred_n_rain <- bind_rows(pred_n_rain_mod0,
                              pred_n_rain_mod1, 
                              pred_n_rain_mod2, 
                              pred_n_rain_mod3, 
                              pred_n_rain_mod4, 
                              pred_n_rain_mod5)
comb_pred_n_rain$Metric<-factor(comb_pred_n_rain$Metric, levels = c("5sec","1min","5min","10min","1hour","night"))


# Vérifiez et renommez les colonnes si nécessaire
comb_pred_n_rain <- comb_pred_n_rain %>%
  rename(
    cumulated_rain.mm = x, 
    recorder = group,
    predicted = predicted, 
    conf.low = conf.low, 
    conf.high = conf.high
  )

mean_rain <- mean(df_5sec$cumulated_rain.mm, na.rm = TRUE)  # Remplacez df_5sec par votre dataframe original
std_dev_rain <- sd(df_5sec$cumulated_rain.mm, na.rm = TRUE)  # Remplacez df_5sec par votre dataframe original
comb_pred_n_rain$mean_windspeed.ms <- inverse_scale(comb_pred_n_rain$cumulated_rain.mm, mean_windspeed, std_dev_windspeed)

#plot !!
ggplot(comb_pred_n_rain, aes(x = cumulated_rain.mm, y = predicted, color = Metric)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1, outline.type = "both", size = 0.1) +
  #facet_wrap(~ recorder, scales = "free_y") +
  labs(x = "Accumulation de pluie (mm)", y = bquote("Nombre de contacts ("~italic("N. leisleri")~")"), color = "Intervalle temporel") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_viridis_d(labels = c("5sec **", "1min **", "5min ***", "10min ***", "1heure ***", "nuit ***"))


ggplot(comb_pred_n_rain, aes(x = cumulated_rain.mm, y = predicted, color = Metric, fill = Metric)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1, color = NA) + # Color matches line, fill matches legend
  labs(x = "Accumulation de pluie (mm)", y = bquote("Nombre de contacts ("~italic("N. leisleri")~")"), color = "Intervalle temporel", fill = "Intervalle temporel") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") + # Ensure a single legend
  scale_color_viridis_d(labels = c("5sec **", "1min **", "5min ***", "10min ***", "1heure ***", "nuit ***")) +
  scale_fill_viridis_d(labels = c("5sec **", "1min **", "5min ***", "10min ***", "1heure ***", "nuit ***"))

# ADD STAR SIGN PIPPIP-----------------------------------------------------------

#ADD star of p_value
mod_p_list <- list(
  "5sec" = mod_p_cf_5sec, 
  "1min" = mod_p_cf_1min, 
  "5min" = mod_p_cf_5min,
  "10min" = mod_p_cf_10min,
  "1hour" = mod_p_cf_1hour,
  "night" = mod_p_cf_night
)

# Fonction pour transformer les modèles en résumés
transform_model <- function(model, metric) {
  tidy_model <- tidy(model)
  tidy_model <- tidy_model %>% select(term,p.value)
  tidy_model$metric <- metric
  return(tidy_model)
}

# Appliquer la fonction à chaque modèle
transformed_p_list <- lapply(names(mod_p_list), function(x) transform_model(mod_p_list[[x]], x))

# Combiner tous les résumés dans une table
combined_p_table <- bind_rows(transformed_p_list)

# Réorganiser la table pour avoir les métriques de temps en première colonne
combined_p_table <- combined_p_table %>% select(metric, term, p.value)

# Afficher le tableau final
print(combined_p_table)

combined_p_table <- combined_p_table %>%
  pivot_wider(names_from = term, values_from = c(p.value))

# Afficher la table finale
print(combined_p_table)

# Ajouter les étoiles de significativité
significance_stars <- function(p_value) {
  if (is.na(p_value)) {
    return("")
  } else if (p_value < 0.001) {
    return("***")
  } else if (p_value < 0.01) {
    return("**")
  } else if (p_value < 0.05) {
    return("*")
  } else if (p_value < 0.1) {
    return(".")
  } else {
    return("")
  }
}

# Appliquer la fonction pour remplacer les p-values par des étoiles
for (col in 2:9) {
  combined_p_table[[col]] <- sapply(combined_p_table[[col]], significance_stars)
}

# Afficher la table finale
print(combined_p_table)

# ADD STAR SIGN NYCLEI-----------------------------------------------------------

#ADD star of p_value
mod_n_list <- list(
  "5sec" = mod_n_cf_5sec, 
  "1min" = mod_n_cf_1min, 
  "5min" = mod_n_cf_5min,
  "10min" = mod_n_cf_10min,
  "1hour" = mod_n_cf_1hour,
  "night" = mod_n_cf_night
)

# Fonction pour transformer les modèles en résumés
transform_n_model <- function(model, metric) {
  tidy_model <- tidy(model)
  tidy_model <- tidy_model %>% select(term,p.value)
  tidy_model$metric <- metric
  return(tidy_model)
}

mod_1<-tidy(mod_p_cf_5min)
# Appliquer la fonction à chaque modèle
transformed_n_list <- lapply(names(mod_n_list), function(x) transform_n_model(mod_n_list[[x]], x))

# Combiner tous les résumés dans une table
combined_n_table <- bind_rows(transformed_n_list)

# Réorganiser la table pour avoir les métriques de temps en première colonne
combined_n_table <- combined_n_table %>% select(metric, term, p.value)

# Afficher le tableau final
print(combined_n_table)

combined_n_table <- combined_n_table %>%
  pivot_wider(names_from = term, values_from = c(p.value))

# Afficher la table finale
print(combined_n_table)

# Ajouter les étoiles de significativité
significance_stars <- function(p_value) {
  if (is.na(p_value)) {
    return("")
  } else if (p_value < 0.001) {
    return("***")
  } else if (p_value < 0.01) {
    return("**")
  } else if (p_value < 0.05) {
    return("*")
  } else if (p_value < 0.1) {
    return(".")
  } else {
    return("")
  }
}

# Appliquer la fonction pour remplacer les p-values par des étoiles
for (col in 2:9) {
  combined_n_table[[col]] <- sapply(combined_n_table[[col]], significance_stars)
}

# Afficher la table finale
print(combined_n_table)

setwd("/Users/alexandregrave/Desktop/STAGE_CEFE/Data/summaries_mod")
# Sauvegarder le tableau final en CSV
write.table(combined_p_table, "final_estimate_pvalue_table_pippip.csv", sep = ";", row.names = FALSE)

#test
# Calcul de la moyenne des prédictions pour chaque métrique
mean_pred_5sec <- mean(pred_bp_n_mod0$predicted)
mean_pred_1min <- mean(pred_bp_n_mod1$predicted)
mean_pred_5min <- mean(pred_bp_n_mod2$predicted)
mean_pred_10min <- mean(pred_bp_n_mod3$predicted)
mean_pred_1hour <- mean(pred_bp_n_mod4$predicted)
mean_pred_night <- mean(pred_bp_n_mod5$predicted)

# Stockage des résultats dans un tableau
mean_preds <- data.frame(
  Metric = c("5 sec", "1 min", "5 min", "10 min", "1 hour", "Night"),
  Mean_Prediction = c(mean_pred_5sec, mean_pred_1min, mean_pred_5min, mean_pred_10min, mean_pred_1hour, mean_pred_night)
)

# Affichage du tableau
print(mean_preds)

# Calcul des différences entre les moyennes des prédictions de chaque métrique
diff_5sec_1min <- mean_pred_1min - mean_pred_5sec
diff_1min_5min <- mean_pred_5min - mean_pred_1min
diff_5min_10min <- mean_pred_10min - mean_pred_5min
diff_10min_1hour <- mean_pred_1hour - mean_pred_10min
diff_1hour_night <- mean_pred_night - mean_pred_1hour

# Stockage des résultats dans un tableau
diffs <- data.frame(
  Comparison = c("5 sec - 1 min", "1 min - 5 min", "5 min - 10 min", "10 min - 1 hour", "1 hour - Night"),
  Difference = c(diff_5sec_1min, diff_1min_5min, diff_5min_10min, diff_10min_1hour, diff_1hour_night)
)

# Affichage du tableau
print(diffs)

# Calcul de l'écart-type des prédictions pour chaque métrique
sd_pred_5sec <- sd(pred_bp_n_mod0$predicted)
sd_pred_1min <- sd(pred_bp_n_mod1$predicted)
sd_pred_5min <- sd(pred_bp_n_mod2$predicted)
sd_pred_10min <- sd(pred_bp_n_mod3$predicted)
sd_pred_1hour <- sd(pred_bp_n_mod4$predicted)
sd_pred_night <- sd(pred_bp_n_mod5$predicted)

# Stockage des résultats dans un tableau
sd_preds <- data.frame(
  Metric = c("5 sec", "1 min", "5 min", "10 min", "1 hour", "Night"),
  SD_Prediction = c(sd_pred_5sec, sd_pred_1min, sd_pred_5min, sd_pred_10min, sd_pred_1hour, sd_pred_night)
)

# Affichage du tableau
print(sd_preds)

ggplot(mean_preds, aes(x = Metric, y = Mean_Prediction)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Mean Predicted Activity by Temporal Metric",
       x = "Temporal Metric",
       y = "Mean Predicted Activity")

# Exemple de boxplot pour visualiser la distribution des prédictions
all_preds <- data.frame(
  Metric = rep(c("5 sec", "1 min", "5 min", "10 min", "1 hour", "Night"), 
               each = length(pred_bp_n_mod0$predicted)),
  Prediction = c(pred_bp_n_mod0$predicted, pred_bp_n_mod1$predicted, pred_bp_n_mod2$predicted,
                 pred_bp_n_mod3$predicted, pred_bp_n_mod4$predicted, pred_bp_n_mod5$predicted)
)

all_preds$Metric <- factor(all_preds$Metric, levels = c("5 sec", "1 min", "5 min", "10 min", "1 hour", "Night"))

ggplot(all_preds, aes(x = Metric, y = Prediction)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Activité prédite en fonction de la métrique temporelle",
       x = "Métrique temporelle",
       y = "Activité prédite")

mean_predictions <- all_preds %>%
  group_by(Metric) %>%
  summarize(mean_prediction = mean(Prediction, na.rm = TRUE))

# Afficher les résultats
print(mean_predictions)

anova_result <- aov(Prediction ~ Metric, data = all_preds)
summary(anova_result)

tukey_result <- TukeyHSD(anova_result)
print(tukey_result)


# Test post-hoc Tukey pour les comparaisons multiples
posthoc_5sec  <- summary(glht(mod_p_cf_5sec, linfct = mcp(recorder = "Tukey")))
posthoc_1min  <- summary(glht(mod_p_cf_1min, linfct = mcp(recorder = "Tukey")))
posthoc_5min  <- summary(glht(mod_p_cf_5min, linfct = mcp(recorder = "Tukey")))
posthoc_10min <- summary(glht(mod_p_cf_10min, linfct = mcp(recorder = "Tukey")))
posthoc_1hour <- summary(glht(mod_p_cf_1hour, linfct = mcp(recorder = "Tukey")))
posthoc_night <- summary(glht(mod_p_cf_night, linfct = mcp(recorder = "Tukey")))

# Afficher les résultats
posthoc_5sec
posthoc_1min
posthoc_5min
posthoc_10min
posthoc_1hour
posthoc_night


posthoc_5sec_n  <- summary(glht(mod_n_cf_5sec, linfct = mcp(recorder = "Tukey")))
posthoc_1min_n  <- summary(glht(mod_n_cf_1min, linfct = mcp(recorder = "Tukey")))
posthoc_5min_n  <- summary(glht(mod_n_cf_5min, linfct = mcp(recorder = "Tukey")))
posthoc_10min_n <- summary(glht(mod_n_cf_10min, linfct = mcp(recorder = "Tukey")))
posthoc_1hour_n <- summary(glht(mod_n_cf_1hour, linfct = mcp(recorder = "Tukey")))
posthoc_night_n <- summary(glht(mod_n_cf_night, linfct = mcp(recorder = "Tukey")))

posthoc_5sec_n 
posthoc_1min_n 
posthoc_5min_n 
posthoc_10min_n
posthoc_1hour_n
posthoc_night_n

summary_mod_n_cf_5sec<-summary(mod_n_cf_5sec)
summary_mod_n_cf_1min<-summary(mod_n_cf_1min)
summary_mod_n_cf_5min<-summary(mod_n_cf_5min)
summary_mod_n_cf_10min<-summary(mod_n_cf_10min)
summary_mod_n_cf_1hour<-summary(mod_n_cf_1hour)
summary_mod_n_cf_night<-summary(mod_n_cf_night)

summary(mod_n_cf_5sec)
summary(mod_n_cf_1min)
summary(mod_n_cf_5min)
summary(mod_n_cf_10min)
summary(mod_n_cf_1hour)
summary(mod_n_cf_night)



coef_table_5sec_n<-summary_mod_n_cf_5sec$coefficients
coef_table_1min_n<-summary_mod_n_cf_1min$coefficients
coef_table_5min_n<-summary_mod_n_cf_5min$coefficients
coef_table_10min_n<-summary_mod_n_cf_10min$coefficients
coef_table_1hour_n<-summary_mod_n_cf_1hour$coefficients
coef_table_night_n<-summary_mod_n_cf_night$coefficients

extract_coefficients <- function(model_summary){
  coef_table <- summary(model_summary)$coefficients
  if("poly(julian_day_scaled, 2)2" %in% rownames(coef_table$cond)){
    estimate <- coef_table$cond["poly(julian_day_scaled, 2)2", "Estimate"]
    std_error <- coef_table$cond["poly(julian_day_scaled, 2)2", "Std. Error"]
    return(list(estimate = estimate, std_error = std_error))
  }
}


coef_5sec_n <- extract_coefficients(mod_n_cf_5sec)
coef_1min_n <- extract_coefficients(mod_n_cf_1min)
coef_5min_n <- extract_coefficients(mod_n_cf_5min)
coef_10min_n <- extract_coefficients(mod_n_cf_10min)
coef_1hour_n <- extract_coefficients(mod_n_cf_1hour)
coef_night_n <- extract_coefficients(mod_n_cf_night)

wald_test <- function(coef1, coef2) {
  diff_coef <- coef1$estimate - coef2$estimate
  se_diff <- sqrt(coef1$std_error^2 + coef2$std_error^2)
  z_value <- diff_coef / se_diff
  p_value <- 2 * pnorm(-abs(z_value))  # Test bilatéral
  return(list(diff_coef = diff_coef, se_diff = se_diff, z_value = z_value, p_value = p_value))
}

# Comparer les coefficients entre chaque paire de modèles
comparison_5sec_vs_1min_n <- wald_test(coef_5sec_n, coef_1min_n)
comparison_1min_vs_5min_n <- wald_test(coef_1min_n, coef_5min_n)
comparison_5min_vs_10min_n <- wald_test(coef_5min_n, coef_10min_n)
comparison_10min_vs_1hour_n <- wald_test(coef_10min_n, coef_1hour_n)
comparison_1hour_vs_night_n <- wald_test(coef_1hour_n, coef_night_n)
comparison_5sec_vs_night_n <- wald_test(coef_5sec_n, coef_night_n)

print_comparison <- function(comparison, model1, model2) {
  cat(paste("Comparaison entre", model1, "et", model2, ":\n"))
  cat("Différence des coefficients: ", comparison$diff_coef, "\n")
  cat("Erreur standard de la différence: ", comparison$se_diff, "\n")
  cat("Valeur Z du test de Wald: ", comparison$z_value, "\n")
  cat("Valeur p: ", comparison$p_value, "\n\n")
}

# Afficher les résultats
print_comparison(comparison_5sec_vs_1min_n, "5 sec", "1 min")
print_comparison(comparison_1min_vs_5min_n, "1 min", "5 min")
print_comparison(comparison_5min_vs_10min_n, "5 min", "10 min")
print_comparison(comparison_10min_vs_1hour_n, "10 min", "1 hour")
print_comparison(comparison_1hour_vs_night_n, "1 hour", "night")
print_comparison(comparison_5sec_vs_night_n, "5 sec", "night")




#permutation 
extract_coefficients <- function(model_summary){
  coef_table <- summary(model_summary)$coefficients
  if("windspeed_scaled" %in% rownames(coef_table$cond)){
    estimate <- coef_table$cond["windspeed_scaled", "Estimate"]
    std_error <- coef_table$cond["windspeed_scaled", "Std. Error"]
    return(list(estimate = estimate, std_error = std_error))
  }
  return(list(estimate = NA, std_error = NA))  # Renvoie NA si la variable n'est pas présente
}

# Fonction pour la méthode par permutation
permutation_test <- function(model1, model2, data1, data2, n_permutations = 500) {
  observed_diff <- extract_coefficients(model1)$estimate - extract_coefficients(model2)$estimate
  perm_diff <- numeric(n_permutations)
  
  combined_data <- rbind(data1, data2)
  n1 <- nrow(data1)
  
  for (i in 1:n_permutations) {
    perm_indices <- sample(1:nrow(combined_data))
    perm_data1 <- combined_data[perm_indices[1:n1], ]
    perm_data2 <- combined_data[perm_indices[(n1 + 1):nrow(combined_data)], ]
    
    perm_model1 <- update(model1, data = perm_data1)
    perm_model2 <- update(model2, data = perm_data2)
    
    perm_diff[i] <- extract_coefficients(perm_model1)$estimate - extract_coefficients(perm_model2)$estimate
  }
  
  p_value <- mean(abs(perm_diff) >= abs(observed_diff))
  
  return(list(observed_diff = observed_diff, p_value = p_value))
}

# Comparer les coefficients entre chaque paire de modèles par permutation
comparison_5sec_vs_1min_n <- permutation_test(mod_n_cf_5sec, mod_n_cf_1min, df_5sec, df_1min)
comparison_1min_vs_5min_n <- permutation_test(mod_n_cf_1min, mod_n_cf_5min, df_1min, df_5min)
comparison_5min_vs_10min_n <- permutation_test(mod_n_cf_5min, mod_n_cf_10min, df_5min, df_10min)
comparison_10min_vs_1hour_n <- permutation_test(mod_n_cf_10min, mod_n_cf_1hour, df_10min, df_1hour)
comparison_1hour_vs_night_n <- permutation_test(mod_n_cf_1hour, mod_n_cf_night, df_1hour, df_night)
comparison_5sec_vs_night_n <- permutation_test(mod_n_cf_5sec, mod_n_cf_night, df_5sec, df_night)

# Fonction pour afficher les résultats des comparaisons
print_comparison <- function(comparison, model1, model2) {
  cat(paste("Comparaison entre", model1, "et", model2, ":\n"))
  cat("Différence observée des coefficients: ", comparison$observed_diff, "\n")
  cat("Valeur p: ", comparison$p_value, "\n\n")
}

# Afficher les résultats pour chaque comparaison
print_comparison(comparison_5sec_vs_1min_n, "5 sec", "1 min")
print_comparison(comparison_1min_vs_5min_n, "1 min", "5 min")
print_comparison(comparison_5min_vs_10min_n, "5 min", "10 min")
print_comparison(comparison_10min_vs_1hour_n, "10 min", "1 hour")
print_comparison(comparison_1hour_vs_night_n, "1 hour", "night")
print_comparison(comparison_5sec_vs_night_n, "5 sec", "night")

getver