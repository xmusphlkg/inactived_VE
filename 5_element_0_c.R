
# packages ----------------------------------------------------------------

library(tidyverse)
library(openxlsx)
library(lubridate)

remove(list = ls())

fill_color <- paletteer::paletteer_d("ggsci::default_nejm")

# data clean --------------------------------------------------------------

datafile_cont_raw_1 <-read.xlsx('../data/vaccine_infections.xlsx', sheet = "quanzhou")
datafile_cont_raw_2 <-read.xlsx('../data/vaccine_infections.xlsx', sheet = "ningde")
datafile_cont_raw_3 <-read.xlsx('../data/vaccine_infections.xlsx', sheet = "xiamen")[,-14]
datafile_cont_raw_1$onsetdate[is.na(datafile_cont_raw_1$onsetdate)] <- as.numeric(as.Date('2022/4/2')-as.Date('1900/1/1')+2)
datafile_cont_raw_2$onsetdate[is.na(datafile_cont_raw_2$onsetdate)] <- as.numeric(as.Date('2022/4/14')-as.Date('1900/1/1')+2)
datafile_cont_raw_3$onsetdate[is.na(datafile_cont_raw_3$onsetdate)] <- as.numeric(as.Date('2022/3/14')-as.Date('1900/1/1')+2)

datafile_cont_raw <- rbind(datafile_cont_raw_1, datafile_cont_raw_2, datafile_cont_raw_3)
remove(datafile_cont_raw_1, datafile_cont_raw_2, datafile_cont_raw_3)

## add var of vaccine product

# datafile_cont_raw$onsetdate[is.na(datafile_cont_raw$onsetdate)] <- as.numeric(as.Date('2022/4/1')-as.Date('1900/1/1')+2)

datafile_cont <- datafile_cont_raw %>%
  mutate_at(vars(contains('date')), convertToDate) %>%
  mutate(vaccine = if_else(vaccine_accept > 0 &
                             lastvaccinedate > onsetdate - 14 &
                             !is.na(lastvaccinedate),
                           vaccine_accept - 1,
                           vaccine_accept),
         firstvaccinedate_raw = firstvaccinedate,
         secondvaccinedate_raw = secondvaccinedate,
         thirdvaccinedate_raw = thirdvaccinedate) |>
  separate(vaccine_type,
           sep = '_',
           into = c('firstvaccineproduce',
                    'secondvaccineproduce',
                    'thirdvaccineproduce'))

table(datafile_cont$vaccine)
table(datafile_cont$vaccine_accept)
table(datafile_cont$vaccine_accept - datafile_cont$vaccine)

# datafile_cont$vaccine_produce[datafile_cont$vaccine_produce == ""] <- 'Unvaccinated'

## fixed last vaccine dose date
datafile_cont_dose_0 <- datafile_cont %>%
  filter(vaccine == 0) %>%
  mutate(lastvaccinedate = NA,
         firstvaccinedate = NA,
         secondvaccinedate = NA,
         thirdvaccinedate = NA,
         firstvaccineproduce = "",
         secondvaccineproduce = "",
         thirdvaccineproduce = "")

datafile_cont_dose_1 <- datafile_cont %>%
  filter(vaccine == 1) %>%
  mutate(lastvaccinedate = firstvaccinedate,
         secondvaccinedate = NA,
         thirdvaccinedate = NA,
         secondvaccineproduce = "",
         thirdvaccineproduce = "")

datafile_cont_dose_2 <- datafile_cont %>%
  filter(vaccine == 2) %>%
  mutate(lastvaccinedate = secondvaccinedate,
         thirdvaccinedate = NA,
         thirdvaccineproduce = "")

datafile_cont_dose_3 <- datafile_cont %>%
  filter(vaccine == 3) %>%
  mutate(lastvaccinedate = thirdvaccinedate)

datafile_cont <-rbind(datafile_cont_dose_3,
                      datafile_cont_dose_2,
                      datafile_cont_dose_1,
                      datafile_cont_dose_0) %>%
  mutate(vaccine_d = onsetdate - lastvaccinedate)

remove(datafile_cont_dose_3,
       datafile_cont_dose_2,
       datafile_cont_dose_1,
       datafile_cont_dose_0)

datafile_cont <- datafile_cont |>
  mutate(vaccine = if_else(vaccine > 0 & vaccine_d < 14 & !is.na(lastvaccinedate),
                           vaccine - 1,
                           vaccine),
         # gender = if_else(gender == 'Male', '1', '0'),
         age = if_else(is.na(age_inf), age, age_inf),
         age_g = if_else(age <18 & age >=0, 'c', 'a'),
         age_g = if_else(age >=65, 'o', age_g),
         age_g = factor(age_g, levels = c('c', 'a', 'o')),
         age_inf = round(age_inf),
         outcome_s = if_else(type %in% c('Mild', 'Moderate'), 1, 0),
         outcome_h = if_else(type %in% c('Moderate'), 1, 0))

## fixed last vaccine dose date
datafile_cont_dose_0 <- datafile_cont %>%
  filter(vaccine == 0) %>%
  mutate(lastvaccinedate = NA,
         firstvaccinedate = NA,
         secondvaccinedate = NA,
         thirdvaccinedate = NA,
         firstvaccineproduce = "",
         secondvaccineproduce = "",
         thirdvaccineproduce = "")

datafile_cont_dose_1 <- datafile_cont %>%
  filter(vaccine == 1) %>%
  mutate(lastvaccinedate = firstvaccinedate,
         secondvaccinedate = NA,
         thirdvaccinedate = NA,
         secondvaccineproduce = "",
         thirdvaccineproduce = "")

datafile_cont_dose_2 <- datafile_cont %>%
  filter(vaccine == 2) %>%
  mutate(lastvaccinedate = secondvaccinedate,
         thirdvaccinedate = NA,
         thirdvaccineproduce = "")

datafile_cont_dose_3 <- datafile_cont %>%
  filter(vaccine == 3) %>%
  mutate(lastvaccinedate = thirdvaccinedate)

datafile_cont <-rbind(datafile_cont_dose_3,
                      datafile_cont_dose_2,
                      datafile_cont_dose_1,
                      datafile_cont_dose_0) %>%
  mutate(vaccine_d = onsetdate - lastvaccinedate)

remove(datafile_cont_dose_3,
       datafile_cont_dose_2,
       datafile_cont_dose_1,
       datafile_cont_dose_0)

## add age group and outcome values
datafile_cont_reg <-datafile_cont |>
  mutate(vaccine_produce = paste(firstvaccineproduce, secondvaccineproduce, thirdvaccineproduce,
                                 sep = "_"),
         vaccine_produce = toupper(str_remove_all(vaccine_produce, "_")),
         vaccine_produce = sapply(vaccine_produce, FUN = function(x){
           paste(sort(unique(strsplit(x, "")[[1]])), collapse = '')
         }),
         vaccine_inactivate = case_when(
           vaccine_produce == ''& vaccine == 0 ~ "Unvaccine",
           vaccine_produce %in% c("P", "V", "PV") ~ 'Inactivate',
           TRUE ~ 'Other'
         ),
         vaccine_mix = if_else(nchar(vaccine_produce) < 2, '0', '1')) |>
  filter(vaccine_inactivate != 'Other') |>
  mutate(vaccine_g = if_else(vaccine == 0, 0, 1),
         vaccine = as.factor(vaccine))
datafile_cont_reg$vaccine_produce[datafile_cont_reg$vaccine_produce == ""] <- 'Unvaccinated'

save(datafile_cont_raw, datafile_cont_reg, datafile_cont, fill_color,
     file = './data/omicron_ve.RData')












