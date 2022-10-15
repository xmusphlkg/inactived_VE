
## VE against symptom

# packages ----------------------------------------------------------------

library(tidyverse)
library(openxlsx)
library(RColorBrewer)
library(patchwork)
library(paletteer)
library(DescrTab2)
library(ggpubr)
library(gridExtra)
library(survival)
library(survminer)

remove(list = ls())

load('./data/omicron_ve.RData')

set.seed(202205)

# data clean --------------------------------------------------------------

datafile_cont_reg <- datafile_cont_reg |>
  mutate(vaccine_expire = case_when(vaccine_g == 1 & vaccine_d <= 4*4*7 ~ 'N',
                                    vaccine_g == 1 & vaccine_d > 4*4*7 ~ 'Y',
                                    TRUE ~ 'U'))
# datafile_cont_reg$vaccine[datafile_cont_reg$vaccine_d > 365] <- '0'

table(datafile_cont_reg$vaccine, datafile_cont_reg$vaccine_expire)

datafile_cont_infections <- datafile_cont_reg |>
  filter(outcome_s == 1)
table(datafile_cont_infections$vaccine)
table(datafile_cont_infections$vaccine[datafile_cont_infections$vaccine_expire == 'N'])


# participants characteristic -----------------------------------------------

datafile_cont_table <- datafile_cont_reg |>
  mutate(title = paste(outcome_s, vaccine, sep = '_'),
         gender = str_to_title(gender),
         vaccine_produce = factor(vaccine_produce, levels = c('P', 'V', 'PV'))) |>
  select(title, outcome_s, age, age_g, gender, vaccine_produce, vaccine_expire)
datafile_cont_table$title[datafile_cont_table$outcome_s == 0] <- '0'
datafile_cont_table$title <- factor(datafile_cont_table$title,
                                    levels = c('1_0', '1_1', '1_2', '1_3', '0'))
datafile_cont_table$gender[datafile_cont_table$gender == 'Missing'] <- NA
group_labels <- list('Unvaccinated cases', '1 dose cases',
                     '2 doses cases', 'Booster dose cases',
                     'Controls')
names(group_labels) <- levels(datafile_cont_table$title)

descr(select(datafile_cont_table, -outcome_s),
      group = 'title',
      group_labels = group_labels,
      summary_stats_cont = list(
        median = DescrTab2:::.median,
        Q1 = DescrTab2:::.Q1,
        Q3 = DescrTab2:::.Q3
      ),
      reshape_rows = list(
        `Q1 - Q3` = list(args = c("Q1", "Q3"), fun = function(Q1, Q3) { paste0(Q1, "-", Q3) })),
      test_options = list(
        nonparametric = F,
        exact = F
      ),
      format_options = list(
        print_p = F,
        print_CI = F,
        print_Total = F,
        percent_suffix = "",
        combine_median_Q1_Q3 = T,
        Nmiss_row_percent = F,
        omit_missings_in_categorical_var = F,
        omit_missings_in_group = F,
        categorical_missing_percent_mode = "missing_as_regular_category",
        percent_accuracy = 0.1,
        combine_mean_sd=TRUE))


# vaccine effectiveness OR ------------------------------------------------

res_clog <- clogit(formula = outcome_s ~ vaccine + strata(age_g) + strata(gender),
                   data = datafile_cont_reg,
                   method = 'efron') %>%
  summary() %>%
  .[["conf.int"]] %>%
  as.data.frame() %>%
  select(-`exp(-coef)`) %>%
  rownames_to_column('var')
res_clog$group <- 'Symptomatic'
res_clog$adjust <- 'all'
datafile_clog <- res_clog

names(datafile_clog)[2:4] <- c('OR', 'OR_1', 'OR_2')

# vaccine effectiveness OR subgroup ---------------------------------------

for (g in levels(datafile_cont_reg$age_g)) {
  res_clog <- clogit(formula = outcome_s ~ vaccine  + strata(age_g) + strata(gender),
                     data = filter(datafile_cont_reg, age_g == g),
                     method = 'efron') %>%
    summary() %>%
    .[["conf.int"]] %>%
    as.data.frame() %>%
    select(-`exp(-coef)`) %>%
    rownames_to_column('var')
  names(res_clog)[2:4] <- c('OR', 'OR_1', 'OR_2')
  res_clog$group <- g
  res_clog$adjust <- 'age'
  datafile_clog <- rbind(datafile_clog, res_clog)
}

for (g in c('V', 'P', 'PV')) {
  res_clog <- clogit(formula = outcome_s ~ vaccine  + strata(age_g) + strata(gender),
                     data = filter(datafile_cont_reg, vaccine_produce == g | vaccine_g == 0),
                     method = 'efron') %>%
    summary() %>%
    .[["conf.int"]] %>%
    as.data.frame() %>%
    select(-`exp(-coef)`) %>%
    rownames_to_column('var')
  names(res_clog)[2:4] <- c('OR', 'OR_1', 'OR_2')
  res_clog$group <- g
  res_clog$adjust <- 'produce'
  datafile_clog <- rbind(datafile_clog, res_clog)
}

for (e in c('N', 'Y')) {
  res_clog <- clogit(formula = outcome_s ~ vaccine  + strata(age_g) + strata(gender),
                     data = filter(datafile_cont_reg, vaccine_expire %in% c(e, 'U')),
                     method = 'efron') %>%
    summary() %>%
    .[["conf.int"]] %>%
    as.data.frame() %>%
    select(-`exp(-coef)`) %>%
    rownames_to_column('var')
  names(res_clog)[2:4] <- c('OR', 'OR_1', 'OR_2')
  res_clog$group <- e
  res_clog$adjust <- 'expire'
  datafile_clog <- rbind(datafile_clog, res_clog)
}


# plot --------------------------------------------------------------------

datafile_plot <- datafile_clog
# arrange(adjust)
datafile_plot$order <- rep(rep(c(1, 3:5, 7:9, 11:12), each = 3), times = 1)
datafile_plot$vjust <- rep(c(-0.2, 0, 0.2), times = 9)
datafile_plot <- arrange(datafile_plot, order) %>%
  filter(!is.infinite(OR_2))
datafile_plot$group <- factor(datafile_plot$group, levels = unique(datafile_plot$group))

Subgroup = c('Overall',
             'By age group, y',
             '    0-17', '    18-64', '    65-',
             'By vaccine product',
             '    Sinovac',
             '    Sinopharm',
             '    Sinovac & Sinopharm',
             'By vaccine interval',
             '    2-16 weeks',
             '    >16 weeks')



fig_1 <- ggplot(data = datafile_plot,
                mapping = aes(x = OR,
                              y = order + vjust,
                              color = var))+
  geom_point()+
  geom_pointrange(mapping = aes(xmin = OR_1,
                                xmax = OR_2))+
  # annotate('rect', alpha = 0.3,
  #          xmin = 0.1, xmax = 4,
  #          ymin = -0.5, ymax = 0.5)+
  annotate('text',
           x = 1.11,
           y = -0.2,
           hjust = 0,
           size = 12*5/14,
           fontface = 'bold',
           label = 'Vaccinated associated\nwith symptom')+
  annotate('text',
           x = 0.9,
           y = -0.2,
           hjust = 1,
           fontface = 'bold',
           size = 12*5/14,
           label = 'Unvaccinated associated\nwith symptom')+
  geom_vline(xintercept = 1,
             linetype = 'dashed',
             color = 'black')+
  scale_x_continuous(limits = c(0.1, 4),
                     breaks = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.8, 1,2,3, 4),
                     # labels = c('0.1', '0.5', '1', '5'),
                     expand = c(0, 0),
                     trans = 'log10')+
  scale_y_continuous(expand = expansion(add = c(0.5, 0.7)),
                     breaks = seq(1, 12, 1),
                     # limits = c(0, 12),
                     labels = NULL,
                     # labels = Subgroup,
                     trans = 'reverse')+
  scale_color_manual(values = fill_color[c(1,3:4)],
                     labels = c('1 dose vs. Unvaccinated', '2 doses vs. Unvaccinated', 'Booster dose vs. Unvaccinated'),
                     na.translate = F)+
  theme_classic()+
  theme(axis.text.x = element_text(size = 10, hjust = .5, vjust = 0.5, face = 'plain', color = 'black'),
        axis.text.y = element_text(size = 10, hjust = 0, vjust = 0.5, face = 'plain', color = 'black'),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(0.8, 'lines'),
        plot.margin = margin(5, 20, 5, 5),
        plot.title = element_text(hjust = 0, size = 16, face = 'bold'),
        strip.text = element_text(hjust = 0, size = 16, face = 'bold'),
        strip.background = element_blank(),
        # legend.position = c(0.999, 0.999),
        # legend.justification = c(1, 1),
        legend.position = 'none',
        legend.justification = 'right',
        # legend.background = element_rect(color = 'black'),
        # legend.margin=margin(5,5,5,5),
        legend.box.margin=margin(0,-10,-10,-10),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))+
  labs(y = NULL,
       x = 'OR(95% CI)',
       # title = 'A',
       color = NULL
       # color = 'Vaccine dose'
       )+
  guides(color=guide_legend(title.position="left",
                            title.hjust =0.5))
fig_1

# table -------------------------------------------------------------------

datafile_table <- datafile_plot |>
  mutate_at(vars(contains('OR')), sprintf, fmt = "%.2f") |>
  mutate(OR = paste0(OR,"(",OR_1,"-",OR_2,")"),
         label = paste(adjust, group, sep = '_')) |>
  select(var, OR, order) |>
  pivot_wider(names_from = var,
              values_from = OR,
              values_fill = '/') |>
  mutate(order = as.numeric(order)) |>
  complete(order = full_seq(order, 1)) |>
  select(-order)
datafile_table[is.na(datafile_table)] <- ""
names(datafile_table) <- c('1 dose vs.\nUnvaccinated', '2 doses vs.\nUnvaccinated', 'Booster dose vs.\nUnvaccinated')

datafile_table$Subgroup <- Subgroup


table <- ggtexttable(datafile_table[,c(4, 1:3)],
                     rows = NULL,
                     theme = ttheme("blank", base_size = 12, padding = unit(c(9, 9), "mm"))) |>
  tab_add_hline(at.row = 3:13, row.side = "top", linewidth = 1) |>
  tab_add_hline(at.row = 1:2, row.side = "top", linewidth = 2)

table + fig_1

ggsave('../outcome/20220819/fig1.pdf', width = 14, height = 7)

