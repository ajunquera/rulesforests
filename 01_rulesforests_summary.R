#*******************************************************************************
# From rules to forests - Summary statistics
# Author: Álvaro F. Junquera (UAB)
#*******************************************************************************

library(tidyverse)
library(lubridate)
library(readxl)
library(tidytable)
library(descr)
library(janitor)
library(ivs)
library(moder)

# Read ------
dsp1723x <- readRDS("C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Dataset_final/dsp1723xids.RDS")


# Table 1 -----------
tab1_c1 <- dsp1723x %>%
  filter(yearstart != 2023) %>%
  tabyl(yearstart) %>%
  select(yearstart, n)
colnames(tab1_c1)[2] <- "unemp_epis"

tab1_c2 <- dsp1723x %>%
  filter(yearstart != 2023) %>%
  tabyl(yearstart, ltu) %>%
  select(yearstart, 'Yes')
colnames(tab1_c2)[2] <- "LTU_epis"

tab1_c3 <- dsp1723x %>%
  filter(yearstart != 2023) %>%
  tabyl(yearstart, ltu) %>%
  adorn_percentages() %>%
  adorn_pct_formatting() %>%
  select(yearstart, 'Yes')
colnames(tab1_c3)[2] <- "LTU_epis_pct"

tab1_c4 <- dsp1723x %>%
  filter(yearstart != 2023) %>%
  distinct(id_ind, yearstart) %>%
  group_by(yearstart) %>%
  summarise(size = n())
colnames(tab1_c4)[2] <- "individuals" # they are not *unique* individuals, they are individuals who have at least one unemp episode in that year

tab1_c5 <- dsp1723x %>%
  filter(yearstart != 2023 & ltu == "Yes") %>%
  distinct(id_ind, yearstart) %>%
  group_by(yearstart) %>%
  summarise(size = n())
colnames(tab1_c5)[2] <- "inds_atleast1LTU"

tab1 <- reduce(.x = list(tab1_c1, tab1_c2, tab1_c3,
                         tab1_c4, tab1_c5),
               left_join, by = "yearstart")

tab1$inds_atleast1LTU_pct <- paste0(round(tab1$inds_atleast1LTU / tab1$individuals * 100, 2), "%")

tab1x <- data.frame(yearstart = tab1$yearstart,
                    unemp_epis = tab1$unemp_epis,
                    LTUepis = paste0(tab1$LTU_epis, " ", "(", tab1$LTU_epis_pct, ")"),
                    individuals = tab1$individuals,
                    indsatleast1LTU = paste0(tab1$inds_atleast1LTU, " ", "(", tab1$inds_atleast1LTU_pct, ")"))

## Last row (total)
total_indsatleast1LTU <- dsp1723x %>%
  filter(yearstart != 2023 & ltu == "Yes") %>%
  distinct(id_ind) %>%
  count()

lastrow <- data.frame(yearstart = "Total",
                      unemp_epis = nrow(dsp1723x),
                      LTUepis = paste0(sum(dsp1723x$ltu == "Yes"), " ", "(",
                                       round(sum(dsp1723x$ltu == "Yes") / nrow(dsp1723x) * 100, 2), "%)"),
                      individuals = length(unique(dsp1723x$id_ind)),
                      indsatleast1LTU = paste0(total_indsatleast1LTU$n, " ", "(",
                                               round(total_indsatleast1LTU$n / length(unique(dsp1723x$id_ind)) * 100, 2), "%)"))
tab1x <- bind_rows(tab1x, lastrow)

openxlsx::write.xlsx(tab1x, 'tab1x.xlsx')

# Table E1 --------------
dsp1723x %>%
  select(sex_ti, edu_ti, disab_ti, nationali) %>%
  modelsummary::datasummary_skim(type = "categorical",
                                 fmt = 3,
                                 output = "summstatistics_qual.docx")

# Table E2 --------------
dsp1723x %>%
  select(age) %>%
  summarise(
    mean = mean(age),
    min = fivenum(age)[1],
    Q1 = fivenum(age)[2],
    median = fivenum(age)[3],
    Q3 = fivenum(age)[4],
    max = fivenum(age)[5]
  )
