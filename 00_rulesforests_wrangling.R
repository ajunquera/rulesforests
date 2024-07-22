#...............................................................................
# From rules to forests - Data wrangling
# Author: Álvaro F. Junquera
#...............................................................................

library(tidyverse)
library(lubridate)
library(readxl)
library(tidytable)
library(descr)
library(janitor)
library(ivs)
library(moder)

setwd("C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Intermediate_outputs")

# 0. Some functions ---------
## This function calculates the "rolling mean" (moving average), but having discarded the NA both at the numerator and the denominator
cummean.na <- function(x, na.rm = T, force.numeric = F){
  if(force.numeric == T) {
    # x = c(NA, seq(1, 10, 1)); na.rm = T
    n <- length(x)
    op <- rep(NA, n) # full missing
    for(i in 1:n) {op[i] <- ifelse(is.na(x[i]), NA, mean(x[1:i], na.rm = !!na.rm))}
    rm(x, na.rm, n, i)
    op.num <- as.numeric(op)
    return(op.num)
  } else{
    # x = c(NA, seq(1, 10, 1)); na.rm = T
    n <- length(x)
    op <- rep(NA, n) # full missing
    for(i in 1:n) {op[i] <- ifelse(is.na(x[i]), NA, mean(x[1:i], na.rm = !!na.rm))}
    rm(x, na.rm, n, i)
    return(op)
  }
} # this function *avoids* to value those inputs with NA with the previous cumulated mean, see cummean.na(c(NA, NA, NA, 1, NA, NA, 2))


cummax_na = function(x, na.rm = TRUE) {
  if(!na.rm) return(cummax(x))
  if(all(is.na(x))) return(x)
  # check for leading NAs to keep
  first_non_na = match(TRUE, !is.na(x))
  x = dplyr::coalesce(x, -Inf)
  result = cummax(x)
  if(first_non_na > 1) result[1:(first_non_na - 1)] = NA
  result
} # solution developed by Gregor Thomas (https://stackoverflow.com/questions/35989365/need-to-get-r-cummax-but-dealing-properly-with-nas)

cummin_na = function(x, na.rm = TRUE) {
  if(!na.rm) return(cummin(x))
  if(all(is.na(x))) return(x)
  # check for leading NAs to keep
  first_non_na = match(TRUE, !is.na(x))
  x = dplyr::coalesce(x, Inf)
  result = cummin(x)
  if(first_non_na > 1) result[1:(first_non_na - 1)] = NA
  result
}

checknas <- function(data_frame) {
  na_counts <- sapply(data_frame, function(x) sum(is.na(x)))
  return(na_counts)
}

# 1. Upload data -------------
## Claims (DEM)
cla1 <- read_excel("C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Data_xxx/4taextra/Extraccion_1.xlsx",
                   sheet = "Demandas_sit_Adm_P1")
cla2 <- read_excel("C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Data_xxx/4taextra/Extraccion_2.xlsx",
                   sheet = "Demandas_sit_Adm_P1")
cla3 <- read_excel("C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Data_xxx/4taextra/Extraccion_3.xlsx",
                   sheet = "Demandas_sit_Adm_P1")
cla4 <- read_excel("C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Data_xxx/4taextra/Extraccion_4.xlsx",
                   sheet = "Demandas_sit_Adm_P1")

## Contracts (CON)
con1 <- read_excel("C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Data_xxx/4taextra/Extraccion_1.xlsx",
                   sheet = "CONTRATOS")
con2 <- read_excel("C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Data_xxx/4taextra/Extraccion_2.xlsx",
                   sheet = "CONTRATOS")
con3 <- read_excel("C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Data_xxx/4taextra/Extraccion_3.xlsx",
                   sheet = "CONTRATOS")
con4 <- read_excel("C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Data_xxx/4taextra/Extraccion_4.xlsx",
                   sheet = "CONTRATOS")

## Benefits (BEN)
ben1 <- read_excel("C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Data_xxx/4taextra/Extraccion_1.xlsx",
                   sheet = "PRESTACIONES")
ben2 <- read_excel("C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Data_xxx/4taextra/Extraccion_2.xlsx",
                   sheet = "PRESTACIONES")
ben3 <- read_excel("C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Data_xxx/4taextra/Extraccion_3.xlsx",
                   sheet = "PRESTACIONES")
ben4 <- read_excel("C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Data_xxx/4taextra/Extraccion_4.xlsx",
                   sheet = "PRESTACIONES")

## ALMP (ACT)
act1 <- read_excel("C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Data_xxx/4taextra/Extraccion_1.xlsx",
                   sheet = "SERVEIS")
act2 <- read_excel("C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Data_xxx/4taextra/Extraccion_2.xlsx",
                   sheet = "SERVEIS")
act3 <- read_excel("C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Data_xxx/4taextra/Extraccion_3.xlsx",
                   sheet = "SERVEIS")

act4 <- read_excel("C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Data_xxx/4taextra/Extraccion_4.xlsx",
                   sheet = "SERVEIS")

## Classification of ALMPs
clalmp <- read_excel("C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Intermediate_outputs/classification_almps_types.xlsx",
                     sheet = "removingQ")

## Education credentiales (EDU)
edu1 <- read_excel("C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Data_xxx/4taextra/Extraccion_1.xlsx",
                   sheet = "Titol")
edu2 <- read_excel("C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Data_xxx/4taextra/Extraccion_2.xlsx",
                   sheet = "Titol")
edu3 <- read_excel("C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Data_xxx/4taextra/Extraccion_3.xlsx",
                   sheet = "Titol")
edu4 <- read_excel("C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Data_xxx/4taextra/Extraccion_4.xlsx",
                   sheet = "Titol")

edu <- bind_rows(list(edu1, edu2, edu3, edu4))

data.table::setnames(edu, "CODI TITOL", "titlecode")

edu$code12 <- str_sub(edu$titlecode, 1, 2)
edu$code345 <- str_sub(edu$titlecode, 3, 5)
edu$code34 <- str_sub(edu$titlecode, 3, 4)


## Industries classification
cnae_ccae_d <- read_excel("C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Intermediate_outputs/industries_c.xlsx")

cnae_all <- read_excel("C:/Users/1604834/OneDrive - UAB/Datos/Classifications/Sectores/Tabla_CNAE_AEAT.xlsx",
                       sheet = "relación")

cnae_ccae_dg <- left_join(cnae_ccae_d, cnae_all[, c("COD_CNAE2009", "GRUPO")],
                          by = c("CNAE_CCAE_D" = "COD_CNAE2009"))

data.table::setnames(cnae_ccae_dg, "GRUPO", "section")

### Education credentials classification (levels) - CNED-2000 and ISCED-11
if(file.exists("C:/Users/1604834/OneDrive - UAB/Datos/Classifications/Educación/Nivell-formatiu-Codis (Version AFJ).xlsx")) {
  level <- read_excel("C:/Users/1604834/OneDrive - UAB/Datos/Classifications/Educación/Nivell-formatiu-Codis (Version AFJ).xlsx",
                      sheet = "level_from_codes")
} else{
  level <- read_excel("C:/Users/alvar/UAB/OneDrive - Universitat Autònoma de Barcelona/Datos/Classifications/Educación/Nivell-formatiu-Codis (Version AFJ).xls",
                      sheet = "level_from_codes")
}

### Education credentials classification (fields) - CNED-2000
if(file.exists("C:/Users/1604834/OneDrive - UAB/Datos/Classifications/Educación/Nivell-formatiu-Codis (Version AFJ).xlsx")) {
  fields <- read_excel("C:/Users/1604834/OneDrive - UAB/Datos/Classifications/Educación/cned2000_all.xlsx",
                       sheet = "CNEDSEC_sectores", skip = 2)
} else{
  fields <- read_excel("C:/Users/alvar/UAB/OneDrive - Universitat Autònoma de Barcelona/Datos/Classifications/Educación/cned2000_all.xlsx",
                       sheet = "CNEDSEC_sectores", skip = 2)
}

fields2d <- fields %>% filter(str_length(`Código`) %in% c(1,2))
colnames(fields2d) <- c("code", "field2d")
fields2d <- fields2d %>%
  mutate(code2d = if_else(str_length(code) == 1, paste0(code, "0"), code))

### Local labour markets (markets)
if(file.exists("C:/Users/alvar/UAB/OneDrive - Universitat Autònoma de Barcelona/PhD thesis/00A_thesis/4_training_Cat")) {
  markets <- read_excel("C:/Users/alvar/UAB/OneDrive - Universitat Autònoma de Barcelona/Datos/LMAs_and_clusters_Spain2011_Improved-GEA_Database/ES2011_LMAs_and_clusters_notes_and_statistics.xls",
                        sheet = "Grouping")
} else{
  markets <- read_excel("C:/Users/1604834/OneDrive - UAB/Datos/LMAs_and_clusters_Spain2011_Improved-GEA_Database/ES2011_LMAs_and_clusters_notes_and_statistics.xls",
                        sheet = "Grouping")
}

### Zip codes and municipalities ---------
if(file.exists("C:/Users/alvar/UAB/OneDrive - Universitat Autònoma de Barcelona/PhD thesis/00A_thesis/4_training_Cat")) {
  zips <- read_csv("C:/Users/1604834/OneDrive - UAB/Datos/Municipios/codigos_postales_municipiosid.csv")
} else{
  zips <- read_csv("C:/Users/1604834/OneDrive - UAB/Datos/Municipios/codigos_postales_municipiosid.csv")
}

if(file.exists("C:/Users/alvar/UAB/OneDrive - Universitat Autònoma de Barcelona/PhD thesis/00A_thesis/4_training_Cat")) {
  zips_names <- read_csv("C:/Users/1604834/OneDrive - UAB/Datos/Municipios/codigos_postales_municipios_entidades.csv")
} else{
  zips_names <- read_csv("C:/Users/1604834/OneDrive - UAB/Datos/Municipios/codigos_postales_municipios_entidades.csv")
}  # https://github.com/inigoflores/ds-codigos-postales-ine-es/blob/master/data/codigos_postales_municipios_entidades.csv

munis <- zips_names %>%
  group_by(codigo_postal) %>%
  summarise(municipa_id = mode_first(municipio_id, na.rm = T),
            municipa_des = mode_first(municipio_nombre, na.rm = T)) %>%
  ungroup()


## Current profiling (QPRO)
qpro1 <- read_excel("C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Data_xxx/5taextra/Extraccion_1.xlsx",
                    sheet = "CUESTIONARIO Q")
qpro2 <- read_excel("C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Data_xxx/5taextra/Extraccion_2.xlsx",
                    sheet = "CUESTIONARIO Q")
qpro3 <- read_excel("C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Data_xxx/5taextra/Extraccion_3.xlsx",
                    sheet = "CUESTIONARIO Q")
qpro4 <- read_excel("C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Data_xxx/5taextra/Extraccion_4.xlsx",
                    sheet = "CUESTIONARIO Q")

#data.table::setnames(qpro1, "Puntuació q", "qscore")
#data.table::setnames(qpro2, "Puntuació q", "qscore")
#data.table::setnames(qpro3, "Puntuació Q", "qscore")
#data.table::setnames(qpro4, "Puntuacio Q", "qscore")

qpro <- bind_rows(list(qpro1, qpro2, qpro3, qpro4))

### Geographic areas
if(file.exists("C:/Users/alvar/UAB/OneDrive - Universitat Autònoma de Barcelona/PhD thesis/00A_thesis/4_training_Cat")) {
  geos <- read_excel("C:/Users/alvar/UAB/OneDrive - Universitat Autònoma de Barcelona/Datos/Classifications/Territorios/geographic_regions_UN_M49.xlsx")
} else{
  geos <- read_excel("C:/Users/1604834/OneDrive - UAB/Datos/Classifications/Territorios/geographic_regions_UN_M49.xlsx")
}

geos <- geos %>%
  filter(!is.na(`M49 code`)) %>%
  fill(group1, .direction = "down") %>%
  fill(group2, .direction = "down") %>%
  group_by(group2) %>%
  fill(group3, .direction = "down") %>%
  ungroup() %>%
  filter(!is.na(`Country or Area`))

europeanunion <- c("40", "56", "100", "196", "203", "276", "208", "233", "724", "246", "250", 
                   "300", "191", "348", "372", "380", "440", "442", "428", "470", "528", 
                   "616", "620", "642", "752", "705", "703", "826")

geos <- geos %>%
  mutate(tegroup = case_when(`M49 code` %in% europeanunion ~ "EU",
                             group1 == "Europe" ~ "Europe_notEU",
                             group1 == "Oceania" ~ "Oceania",
                             group1 == "Asia" ~ "Asia",
                             group1 == "Americas" ~ group3,
                             T ~ group2))

geos$geocode <- as.character(geos$`M49 code`)


# 2. Preprocess for MAIN events ---------
## 1. Objective: dataframe in which each row is an event ------------

### 1.1. Events of claims ----------
claims <- bind_rows(list(cla1, cla2, cla3, cla4)) %>%
  select(PER_COD, DATA_INICI, CAU_COD, DESC_CAUSA_ADM,
         SEXE, `DATA NAIXEMENT`, COD_NIV_FORMATIU, DESC_NIV_FORMATIU, TIENE_DISCAPACIDAD, CODI_POSTAL,
         COD_NAC, NACIONALITAT)

colnames(claims) <- c("id_ind", "starting_date", "cause_c", "cause_desc", # removed "labour_status" (DESC_SIT_LABORAL) & "labour_status_starting" (DEM_FEC_INI_SIT_LAB)
                      "sex", "birthday", "educlevel", "educlevel_des", "disability", "zipcode",
                      "nation_c", "nation_d")

### Declaring date format
claims$startingdate <- ymd(claims$starting_date)

### Removing events without starting date and sorting
claims <- claims %>% drop_na(startingdate) %>%
  arrange(id_ind, startingdate)

### Removing claims before the temporal window of interest [2015, 2023]
claims <- claims %>%
  filter(startingdate > ymd("2014-12-31"))

## Dictionary of categories of claim (all possible categories) - only needs to be run once
# cats_claim <- claims %>%
#  select(CAU_COD, DESC_CAUSA_ADM) %>% distinct() %>%
#  arrange(CAU_COD)

#openxlsx::write.xlsx(cats_claim, 'cats_claim.xlsx')

## Flagging each claim as employed, not-employed or out-of-the-labour force
cats_claim_coded <- read_excel("C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Intermediate_outputs/cats_claim_coded.xlsx",
                               sheet = "def_150224")

claimse <- left_join(claims, cats_claim_coded[, c(1, 3)], by = c("cause_c" = "CAU_COD"))
data.table::setnames(claimse, "TIPUS", "claim_type")

claimse <- claimse %>% mutate(eventyear = year(startingdate))


### 1.2. Events of employments -----------
cons <- bind_rows(list(con1, con2, con3, con4))

colnames(cons) <- c("id_ind", "starting_date", "ending_date", "edulevel", "edulevel_des",
                    "occupation4d", "occupation4d_c", "municipality",
                    "typecont", "typecont_des", "fullpart", "hours", "sector")

cons$startingdate <- dmy(cons$starting_date)
cons$labour_status <- "Contract"

cons <- cons %>%
  group_by(id_ind) %>%
  mutate(id_empevent = row_number()) %>%
  ungroup() %>%
  relocate(id_empevent, .after = id_ind)

cons$id_i_emp <- paste0(cons$id_ind, "_", cons$id_empevent)

#### Municipality of each contract ----------
# A. Municipality of the last contract
# 1. From municipality in text to municipality in code
markets$municipality <- str_to_upper(markets$MUN_NOM, locale = "ca")
markets$municipality <- str_replace_all(markets$municipality, "\\.", ",")

# 2. Joining
cons_mB <- left_join(cons, markets, by = "municipality")

cons_mB %>% filter(is.na(MUN_COD)) %>% distinct(municipality) # the rest not corrected are not in Catalonia

markets <- markets %>%
  mutate(municipality_rev = case_when(municipality == "ALBAGÉS, L'" ~ "ALBAGES, L'",
                                      municipality == "ALBANYÀ" ~ "ALBANYA",
                                      municipality == "BIGUES I RIELLS" ~ "BIGUES I RIELLS DEL FAI",
                                      municipality == "BÒRDES, ES" ~ "BORDES, ES",
                                      municipality == "BORRASSÀ" ~ "BORRASSA", #
                                      municipality == "BORREDÀ" ~ "BORREDA", #
                                      municipality == "BRUNYOLA" ~ "BRUNYOLA I SANT MARTI SAPRESA", #
                                      municipality == "CALONGE" ~ "CALONGE I SANT ANTONI",
                                      municipality == "CRUÏLLES, MONELLS I SANT SADURNÍ DE L'HEURA" ~ "CRUÏLLES, MONELLS I SANT SADURNÍ DE L'HE", #
                                      municipality == "ESPONELLÀ" ~ "ESPONELLA",
                                      municipality == "ISONA I CONCA DELLÀ" ~ "ISONA I CONCA DELLA",
                                      municipality == "SANTA MARIA DE CORCÓ" ~ "L'ESQUIROL", #
                                      municipality == "RÀPITA, LA" ~ "LA RÀPITA", #
                                      municipality == "MONTFERRER I CASTELLBÒ" ~ "MONTFERRER I CASTELLBÓ",
                                      municipality == "PINELL DE SOLSONÈS" ~ "PINELL DE SOLSONÉS",
                                      municipality == "RODA DE BERÀ" ~ "RODA DE BARÀ", #
                                      municipality == "SANTA MARIA DE MERLÈS" ~ "SANTA MARIA DE MERLES",
                                      municipality == "ÁVILA" ~ "AVILA",
                                      municipality == "SANT CARLES DE LA RÀPITA" ~ "LA RÀPITA",
                                      municipality == "CASTELL-PLATJA D'ARO" ~ "CASTELL D'ARO, PLATJA D'ARO I S'AGARÓ",
                                      municipality == "FORÈS" ~ "FORES",
                                      T ~ municipality))

cons_mB <- left_join(cons, markets[, c("municipality_rev", "MUN_COD")], by = c("municipality" = "municipality_rev"))

cons_mB %>% filter(is.na(MUN_COD)) %>% distinct(municipality) # the rest not corrected are not in Catalonia
data.table::setnames(cons_mB, "MUN_COD", "muni_code")

# 3. Result
cons_m_ok <- cons_mB %>%
  select(id_ind, id_i_emp, muni_code)


#### Constructing -------
##### b. if employment episode = contract ----------
cons <- cons %>%
  mutate(typecont_des2 = case_when(typecont_des == "LABORAL INDEFINIT" ~ "openended",
                                   typecont_des %in% c("LABORAL TEMPORAL", "MERCANTIL (NOMÉS OFERTES)") ~ "temporary",
                                   typecont_des == "SENSE RELACIÓ CONTRACTUAL - PRÀCTIQUES" ~ "intern_nocont",
                                   TRUE ~ NA),
         fulltime = case_when(fullpart == "S" ~ T,
                              fullpart %in% c("A", "D", "M") ~ F,
                              T ~ NA),
         occupation2d = str_sub(occupation4d, 1, 2)) %>%
  filter(typecont_des2 != "intern_nocont") # with this I remove internships without labour contracts!

# Adding proper coding of level of education
cons <- cons %>%
  left_join(., level, by = c("edulevel" = "cneda2000"))

cons$isced2011 <- as.numeric(cons$isced2011)

# Adding proper coding of sector
cons <- cons %>%
  left_join(., cnae_ccae_dg[, c("sector", "section")], by = "sector")

# Adding municipalities of each contract
cons <- cons %>%
  left_join(., cons_m_ok[, c("id_i_emp", "muni_code")],
            by = "id_i_emp")

### 1.3. Binding claims and employment events and sorting ------------
claimsec <- bind_rows(select(claimse, -starting_date),
                      select(cons, -starting_date))

claimsec <- claimsec %>%
  arrange(id_ind, startingdate) %>%
  group_by(id_ind) %>%
  mutate(id_event = row_number()) %>% ungroup() %>%
  mutate(type = case_when(labour_status == "Contract" ~ "EmployedC",
                          TRUE ~ claim_type)) %>%
  ungroup() %>%
  #filter(is.na(labour_status) | labour_status == "Contract") %>% # Those rows with labour_status != "Contract" are affiliations events (claims do not inform that variable), so better to remove them! (see chapter's appendix)
  mutate(event_group = if_else(type %in% c("Employed", "EmployedC"), paste0(id_ind, "_", id_event),
                               type)) # variable event_group will help me flag different employment episodes (defining each contract as a new employment episode)

# filter(type != "Not_OR_Out") # these events do not inform if individual is not-employed or out (this line only makes sense if we include affiliations!)

## 2. Objective: dataframe in which each row is an episode -------------
# duration_thisnext is a naïve variable for employment events, since it considers that the employment episode lasts until the next event

claimsec <- claimsec %>%
  arrange(id_ind, startingdate) %>%
  mutate(duration_thisnext = case_when(id_ind == lead(id_ind) ~ interval(startingdate, lead(startingdate)) / ddays(1),
                                       id_ind != lead(id_ind) ~ interval(startingdate, ymd("2023-11-13")) / ddays(1))) # "2023-11-13" == max(claimsec$startingdate, na.rm = T)

# dataframe in which each row is an episode
### 2.1. Each row is still an event (but now we have columns with flag of episode and cumulated sum of days by episode) --------------
#### 2.1.b. Defining each employment episode as the registry of a new contract -----------
claimsec_epi2 <- claimsec %>%
  mutate(id_epi = data.table::rleid(event_group)) %>%
  group_by(id_ind, id_epi) %>% # With rleid I flag consecutive rows with the same event_group value (for the same or different individual). With group_by I select same id and same period of type
  mutate(duration_episode = cumsum(duration_thisnext), # I use duration_row since it considers absolute values (assuming wrong dates are flipped dates)
         start_episode = first(startingdate)) %>% 
  ungroup() %>%
  relocate(id_epi, .after = id_ind)

### 2.2. Now each row is an episode ------------
#### 2.2.b. Defining each employment episode as the registry of a new contract -----------
### (so, 3 consecutive contracts form 3 employment episodes)
episodes2 <- claimsec_epi2 %>%
  group_by(id_ind, id_epi) %>%
  summarise(durationepi = last(duration_episode),
            employed_typeepi = first(type), # we can safely use first, since employment episodes only include 1 contract
            highestcontract = first(typecont_des2),
            skillc = first(isced2011), # current
            sector_lastjob = last(section),
            fullt = first(fulltime),
            occup = first(occupation2d),
            municipality_con = first(muni_code),
            startepisode = first(start_episode)) %>%
  mutate(skill_vocational = case_when(skillc %in% c(25, 35, 55) ~ T,
                                      is.na(skillc) ~ NA,
                                      T ~ F))


# Identificator of episode for each individual
episodes <- episodes2 %>%
  group_by(id_ind) %>%
  mutate(id_epi = row_number()) %>%
  ungroup() %>%
  relocate(id_epi, .after = id_ind)

# Identificator of individual x episode
episodes <- episodes %>%
  mutate(id_indepi = paste0(id_ind, "_", id_epi)) %>%
  relocate(id_indepi, .after = id_epi)

## 3. Discarding employment episodes coming from CLAIMS (they only served to get the ending dates of unemployment episodes)
episodes <- episodes %>%
  filter(employed_typeepi != "Employed")


## (4). Retaining only unemployment episodes
episodesUN <- episodes %>% filter(employed_typeepi == "Not_employed") %>%
  mutate(epi_class = case_when(durationepi > 364 ~ "LTU",
                               durationepi > 364/2 ~ "MTU",
                               TRUE ~ "STU"))

# 3. Preprocess for events of SECOND-LEVEL -------------------
## 3.1. Benefits (passive labour market policies) -----------
bens <- bind_rows(list(ben1, ben2, ben3, ben4))

data.table::setnames(bens, "PER_COD", "id_ind")

bens$startingdate <- ymd(bens$`DATA INI`)
bens$endingdate <- ymd(bens$`DATA FI PRES`)

bens <- bens %>%
  arrange(id_ind, startingdate) %>%
  relocate(startingdate, .after = id_ind) %>%
  relocate(endingdate, .after = startingdate)

# The creation of a dataframe of episodes is more complicated in this case.
# The main events of first-level (claims) present intervals of duration that are disjoint!
# In the case of PLMP or ALMP the intervals may overlap...

### Each row is an episode ----------
# Previous check
bens$properinterval <- bens$endingdate > bens$startingdate
bens$properinterval <- bens$endingdate >= bens$startingdate # we need to add 1 day to 0 days intervals to avoid problems

bens <- bens %>%
  mutate(endingdate_ok = case_when(startingdate == endingdate ~ endingdate + days(1),
                                   TRUE ~ endingdate))

# The following chunk must be run without having executed {tidytable}!!!
detach("package:tidytable", unload = TRUE)

bens_epis1 <- bens %>%
  mutate(time = iv(startingdate, endingdate_ok), .keep = "unused") %>%
  group_by(id_ind) %>%
  summarise(time = iv_groups(time, abutting = F))
# see https://stackoverflow.com/questions/28938147/how-to-flatten-merge-overlapping-time-periods/71754454#71754454 and
# https://stackoverflow.com/questions/53213418/collapse-and-merge-overlapping-time-intervals 

bens_epis1$epi_start <- iv_start(bens_epis1$time)
bens_epis1$epi_end <- iv_end(bens_epis1$time)
bens_epis1$time <- NULL

data.table::setnames(bens_epis1, "epi_start", "startepisode")
data.table::setnames(bens_epis1, "epi_end", "endepisode")

## 3.2. Active labour market policies -----------------
acts <- bind_rows(list(act1, act2, act3, act4))

data.table::setnames(acts, "PER_COD", "id_ind")

acts$startingdate <- ymd(acts$`SRV_FEC_INI`)
acts$endingdate <- ymd(acts$`SRV_FEC_FIN`)

acts <- acts %>%
  arrange(id_ind, startingdate) %>%
  relocate(startingdate, .after = id_ind) %>%
  relocate(endingdate, .after = startingdate)

almps <- acts %>% tabyl(`DESCRIPCIO CODI SERVEI`)
actives <- acts %>% distinct(`CODI SERVEI`, `DESCRIPCIO CODI SERVEI`)
#openxlsx::write.xlsx(as, 'programs_almp.xlsx')

almps <- almps[-163, ] # removing "QÜESTIONARI Q" (row 163), since this isn't actually an intervention, but just profiling
almps$percent <- NULL
almps <- almps %>% arrange(desc(n))
almps$fr <- round(almps$n / sum(almps$n) * 100, 4)
almps$cumfr <- cumsum(almps$fr)

almps <- almps %>%
  left_join(., actives[, c("CODI SERVEI", "DESCRIPCIO CODI SERVEI")],
            by = "DESCRIPCIO CODI SERVEI")

almps_class <- almps %>%
  left_join(., clalmp[, c("SRV CODI", "type", "type_des")],
            by = c("CODI SERVEI" = "SRV CODI"))

# % of coded programs (96 % !!!!)
almps_class %>% filter(!is.na(type_des)) %>%
  distinct(`DESCRIPCIO CODI SERVEI`, .keep_all = T) %>%
  summarise(sum_coded = sum(fr))  # The join almps_actives doubled rows: programs with same name and different code (it changed along time...). Solved with distinct()

# % of not-coded programs (4 %)
almps_class %>% filter(is.na(type_des)) %>%
  distinct(`DESCRIPCIO CODI SERVEI`, .keep_all = T) %>%
  summarise(sum_coded = sum(fr))


clalmp <- clalmp %>%
  mutate(class = case_when(type == 3 ~ "Training",
                           type == 2 ~ "Brokering",
                           type == 1 ~ "Assistance_monitoring",
                           type == 4 ~ "Subsidy",
                           type == 5 ~ "Local_development",
                           type == 6 ~ "Selfemployment_promotion",
                           type == 7 ~ "Mobility_promotion",
                           TRUE ~ NA))

### Each row is an episode ----------
# Previous check
acts$properinterval <- acts$endingdate > acts$startingdate
acts$properinterval <- acts$endingdate >= acts$startingdate # we need to add 1 day to 0 days intervals to avoid problems

acts <- acts %>%
  mutate(endingdate_ok = case_when(startingdate == endingdate ~ endingdate + days(1),
                                   TRUE ~ endingdate))

acts <- acts %>%
  left_join(., clalmp[, c("SRV CODI", "class")],
            by = c("CODI SERVEI" = "SRV CODI"))

# The following chunk must be run without having executed {tidytable}!!!
acts_episC <- acts %>%
  mutate(time = iv(startingdate, endingdate_ok), .keep = "unused") %>%
  group_by(id_ind, class) %>%
  summarise(time = iv_groups(time, abutting = F))

# see https://stackoverflow.com/questions/28938147/how-to-flatten-merge-overlapping-time-periods/71754454#71754454 and
# https://stackoverflow.com/questions/53213418/collapse-and-merge-overlapping-time-intervals 

acts_episC$startepisode <- iv_start(acts_episC$time)
acts_episC$endepisode <- iv_end(acts_episC$time)
acts_episC$time <- NULL


## 3.3. Binding with episodes of first-level -----------
library(tidytable)

episodes$level <- "first"
bens_epis1$level <- "bens"
acts_episC$level <- "almp"

# We must consider that an ALMP episode may starts before an unemployment episode,
# but it can finish *after* the starting of the unemployment episode.
# So, to arrange all episodes (first and second level) we must define a new variable
# called key_date (startingdate for episodes of first level, endingdate for ep. of 2nd level)

episodes_augprev <- bind_rows(episodes, bens_epis1)
episodes_aug <- bind_rows(episodes_augprev, acts_episC)
rm(episodes_augprev)

episodes_aug <- episodes_aug %>%
  mutate(key_date = if_else(level == "first", startepisode, endepisode)) %>%
  arrange(id_ind, key_date) %>%
  relocate(key_date, .after = id_indepi) %>%
  relocate(level, .after = startepisode)

# Identificator of episode of ALMP/PLMP for each individual
episodes_aug <- episodes_aug %>%
  mutate(lmp = if_else(level %in% c("bens", "almp"), T, NA)) %>%
  group_by(id_ind, lmp) %>%
  mutate(id_epiLMP = row_number()) %>%
  ungroup() %>%
  mutate(id_epiLMP = if_else(level %in% c("bens", "almp"), id_epiLMP, NA)) %>% # I want that this variable was only valued if the episode collects a labour market policy
  relocate(id_epiLMP, .after = id_indepi)

# Identificator of individual x episode of LMP
episodes_aug <- episodes_aug %>%
  mutate(id_indepiLMP = if_else(is.na(id_epiLMP), NA,
                                paste0(id_ind, "_", id_epiLMP))) %>%
  relocate(id_indepiLMP, .after = id_epiLMP)

## 3.4. Constructing predictors ------------
#### 1) Started unemployment during a benefit interval ------------------
# (this variable requires the old sorting based on startepisode and not on key_date)
episodes_augOLD <- episodes_aug %>%
  arrange(id_ind, startepisode)

un1 <- episodes_augOLD %>%
  mutate(ben_interval_start = if_else(level == "bens", startepisode, NA),
         ben_interval_end = if_else(level == "bens", endepisode, NA)) %>%
  group_by(id_ind) %>%
  fill(ben_interval_start, .direction = "down") %>%
  fill(ben_interval_end, .direction = "down") %>%
  ungroup() %>%
  mutate(unemp_in_ben = if_else(employed_typeepi == "Not_employed",
                                startepisode >= ben_interval_start & startepisode <= ben_interval_end, NA))

un1 <- un1 %>%
  filter(employed_typeepi == "Not_employed")

freq(un1$unemp_in_ben) # NAs coming from impossible evaluation of startepisode >= ben_interval_start & startepisode <= ben_interval_end

un1 <- un1 %>%
  mutate(unemp_in_ben = if_else(is.na(unemp_in_ben), F, unemp_in_ben))

un1 <- un1 %>% select(id_indepi, unemp_in_ben, durationepi)

#### 2) Number of benefit episodes (completed) in the past -------------
un2 <- episodes_aug %>%
  mutate(flag_ben = if_else(level == "bens", 1, 0,
                            missing = 0)) %>%
  group_by(id_ind) %>%
  mutate(n_bens = cumsum(flag_ben)) %>%
  ungroup() %>%
  filter(employed_typeepi == "Not_employed") %>%
  select(id_indepi, n_bens)

#### 3) Total duration of previous benefit episodes ----------
un3prev <- episodes_aug %>%
  mutate(durationepisode = interval(startepisode, endepisode) / ddays(1)) %>%
  filter(level == "bens") %>%
  group_by(id_ind) %>%
  mutate(total_bens = cumsum(replace_na(durationepisode, 0))) %>% # needs to treat properly NAs! see cumsum(c(1, 2, 3, NA, 4, 5))
  ungroup() %>%
  select(id_indepiLMP, total_bens) # same logic as in 19

un3 <- episodes_aug %>%
  left_join(., un3prev[, c("id_indepiLMP", "total_bens")],
            by = "id_indepiLMP") %>%
  group_by(id_ind) %>%
  fill(total_bens, .direction = "down") %>%
  ungroup() %>%
  mutate(total_bens = if_else(is.na(total_bens), 0, total_bens)) %>% # NA in duration is 0 duration
  filter(employed_typeepi == "Not_employed") %>%
  select(id_indepi, total_bens)

rm(un3prev)

#### 4) Mean duration of previous benefit episodes ----------
un4 <- un2 %>%
  left_join(., un3, by = "id_indepi") %>%
  mutate(mean_bens = if_else(n_bens == 0, 0,
                             round(total_bens / n_bens, 2)))

un4 <- un4 %>% select(id_indepi, mean_bens)

#### 6) Total duration of employment subsidy participations ----------
un6prev <- episodes_aug %>%
  mutate(durationepisode = interval(startepisode, endepisode) / ddays(1)) %>%
  filter(class == "Subsidy") %>%
  group_by(id_ind) %>%
  mutate(total_subs = cumsum(replace_na(durationepisode, 0))) %>% # needs to treat properly NAs! see cumsum(c(1, 2, 3, NA, 4, 5))
  ungroup() %>%
  select(id_indepiLMP, total_subs) # same logic as in 19

un6 <- episodes_aug %>%
  left_join(., un6prev[, c("id_indepiLMP", "total_subs")],
            by = "id_indepiLMP") %>%
  group_by(id_ind) %>%
  fill(total_subs, .direction = "down") %>%
  ungroup() %>%
  mutate(total_subs = if_else(is.na(total_subs), 0, total_subs)) %>% # NA in duration is 0 duration
  filter(employed_typeepi == "Not_employed")

rm(un6prev)

un6 <- un6 %>% select(id_indepi, total_subs)

#### 7) Number of JSA/JSM participations in the past ----------
un7 <- episodes_aug %>%
  mutate(flag_jsam = if_else(class == "Assistance_monitoring", 1, 0,
                             missing = 0)) %>%
  group_by(id_ind) %>%
  mutate(n_jsam = cumsum(flag_jsam)) %>%
  ungroup() %>%
  filter(employed_typeepi == "Not_employed") %>%
  select(id_indepi, n_jsam)

#### 8) Number of training participations in the past ----------
un8 <- episodes_aug %>%
  mutate(flag_tr = if_else(class == "Training", 1, 0,
                           missing = 0)) %>%
  group_by(id_ind) %>%
  mutate(n_tr = cumsum(flag_tr)) %>%
  ungroup() %>%
  filter(employed_typeepi == "Not_employed") %>%
  select(id_indepi, n_tr)

#### 9) Total durations of JSA/JSM participations in the past ----------
un9prev <- episodes_aug %>%
  mutate(durationepisode = interval(startepisode, endepisode) / ddays(1)) %>%
  filter(class == "Assistance_monitoring") %>%
  group_by(id_ind) %>%
  mutate(total_jsam = cumsum(replace_na(durationepisode, 0))) %>% # needs to treat properly NAs! see cumsum(c(1, 2, 3, NA, 4, 5))
  ungroup() %>%
  select(id_indepiLMP, total_jsam) # same logic as in 19

un9 <- episodes_aug %>%
  left_join(., un9prev[, c("id_indepiLMP", "total_jsam")],
            by = "id_indepiLMP") %>%
  group_by(id_ind) %>%
  fill(total_jsam, .direction = "down") %>%
  ungroup() %>%
  mutate(total_jsam = if_else(is.na(total_jsam), 0, total_jsam)) %>% # NA in duration is 0 duration
  filter(employed_typeepi == "Not_employed") %>%
  select(id_indepi, total_jsam)

rm(un9prev)


#### 10) Total durations of training participations in the past ----------
un10prev <- episodes_aug %>%
  mutate(durationepisode = interval(startepisode, endepisode) / ddays(1)) %>%
  filter(class == "Training") %>%
  group_by(id_ind) %>%
  mutate(total_tr = cumsum(replace_na(durationepisode, 0))) %>% # needs to treat properly NAs! see cumsum(c(1, 2, 3, NA, 4, 5))
  ungroup() %>%
  select(id_indepiLMP, total_tr) # same logic as in 19

un10 <- episodes_aug %>%
  left_join(., un10prev[, c("id_indepiLMP", "total_tr")],
            by = "id_indepiLMP") %>%
  group_by(id_ind) %>%
  fill(total_tr, .direction = "down") %>%
  ungroup() %>%
  mutate(total_tr = if_else(is.na(total_tr), 0, total_tr)) %>% # NA in duration is 0 duration
  filter(employed_typeepi == "Not_employed") %>%
  select(id_indepi, total_tr)

rm(un10prev)

#### 11a) Mean duration of training participations in the past (in days) ----------
un11a <- un8 %>%
  left_join(., un10, by = "id_indepi") %>%
  mutate(mean_tr = if_else(n_tr == 0, 0,
                           round(total_tr / n_tr, 2)))

un11a <- un11a %>% select(id_indepi, mean_tr)

#### 11b) Mean duration of JSAM participations in the past (in days) ----------
un11b <- un7 %>%
  left_join(., un9, by = "id_indepi") %>%
  mutate(mean_jsam = if_else(n_jsam == 0, 0,
                             round(total_jsam / n_jsam, 2)))

un11b <- un11b %>% select(id_indepi, mean_jsam)

# 4. Construction of predictors -------------
## 4.1. Socio-demographics -----------
### 45) Sex, 49) Disability, 50) Local labour market, 51) Nationality ------------
# Number of observations per individual
claims %>% filter(!is.na(zipcode)) %>% group_by(id_ind) %>%
  summarise(sex_unicos = length(unique(sex, na.rm = T))) %>% tabyl(sex_unicos) # 1 obs/ind
claims %>% filter(!is.na(zipcode)) %>% group_by(id_ind) %>%
  summarise(edu_unicos = length(unique(educlevel, na.rm = T))) %>% tabyl(edu_unicos) # 1 obs/ind

cla1 %>% filter(!is.na(CODI_POSTAL)) %>% group_by(PER_COD) %>%
  summarise(munis_unicos = length(unique(CODI_POSTAL, na.rm = T))) %>% tabyl(munis_unicos) # 1 obs/ind

claims %>% filter(!is.na(zipcode)) %>% tabyl(sex) # no NAs
claims %>% filter(!is.na(zipcode)) %>% tabyl(educlevel) # no NAs

claims %>% filter(!is.na(cause_c)) %>% distinct(id_ind, disability) %>% group_by(id_ind) %>%
  summarise(count_dis = n()) %>% tabyl(count_dis) # only 1 observation per individual for disability

claims %>% filter(!is.na(cause_c)) %>% distinct(id_ind, zipcode) %>% group_by(id_ind) %>%
  summarise(count_zip = n()) %>% tabyl(count_zip) # only 1 zip code per individual... the problem is that some zip codes collect more than 1 municipality

# Nationality

claims <- claims %>%
  mutate(nation_cn = as.numeric(nation_c)) %>%
  left_join(., geos[, c("M49 code", "tegroup", "group1")],
            by = c("nation_cn" = "M49 code")) %>%
  mutate(nationality = case_when(tegroup %in% c("Caribbean", "Central America", "South America") ~ "LAC",
                                 tegroup %in% c("EU", "Northern America", "Oceania") ~ "EU_NAO", # + North America and Oceania
                                 T ~ tegroup))

claims$nationality <- as.factor(claims$nationality)
claims$group1 <- NULL

#
sociod <- claims %>%
  filter(!is.na(cause_c)) %>% # remove annoying information on affiliations
  mutate(disab = if_else(disability == "S", "Yes", "No")) %>%
  left_join(., level, by = c("educlevel" = "cneda2000")) %>%
  group_by(id_ind) %>%
  summarise(sex_ti = first(sex), # I can safely use first because I know there is only 1 value of sex by id_ind
            edu_ti = first(isced2011),
            disab_ti = first(disab),
            zip_code = first(zipcode),
            nationali = first(nationality)) %>%
  ungroup() %>%
  left_join(., munis, by = c("zip_code" = "codigo_postal")) %>%
  left_join(., markets, by = c("municipa_id" = "MUN_COD"))

### 46) Age ---------
birthdays <- claims %>%
  group_by(id_ind) %>%
  summarise(birth = first(birthday)) %>%
  ungroup()

birthdays$birth <- ymd(birthdays$birth)

age <- episodes[, c("id_ind", "id_indepi", "startepisode")] %>%
  left_join(., birthdays, by = "id_ind") %>%
  mutate(age = trunc(time_length(interval(birth, startepisode), "year")))

age <- age %>% select(id_indepi, age)

### 47) Field(s) of education --------------
eduf <- edu %>%
  filter(!is.na(`DECRIPCIO CODI TITOL`)) %>% # this removes those rows that indicate the level but no the credential declared by the individual
  left_join(., fields2d[, c("code2d", "field2d")], by = c("code34" = "code2d"))

edu_cned_na <- eduf %>%
  filter(is.na(field2d))

eduf %>% group_by(PER_COD) %>%
  summarise(count = n()) %>% tabyl(count) ## even after removing NAs, there's still more than 1 title for the 30% of individuals

#### Correcting the case of a tiny category (1 observation)
eduf <- eduf %>%
  mutate(field2dc = if_else(field2d == "Mecánica, electrónica y otra formación técnica; industria manufacturera y construcción",
                            "Mecánica, electrónica y otra formación técnica", field2d),
         code34c = if_else(code34 == "50", "52", code34))

#### From long to wide
eduf_w <- eduf %>%
  mutate(haveit = T) %>%
  select(PER_COD, code34c, haveit) %>%
  pivot_wider(., names_from = "code34c", names_prefix = "field_",
              values_from = "haveit", values_fill = F)

eduf_w2 <- eduf_w %>%
  mutate(across(field_34:field_88,
                .fns = ~ if_else(. > 0, T, F, missing = F)))

### 48) Level(s) of education --------------
eduf %>%
  group_by(PER_COD) %>%
  summarise(count = n()) %>%
  tabyl(count) # for 30 % of individuals, there's more than 1 credential

#### From long to wide
level$isced2011_1 <- str_sub(level$isced2011, 1, 1)

edul_w <- eduf %>%
  mutate(haveit = T) %>%
  select(PER_COD, code12, haveit) %>%
  left_join(., level[, c("cneda2000", "isced2011_1")],
            by = c("code12" = "cneda2000")) %>%
  pivot_wider(., names_from = "isced2011_1", names_prefix = "level_",
              values_from = "haveit", values_fill = F)

edul_w %>% mutate(totallevels = level_3+level_5+level_1+level_7+level_2+level_6+level_0+level_8) %>% tabyl(totallevels)

edul_w2 <- edul_w %>%
  mutate(across(level_3:level_8,
                .fns = ~ if_else(. > 0, T, F, missing = F)))

# 1 row per individual
edul_w3 <- edul_w2 %>%
  group_by(PER_COD) %>%
  summarise(across(level_3:level_8,
                   .fns = ~ any(.)))

## 4.2. Unemployment or inactivity ---------------
### 15) Number of unemployment episodes in the past (inside the window) -------
un15 <- episodesUN %>%
  group_by(id_ind) %>%
  mutate(id_epi_un = row_number()) %>%
  ungroup() %>%
  mutate(n_un = id_epi_un - 1) %>%
  select(id_indepi, n_un)

### 16) Total duration of unemployment episodes in the past (until the present episode, not included) ---------
un16 <- episodesUN %>% group_by(id_ind) %>%
  mutate(totalund = lag(cumsum(replace_na(durationepi, 0)))) %>% # needs to treat properly NAs! see cumsum(c(1, 2, 3, NA, 4, 5))
  ungroup() %>%
  select(id_indepi, totalund)

### 17) Mean duration of unemployment episodes until the present (until the present episode, not included) ------
un17 <- episodesUN %>% group_by(id_ind) %>%
  mutate(meanund = lag(cummean.na(durationepi))) %>%
  ungroup() %>%
  select(id_indepi, meanund)


### 18) Days since last unemployment episode ----------
un18 <- episodesUN %>%
  mutate(prev_unep = if_else(lag(id_ind) == id_ind, lag(startepisode), NA)) %>%
  mutate(time_lastu = interval(prev_unep, startepisode) / ddays(1)) %>%
  select(id_indepi, time_lastu)


### 22) Total duration of inactivity episodes -----------
un22prev <- episodes %>%
  filter(employed_typeepi == "Out") %>%
  group_by(id_ind) %>%
  mutate(totalout = cumsum(replace_na(durationepi, 0))) %>% # needs to treat properly NAs! see cumsum(c(1, 2, 3, NA, 4, 5))
  ungroup() %>%
  select(id_indepi, totalout) # same logic as in 19

un22 <- episodes %>%
  left_join(., un22prev[, c("id_indepi", "totalout")],
            by = "id_indepi") %>%
  group_by(id_ind) %>%
  fill(totalout, .direction = "down") %>%
  ungroup() %>%
  filter(employed_typeepi == "Not_employed")

rm(un22prev)

un22 <- un22 %>% select(id_indepi, totalout)

### 23) Mean duration of inactivity episodes until the present ------
un23prev <- episodes %>%
  filter(employed_typeepi == "Out") %>%
  group_by(id_ind) %>%
  mutate(meanout = cummean.na(durationepi)) %>%
  ungroup() %>%
  select(id_indepi, meanout) # same logic as before

un23 <- episodes %>%
  left_join(., un23prev[, c("id_indepi", "meanout")],
            by = "id_indepi") %>%
  group_by(id_ind) %>%
  fill(meanout, .direction = "down") %>%
  ungroup() %>%
  filter(employed_typeepi == "Not_employed")

rm(un23prev)

un23 <- un23 %>% select(id_indepi, meanout)

### 24) Number of inactivity episodes in the past ---------------
un24prev <- episodes %>%
  filter(employed_typeepi == "Out") %>%
  group_by(id_ind) %>%
  mutate(id_epi_out = row_number()) %>%
  ungroup() %>%
  mutate(n_out = id_epi_out) %>%
  select(id_indepi, n_out)

un24 <- episodes %>%
  left_join(., un24prev[, c("id_indepi", "n_out")],
            by = "id_indepi") %>%
  group_by(id_ind) %>%
  fill(n_out, .direction = "down") %>%
  ungroup() %>%
  mutate(n_out = if_else(is.na(n_out), 0, n_out)) %>% # this is ok, isn't it?
  filter(employed_typeepi == "Not_employed")

rm(un24prev)

un24 <- un24 %>% select(id_indepi, n_out)

## 4.3. Employment episodes ------------
### 19) Days since first employment (in the window) ---------------
un19prev <- episodes %>%
  filter(employed_typeepi == "EmployedC") %>%
  group_by(id_ind) %>%
  mutate(first_ee = if_else(row_number() == 1, startepisode, NA)) %>%
  ungroup() # with this I *exclusively* value in a new column the first employment episode of each individual

un19 <- episodes %>%
  left_join(., un19prev[, c("id_indepi", "first_ee")],
            by = "id_indepi") %>%
  group_by(id_ind) %>%
  fill(first_ee, .direction = "down") %>%
  ungroup() %>%
  mutate(time_firstE = interval(first_ee, startepisode) / ddays(1)) %>%
  filter(employed_typeepi == "Not_employed")

rm(un19prev)

un19 <- un19 %>% select(id_indepi, time_firstE)

### 20) Days since (the beginning of) the last employment episode -----------
un20 <- episodes %>%
  mutate(starting_ee = if_else(employed_typeepi == "EmployedC", startepisode, NA)) %>%
  group_by(id_ind) %>%
  fill(starting_ee, .direction = "down") %>%
  ungroup() %>%
  mutate(time_lastE = interval(starting_ee, startepisode) / ddays(1)) %>%
  filter(employed_typeepi == "Not_employed")

un20 <- un20 %>% select(id_indepi, time_lastE)

### 21) Days since (the beginning of) the last full-time employment episode ----------
un21 <- episodes %>%
  mutate(starting_eeFT = if_else(employed_typeepi == "EmployedC" & fullt == T, startepisode, NA)) %>%
  group_by(id_ind) %>%
  fill(starting_eeFT, .direction = "down") %>%
  ungroup() %>%
  mutate(time_lastEFT = interval(starting_eeFT, startepisode) / ddays(1)) %>%
  filter(employed_typeepi == "Not_employed")

un21 <- un21 %>% select(id_indepi, time_lastEFT)


### 27) Type (occupation) of last job and 28) Type of last job missing ----------------
n27 <- episodes %>%
  group_by(id_ind) %>%
  fill(occup, .direction = "down") %>%
  ungroup() %>%
  mutate(occup_NA = if_else(is.na(occup), T, F)) %>%
  filter(employed_typeepi == "Not_employed")

n27 <- n27 %>% select(id_indepi, occup, occup_NA)

### 29) Last job was part-time and 30) Last job was part-time, missing -------
n29 <- episodes %>%
  #mutate(best_hours = if_else(best_hours == -Inf, NA, best_hours)) %>%
  group_by(id_ind) %>%
  fill(fullt, .direction = "down") %>%
  ungroup() %>%
  mutate(parttime = if_else(fullt == F, T, F),
         parttime_NA = if_else(is.na(fullt), T, F)) %>%
  filter(employed_typeepi == "Not_employed")

n29 <- n29 %>% select(id_indepi, parttime, parttime_NA)

### 31) Skill level required for last job --------------
n31 <- episodes %>%
  #mutate(maxskillc = if_else(maxskillc == -Inf, NA, maxskillc)) %>%
  group_by(id_ind) %>%
  fill(skillc, .direction = "down") %>%
  ungroup() %>%
  mutate(skill_last_ee = skillc) %>%
  filter(employed_typeepi == "Not_employed")

n31 <- n31 %>% select(id_indepi, skill_last_ee)

### 32) Last job was temporary and 33) Last job was temporary (NA) ------------

# highestcontract was not defined for unemployment episodes,
# with fill i define them taking the value of the previous defined cell (within the cells of each individual)

n32 <- episodes %>%
  mutate(highestcontract = if_else(highestcontract == -Inf, NA, highestcontract)) %>%
  group_by(id_ind) %>%
  fill(highestcontract, .direction = "down") %>%
  ungroup() %>%
  mutate(last_TEMP = if_else(highestcontract == 1, T, F),
         last_NA = if_else(is.na(highestcontract), T, F)) %>%
  filter(employed_typeepi == "Not_employed")

n32 <- n32 %>% select(id_indepi, last_TEMP, last_NA)

### 34) Industry of last job ----------------
n34 <- episodes %>%
  group_by(id_ind) %>%
  fill(sector_lastjob, .direction = "down") %>%
  ungroup() %>%
  filter(employed_typeepi == "Not_employed")

n34 <- n34 %>% select(id_indepi, sector_lastjob)

### 35) Commuted for last job and 36) Commuted for last job, missing --------------

n35prev <- episodes %>%
  left_join(., sociod[, c("id_ind", "municipa_id")],
            by = "id_ind") %>%
  group_by(id_ind) %>%
  fill(municipality_con, .direction = "down") %>%
  ungroup() %>%
  mutate(commutedlast = if_else(municipa_id != municipality_con, 1, 0),
         commutedlast_NA = if_else(is.na(commutedlast), T, F)) %>%
  filter(employed_typeepi == "Not_employed")

n35 <- n35prev %>% select(id_indepi, commutedlast, commutedlast_NA)


### 37) Proportion of jobs with commuting (rolling) ----------
n35prev_all <- episodes %>%
  left_join(., sociod[, c("id_ind", "municipa_id")],
            by = "id_ind") %>%
  group_by(id_ind) %>%
  fill(municipality_con, .direction = "down") %>%
  ungroup() %>%
  mutate(commutedlast = if_else(municipa_id != municipality_con, 1, 0),
         commutedlast_NA = if_else(is.na(commutedlast), T, F))

n37prev <- n35prev_all %>%
  filter(employed_typeepi == "EmployedC") %>%
  group_by(id_ind) %>%
  summarise(id_indepi = id_indepi,
            propcommuted = cummean.na(commutedlast, force.numeric = T)) %>%
  ungroup()

n37 <- episodes %>%
  left_join(., n37prev[, c("id_indepi", "propcommuted")],
            by = "id_indepi") %>%
  group_by(id_ind) %>%
  fill(propcommuted, .direction = "down") %>%
  ungroup() %>%
  filter(employed_typeepi == "Not_employed") %>%
  select(id_indepi, propcommuted)

### 38) Number of employment episodes *without* any vocational training held in the past -------
n38 <- episodes %>%
  mutate(skill_general = case_when(skill_vocational == F ~ 1,
                                   T ~ 0)) %>% # since we are interested in a SUM (and not a mean), it's safe to replace NAs with 0
  group_by(id_ind) %>%
  mutate(n_GEN = cumsum(skill_general)) %>% # using cumsum() is safe because there are no NAs! episodes %>% filter(employed_typeepi == "EmployedC") %>% tabyl(skill_vocational)
  ungroup() %>%
  group_by(id_ind) %>%
  fill(n_GEN, .direction = "down") %>%
  ungroup() # GENeral vs vocational

n38 <- n38 %>% select(id_indepi, n_GEN)


### 39) Number of occupations held in the past -------
n39 <- episodes %>%
  group_by(id_ind) %>%
  mutate(cum_unique_occus = cumsum(!duplicated(occup)),
         idwithNA = any(is.na(occup))) %>%
  ungroup() %>%
  mutate(cum_u_o_woNAs = if_else(idwithNA == T, cum_unique_occus - 1, cum_unique_occus)) %>% # I need this because duplicated considers NA as an occupation! so sum(!duplicated(c(NA, 1, 2))) would be 3!
  mutate(n_occus_untilnow = if_else(cum_u_o_woNAs == 0, NA, cum_u_o_woNAs)) %>%
  group_by(id_ind) %>%
  fill(n_occus_untilnow, .direction = "down") %>%
  ungroup()

n39 <- n39 %>% select(id_indepi, n_occus_untilnow)

### 40) Number of employment episodes in the past ------
#episodiosNO$employed_typeepi_n <- ifelse(episodiosNO$employed_typeepi == "Yes", 1, 0)

n40 <- episodes %>%
  mutate(flag_emp = if_else(employed_typeepi == "EmployedC", 1, 0)) %>%
  group_by(id_ind) %>%
  mutate(n_emp = cumsum(flag_emp)) %>%
  ungroup()

n40 <- n40 %>% select(id_indepi, n_emp)

### 41) Number of open-ended contracts in the past ------
n41 <- episodes %>%
  mutate(flag_OEC = if_else(highestcontract == "openended", 1, 0, missing = F)) %>%
  group_by(id_ind) %>%
  mutate(n_OEC = cumsum(flag_OEC)) %>%
  ungroup() %>%
  select(id_indepi, n_OEC)

### 42) Number of temporary contracts in the past ------
n42 <- episodes %>%
  mutate(flag_TEMP = if_else(highestcontract == "temporary", 1, 0, missing = F)) %>%
  group_by(id_ind) %>%
  mutate(n_TEMP = cumsum(flag_TEMP)) %>%
  ungroup() %>%
  select(id_indepi, n_TEMP)

### 43 and 44) Maximum and minimum skill level required for past employment episodes ----------
n43n44 <- episodes %>%
  #mutate(maxskillc = if_else(skillc == -Inf, NA, maxskillc),
  #       minskillc = if_else(skillc == Inf, NA, minskillc)) %>%
  group_by(id_ind) %>%
  mutate(max_skill = cummax_na(skillc),
         min_skill = cummin_na(skillc)) %>%
  ungroup() # a bit slow

n43n44 <- n43n44 %>% select(id_indepi, max_skill, min_skill)

# 5. Joining all predictors -------------
## Time-variant covariates
mainlist <- list(un1, un2, un3, un4,
                 un6, un7, un8, un9, un10,
                 un11a, un11b,
                 un15, un16, un17, un18, un19,
                 un20, un21, un22, un23, un24,
                 n27, n29, n31, n32, n34, n35,
                 n37, n38, n39, n40, n41, n42,
                 n43n44, age)

#lapply(mainlist, nrow)
#sapply(mainlist, function(df) "id_indepi" %in% names(df))
#sapply(mainlist, function(df) colnames(df))
#sapply(mainlist, function(df) nrow(df))


dsp <- purrr::reduce(mainlist,
                     left_join, by = "id_indepi")

sum(duplicated(dsp$id_indepi)) # 0 !

dsp <- dsp %>%
  mutate(id_ind = word(id_indepi, sep = "_")) %>%
  relocate(id_ind)

## Time-invariant covariates
# sociod, eduf_w, edul_w will be joined by id_ind

data.table::setnames(eduf_w2, "PER_COD", "id_ind")
data.table::setnames(edul_w3, "PER_COD", "id_ind")

sociod$id_ind <- as.character(sociod$id_ind)
eduf_w2$id_ind <- as.character(eduf_w2$id_ind)
edul_w3$id_ind <- as.character(edul_w3$id_ind)

dsp <- dsp %>%
  left_join(., sociod[, c("id_ind", "sex_ti", "edu_ti", "disab_ti", "MERC_COD", "nationali")], by = "id_ind") %>%
  left_join(., eduf_w2, by = "id_ind") %>%
  left_join(., edul_w3, by = "id_ind") # here is the problem

sum(duplicated(dsp$id_indepi)) 

## Last covariates (those scaled)
dsp <- dsp %>%
  mutate(total_bens_age = total_bens / age,
         total_jsam_age = total_jsam / age,
         total_tr_age = total_tr / age,
         totalund_age = totalund / age,
         max_skill4 = case_when(max_skill == 0 ~ "Less_than_primary",
                                max_skill == 1 ~ "Primary",
                                max_skill %in% 24:35 ~ "Secondary",
                                max_skill %in% 55:86 ~ "Tertiary",
                                is.na(max_skill) ~ NA))

## Year and starting date of the episode
episodes$yearstart <- year(episodes$startepisode)

dsp <- dsp %>%
  left_join(., episodes[, c("id_indepi", "startepisode", "yearstart")],
            by = "id_indepi")

## Dsp [2017, 2023]
dsp1723 <- dsp %>% filter(yearstart %in% 2017:2023)

## Outcome variable
dsp1723 <- dsp1723 %>%
  mutate(ltu = if_else(durationepi >= 365, T, F))

dsp1723 <- dsp1723 %>% drop_na(ltu) # there is 1 case missing, removed!



## 5.1. Dealing with NAs --------------
### 5.1.a. "Deductive imputation" + "Indicative imputation" ------------
### DEDUCTIVE - field and level of education: We can impute those with edu_ti %in% c(0, 1, 24) as general *field* of education
dsp1723 %>% filter(is.na(field_01)) %>% tabyl(edu_ti) %>% adorn_pct_formatting()

dsp1723 <- dsp1723 %>%
  mutate(across(field_52:field_88,
                .fns = ~ if_else(edu_ti %in% c("0", "1", "24"), F, .)),
         field_34 = if_else(edu_ti %in% c("0", "1", "24"), F, field_34),
         field_01 = if_else(edu_ti %in% c("0", "1", "24"), T, field_01))

dsp1723 <- dsp1723 %>%
  mutate(level_3 = if_else(edu_ti %in% c("34", "35"), T, level_3),
         level_5 = if_else(edu_ti %in% c("55"), T, level_5),
         level_1 = if_else(edu_ti %in% c("1"), T, level_1),
         level_7 = if_else(edu_ti %in% c("76"), T, level_7),
         level_2 = if_else(edu_ti %in% c("24", "25"), T, level_2),
         level_6 = if_else(edu_ti %in% c("66"), T, level_6),
         level_0 = if_else(edu_ti %in% c("0"), T, level_0),
         level_8 = if_else(edu_ti %in% c("86"), T, level_8))

## After this we can safely recode NAs as 0s (FALSEs)
dsp1723 <- dsp1723 %>%
  mutate(across(field_34:level_8,
                .fns = ~ if_else(is.na(.), F, .)))

# Saving dsp1723

### INDICATIVE
### This missingness could come from: (a) left-censoring or (b) true missingness
### Here we impute with the censore point

dsp1723x <- dsp1723 %>%
  # indicators of missing blocks
  mutate(MIndicatorEE = if_else(is.na(occup), T, F),
         MIndicatorUE = if_else(is.na(time_lastu), T, F),
         MIndicatorLLM = if_else(is.na(MERC_COD), T, F)) %>%
  # related with employment episodes (CONS dataset starts at 2015-01-01)
  ## a- scale: interval
  mutate(time_firstE = if_else(is.na(time_firstE), abs(interval(startepisode, ymd("2015-01-01")) / ddays(1)), time_firstE),
         time_lastE = if_else(is.na(time_lastE), abs(interval(startepisode, ymd("2015-01-01")) / ddays(1)), time_lastE),
         time_lastEFT = if_else(is.na(time_lastEFT), abs(interval(startepisode, ymd("2015-01-01")) / ddays(1)), time_lastEFT),
         n_occus_untilnow = if_else(is.na(n_occus_untilnow), 0, n_occus_untilnow),
         propcommuted = if_else(is.na(propcommuted), 0, propcommuted)) %>%
  ## b- scale: nominal
  ## (first convert those logical variables to character variables)
  mutate(parttime = as.character(parttime),
         skill_last_ee = as.character(skill_last_ee),
         last_TEMP = as.character(last_TEMP),
         commutedlast = case_when(commutedlast == 1 ~ "Yes",
                                  commutedlast == 0 ~ "No",
                                  TRUE ~ NA),
         max_skill = as.character(max_skill),
         min_skill = as.character(min_skill)) %>%
  mutate(occup = if_else(is.na(occup), "Missing", occup),
         parttime = if_else(is.na(parttime), "Missing", parttime),
         skill_last_ee = if_else(is.na(skill_last_ee), "Missing", skill_last_ee),
         last_TEMP = if_else(is.na(last_TEMP), "Missing", last_TEMP),
         sector_lastjob = if_else(is.na(sector_lastjob), "Missing", sector_lastjob),
         commutedlast = if_else(is.na(commutedlast), "Missing", commutedlast),
         max_skill = if_else(is.na(max_skill), "Missing", max_skill),
         min_skill = if_else(is.na(min_skill), "Missing", min_skill),
         max_skill4 = if_else(is.na(max_skill4), "Missing", max_skill4)) %>%
  # related with unemployment or out-of labour market episodes
  mutate(time_lastu = if_else(is.na(time_lastu), abs(interval(startepisode, ymd("2015-01-01")) / ddays(1)), time_lastu),
         totalout = if_else(is.na(totalout), 0, totalout),
         meanout = if_else(is.na(meanout), 0, meanout),
         totalund = if_else(is.na(totalund), 0, totalund), # !!
         meanund = if_else(is.na(meanund), 0, meanund),
         totalund_age = if_else(is.na(totalund_age), 0, totalund_age)) %>%
  # related with local labour market
  mutate(MERC_COD = if_else(is.na(MERC_COD), "Missing", MERC_COD),
         nationali = if_else(is.na(nationali), "Missing", nationali))

# Now these binary indicators of missingness are not necessary, since we created binary indicators of "block" (set of variables) missingness
dsp1723x$occup_NA <- NULL
dsp1723x$parttime_NA <- NULL
dsp1723x$last_NA <- NULL
dsp1723x$commutedlast_NA <- NULL

## 5.2. Filtering target group by age (> 15 & < 65) ------------
## Some restrictions: removing those episodes with age < 16 years
dsp1723x <- dsp1723x %>%
  filter(age > 15 & age < 65)

## 5.3. Ensuring factors have proper names of levels ----------------

# Relocating and saving categorical variables as factors
dsp1723x <- dsp1723x %>%
  arrange(startepisode) %>%
  relocate(startepisode, .after = "id_indepi") %>%
  mutate(across(MIndicatorEE:MIndicatorLLM, .fns = as.character))

dsp1723x <- dsp1723x %>%
  relocate(ltu)

#dsp1723x <- dsp1723x %>% relocate(ltu, .after = id_indepi)

dsp1723x$unemp_in_ben <- as.factor(dsp1723x$unemp_in_ben)
dsp1723x$max_skill4 <- as.factor(dsp1723x$max_skill4)
dsp1723x$occup <- as.factor(dsp1723x$occup)
dsp1723x$parttime <- as.factor(dsp1723x$parttime)

dsp1723x$skill_last_ee <- as.factor(dsp1723x$skill_last_ee)
dsp1723x$last_TEMP <- as.factor(dsp1723x$last_TEMP)
dsp1723x$sector_lastjob <- as.factor(dsp1723x$sector_lastjob)
dsp1723x$commutedlast <- as.factor(dsp1723x$commutedlast)
dsp1723x$max_skill <- as.factor(dsp1723x$max_skill)
dsp1723x$min_skill <- as.factor(dsp1723x$min_skill)
dsp1723x$sex_ti <- as.factor(dsp1723x$sex_ti)

dsp1723x <- dsp1723x %>%
  mutate(across(field_34:level_8, .fns = as.factor))

dsp1723x$MERC_COD <- as.factor(dsp1723x$MERC_COD)
dsp1723x$disab_ti <- as.factor(dsp1723x$disab_ti)
dsp1723x$nationali <- as.factor(dsp1723x$nationali)

## Changing names of levels
dsp1723x <- dsp1723x %>%
  mutate(ltu = if_else(ltu == "FALSE", "No", "Yes"),
         unemp_in_ben = case_when(unemp_in_ben == "FALSE" ~ "No",
                                  unemp_in_ben == "TRUE" ~ "Yes",
                                  T ~ unemp_in_ben),
         parttime = case_when(parttime == "FALSE" ~ "No",
                              parttime == "TRUE" ~ "Yes",
                              T ~ parttime),
         last_TEMP = case_when(last_TEMP == "FALSE" ~ "No",
                               last_TEMP == "TRUE" ~ "Yes",
                               T ~ last_TEMP),
         across(field_34:level_8, .fns = ~ case_when(.x == "FALSE" ~ "No",
                                                     .x == "TRUE" ~ "Yes",
                                                     T ~ .x)),
         across(MIndicatorEE:MIndicatorLLM, .fns = ~ case_when(.x == "FALSE" ~ "No",
                                                               .x == "TRUE" ~ "Yes",
                                                               T ~ .x)))

dsp1723x <- dsp1723x %>%
  mutate(across(.cols = c(ltu, parttime, last_TEMP, field_34:level_8, MIndicatorEE:MIndicatorLLM),
                .fns = as.factor))

## 5.4. Saving the final dataset ----------

dsp1723x$ltu <- as.factor(dsp1723x$ltu)
#saveRDS(dsp1723x, "C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Dataset_final/dsp1723xids_wDUR.RDS")

dsp1723x$durationepi <- NULL
#saveRDS(dsp1723x, "C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Dataset_final/dsp1723xids.RDS")
