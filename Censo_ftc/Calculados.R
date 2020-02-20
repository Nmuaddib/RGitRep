## Semetralização de Medicina
## Nelson Simões - 05/2019
##########################################
library(readr)
library(stringr)
library(tibble)
library(tidyr)
library(magrittr)
library(dplyr)
library(readxl)
library(fuzzyjoin)
##########################################

ds_main <- read_xlsx('Relatório Semestralização Alunos Medicina 2.xlsx') %>% as_tibble()
ds_semestre <- read_xlsx('semestre.xlsx') %>% as_tibble()
ds_enquadra <- read_xlsx('enquadra.xlsx') %>% as_tibble()

v_grade <- c(13,17,21,25,29,33,37,41,45,49,53,57)
v_hist <- v_grade
for (i in 1:12) v_hist[i] <- v_hist[i]+1
v_chgrade <- v_hist
for (i in 1:12) v_chgrade[i] <- v_chgrade[i]+1
v_chhist <- v_chgrade
for (i in 1:12) v_chhist[i] <- v_chhist[i]+1

#sum(as.double(ds_main[1,v_chhist]))

v_name <- c(1:12)
for (i in 1:12) v_name[i] <- paste0("DELTA_GRADE_",as.character(i))

f_add12 <- function(ds_param) {
  ds_param %<>% mutate(c1 = 0, c2 = 0, c3 = 0, c4 = 0, c5 = 0, c6 = 0,
                       c7 = 0, c8 = 0, c9 = 0, c10 = 0, c11 = 0, c12 = 0) 
  return(ds_param)
}

ds_main %<>%  f_add12(.)

names(ds_main)[61:72] <- v_name

for (i in 1:12) v_name[i] <- paste0("DELTA_CH_",as.character(i))

ds_main %<>%  f_add12(.)

names(ds_main)[73:84] <- v_name

ds_main %<>% mutate(HIST_TOTAL = 0, CH_HIST_TOTAL = 0, HIST_DELTA_TOTAL = 0, CH_HIST_DELTA_TOTAL = 0)

for (i in 1:nrow(ds_main)) {
  for (s in 1:12){
    c_grade <- as.double( ifelse( ds_main[i,v_grade[s]] == "NULL" , 0, ds_main[i,v_grade[s]] ) ) - as.double( ifelse( ds_main[i,v_hist[s]] == "NULL" , 0, ds_main[i,v_hist[s]] ) )
    c_ch <- as.double( ifelse( ds_main[i,v_chgrade[s]] == "NULL" , 0, ds_main[i,v_chgrade[s]] ) ) - as.double( ifelse( ds_main[i,v_chhist[s]] == "NULL" , 0, ds_main[i,v_chhist[s]] ) )
    ds_main[i,(60+s)] <- c_grade
    ds_main[i,(72+s)] <- c_ch
    ds_main[i,"HIST_TOTAL"] <- ds_main[i,"HIST_TOTAL"] + as.double( ifelse( ds_main[i,v_hist[s]] == "NULL" , 0, ds_main[i,v_hist[s]] ) )
    ds_main[i,"CH_HIST_TOTAL"] <- ds_main[i,"CH_HIST_TOTAL"] + as.double( ifelse( ds_main[i,v_chhist[s]] == "NULL" , 0, ds_main[i,v_chhist[s]] ) )
    ds_main[i,"HIST_DELTA_TOTAL"] <- ds_main[i,"HIST_DELTA_TOTAL"] + c_grade
    ds_main[i,"CH_HIST_DELTA_TOTAL"] <- ds_main[i,"CH_HIST_DELTA_TOTAL"] + c_ch
  }
}

#names(ds_semestre)
#names(ds_main)

ds_main %<>% merge(., ds_semestre, by.x = c("ANO_INGRESSO","SEM_INGRESSO" ), by.y = c("Ano","Semestre"))
names(ds_main)[89] <- "SERIE_IDEAL"

ds_main %<>% fuzzy_left_join(., ds_enquadra, by = c("GRADE_TOTAL" = "CURR",
                                                    "HIST_TOTAL" = "DIS_INF",
                                                    "HIST_TOTAL" = "DIS_SUP"), match_fun = list(`==`,`>=`,`<=`))

ds_main %<>% select(-c(91:99))

names(ds_main)[90] <- "SERIE_IDEAL_DISC"

ds_main %<>% fuzzy_left_join(., ds_enquadra, by = c("GRADE_TOTAL" = "CURR",
                                                    "CH_HIST_TOTAL" = "CH_INF",
                                                    "CH_HIST_TOTAL" = "CH_SUP"), match_fun = list(`==`,`>=`,`<=`))

ds_main %<>% select(-c(92:100))

names(ds_main)[91] <- "SERIE_IDEAL_CH"

ds_back <- ds_main
#ds_main <- ds_back

ds_main %<>% mutate(STATUS_ENTRADA = if_else(.$SERIE != .$SERIE_IDEAL, "Não semestralizado", "OK"),
                    STATUS_DISCIPLINA = if_else(.$SERIE != .$SERIE_IDEAL_DISC, "Não semestralizado", "OK"),
                    STATUS_CH = if_else(.$SERIE != .$SERIE_IDEAL_CH, "Não semestralizado", "OK")) %>% 
                    select(1:8, 10:12, 9, 89:94, 85:88, 13:84)

write_excel_csv2(ds_main, "Relatório Semestralização Medicina.csv")

