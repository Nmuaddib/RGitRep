library("stringr")
library("dplyr")
library("ggplot2")
library("readr")
library("tibble")
library("tidyr")
library("reshape2")
library("magrittr")
library("readxl")
library("modelr")
library("purrr")
library("forcats")

#ds_aratdo <- read_excel('Areas_de_atuacao_docentes_25_06_2019.xlsx')
#ds_appprg <- read_excel("Dados Aplicativos com Programa.xlsx")
#ds_patprg <- read_excel("Dados Patentes com Programa.xlsx")
#ds_prdprg <- read_excel("Dados Produtos com Programa.xlsx")
#ds_frmdic <- read_excel("Formação dos discentes 2013-2016 - Tese.xlsx")
#ds_frmdoc <- read_excel("Formação dos docentes tese.xlsx")
ds_prj013 <- read_excel("Membros dos projetos 2013.xlsx")
#ds_prj014 <- read_excel("Membros dos projetos 2014.xlsx")
#ds_prj015 <- read_excel("Membros dos projetos 2015.xlsx")
#ds_prj016 <- read_excel("Membros dos projetos 2016.xlsx")
ds_prgppg <- read_excel("Programas PPG 2016.xlsx")
ds_prjppg <- read_excel("Projetos PPG 2016.xlsx")

ls(ds_prgppg)
ls(ds_prjppg)

ds_pgpj <- merge(ds_prjppg, ds_prgppg, by = "CD_PROGRAMA_IES", no.dups = T) %>% 
  mutate(ANO_PRJ = str_sub(.$DH_INICIO, 1, 4)) %>% 
  select(ANO_INICIO_PROGRAMA,
         ANO_PRJ,
         CD_AREA_AVALIACAO, 
         NM_AREA_AVALIACAO = NM_AREA_AVALIACAO.x, 
         CD_PROGRAMA_IES, 
         NM_PROGRAMA_IES = NM_PROGRAMA_IES.x, 
         ID_PROJETO, DS_PROJETO) %>% 
  filter(ANO_PRJ >= 2013, ) %>% 
  group_by(NM_AREA_AVALIACAO, NM_PROGRAMA_IES) %>% 
  summarise(QT_PRJ = n_distinct(ID_PROJETO)) %>% 
  arrange(NM_AREA_AVALIACAO, desc(QT_PRJ))

write.csv2(ds_pgpj, "Qt_Projetos por Programa 2013-2016.csv")

rm(ds_pgpj)

ds_anos_prj <- ds_prjppg %>% 
  mutate(ANO_PRJ = str_sub(.$DH_INICIO, 1, 4)) %>%
  group_by(ANO_PRJ) %>% 
  summarise(n())

write.csv2(ds_anos_prj, "Projetos por ano inicio do programa.csv")

?filter
?stringr


