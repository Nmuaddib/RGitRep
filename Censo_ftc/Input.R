## Tratamento de Arquivos do Censo
## Nelson Simões - 04/2019
##########################################
library(readr)
library(stringr)
library(tibble)
library(tidyr)
library(magrittr)
library(dplyr)
library(readxl)
##########################################

nvector <- c(1:63)
for (i in seq_along(nvector)) nvector[i] <- paste0("X", str_pad(i, side = "left", width = 2, pad = "0"))

tvector <- ""
for (i in 1:63) tvector <- paste0(tvector,"c")

mpath <- "C:/Users/Lorena/Documents/RGitRep/Censo_ftc/" 

f.tratamento <- function(ds_param, arq_output = "xput_l.txt", fast_output = "Y") {
  
  for (i in (1:nrow(ds_param))) ds_param[i,"idx"] <- i
  
  ####
  
  ds_alunos <- ds_param[,c("X01","idx")] %>% 
    filter(X01 == "41") %>% 
    select(idx) %>% 
    mutate(idxi=idx+1,idxf = 1)##"[["(.,1)
  
  for (i in 1:nrow(ds_alunos)) ds_alunos[i,3] <- ds_alunos[i+1, 1]-1
  
  ds_alunos %<>% mutate(diff = idxf - idxi)
  ds_alunos_multicurs <- ds_alunos %>% 
    filter(diff != 0)
  
  ####
  
  ds_cursosdup <- ds_param[ds_alunos_multicurs[1,"idxi"][[1]]:ds_alunos_multicurs[1,"idxf"][[1]], ] %>% 
    select(X03, X06) %>% 
    mutate(ALUNO = ds_param[ds_alunos_multicurs[1,"idx"][[1]],"X04"][[1]], IDX = ds_param[ds_alunos_multicurs[1,"idx"][[1]],"idx"][[1]], CT = 1) %>% 
    group_by(ALUNO,IDX,X03,X06) %>% 
    summarise(CD = sum(CT, na.rm = T))## %>% 
  ##filter(CD > 1)
  
  for(i in 2:nrow(ds_alunos_multicurs)) {
    ds_temp <- ds_param[ds_alunos_multicurs[i,"idxi"][[1]]:ds_alunos_multicurs[i,"idxf"][[1]], ] %>% 
      select(X03, X06) %>% 
      mutate(ALUNO = ds_param[ds_alunos_multicurs[i,"idx"][[1]],"X04"][[1]], IDX = ds_param[ds_alunos_multicurs[i,"idx"][[1]],"idx"][[1]], CT = 1) %>% 
      group_by(ALUNO,IDX,X03,X06) %>% 
      summarise(CD = sum(CT, na.rm = T))## %>% 
    ##filter(CD > 1)
    ds_cursosdup %<>%  rbind(., ds_temp)
  }
  
  ds_cursosdup %<>% filter(CD > 1)
  
  ds_cursosdup %<>% merge(., ds_alunos_multicurs, by.x = "IDX", by.y = "idx", all.y = FALSE)
  
  ####
  
  del_idx <- double()
  
  for (i in 1:nrow(ds_cursosdup)) {
    ds_temp <- ds_param[ds_cursosdup[i,"idxi"][[1]]:ds_cursosdup[i,"idxf"][[1]],] %>% 
      filter(X03 == ds_cursosdup[i,"X03"][[1]], X06 == ds_cursosdup[i,"X06"][[1]]) %>% 
      mutate(INGR = paste0(str_sub(.$X11, 3, 6), str_sub(.$X11, 1, 2))) %>% 
      arrange(INGR)
    for (j in 1:ds_cursosdup[i,"CD"][[1]]-1) del_idx <- append(del_idx, ds_temp[j,"idx"][[1]])
  }
  
  ds_main_cl <- ds_param[-del_idx,]
  
  if (fast_output == "Y") {
    for (i in 1:nrow(ds_main_cl)) {
      if (ds_main_cl[i,1] == '40') {
        ds_main_cl[i,4:63] <- "*"
      } else if (ds_main_cl[i,1] == '41') {
        ds_main_cl[i,28:63] <- "*"
      }
    }
    write_delim(ds_main_cl[,1:63],arq_output, delim = '|', na = '')
  } else {
    write_lines(ds_main_cl[1,1:2], arq_output, sep = "|", na = "", append = FALSE)
    write_lines(ds_main_cl[1,3], arq_output, na = "", append = TRUE)
    for (i in 2:nrow(ds_main_cl)) {
      if (ds_main_cl[i,1] == '41') {
        write_lines(ds_main_cl[i,1:26], arq_output, sep = "|", na = "", append = TRUE)
        write_lines(ds_main_cl[i,27], arq_output, na = "", append = TRUE)
      } else if (ds_main_cl[i,1] == '42') {
        write_lines(ds_main_cl[i,1:62], arq_output, sep = "|", na = "", append = TRUE)
        write_lines(ds_main_cl[i,63], arq_output, sep = ifelse(i == nrow(ds_main_cl), "", "\n"), na = "", append = TRUE)    
      }
    }
  }
  return(ds_main_cl)
}

#----
# ds_main <- ds_com %>% 
#   rbind(.,ds_jeq) %>% 
#   rbind(.,ds_fsa1) %>% 
#   rbind(.,ds_fsa2) %>% 
#   rbind(.,ds_sal1) %>% 
#   rbind(.,ds_sal2) %>% 
#   rbind(.,ds_vca1) %>% 
#   rbind(.,ds_vca2) %>% 
#   rbind(.,ds_ita) %>% 
#   rbind(.,ds_jua) %>% 
#   rbind(.,ds_pet) %>% 
#   rbind(.,ds_sp) %>% 
#   mutate(idx = 1)
#####

##ds_dupcurso <-  read_excel(paste0(mpath,"dupcurso.xlsx"))
##ds_transfint <- read_excel(paste0(mpath,"transfint.xlsx"))

### FTC-COMERCIO
ds_com <- read_delim(paste0(mpath,"FTC-COMERCIO/29FCS_20190408.txt"),"|", col_names = nvector, col_types = tvector) %>% mutate(ID = "com")
ds_out <- f.tratamento(ds_com,paste0(mpath,"FTC-COMERCIO/29FCS_20190408_t.txt"), "N")
rm(i)
##rm(ds_com)

### FTC-JEQ
ds_jeq <- read_delim(paste0(mpath,"FTC-JEQ/06JEQ_20180408.txt"),"|", col_names = nvector, col_types = tvector) %>% mutate(ID = "jeq")

### FTC-FSA
##ds_fsa  <- read_delim(paste0(mpath,"FTC-FSA/03_FSA_20190408.txt"),"|", col_names = nvector, col_types = tvector) %>% mutate(ID = "fsa")
ds_fsa1  <- read_delim(paste0(mpath,"FTC-FSA/03_FSA_20190408_1.txt"),"|", col_names = nvector, col_types = tvector) %>% mutate(ID = "fsa1")
ds_fsa2  <- read_delim(paste0(mpath,"FTC-FSA/03_FSA_20190408_2.txt"),"|", col_names = nvector, col_types = tvector) %>% mutate(ID = "fsa2")

### FTC-SALVADOR
##ds_sal <- read_delim(paste0(mpath,"FTC-SALVADOR/04SSA08042019.txt"),"|", col_names = nvector, col_types = tvector) %>% mutate(ID = "sal")
ds_sal1 <- read_delim(paste0(mpath,"FTC-SALVADOR/04SSA08042019_1.txt"),"|", col_names = nvector, col_types = tvector) %>% mutate(ID = "sal1")
ds_sal2 <- read_delim(paste0(mpath,"FTC-SALVADOR/04SSA08042019_2.txt"),"|", col_names = nvector, col_types = tvector) %>% mutate(ID = "sal2")

### FTC-VCA
##ds_vca <- read_delim(paste0(mpath,"FTC-VCA/05FTC-VCA08042019.txt"),"|", col_names = nvector, col_types = tvector) %>% mutate(ID = "vca")
ds_vca1 <- read_delim(paste0(mpath,"FTC-VCA/05FTC-VCA08042019_1.txt"),"|", col_names = nvector, col_types = tvector) %>% mutate(ID = "vca1")
ds_vca2 <- read_delim(paste0(mpath,"FTC-VCA/05FTC-VCA08042019_2.txt"),"|", col_names = nvector, col_types = tvector) %>% mutate(ID = "vca2")

### OTE-ITABUNA
ds_ita <- read_delim(paste0(mpath,"OTE-ITABUNA/07_ITA_20190408.txt"),"|", col_names = nvector, col_types = tvector) %>% mutate(ID = "ita")

### OTE-JUA
ds_jua <- read_delim(paste0(mpath,"OTE-JUA/21OTEJUA_20190408.txt"),"|", col_names = nvector, col_types = tvector) %>% mutate(ID = "jua")

### OTE-PETROLINA
ds_pet <- read_delim(paste0(mpath,"OTE-PETROLINA/22OTEPET_20190408.txt"),"|", col_names = nvector, col_types = tvector) %>% mutate(ID = "pet")

### OTE-SP
ds_sp <- read_delim(paste0(mpath,"OTE-SP/20_OTESP_20190408.txt"),"|", col_names = nvector, col_types = tvector) %>% mutate(ID = "sp")

########

# for (i in (1:nrow(ds_main_cl))) ds_main_cl[i,"idx"] <- i
# ###
# ds_alunos <- ds_main_cl[,c("X01","idx")] %>%
#   filter(X01 == "41") %>%
#   select(idx) %>%
#   mutate(idxi=idx+1,idxf = 1)##"[["(.,1)
# 
# for (i in 1:nrow(ds_alunos)) ds_alunos[i,3] <- ds_alunos[i+1, 1]-1
# 
# ds_alunos %<>% mutate(diff = idxf - idxi)
# ds_alunos_multicurs <- ds_alunos %>%
#   filter(diff != 0)
# 
# ls(ds_alunos_multicurs)
# ###
# ds_cursosdup <- ds_main_cl[ds_alunos_multicurs[1,"idxi"][[1]]:ds_alunos_multicurs[1,"idxf"][[1]], ] %>%
#   select(X03, X06) %>%
#   mutate(ALUNO = ds_main_cl[ds_alunos_multicurs[1,"idx"][[1]],"X04"][[1]], IDX = ds_main_cl[ds_alunos_multicurs[1,"idx"][[1]],"idx"][[1]], CT = 1) %>%
#   group_by(ALUNO,IDX,X03,X06) %>%
#   summarise(CD = sum(CT, na.rm = T))## %>%
# ##filter(CD > 1)
# 
# for(i in 2:nrow(ds_alunos_multicurs)) {
#   ds_temp <- ds_main_cl[ds_alunos_multicurs[i,"idxi"][[1]]:ds_alunos_multicurs[i,"idxf"][[1]], ] %>%
#     select(X03, X06) %>%
#     mutate(ALUNO = ds_main_cl[ds_alunos_multicurs[i,"idx"][[1]],"X04"][[1]], IDX = ds_main_cl[ds_alunos_multicurs[i,"idx"][[1]],"idx"][[1]], CT = 1) %>%
#     group_by(ALUNO,IDX,X03,X06) %>%
#     summarise(CD = sum(CT, na.rm = T))## %>%
#   ##filter(CD > 1)
#   ds_cursosdup %<>%  rbind(., ds_temp)
# }
# 
# ds_cursosdup %<>% filter(CD > 1)

###


#----
# ixini <- match("01070930571",ds_main$X04)
# ixini
# i <- ixini + 1
# while (ds_main[i,1][[1]] == "42") {
#   ixfim <- i
#   i <- i + 1
# }
# 
# temp <- as.tibble(ds_main[ixini:(ixfim),])
# view(temp)
#----
# ixini <- match("02214916512",ds_main_cl$X04)
# ixini
# i <- ixini + 1
# while (ds_main_cl[i,1][[1]] == "42") {
#   ixfim <- i
#   i <- i + 1
# }
# 
# temp <- as.tibble(ds_main_cl[ixini:(ixfim),])
# view(temp)


