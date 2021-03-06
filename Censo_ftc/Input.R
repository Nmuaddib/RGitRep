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

mpath <- "C:/R/RGitRep/Censo_ftc/" 

fwrt <- "N"

f.tratamento <- function(ds_param, arq_output = "xput_l.txt", fast_output = "Y") {
  
  for (i in (1:nrow(ds_param))) ds_param[i,"idx"] <- i
  
  ####

  ds_alunos <- ds_param[,c("X01","X04","idx")] %>% 
    filter(X01 == "41") %>% 
    select(ALUNO = X04,idx) %>% 
    mutate(idxi=idx+1,idxf = 1)##"[["(.,1)
  
  for (i in 1:nrow(ds_alunos)) ds_alunos[i,4] <- ds_alunos[i+1, 2]-1
  
  ds_alunos %<>% mutate(diff = idxf - idxi)
  ds_alunos_multicurs <- ds_alunos[,-1] %>% 
    filter(diff != 0)
    
  # ds_alunos <- ds_param[,c("X01","idx")] %>% 
  #   filter(X01 == "41") %>% 
  #   select(X04,idx) %>%  
  #   mutate(idxi=idx+1,idxf = 1)##"[["(.,1)
  # 
  # for (i in 1:nrow(ds_alunos)) ds_alunos[i,3] <- ds_alunos[i+1, 1]-1
  # 
  # ds_alunos %<>% mutate(diff = idxf - idxi)
  # ds_alunos_multicurs <- ds_alunos %>% 
  #   filter(diff != 0)
  
  ####
  if (nrow(ds_alunos_multicurs) > 0) {
    ds_cursosdup <- ds_param[ds_alunos_multicurs[1,"idxi"][[1]]:ds_alunos_multicurs[1,"idxf"][[1]], ] %>% 
      select(X03, X06) %>% 
      mutate(ALUNO = ds_param[ds_alunos_multicurs[1,"idx"][[1]],"X04"][[1]], IDX = ds_param[ds_alunos_multicurs[1,"idx"][[1]],"idx"][[1]], CT = 1) %>% 
      group_by(ALUNO,IDX,X03,X06) %>% 
      summarise(CD = sum(CT, na.rm = T))## %>% 
    ##filter(CD > 1)
    if (nrow(ds_alunos_multicurs) > 1) {
      for(i in 2:nrow(ds_alunos_multicurs)) {
        ds_temp <- ds_param[ds_alunos_multicurs[i,"idxi"][[1]]:ds_alunos_multicurs[i,"idxf"][[1]], ] %>% 
          select(X03, X06) %>% 
          mutate(ALUNO = ds_param[ds_alunos_multicurs[i,"idx"][[1]],"X04"][[1]], IDX = ds_param[ds_alunos_multicurs[i,"idx"][[1]],"idx"][[1]], CT = 1) %>% 
          group_by(ALUNO,IDX,X03,X06) %>% 
          summarise(CD = sum(CT, na.rm = T))## %>% 
        ##filter(CD > 1)
        ds_cursosdup %<>%  rbind(., ds_temp)
      }
    }
    ds_cursosdup %<>% filter(CD > 1)
    
    if (nrow(ds_cursosdup) > 0) {    
      ds_cursosdup %<>% merge(., ds_alunos_multicurs, by.x = "IDX", by.y = "idx", all.y = FALSE)
      
      ####
      
      del_idx <- double()
      
      for (i in 1:nrow(ds_cursosdup)) {
        if (!is.na(ds_cursosdup[i,"X06"][[1]])) {
          ds_temp <- ds_param[ds_cursosdup[i,"idxi"][[1]]:ds_cursosdup[i,"idxf"][[1]],] %>% 
            filter(X03 == ds_cursosdup[i,"X03"][[1]], X06 == ds_cursosdup[i,"X06"][[1]]) %>% 
            mutate(INGR = paste0(str_sub(.$X11, 3, 6), str_sub(.$X11, 1, 2))) %>% 
            arrange(INGR)
        } else {
          ds_temp <- ds_param[ds_cursosdup[i,"idxi"][[1]]:ds_cursosdup[i,"idxf"][[1]],] %>% 
            filter(X03 == ds_cursosdup[i,"X03"][[1]]) %>% 
            mutate(INGR = paste0(str_sub(.$X11, 3, 6), str_sub(.$X11, 1, 2))) %>% 
            arrange(INGR)       
        }
        for (j in 1:ds_cursosdup[i,"CD"][[1]]-1) del_idx <- append(del_idx, ds_temp[j,"idx"][[1]])
      }
      
      ds_main_cl <- ds_param[-del_idx,]
    } else {
      ds_main_cl <- ds_param
    }
  } else {
    ds_main_cl <- ds_param
  }  
############################################################################  
  
  ds_transf_cpf <- ds_transfint[,2] %>%
    mutate(lin = 1) %>%
    group_by(CPF) %>%
    summarise(CT = sum(lin))
  v_transf <- ds_transf_cpf[,1][[1]]
  ds_main_cl %<>% mutate(ALUNO = "")
  
  for (i in (1:nrow(ds_main_cl))) ds_main_cl[i,"idx"] <- i
  
  ds_alunos <- ds_main_cl[,c("X01","X04","idx")] %>% 
    filter(X01 == "41") %>% 
    select(ALUNO = X04,idx) %>% 
    mutate(idxi=idx+1,idxf = 1)##"[["(.,1)
  
  for (i in 1:nrow(ds_alunos)) ds_alunos[i,4] <- ds_alunos[i+1, 2]-1
  
  ds_alunos %<>% mutate(diff = idxf - idxi)
  
  for (i in seq_along(v_transf)) {
    ds_range <- ds_alunos[which(ds_alunos[,"ALUNO"] == v_transf[i]), c(1,3:4)]
    if (nrow(ds_range) > 0) ds_main_cl[ds_range[1,2][[1]]:ds_range[1,3][[1]], 66] <- v_transf[i]
  }
  
  ds_exist <- merge(ds_transfint[,c(2,5,7,11,12)], ds_main_cl[,c(3,6,7,66)], by.x = c("CPF","INEP_CURSO"), by.y = c("ALUNO","X03")) %>% 
    mutate(NST = "")
  
  if (nrow(ds_exist) > 0) {
    ds_exist[which(ds_exist[,"SIT_ALUNO"] == "TRANSF. INTERNA"),"NST"] <- "5"
    ds_exist[which(ds_exist[,"SIT_ALUNO"] != "TRANSF. INTERNA"),"NST"] <- "OT"
    
    ds_orig_trans <- ds_exist %>% 
      filter(NST == "5") %>% 
      group_by(CPF) %>% 
      summarise(DT = max(DT_TRANS)) %>% 
      merge(., ds_exist, by.x = c("CPF","DT"), by.y = c("CPF","DT_TRANS"))
    
    ds_dest_trans <- ds_exist %>% 
      filter(NST == "OT") %>% 
      merge(., ds_orig_trans[,c(1,3)], by.x = "CPF", by.y = "CPF")
    
    for (i in 1:nrow(ds_orig_trans)) {
      ds_main_cl[which((ds_main_cl[,3] == ds_orig_trans[i, 3]) & (ds_main_cl[,66] == ds_orig_trans[i, 1])),c(7,23)] <- c(ds_orig_trans[i, "NST"],"0")
    }
    
    for (i in 1:nrow(ds_dest_trans)) {
      ds_main_cl[which((ds_main_cl[,3] == ds_dest_trans[i, 2]) & (ds_main_cl[,66] == ds_dest_trans[i, 1])),c(8,23)] <- c(ds_dest_trans[i, "INEP_CURSO.y"],"0")
    }
  }  
  
  ds_main_cl[which((is.na(ds_main_cl[,23])) & (ds_main_cl[,1]=="42")), 23] <- "0"
  
############################################################################
  
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
ds_transfint <- read_excel(paste0(mpath,"transfint.xlsx"))

### FTC-COMERCIO
# ds_com <- read_delim(paste0(mpath,"FTC-COMERCIO/FCS29-15042019.txt"),"|", col_names = nvector, col_types = tvector) %>% mutate(ID = "com")
# ds_com_c <- f.tratamento(ds_com,paste0(mpath,"FTC-COMERCIO/FCS29-15042019_t.txt"), fwrt)

### FTC-JEQ
# ds_jeq <- read_delim(paste0(mpath,"FTC-JEQ/06JEQ_20190415.txt"),"|", col_names = nvector, col_types = tvector) %>% mutate(ID = "jeq")
# ds_jeq_c <- f.tratamento(ds_jeq,paste0(mpath,"FTC-JEQ/06JEQ_20190415_t.txt"), fwrt)

### FTC-EAD
# ds_ead <- read_delim(paste0(mpath,"FTC-EAD/30EAD_20190415.txt"),"|", col_names = nvector, col_types = tvector) %>% mutate(ID = "ead")
# ds_ead_c <- f.tratamento(ds_ead,paste0(mpath,"FTC-EAD/30EAD_20190415_t.txt"), fwrt)

### FTC-FSA
##ds_fsa  <- read_delim(paste0(mpath,"FTC-FSA/03_FSA_20190408.txt"),"|", col_names = nvector, col_types = tvector) %>% mutate(ID = "fsa")
# ds_fsa1  <- read_delim(paste0(mpath,"FTC-FSA/ftcfsa15042019_1.txt"),"|", col_names = nvector, col_types = tvector) %>% mutate(ID = "fsa1")
# ds_fsa1_c <- f.tratamento(ds_fsa1,paste0(mpath,"FTC-FSA/ftcfsa15042019_1_t.txt"), fwrt)
# 
# ds_fsa2  <- read_delim(paste0(mpath,"FTC-FSA/ftcfsa15042019_2.txt"),"|", col_names = nvector, col_types = tvector) %>% mutate(ID = "fsa2")
# ds_fsa2_c <- f.tratamento(ds_fsa2,paste0(mpath,"FTC-FSA/ftcfsa15042019_2_t.txt"), fwrt)

### FTC-SALVADOR
##ds_sal <- read_delim(paste0(mpath,"FTC-SALVADOR/04SSA08042019.txt"),"|", col_names = nvector, col_types = tvector) %>% mutate(ID = "sal")
ds_sal1 <- read_delim(paste0(mpath,"FTC-SALVADOR/04SSA_20190416_1.txt"),"|", col_names = nvector, col_types = tvector) %>% mutate(ID = "sal1")
ds_sal1_c <- f.tratamento(ds_sal1,paste0(mpath,"FTC-SALVADOR/04SSA_20190416_1_t.txt"), fwrt)

ds_sal2 <- read_delim(paste0(mpath,"FTC-SALVADOR/04SSA_20190416_2.txt"),"|", col_names = nvector, col_types = tvector) %>% mutate(ID = "sal2")
ds_sal2_c <- f.tratamento(ds_sal2,paste0(mpath,"FTC-SALVADOR/04SSA_20190416_2_t.txt"), fwrt)

### FTC-VCA
##ds_vca <- read_delim(paste0(mpath,"FTC-VCA/05FTC-VCA08042019.txt"),"|", col_names = nvector, col_types = tvector) %>% mutate(ID = "vca")
# ds_vca1 <- read_delim(paste0(mpath,"FTC-VCA/FTCVIC02-15042019_1.txt"),"|", col_names = nvector, col_types = tvector) %>% mutate(ID = "vca1")
# ds_vca1_c <- f.tratamento(ds_vca1,paste0(mpath,"FTC-VCA/FTCVIC02-15042019_1_t.txt"), fwrt)
# 
# ds_vca2 <- read_delim(paste0(mpath,"FTC-VCA/FTCVIC02-15042019_2.txt"),"|", col_names = nvector, col_types = tvector) %>% mutate(ID = "vca2")
# ds_vca2_c <- f.tratamento(ds_vca2,paste0(mpath,"FTC-VCA/FTCVIC02-15042019_2_t.txt"), fwrt)

### OTE-ITABUNA
# ds_ita1 <- read_delim(paste0(mpath,"OTE-ITABUNA/07ITA_20190415_1.txt"),"|", col_names = nvector, col_types = tvector) %>% mutate(ID = "ita")
# ds_ita1_c <- f.tratamento(ds_ita1,paste0(mpath,"OTE-ITABUNA/07ITA_20190415_1_t.txt"), fwrt)
# 
# ds_ita2 <- read_delim(paste0(mpath,"OTE-ITABUNA/07ITA_20190415_2.txt"),"|", col_names = nvector, col_types = tvector) %>% mutate(ID = "ita")
# ds_ita2_c <- f.tratamento(ds_ita2,paste0(mpath,"OTE-ITABUNA/07ITA_20190415_2_t.txt"), fwrt)

### OTE-JUA
ds_jua <- read_delim(paste0(mpath,"OTE-JUA/21JUA_20190416.txt"),"|", col_names = nvector, col_types = tvector) %>% mutate(ID = "jua")
ds_jua_c <- f.tratamento(ds_jua,paste0(mpath,"OTE-JUA/21JUA_20190416_t.txt"), fwrt)

### OTE-PETROLINA
ds_pet <- read_delim(paste0(mpath,"OTE-PETROLINA/22PET_20190416.txt"),"|", col_names = nvector, col_types = tvector) %>% mutate(ID = "pet")
ds_pet_c <- f.tratamento(ds_pet,paste0(mpath,"OTE-PETROLINA/22PET_20190416_t.txt"), fwrt)

### OTE-SP
ds_sp <- read_delim(paste0(mpath,"OTE-SP/20SP_20190416.txt"),"|", col_names = nvector, col_types = tvector) %>% mutate(ID = "sp")
ds_sp_c <- f.tratamento(ds_sp,paste0(mpath,"OTE-SP/20SP_201904168_t.txt"), fwrt)

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
# ixini <- match("00359005551",ds_main$X04)
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


