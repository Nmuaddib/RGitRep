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

mpath <- "C:/R/RGitRep/Censo_ftc/"
arq_output <- paste0(mpath,"FTC-EAD/30EAD_2_20190410_t.txt")

fast_output <- "Y"

nvector <- c(1:63)
for (i in seq_along(nvector)) nvector[i] <- paste0("X", str_pad(i, side = "left", width = 2, pad = "0"))

tvector <- ""
for (i in 1:63) tvector <- paste0(tvector,"c")

ds_ead <- read_delim(paste0(mpath,"FTC-EAD/30EAD_2_20190410.txt"),"|", col_names = nvector, col_types = tvector) %>% mutate(ID = "ead")

ds_main <- ds_ead %>% 
  # ds_com %>%
  # rbind(.,ds_jeq) %>%
  # rbind(.,ds_fsa1) %>%
  # rbind(.,ds_fsa2) %>%
  # rbind(.,ds_sal1) %>%
  # rbind(.,ds_sal2) %>%
  # rbind(.,ds_vca1) %>%
  # rbind(.,ds_vca2) %>%
  # rbind(.,ds_ita) %>%
  # rbind(.,ds_jua) %>%
  # rbind(.,ds_pet) %>%
  # rbind(.,ds_sp[,1:64]) %>% 
  mutate(idx = 1)

for (i in (1:nrow(ds_main))) ds_main[i,"idx"] <- i

####

ds_alunos <- ds_main[,c("X01","X04","idx")] %>% 
  filter(X01 == "41") %>% 
  select(ALUNO = X04,idx) %>% 
  mutate(idxi=idx+1,idxf = 1)##"[["(.,1)

for (i in 1:nrow(ds_alunos)) ds_alunos[i,4] <- ds_alunos[i+1, 2]-1

ds_alunos %<>% mutate(diff = idxf - idxi)
ds_alunos_multicurs <- ds_alunos[,-1] %>% 
  filter(diff != 0)

####
rm(ds_cursosdup)
ds_cursosdup <- ds_main[ds_alunos_multicurs[1,"idxi"][[1]]:ds_alunos_multicurs[1,"idxf"][[1]], ] %>% 
  select(X03, ifelse(is.na("X06"),"99","X06")) %>% 
  mutate(ALUNO = ds_main[ds_alunos_multicurs[1,"idx"][[1]],"X04"][[1]], IDX = ds_main[ds_alunos_multicurs[1,"idx"][[1]],"idx"][[1]], CT = 1) %>% 
  group_by(ALUNO,IDX,X03,X06) %>% 
  summarise(CD = sum(CT, na.rm = T))## %>% 
##filter(CD > 1)

if (nrow(ds_alunos_multicurs) > 1) {
 for(i in 2:nrow(ds_alunos_multicurs)) {
   ds_temp <- ds_main[ds_alunos_multicurs[i,"idxi"][[1]]:ds_alunos_multicurs[i,"idxf"][[1]], ] %>% 
     select(X03, X06) %>% 
     mutate(ALUNO = ds_main[ds_alunos_multicurs[i,"idx"][[1]],"X04"][[1]], IDX = ds_main[ds_alunos_multicurs[i,"idx"][[1]],"idx"][[1]], CT = 1) %>% 
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
   if (!is.na(ds_cursosdup[1,"X06"][[1]])) {
    ds_temp <- ds_main[ds_cursosdup[i,"idxi"][[1]]:ds_cursosdup[i,"idxf"][[1]],] %>% 
      filter(X03 == ds_cursosdup[i,"X03"][[1]], X06 == ds_cursosdup[i,"X06"][[1]]) %>% 
      mutate(INGR = paste0(str_sub(.$X11, 3, 6), str_sub(.$X11, 1, 2))) %>% 
      arrange(INGR)
   } else {
     ds_temp <- ds_main[ds_cursosdup[i,"idxi"][[1]]:ds_cursosdup[i,"idxf"][[1]],] %>% 
       filter(X03 == ds_cursosdup[i,"X03"][[1]]) %>% 
       mutate(INGR = paste0(str_sub(.$X11, 3, 6), str_sub(.$X11, 1, 2))) %>% 
       arrange(INGR)     
   }
   for (j in 1:ds_cursosdup[i,"CD"][[1]]-1) del_idx <- append(del_idx, ds_temp[j,"idx"][[1]])
 }
 
 ds_main_cl <- ds_main[-del_idx,]
} else {
 ds_main_cl <- ds_main
}

############################################################################  

ds_transf_cpf <- ds_transfint[,2] %>%
  mutate(lin = 1) %>% 
  group_by(CPF) %>% 
  summarise(CT = sum(lin))
v_transf <- ds_transf_cpf[,1][[1]]

# for (i in seq_along(v_transf)) {
#   
# }

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
    } else {
      print("teste")
    }
  }
}



