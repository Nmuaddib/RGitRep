library("stringr")  # funções de manipulação de chars
library("dplyr")    # gramática de wrangling
library("ggplot2")  # apresentação gráfica
library("tibble")   # data sets melhorados
library("magrittr") # semantica serializada de instruções
library("readxl")   # leitor de arquivos excel
library("purrr")    # programação funcional
library("knitr")

## breezedark
## espresso

ds_prgppg <- read_excel("Programas PPG 2016.xlsx")
ds_prjppg <- read_excel("Projetos PPG 2016.xlsx")
ds_prg <- ds_prgppg %>% select(CD_PROGRAMA_IES,
                               NM_AREA_AVALIACAO,
                               CS_STATUS_JURIDICO,
                               DS_DEPENDENCIA_ADMINISTRATIVA,
                               ANO_INICIO_PROGRAMA,
                               NM_MODALIDADE_PROGRAMA,
                               CD_CONCEITO_PROGRAMA)

##### Filtro projetos de 2013-2016

ds_prjppg_13_16 <- ds_prjppg %>% 
  mutate(ANO_PRJ = str_sub(.$DH_INICIO, 1, 4)) %>%
  filter(ANO_PRJ >= 2013)

rm(ds_prjppg)

##### Filtro de membros discentes, docentes e junção com Projetos

f.membros <- function(file){
  ds <- read_excel(file) %>%
    arrange(DS_TIPO_MEMBRO) %>% 
    filter(DS_TIPO_MEMBRO %in% c("DISCENTE","DOCENTE")) %>% 
    merge(., ds_prjppg_13_16, by = "ID_PROJETO")
  return(ds)
}

ds_membros_13_16 <- rbind(f.membros("Membros dos projetos 2013.xlsx"),
                          f.membros("Membros dos projetos 2014.xlsx"),
                          f.membros("Membros dos projetos 2015.xlsx"),
                          f.membros("Membros dos projetos 2016.xlsx"))


##### -------------------------------------------------------------------------------------------

v_form_campos <- c("nro_id_cnpq", "seq_pessoa_fisica",
                   "nome_sucupira", "nome_cvlattes",
                   "nome_filtro_cvlattes", "sgl_instituicao",
                   "nme_instituicao", "cod_programa",
                   "nme_programa", "nme_area_avaliacao",
                   "seq_area_basica", "area_basica",
                   "grande_area_basica", "nro_nota_doutorado",
                   "nro_nota_mestrado", "nro_nota_mestrado_prof",
                   "dta_fim", "seq_tipo_categoria_vinculo",
                   "nivel_formacao", "ano_inicio_formacao",
                   "ano_fim_formacao", "formacao_concluida",
                   "sigla_pais_ies_formacao", "sigla_uf_ies_formacao",
                   "sigla_ies_formacao", "nome_ies_formacao",
                   "nome_curso_formacao", "cod_area_curso_formacao",
                   "grande_area_curso_formacao", "area_curso_formacao")

ds_frmdic <- read_excel("Formação dos discentes 2013-2016 - Tese.xlsx") %>% 
  mutate(nro_id_cnpq = str_pad(as.character(nro_id_cnpq), 
                               width =  16, 
                               side = "left", 
                               pad = "0"),
         seq_pessoa_fisica = as.character(seq_pessoa_fisica)) %>% 
  select(v_form_campos) %>% 
  mutate(doc_ou_disc = "DISCENTE")

ds_frmdic_mestrado <- read_excel("Dicentes mestrado tratada -tese.xlsx") %>% 
  mutate(nro_id_cnpq = str_pad(as.character(nro_id_cnpq), 
                               width =  16, 
                               side = "left", 
                               pad = "0"),
         seq_pessoa_fisica = as.character(seq_pessoa_fisica)) %>% 
  select(v_form_campos) %>% 
  mutate(doc_ou_disc = "DISCENTE")

ds_frmdoc <- read_excel("Formação dos docentes tese.xlsx") %>% 
  select(v_form_campos) %>% 
  mutate(doc_ou_disc = "DOCENTE")

## IMI ## ---------------------------------------------------------------------------------------

ds_IMI1_QPPP <- ds_frmdoc %>% 
  group_by(cod_programa, nome_filtro_cvlattes) %>% 
  summarise(QT = n()) %>% 
  group_by(cod_programa) %>% 
  summarise(QPPP = n())

ds_IMI1_QFDPD <- ds_frmdoc %>% 
  filter(nivel_formacao == "Doutorado") %>% 
  group_by(cod_programa, nome_curso_formacao) %>% 
  summarise(QT = n()) %>%   
  group_by(cod_programa) %>% 
  summarise(QFDPD = n()) 

ds_IMI1_QFDPM <- ds_frmdoc %>% 
  filter(nivel_formacao == "Mestrado") %>% 
  group_by(cod_programa, nome_curso_formacao) %>% 
  summarise(QT = n()) %>%   
  group_by(cod_programa) %>% 
  summarise(QFDPM = n()) 

ds_IMI1_QFDPG <- ds_frmdoc %>% 
  filter(nivel_formacao == "Graduação") %>% 
  group_by(cod_programa, nome_curso_formacao) %>% 
  summarise(QT = n()) %>%   
  group_by(cod_programa) %>% 
  summarise(QFDPG = n()) 

ds_IMI1_FCDo <- merge(ds_IMI1_QFDPD, ds_IMI1_QFDPM, by = "cod_programa", all.x = T) %>% 
  merge(., ds_IMI1_QFDPG, by = "cod_programa", all.x = T) %>% 
  merge(., ds_IMI1_QPPP, by = "cod_programa", all.x = T) %>% 
  mutate(DFDDo = QFDPD/QPPP,
         DFDM = QFDPM/QPPP,
         DFDG = QFDPG/QPPP) %>% 
  filter(DFDDo <= 1, DFDM <= 1, DFDG <= 1) %>%
  mutate(FCDo = (DFDDo + DFDM + DFDG)/3) %>% 
  merge(ds_prg, ., by.x = "CD_PROGRAMA_IES", by.y = "cod_programa", all.y = T)

 write.csv2(ds_IMI1_FCDo, "~/RGitRep/Tese MIDxPT/Analises/FCDo.csv")

##### -------------------------------------------------------------------------------------------

ds_IMI2_QDDP <- ds_frmdic %>% 
  group_by(cod_programa, nome_filtro_cvlattes) %>% 
  summarise(QT = n()) %>% 
  group_by(cod_programa) %>% 
  summarise(QDDP = n())

ds_IMI2_QFMDi <- ds_frmdic %>% 
  filter(nivel_formacao == "Mestrado") %>% 
  group_by(cod_programa, nome_curso_formacao) %>% 
  summarise(QT = n()) %>%   
  group_by(cod_programa) %>% 
  summarise(QFMDi = n()) 

ds_IMI2_QFGDi <- ds_frmdic %>% 
  filter(nivel_formacao == "Graduação") %>% 
  group_by(cod_programa, nome_curso_formacao) %>% 
  summarise(QT = n()) %>%   
  group_by(cod_programa) %>% 
  summarise(QFGDi = n()) 

ds_IMI2_FCDi <- merge(ds_IMI2_QFMDi, ds_IMI2_QFGDi, by = "cod_programa", all.y = T) %>% 
  merge(., ds_IMI2_QDDP, by = "cod_programa", all.x = T) %>% 
  mutate(DFMDi = QFMDi/QDDP,
         DFGDi = QFGDi/QDDP) %>% 
  filter(DFMDi <= 1, DFGDi <= 1) %>% 
  mutate(FCDi = (DFMDi + DFGDi)/2)
  
ds_IMI2_QDDPm <- ds_frmdic_mestrado %>% 
  group_by(cod_programa, nome_filtro_cvlattes) %>% 
  summarise(QT = n()) %>% 
  group_by(cod_programa) %>% 
  summarise(QDDPm = n())
  
ds_IMI2_QFGDim <- ds_frmdic_mestrado %>% 
  filter(nivel_formacao == "Graduação") %>% 
  group_by(cod_programa, nome_curso_formacao) %>% 
  summarise(QT = n()) %>%   
  group_by(cod_programa) %>% 
  summarise(QFGDim = n())

ds_IMI2_FCDim <- merge(ds_IMI2_QDDPm, ds_IMI2_QFGDim, by = "cod_programa", all = T) %>% 
  mutate(DFGDim = QFGDim/QDDPm) %>%
  filter(DFGDim <= 1) %>% 
  mutate(FCDim = DFGDim)
  
ds_IMI2_FCDi_inter <- merge(ds_IMI2_FCDim, ds_IMI2_FCDi, by = "cod_programa", all = T)

ds_IMI2_FCDi_inter <- ds_IMI2_FCDi_inter[is.na(ds_IMI2_FCDi_inter$FCDi),]

ds_IMI2_FCDi_inter %<>% select(cod_programa,
                               QFMDi,
                               QFGDi = QFGDim,
                               QDDP = QDDPm,
                               DFMDi,
                               DFGDi = DFGDim,
                               FCDi = FCDim)

ds_IMI2_FCDi %<>% rbind(., ds_IMI2_FCDi_inter) %>% 
  merge(ds_prg, ., by.x = "CD_PROGRAMA_IES", by.y = "cod_programa", all.y = T)

# rm(ds_IMI2_QDDP, ds_IMI2_QFMDi, ds_IMI2_QFGDi, ds_IMI2_FCDi)
 write.csv2(ds_IMI2_FCDi, "~/RGitRep/Tese MIDxPT/Analises/FCDi.csv")

##### -------------------------------------------------------------------------------------------

ds_fin <-  rbind(read_excel("Financiadores de proejtos 2013.xlsx"),
                 read_excel("Financiadores de projetos 2014.xlsx"),
                 read_excel("Financiadores de projetos 2015.xlsx"),
                 read_excel("Financiadores de proejtos 2016.xlsx"))

ds_IMI3_QIP <- ds_fin %>% 
  group_by(CD_PROGRAMA_IES, NM_FINANCIADOR) %>% 
  summarise(QT = n()) %>% 
  group_by(CD_PROGRAMA_IES) %>% 
  summarise(QIP = n())

ds_IMI3_QPV <- ds_fin %>% 
  group_by(CD_PROGRAMA_IES, ID_PROJETO) %>% 
  summarise(QT = n()) %>% 
  group_by(CD_PROGRAMA_IES) %>% 
  summarise(QPV = n())

ds_formacao_grd <- rbind(ds_frmdic, ds_frmdoc, ds_frmdic_mestrado) %>% 
  filter(nivel_formacao == "Graduação") %>% 
  group_by(cod_programa, 
           nome_filtro_cvlattes, 
           doc_ou_disc,
           nome_curso_formacao) %>%
  summarise(QT = n())

ds_MbrPrj_grd <- merge(ds_membros_13_16, ds_formacao_grd, 
                      by.x = c("CD_PROGRAMA_IES", "NM_MEMBRO_PROJETO", "DS_TIPO_MEMBRO"), 
                      by.y = c("cod_programa", "nome_filtro_cvlattes", "doc_ou_disc"))

ds_IMI3_DFPP <- ds_MbrPrj_grd %>% 
  group_by(CD_PROGRAMA_IES, ID_PROJETO, nome_curso_formacao) %>% 
  summarise(QT = n()) %>% 
  group_by(CD_PROGRAMA_IES, ID_PROJETO) %>% 
  summarise(DFPP = n()) 

ds_IMI3_QPP <- ds_MbrPrj_grd %>% 
  group_by(CD_PROGRAMA_IES, ID_PROJETO, NM_MEMBRO_PROJETO) %>% 
  summarise(QT = n()) %>% 
  group_by(CD_PROGRAMA_IES, ID_PROJETO) %>% 
  summarise(QPP = n()) 

ds_IMI3_DFCP <- merge(ds_IMI3_DFPP, ds_IMI3_QPP, by = c("CD_PROGRAMA_IES", "ID_PROJETO")) %>% 
  mutate(DFCP = DFPP/QPP) %>%
  filter(DFCP <= 1) %>% 
  group_by(CD_PROGRAMA_IES) %>% 
  summarise(DFPP = mean(DFPP),
            QPP = mean(QPP),
            DFCP = mean(DFCP))

ds_DFCP <- merge(ds_IMI3_DFPP, ds_IMI3_QPP , by = c("CD_PROGRAMA_IES", "ID_PROJETO")) %>% 
  mutate(DFCP = DFPP/QPP) %>% 
  filter(DFCP <= 1)
 
write.csv2(ds_DFCP, "~/RGitRep/Tese MIDxPT/Analises/DFCP.csv")  

ds_IMI3_CC <- merge(ds_IMI3_QIP, ds_IMI3_QPV, by = "CD_PROGRAMA_IES", all.x = T) %>% 
  merge(., ds_IMI3_DFCP, by = "CD_PROGRAMA_IES", all.x = T) %>% 
  mutate(DIPP = QIP/QPV) %>% 
  filter(DIPP <= 1) %>% 
  mutate(CC = (DFCP + DIPP)/2) 
  
ds_IMI3_CC <-  ds_IMI3_CC[,c(1:3, 7, 4:6, 8)]

ds_IMI3_CC %<>% merge(ds_prg, ., by = "CD_PROGRAMA_IES", all.y = T)

# rm(ds_IMI3_QIP, ds_IMI3_QPV, ds_IMI3_QACP, ds_IMI3_QPP, ds_IMI3_DFCP, ds_IMI3_CC)
 write.csv2(ds_IMI3_CC, "~/RGitRep/Tese MIDxPT/Analises/CC.csv")

##### -------------------------------------------------------------------------------------------
 
ds_aratdo <- read_excel('Areas_de_atuacao_docentes_25_06_2019.xlsx') 

ds_IMI4_QE <- ds_aratdo %>% 
  mutate(espec_doc = paste0(grande_area, nome_area, sub_area, especialidade)) %>% 
  group_by(cod_programa, nome_filtro_cvlattes, espec_doc) %>% 
  summarise(QT = n()) %>% 
  group_by(cod_programa, nome_filtro_cvlattes) %>% 
  summarise(QE = n())

ds_IMI4_DGAD <- ds_aratdo %>%
  group_by(cod_programa, nome_filtro_cvlattes, grande_area) %>% 
  summarise(QT = n()) %>% 
  group_by(cod_programa, nome_filtro_cvlattes) %>% 
  summarise(DGAD = n())

ds_IMI4_DACD <- ds_aratdo %>%
  group_by(cod_programa, nome_filtro_cvlattes, nome_area) %>% 
  summarise(QT = n()) %>% 
  group_by(cod_programa, nome_filtro_cvlattes) %>% 
  summarise(DACD = n())

ds_IMI4_CP <- merge(ds_IMI4_DGAD, ds_IMI4_DACD, by = c("cod_programa","nome_filtro_cvlattes"), all.x = T) %>% 
  merge(., ds_IMI4_QE, by = c("cod_programa","nome_filtro_cvlattes"), all.x = T) %>% 
  mutate(DGAC = DGAD/QE,
         DAC = DACD/QE) %>% 
  filter(DGAC <= 1, DAC <= 1) %>% 
  mutate(CP = (DGAC + DAC)/2) 

ds_IMI4_CP_pgr <- ds_IMI4_CP %>% 
  group_by(cod_programa) %>% 
  summarise(CP = mean(CP)) %>% 
  merge(ds_prg, ., by.x = "CD_PROGRAMA_IES", by.y = "cod_programa", all.y = T)

# rm(ds_IMI4_QAI, ds_IMI4_DGAD, ds_IMI4_DACD, ds_IMI4_CP, ds_IMI4_CP_pgr)
 write.csv2(ds_IMI4_CP_pgr, "~/RGitRep/Tese MIDxPT/Analises/CP_programa.csv")
 write.csv2(ds_IMI4_CP, "~/RGitRep/Tese MIDxPT/Analises/CP_docente.csv")
 
 ##### -------------------------------------------------------------------------------------------
 
ds_IMI <- merge(ds_IMI1_FCDo[,c("CD_PROGRAMA_IES", "FCDo")], ds_IMI2_FCDi[,c("CD_PROGRAMA_IES", "FCDi" )], by = "CD_PROGRAMA_IES", all = T) %>% 
  merge(., ds_IMI3_CC[,c("CD_PROGRAMA_IES","CC")], by = "CD_PROGRAMA_IES", all = T) %>% 
  merge(., ds_IMI4_CP_pgr[,c("CD_PROGRAMA_IES","CP")], by = "CD_PROGRAMA_IES", all = T) 

ds_IMI <- ds_IMI[complete.cases(ds_IMI),]

ds_IMI %<>% mutate(IMI = (FCDo + FCDi + CC + CP)/4) %>% 
  merge(ds_prg, ., by = "CD_PROGRAMA_IES", all.y = T)

write.csv2(ds_IMI, "~/RGitRep/Tese MIDxPT/Analises/IMI.csv")

## IPT ## ---------------------------------------------------------------------------------------

ds_PT_appprg <- read_excel("Aplicativos_tese.xlsx")
ds_PT_patprg <- read_excel("Patentes com Programas.xlsx")
ds_PT_prdprg <- read_excel("Produtos_tese.xlsx")

# rm(ds_PT_appprg, ds_PT_patprg, ds_PT_prdprg)

ds_PT_patprg <- ds_PT_patprg[complete.cases(ds_PT_patprg[,"NM_PRODUCAO"]),]

ds_IPT1_SPPAP <- ds_PT_patprg %>%
  group_by(CD_PROGRAMA_IES) %>% 
  summarise(SPPAP = n())

# write.csv2(ds_IPT1_SPPAP, "~/RGitRep/Tese MIDxPT/Analises/SPPAP.csv")

ds_PT_prdprg <- ds_PT_prdprg[complete.cases(ds_PT_prdprg[,"DSFINALIDADE_TRATADA"]),]

ds_IPT2_QPPr <- ds_PT_prdprg %>%
  group_by(CD_PROGRAMA_IES) %>% 
  summarise(QPPr = n())

# write.csv2(ds_IPT2_QPPr, "~/RGitRep/Tese MIDxPT/Analises/QPPr.csv")

ds_PT_appprg <- ds_PT_appprg[complete.cases(ds_PT_appprg[,"DS_FINALIDADE"]),]

ds_IPT3_QPA <- ds_PT_appprg %>%
  group_by(CD_PROGRAMA_IES) %>% 
  summarise(QPA = n())

# write.csv2(ds_IPT3_QPA, "~/RGitRep/Tese MIDxPT/Analises/QPA.csv")

ds_IPT <- merge(ds_IMI1_QPPP, ds_IPT1_SPPAP, by.x = "cod_programa", by.y = "CD_PROGRAMA_IES", all.x = T) %>% 
  select(CD_PROGRAMA_IES = cod_programa, QPPP, SPPAP) %>% 
  merge(., ds_IPT2_QPPr, by = "CD_PROGRAMA_IES", all.x = T) %>% 
  merge(., ds_IPT3_QPA, by = "CD_PROGRAMA_IES", all.x = T)

ds_IPT[is.na(ds_IPT)] <- 0

ds_IPT %<>% mutate(iQPP = SPPAP/QPPP,
                   iQPPr = QPPr/QPPP,
                   iQPA = QPA/QPPP) %>% 
  filter(iQPP <= 1, iQPPr <= 1, iQPA <= 1) %>% ## Limpeza de ocorrências acima de 1.0
  mutate(IPT = (iQPP+iQPPr+iQPA)/3) %>% 
  merge(ds_prg, ., by = "CD_PROGRAMA_IES", all.y = T)

write.csv2(ds_IPT, "~/RGitRep/Tese MIDxPT/Analises/IPT.csv")

## COR ## ---------------------------------------------------------------------------------------

ds_COR <- merge(ds_IMI[,c(1,12)], ds_IPT[,c(1,15)], by = "CD_PROGRAMA_IES", all = T)

ds_COR <- ds_COR[complete.cases(ds_COR),]

ds_COR %<>% filter(IPT > 0)%>% 
  merge(ds_prg, ., by = "CD_PROGRAMA_IES", all.y = T) 

ds_COR_p <- ds_COR %>% 
  select(IMI, IPT) %>% 
  cor(., method = "pearson")

write.csv2(ds_COR, "~/RGitRep/Tese MIDxPT/Analises/COR.csv")
write.csv2(ds_COR_p, "~/RGitRep/Tese MIDxPT/Analises/COR_p.csv")

##### ----------------------------------------------------------------------------------------
f.corr <- function(df, mtd = "pearson"){
  df %<>% select(IMI,IPT) %>% 
    cor(., method = mtd)
  return(df)
}

f.print_corr <- function(df){
  dfo <- tibble(cat = as.character(), cor = as.double())
  for (i in seq_along(df)) {
    print(paste0(names(df[i]), '|', df[[i]][1,2]))
    dfo[i,] <- c(names(df[i]), df[[i]][1,2])
  }
  return(dfo)
}
##### ---------------------------------------------------------------------------------------
ano_f <- factor(ds_COR$ANO_INICIO_PROGRAMA)

ds_COR_ano <- ds_COR %>% select(ANO_INICIO_PROGRAMA,IMI,IPT) %>% 
  split(ano_f, drop = T) %>%
  map(f.corr)

ds_COR_ano <- f.print_corr(ds_COR_ano)
write.csv2(ds_COR_ano, "~/RGitRep/Tese MIDxPT/Analises/COR_ano.csv")
##### ---------------------------------------------------------------------------------------
conceito_f <- factor(ds_COR$CD_CONCEITO_PROGRAMA)

ds_COR_conceito <- ds_COR %>% select(CD_CONCEITO_PROGRAMA,IMI,IPT) %>% 
  split(conceito_f, drop = T) %>%
  map(f.corr)

ds_COR_conceito <- f.print_corr(ds_COR_conceito)
write.csv2(ds_COR_conceito, "~/RGitRep/Tese MIDxPT/Analises/COR_conceito.csv")
##### ---------------------------------------------------------------------------------------
dependencia_f <- factor(ds_COR$DS_DEPENDENCIA_ADMINISTRATIVA)

ds_COR_dependencia <- ds_COR %>% select(DS_DEPENDENCIA_ADMINISTRATIVA,IMI,IPT) %>% 
  split(dependencia_f, drop = T) %>%
  map(f.corr)

ds_COR_dependencia <- f.print_corr(ds_COR_dependencia)
write.csv2(ds_COR_dependencia, "~/RGitRep/Tese MIDxPT/Analises/COR_dependencia.csv")
##### ---------------------------------------------------------------------------------------
area_f <- factor(ds_COR$NM_AREA_AVALIACAO)

ds_COR_area <- ds_COR %>% select(NM_AREA_AVALIACAO,IMI,IPT) %>% 
  split(area_f, drop = T) %>%
  map(f.corr)

ds_COR_area <- f.print_corr(ds_COR_area)
write.csv2(ds_COR_area, "~/RGitRep/Tese MIDxPT/Analises/COR_area.csv")
##### ---------------------------------------------------------------------------------------
status_f <- factor(ds_COR$CS_STATUS_JURIDICO)

ds_COR_status <- ds_COR %>% select(CS_STATUS_JURIDICO,IMI,IPT) %>% 
  split(status_f, drop = T) %>%
  map(f.corr)

ds_COR_status <- f.print_corr(ds_COR_status)
write.csv2(ds_COR_status, "~/RGitRep/Tese MIDxPT/Analises/COR_status.csv")
##### ---------------------------------------------------------------------------------------
modalidade_f <- factor(ds_COR$NM_MODALIDADE_PROGRAMA)

ds_COR_modalidade <- ds_COR %>% select(NM_MODALIDADE_PROGRAMA,IMI,IPT) %>% 
  split(modalidade_f, drop = T) %>%
  map(f.corr)

ds_COR_modalidade <- f.print_corr(ds_COR_modalidade)
write.csv2(ds_COR_modalidade, "~/RGitRep/Tese MIDxPT/Analises/COR_modalidade.csv")
##### ---------------------------------------------------------------------------------------

kable(ds_IMI1_FCDo[1:10,])
