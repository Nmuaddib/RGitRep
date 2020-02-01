library("stringr")
library("dplyr")
library("ggplot2")
library("readr")
library("tibble")
library("magrittr")
library("readxl")
library("modelr")
library("purrr")
#library("forcats")
#library("tidyr")
#library("reshape2")

ds_aratdo <- read_excel('Areas_de_atuacao_docentes_25_06_2019.xlsx')
ds_prgppg <- read_excel("Programas PPG 2016.xlsx")
ds_prjppg <- read_excel("Projetos PPG 2016.xlsx")

ds_prg <- ds_prgppg %>% select(CD_PROGRAMA_IES,
                               NM_AREA_AVALIACAO,
                               CS_STATUS_JURIDICO,
                               DS_DEPENDENCIA_ADMINISTRATIVA,
                               ANO_INICIO_PROGRAMA,
                               NM_MODALIDADE_PROGRAMA,
                               CD_CONCEITO_PROGRAMA)

# ds_prg[is.na(ds_prg)] <- 'Não identificado'
# 
# ds_prg %<>% select(CD_PROGRAMA_IES,
#                    NM_AREA_AVALIACAO = factor("NM_AREA_AVALIACAO"),
#                    CS_STATUS_JURIDICO = factor("CS_STATUS_JURIDICO"),
#                    DS_DEPENDENCIA_ADMINISTRATIVA = factor("DS_DEPENDENCIA_ADMINISTRATIVA"),
#                    ANO_INICIO_PROGRAMA = ANO_INICIO_PROGRAMA,
#                    NM_MODALIDADE_PROGRAMA = factor("NM_MODALIDADE_PROGRAMA"),
#                    CD_CONCEITO_PROGRAMA = factor("CD_CONCEITO_PROGRAMA"))

##### Filtro projetos de 2013-2016

ds_prjppg_13_16 <- ds_prjppg %>% 
  mutate(ANO_PRJ = str_sub(.$DH_INICIO, 1, 4)) %>%
  filter(ANO_PRJ >= 2013)

rm(ds_prjppg)

##### Filtro de membros discentes, docentes e junção com Projetos

ds_prj013 <- read_excel("Membros dos projetos 2013.xlsx") %>%
  arrange(DS_TIPO_MEMBRO) %>% 
  filter(DS_TIPO_MEMBRO %in% c("DISCENTE","DOCENTE")) %>% 
  merge(., ds_prjppg_13_16, by = "ID_PROJETO")

ds_prj014 <- read_excel("Membros dos projetos 2014.xlsx") %>%
  arrange(DS_TIPO_MEMBRO) %>% 
  filter(DS_TIPO_MEMBRO %in% c("DISCENTE","DOCENTE")) %>% 
  merge(., ds_prjppg_13_16, by = "ID_PROJETO")

ds_prj015 <- read_excel("Membros dos projetos 2015.xlsx") %>%
  arrange(DS_TIPO_MEMBRO) %>% 
  filter(DS_TIPO_MEMBRO %in% c("DISCENTE","DOCENTE")) %>% 
  merge(., ds_prjppg_13_16, by = "ID_PROJETO")

ds_prj016 <- read_excel("Membros dos projetos 2016.xlsx") %>%
  arrange(DS_TIPO_MEMBRO) %>% 
  filter(DS_TIPO_MEMBRO %in% c("DISCENTE","DOCENTE")) %>% 
  merge(., ds_prjppg_13_16, by = "ID_PROJETO")

ds_membros_13_16 <- rbind(ds_prj013, ds_prj014) %>% 
  rbind(., ds_prj015) %>% 
  rbind(., ds_prj016)

rm(ds_prj013, ds_prj014, ds_prj015, ds_prj016)

##### -------------------------------------------------------------------------------------------

ds_frmdic <- read_excel("Formação dos discentes 2013-2016 - Tese.xlsx") %>% 
  mutate(nro_id_cnpq = str_pad(as.character(nro_id_cnpq), 
                               width =  16, 
                               side = "left", 
                               pad = "0"),
         seq_pessoa_fisica = as.character(seq_pessoa_fisica))

ds_frmdic_mestrado <- read_excel("Dicentes mestrado.xlsx") %>% 
  mutate(nro_id_cnpq = str_pad(as.character(nro_id_cnpq), 
                               width =  16, 
                               side = "left", 
                               pad = "0"),
         seq_pessoa_fisica = as.character(seq_pessoa_fisica))

ds_frmdic %<>% select(nro_id_cnpq,
                      seq_pessoa_fisica,
                      nome_sucupira,
                      nome_cvlattes,
                      nome_filtro_cvlattes,
                      sgl_instituicao,
                      nme_instituicao,
                      cod_programa,
                      nme_programa,
                      nme_area_avaliacao,
                      seq_area_basica,
                      area_basica,
                      grande_area_basica,
                      nro_nota_doutorado,
                      nro_nota_mestrado,
                      nro_nota_mestrado_prof,
                      dta_fim,
                      seq_tipo_categoria_vinculo,
                      nivel_formacao,
                      ano_inicio_formacao,
                      ano_fim_formacao,
                      formacao_concluida,
                      sigla_pais_ies_formacao,
                      sigla_uf_ies_formacao,
                      sigla_ies_formacao,
                      nome_ies_formacao,
                      nome_curso_formacao,
                      cod_area_curso_formacao,
                      grande_area_curso_formacao,
                      area_curso_formacao) %>% 
  mutate(doc_ou_disc = "DISCENTE")

ds_frmdic_mestrado %<>% select(nro_id_cnpq,
                      seq_pessoa_fisica,
                      nome_sucupira,
                      nome_cvlattes,
                      nome_filtro_cvlattes,
                      sgl_instituicao,
                      nme_instituicao,
                      cod_programa,
                      nme_programa,
                      nme_area_avaliacao,
                      seq_area_basica,
                      area_basica,
                      grande_area_basica,
                      nro_nota_doutorado,
                      nro_nota_mestrado,
                      nro_nota_mestrado_prof,
                      dta_fim,
                      seq_tipo_categoria_vinculo,
                      nivel_formacao,
                      ano_inicio_formacao,
                      ano_fim_formacao,
                      formacao_concluida,
                      sigla_pais_ies_formacao,
                      sigla_uf_ies_formacao,
                      sigla_ies_formacao,
                      nome_ies_formacao,
                      nome_curso_formacao,
                      cod_area_curso_formacao,
                      grande_area_curso_formacao,
                      area_curso_formacao) %>% 
  mutate(doc_ou_disc = "DISCENTE")

ds_frmdoc <- read_excel("Formação dos docentes tese.xlsx")

ds_frmdoc %<>% select(nro_id_cnpq,
                      seq_pessoa_fisica,
                      nome_sucupira,
                      nome_cvlattes,
                      nome_filtro_cvlattes,
                      sgl_instituicao,
                      nme_instituicao,
                      cod_programa,
                      nme_programa,
                      nme_area_avaliacao,
                      seq_area_basica,
                      area_basica,
                      grande_area_basica,
                      nro_nota_doutorado,
                      nro_nota_mestrado,
                      nro_nota_mestrado_prof,
                      dta_fim,
                      seq_tipo_categoria_vinculo,
                      nivel_formacao,
                      ano_inicio_formacao,
                      ano_fim_formacao,
                      formacao_concluida,
                      sigla_pais_ies_formacao,
                      sigla_uf_ies_formacao,
                      sigla_ies_formacao,
                      nome_ies_formacao,
                      nome_curso_formacao,
                      cod_area_curso_formacao,
                      grande_area_curso_formacao,
                      area_curso_formacao) %>% 
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

# rm(ds_IMI1_QPPP, ds_IMI1_QFDPD, ds_IMI1_QFDPM, ds_IMI1_QFDPG, ds_IMI1_FCDo)
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

ds_formacao_grd <- rbind(ds_frmdic, ds_frmdoc, ds_frmdic_mestrado) %>% 
  filter(nivel_formacao == "Graduação")

ds_fin13 <- read_excel("Financiadores de proejtos 2013.xlsx")
ds_fin14 <- read_excel("Financiadores de projetos 2014.xlsx")
ds_fin15 <- read_excel("Financiadores de projetos 2015.xlsx")
ds_fin16 <- read_excel("Financiadores de proejtos 2016.xlsx")

ds_fin <- rbind(ds_fin13, ds_fin14, ds_fin15, ds_fin16)

rm(ds_fin13, ds_fin14, ds_fin15, ds_fin16)

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

# 10001018012P7 - DIPP acima de 1.0 -------------------------------------------------------------
##### -------------------------------------------------------------------------------------------

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

ds_IMI_1 <- merge(ds_IMI1_FCDo[,c(1,15)], ds_IMI2_FCDi[,c(1,13)], by = "CD_PROGRAMA_IES", all = T) %>% 
  merge(., ds_IMI3_CC[,c(1,14)], by = "CD_PROGRAMA_IES", all = T) %>% 
  merge(., ds_IMI4_CP_pgr[,c(1,8)], by = "CD_PROGRAMA_IES", all = T) 

ds_IMI_1 <- ds_IMI_1[complete.cases(ds_IMI_1),]

ds_IMI_1 %<>% mutate(IMI = (FCDo + FCDi + CC + CP)/4,
                     IMI_nDi = (FCDo + CC + CP)/3,
                     DIF = IMI - IMI_nDi,
                     PRC_DIF = (DIF/IMI)*100) %>% 
  merge(ds_prg, ., by = "CD_PROGRAMA_IES", all.y = T)

min(ds_IMI_1$PRC_DIF)
max(ds_IMI_1$PRC_DIF)
mean(ds_IMI_1$PRC_DIF)
                  

ds_IMI_2 <- merge(ds_IMI1_FCDo[,c(1,15)], ds_IMI3_CC[,c(1,14)], by = "CD_PROGRAMA_IES", all = T) %>% 
  merge(., ds_IMI4_CP_pgr[,c(1,8)], by = "CD_PROGRAMA_IES", all = T)

ds_IMI_2 <- ds_IMI_2[complete.cases(ds_IMI_2),]

ds_IMI_2 %<>% mutate(IMI = (FCDo + CC + CP)/3) %>% 
  merge(ds_prg, ., by = "CD_PROGRAMA_IES", all.y = T)

write.csv2(ds_IMI_1, "~/RGitRep/Tese MIDxPT/Analises/IMI_1.csv")
write.csv2(ds_IMI_2, "~/RGitRep/Tese MIDxPT/Analises/IMI_2.csv")

## IPT ## ---------------------------------------------------------------------------------------

ds_PT_appprg <- read_excel("Aplicativos_tese.xlsx")
ds_PT_patprg <- read_excel("Patentes com Programas.xlsx")
ds_PT_prdprg <- read_excel("Produtos_tese.xlsx")

# rm(ds_PT_appprg, ds_PT_patprg, ds_PT_prdprg)

ls(ds_PT_appprg)
ls(ds_PT_patprg)
ls(ds_PT_prdprg)

min(ds_PT_appprg$AN_BASE_PRODUCAO)
min(ds_PT_patprg$AN_BASE)
min(ds_PT_prdprg$AN_BASE_PRODUCAO)

# CD_PROGRAMA_IES

ds_PT_patprg <- ds_PT_patprg[complete.cases(ds_PT_patprg[,8]),]

ds_IPT1_SPPAP <- ds_PT_patprg %>%
  group_by(CD_PROGRAMA_IES) %>% 
  summarise(SPPAP = n())

# write.csv2(ds_IPT1_SPPAP, "~/RGitRep/Tese MIDxPT/Analises/SPPAP.csv")

# nm_produção

ds_PT_prdprg <- ds_PT_prdprg[complete.cases(ds_PT_prdprg[,12]),]

ds_IPT2_QPPr <- ds_PT_prdprg %>%
  group_by(CD_PROGRAMA_IES) %>% 
  summarise(QPPr = n())

# write.csv2(ds_IPT2_QPPr, "~/RGitRep/Tese MIDxPT/Analises/QPPr.csv")

# ds_finalidade_tratada

ds_PT_appprg <- ds_PT_appprg[complete.cases(ds_PT_appprg[,22]),]

ds_IPT3_QPA <- ds_PT_appprg %>%
  group_by(CD_PROGRAMA_IES) %>% 
  summarise(QPA = n())

# write.csv2(ds_IPT3_QPA, "~/RGitRep/Tese MIDxPT/Analises/QPA.csv")

# ds_finalidade

ds_IPT <- merge(ds_IMI1_QPPP, ds_IPT1_SPPAP, by.x = "cod_programa", by.y = "CD_PROGRAMA_IES", all.x = T) %>% 
  select(CD_PROGRAMA_IES = cod_programa, QPPP, SPPAP) %>% 
  merge(., ds_IPT2_QPPr, by = "CD_PROGRAMA_IES", all.x = T) %>% 
  merge(., ds_IPT3_QPA, by = "CD_PROGRAMA_IES", all.x = T)

ds_IPT[is.na(ds_IPT)] <- 0

ds_IPT %<>% mutate(iQPP = SPPAP/QPPP,
                   iQPPr = QPPr/QPPP,
                   iQPA = QPA/QPPP,
                   IPT = (iQPP+iQPPr+iQPA)/3) %>% 
  merge(ds_prg, ., by = "CD_PROGRAMA_IES", all.y = T)

write.csv2(ds_IPT, "~/RGitRep/Tese MIDxPT/Analises/IPT.csv")

## COR ## ---------------------------------------------------------------------------------------

ds_COR_1 <- merge(ds_IMI_1[,c(1,12)], ds_IPT[,c(1,15)], by = "CD_PROGRAMA_IES", all = T)

ds_COR_1 <- ds_COR_1[complete.cases(ds_COR_1),]

ds_COR_1 %<>% filter(IPT > 0)%>% 
  merge(ds_prg, ., by = "CD_PROGRAMA_IES", all.y = T) 

ds_COR_1_p <- ds_COR_1 %>% 
  select(IMI, IPT) %>% 
  cor(., method = "pearson")

ds_COR_2 <- merge(ds_IMI_2[,c(1,11)], ds_IPT[,c(1,15)], by = "CD_PROGRAMA_IES", all = T)

ds_COR_2 <- ds_COR_2[complete.cases(ds_COR_2),]

ds_COR_2 %<>% filter(IPT > 0)%>% 
  merge(ds_prg, ., by = "CD_PROGRAMA_IES", all.y = T) 

ds_COR_2_p <- ds_COR_2 %>% 
  select(IMI, IPT) %>% 
  cor(., method = "pearson")

write.csv2(ds_COR_1, "~/RGitRep/Tese MIDxPT/Analises/COR_1.csv")
write.csv2(ds_COR_1_p, "~/RGitRep/Tese MIDxPT/Analises/COR_1_p.csv")
write.csv2(ds_COR_2, "~/RGitRep/Tese MIDxPT/Analises/COR_2.csv")
write.csv2(ds_COR_2_p, "~/RGitRep/Tese MIDxPT/Analises/COR_2_p.csv")

##### -------------------------------------------------------------------------------------------
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
ano_f <- factor(ds_COR_1$ANO_INICIO_PROGRAMA)

ds_COR_ano <- ds_COR_1 %>% select(ANO_INICIO_PROGRAMA,IMI,IPT) %>% 
  split(ano_f, drop = T) %>%
  map(f.corr)

ds_COR_ano <- f.print_corr(ds_COR_ano)
write.csv2(ds_COR_ano, "~/RGitRep/Tese MIDxPT/Analises/COR_ano.csv")
##### ---------------------------------------------------------------------------------------
conceito_f <- factor(ds_COR_1$CD_CONCEITO_PROGRAMA)

ds_COR_conceito <- ds_COR_1 %>% select(CD_CONCEITO_PROGRAMA,IMI,IPT) %>% 
  split(conceito_f, drop = T) %>%
  map(f.corr)

ds_COR_conceito <- f.print_corr(ds_COR_conceito)
write.csv2(ds_COR_conceito, "~/RGitRep/Tese MIDxPT/Analises/COR_conceito.csv")
##### ---------------------------------------------------------------------------------------
dependencia_f <- factor(ds_COR_1$DS_DEPENDENCIA_ADMINISTRATIVA)

ds_COR_dependencia <- ds_COR_1 %>% select(DS_DEPENDENCIA_ADMINISTRATIVA,IMI,IPT) %>% 
  split(dependencia_f, drop = T) %>%
  map(f.corr)

ds_COR_dependencia <- f.print_corr(ds_COR_dependencia)
write.csv2(ds_COR_dependencia, "~/RGitRep/Tese MIDxPT/Analises/COR_dependencia.csv")
##### ---------------------------------------------------------------------------------------
area_f <- factor(ds_COR_1$NM_AREA_AVALIACAO)

ds_COR_area <- ds_COR_1 %>% select(NM_AREA_AVALIACAO,IMI,IPT) %>% 
  split(area_f, drop = T) %>%
  map(f.corr)

ds_COR_area <- f.print_corr(ds_COR_area)
write.csv2(ds_COR_area, "~/RGitRep/Tese MIDxPT/Analises/COR_area.csv")
##### -------------------------------------------------------------------------------------------
status_f <- factor(ds_COR_1$CS_STATUS_JURIDICO)

ds_COR_status <- ds_COR_1 %>% select(CS_STATUS_JURIDICO,IMI,IPT) %>% 
  split(status_f, drop = T) %>%
  map(f.corr)

ds_COR_status <- f.print_corr(ds_COR_status)
write.csv2(ds_COR_status, "~/RGitRep/Tese MIDxPT/Analises/COR_status.csv")
##### -------------------------------------------------------------------------------------------
modalidade_f <- factor(ds_COR_1$NM_MODALIDADE_PROGRAMA)

ds_COR_modalidade <- ds_COR_1 %>% select(NM_MODALIDADE_PROGRAMA,IMI,IPT) %>% 
  split(modalidade_f, drop = T) %>%
  map(f.corr)

ds_COR_modalidade <- f.print_corr(ds_COR_modalidade)
write.csv2(ds_COR_modalidade, "~/RGitRep/Tese MIDxPT/Analises/COR_modalidade.csv")
##### -------------------------------------------------------------------------------------------

# f.sum_frm <- function(ds){
#  
#   ds %<>% group_by(ID_PESSOA,
#                    NM_MEMBRO_PROJETO,
#                    ID_PROJETO,
#                    NM_PROJETO,
#                    nome_curso_formacao,  
#                    DS_TIPO_MEMBRO,
#                    AN_INICIO_PROGRAMA = AN_INICIO_PROGRAMA.x,
#                    ano_fim_formacao,
#                    ano_inicio_formacao,
#                    ANO_PRJ,
#                    area_basica,
#                    area_curso_formacao,
#                    CD_PROGRAMA_IES,
#                    cod_area_curso_formacao,
#                    DH_INICIO,
#                    DH_INICIO_LINHA,
#                    DS_CATEGORIA_MEMBRO_PROJETO,
#                    DS_PROJETO,
#                    DT_SITUACAO_ATUAL,
#                    dta_fim,
#                    formacao_concluida,
#                    grande_area_basica,
#                    grande_area_curso_formacao,
#                    ID_AREA_AVALIACAO,
#                    IN_BOLSA,
#                    IN_OUTRO_AUXILIO,
#                    nivel_formacao,
#                    NM_AREA_AVALIACAO,
#                    NM_AREA_CONCENTRACAO,
#                    NM_ENTIDADE_ENSINO,
#                    NM_LINHA_PESQUISA,
#                    NM_MODALIDADE_PROGRAMA,
#                    NM_NATUREZA_PROJETO,
#                    NM_PAIS_NACIONALIDADE_MEMBRO,
#                    NM_PROGRAMA_IES,
#                    NM_TIPO_SITUACAO_ATUAL,
#                    nme_area_avaliacao,
#                    nme_instituicao,
#                    nme_programa,
#                    nome_cvlattes,
#                    nome_ies_formacao,
#                    nome_sucupira,
#                    nro_id_cnpq,
#                    nro_nota_doutorado,
#                    nro_nota_mestrado,
#                    nro_nota_mestrado_prof,
#                    seq_area_basica,
#                    seq_pessoa_fisica,
#                    seq_tipo_categoria_vinculo,
#                    SG_ENTIDADE_ENSINO,
#                    sgl_instituicao,
#                    sigla_ies_formacao,
#                    sigla_pais_ies_formacao,
#                    sigla_uf_ies_formacao,
#                    TP_SEXO_MEMBRO
#                    # DT_FIM_VINCULO,
#                    # DT_INICIO_VINCULO,                   
#                    # ID_ADD_CONTEXTO = ID_ADD_CONTEXTO.x,
#                    # ID_ADD_DISCENTE,
#                    # ID_ADD_DOCENTE,
#                    # ID_ADD_FOTO_PROGRAMA = ID_ADD_FOTO_PROGRAMA.x,
#                    # ID_ADD_FOTO_PROGRAMA_IES = ID_ADD_FOTO_PROGRAMA_IES.x,
#                    # ID_ADD_MEMBRO_PROJETO,
#                    # ID_ADD_PARTICIPANTE_EXTERNO,                   
#                    ) %>% 
#           summarise(ULTIMO_ANO = max(AN_BASE.x),
#                     IN_RESPONSAVEL_PROJETO = max(IN_RESPONSAVEL_PROJETO))
#   
#   return(ds)
#   
# }

# rm(ds_Mbr_sum_grd, ds_Mbr_sum_doc, ds_Mbr_sum_mst)
#
# ds_Mbr_sum_grd <- f.sum_frm(ds_MbrPrj_grd)
# ds_Mbr_sum_doc <- f.sum_frm(ds_MbrPrj_doc)
# ds_Mbr_sum_mst <- f.sum_frm(ds_MbrPrj_mst)

# ds_general <- ds_membros_13_16 %>% 
#   group_by(CD_PROGRAMA_IES,
#            NM_PROGRAMA_IES,
#            DS_TIPO_MEMBRO,
#            DS_CATEGORIA_MEMBRO_PROJETO) %>% 
#   summarise(QT = n())

# f.ind <- function(ds){
#   ds %<>% group_by(CD_PROGRAMA_IES,
#                    NM_PROGRAMA_IES,
#                    DS_TIPO_MEMBRO,
#                    #DS_CATEGORIA_MEMBRO_PROJETO,
#                    NM_AREA_AVALIACAO,
#                    nome_curso_formacao,) %>% 
#           summarise(QT = n())
#   
#   return(ds)
# }

# ds_ind_grd <- f.ind(ds_Mbr_sum_grd)
# ds_ind_doc <- f.ind(ds_Mbr_sum_doc)
# ds_ind_mst <- f.ind(ds_Mbr_sum_mst)

##### -------------------------------------------------------------------------------------------

# ds_model <- merge(ds_IMI1_FCDo, ds_IMI2_FCDi, by = "cod_programa") %>% 
#   select(FCDo, FCDi) %>% 
#   as_tibble() #%>% 
#   cor(., method = "pearson", use = "complete.obs")
# 
# o_cor <- symnum(ds_model); o_cor
# 
# f.lm_checkin <- function(ds, modl) {
#   lm.x <- lm(modl, data = ds)
#   print(summary(lm.x))
#   print(confint(lm.x, level=0.99))
#   for (i in c(1:3,5)) {
#     plot(lm.x, which = i)
#   }
#   return(lm.x)
# }

#lm.FC <- f.lm_checkin(ds_model, 'FCDi ~ FCDo')
# ds_IMI_Models <- ds_IMI_1 %>% 
#   mutate(model1 = (FCDo+CC+CP)/3,
#          model2 = (FCDo+CC)/2,
#          model3 = (FCDo+CP)/2,
#          model4 = (CC+CP)/2)
# 
# lm.FC <- f.lm_checkin(ds_IMI_Models, 'FCDi ~ model1')
# ds_IMI_Models %<>% mutate(model = model1)
# 
# lm.FC <- f.lm_checkin(ds_IMI_Models, 'FCDi ~ model2')
# ds_IMI_Models %<>% mutate(model = model2)
# 
# lm.FC <- f.lm_checkin(ds_IMI_Models, 'FCDi ~ model3')
# ds_IMI_Models %<>% mutate(model = model3)
# 
# lm.FC <- f.lm_checkin(ds_IMI_Models, 'FCDi ~ poly(model4, 1, raw = T)')
# ds_IMI_Models %<>% mutate(model = model4)
# 
# 
# lm.FC <- lm(data = ds_IMI_Models, formula = FCDi ~ poly(FCDo, 2, raw = T))
# summary(lm.FC)
# ds_IMI_Models %<>% mutate(model = FCDo)
# 
# 
# lm.FC <- f.lm_checkin(ds_IMI_Models, 'FCDi ~ poly(FCDo, 6, raw = T)')
# lm.FC <- f.lm_checkin(ds_IMI_Models, 'FCDi ~ FCDo')
# ds_IMI_Models %<>% mutate(model = FCDo)
# 
# lm.FC <- f.lm_checkin(ds_IMI_Models, 'FCDi ~ CC')
# ds_IMI_Models %<>% mutate(model = CC)
# 
# lm.FC <- f.lm_checkin(ds_IMI_Models, 'FCDi ~ CP')
# ds_IMI_Models %<>% mutate(model = CP)

##### Incluir conluna com valores previstos ------------------
# ds_IMI_Models %<>% add_predictions(lm.FC) #modelr
##### Gráfico do modelo --------------------------------------
# ggplot(ds_IMI_Models, aes(y = model)) +
#   geom_point(aes(x = FCDi), color = "blue", alpha = 0.5, size = 3) +
#   geom_line(aes(x = pred),color = "red", size = 1.2) +
#   labs(title = 'Modelo proposto', 
#        x = 'FCDi', y = 'Model')
#
###### -------------------------------------------------------------------------------------------