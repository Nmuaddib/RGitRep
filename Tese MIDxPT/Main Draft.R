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

ds_aratdo <- read_excel('Areas_de_atuacao_docentes_25_06_2019.xlsx')
# ds_appprg <- read_excel("Dados Aplicativos com Programa.xlsx")
# ds_patprg <- read_excel("Dados Patentes com Programa.xlsx")
# ds_prdprg <- read_excel("Dados Produtos com Programa.xlsx")
ds_prgppg <- read_excel("Programas PPG 2016.xlsx")
ds_prjppg <- read_excel("Projetos PPG 2016.xlsx")

 ds_appprg <- read_excel("Produtos_tese.xlsx")
 ds_patprg <- read_excel("Patentes_tese.xlsx")
 ds_prdprg <- read_excel("Aplicativos_tese.xlsx")

# ls(ds_prgppg)
# ls(ds_prjppg)

# ds_pgpj <- merge(ds_prjppg, ds_prgppg, by = "CD_PROGRAMA_IES", no.dups = T) %>% 
#   mutate(ANO_PRJ = str_sub(.$DH_INICIO, 1, 4)) %>% 
#   select(ANO_INICIO_PROGRAMA,
#          ANO_PRJ,
#          CD_AREA_AVALIACAO, 
#          NM_AREA_AVALIACAO = NM_AREA_AVALIACAO.x, 
#          CD_PROGRAMA_IES, 
#          NM_PROGRAMA_IES = NM_PROGRAMA_IES.x, 
#          ID_PROJETO, DS_PROJETO) %>% 
#   filter(ANO_PRJ >= 2013) %>% 
#   group_by(NM_AREA_AVALIACAO, NM_PROGRAMA_IES) %>% 
#   summarise(QT_PRJ = n_distinct(ID_PROJETO)) %>% 
#   arrange(NM_AREA_AVALIACAO, desc(QT_PRJ))
# 
# write.csv2(ds_pgpj, "Qt_Projetos por Programa 2013-2016.csv")

# rm(ds_pgpj)

# ds_anos_prj <- ds_prjppg %>% 
#   mutate(ANO_PRJ = str_sub(.$DH_INICIO, 1, 4)) %>%
#   group_by(ANO_PRJ) %>% 
#   summarise(n())
# 
# write.csv2(ds_anos_prj, "Projetos por ano inicio do programa.csv")

##### Filtro projetos de 2013-2016

ds_prjppg_13_16 <- ds_prjppg %>% 
  mutate(ANO_PRJ = str_sub(.$DH_INICIO, 1, 4)) %>%
  filter(ANO_PRJ >= 2013)

rm(ds_prgppg, ds_prjppg)

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

# ls(ds_prj013_dis)
# ls(ds_frmdic)
# ls(ds_disprjfrm)
# ?filter

##### Filtra nivel de formação 

#ds_frmdic_grad <- ds_frmdic %>% filter(nivel_formacao == "Graduação")

ds_frmdic <- read_excel("Formação dos discentes 2013-2016 - Tese.xlsx") %>% 
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

ds_QPPP <- ds_frmdoc %>% 
  group_by(cod_programa, nome_filtro_cvlattes) %>% 
  summarise(QT = n()) %>% 
  group_by(cod_programa) %>% 
  summarise(QPPP = n())

ds_QFDPD <- ds_frmdoc %>% 
  filter(nivel_formacao == "Doutorado") %>% 
  group_by(cod_programa, nome_curso_formacao) %>% 
  summarise(QT = n()) %>%   
  group_by(cod_programa) %>% 
  summarise(QFDPD = n()) 

ds_QFDPM <- ds_frmdoc %>% 
  filter(nivel_formacao == "Mestrado") %>% 
  group_by(cod_programa, nome_curso_formacao) %>% 
  summarise(QT = n()) %>%   
  group_by(cod_programa) %>% 
  summarise(QFDPM = n()) 

ds_QFDPG <- ds_frmdoc %>% 
  filter(nivel_formacao == "Graduação") %>% 
  group_by(cod_programa, nome_curso_formacao) %>% 
  summarise(QT = n()) %>%   
  group_by(cod_programa) %>% 
  summarise(QFDPG = n()) 

ds_FCDo <- merge(ds_QFDPD, ds_QFDPM, by = "cod_programa", all.x = T) %>% 
  merge(., ds_QFDPG, by = "cod_programa", all.x = T) %>% 
  merge(., ds_QPPP, by = "cod_programa", all.x = T) %>% 
  mutate(DFDDo = QFDPD/QPPP,
         DFDM = QFDPM/QPPP,
         DFDG = QFDPG/QPPP) %>% 
  mutate(FCDo = (DFDDo + DFDM + DFDG)/3)

write.csv2(ds_FCDo, "FCDo.csv")

ds_QDDP <- ds_frmdic %>% 
  group_by(cod_programa, nome_filtro_cvlattes) %>% 
  summarise(QT = n()) %>% 
  group_by(cod_programa) %>% 
  summarise(QDDP = n())

ds_QFMDi <- ds_frmdic %>% 
  filter(nivel_formacao == "Mestrado") %>% 
  group_by(cod_programa, nome_curso_formacao) %>% 
  summarise(QT = n()) %>%   
  group_by(cod_programa) %>% 
  summarise(QFMDi = n()) 

ds_QFGDi <- ds_frmdic %>% 
  filter(nivel_formacao == "Graduação") %>% 
  group_by(cod_programa, nome_curso_formacao) %>% 
  summarise(QT = n()) %>%   
  group_by(cod_programa) %>% 
  summarise(QFGDi = n()) 

ds_FCDi <- merge(ds_QFMDi, ds_QFGDi, by = "cod_programa", all.y = T) %>% 
  merge(., ds_QDDP, by = "cod_programa", all.x = T) %>% 
  mutate(DFMDi = QFMDi/QDDP,
         DFGDi = QFGDi/QDDP) %>% 
  mutate(FCDi = (DFMDi + DFGDi)/2)

write.csv2(ds_FCDi, "FCDi.csv")

##### --------------------------------------------------------
ds_formacao_grd <- rbind(ds_frmdic, ds_frmdoc) %>% 
  filter(nivel_formacao == "Graduação")

# ds_formacao_doc <- rbind(ds_frmdic, ds_frmdoc) %>% 
#   filter(nivel_formacao == "Doutorado")
# 
# ds_formacao_mst <- rbind(ds_frmdic, ds_frmdoc) %>% 
#   filter(nivel_formacao == "Mestrado")

##### Junção de Formação (graduação) com membros de projeto por Nome+ID Programa+tipo de membro

# ds_disprjfrm <- merge(ds_prj013_dis, ds_frmdic_grad, 
#                       by.x = c("CD_PROGRAMA_IES", "NM_MEMBRO_PROJETO"), 
#                       by.y = c("cod_programa", "nome_filtro_cvlattes"))

ds_MbrPrj_grd <- merge(ds_membros_13_16, ds_formacao_grd, 
                      by.x = c("CD_PROGRAMA_IES", "NM_MEMBRO_PROJETO", "DS_TIPO_MEMBRO"), 
                      by.y = c("cod_programa", "nome_filtro_cvlattes", "doc_ou_disc"))

# ds_MbrPrj_doc <- merge(ds_membros_13_16, ds_formacao_doc, 
#                       by.x = c("CD_PROGRAMA_IES", "NM_MEMBRO_PROJETO", "DS_TIPO_MEMBRO"), 
#                       by.y = c("cod_programa", "nome_filtro_cvlattes", "doc_ou_disc"))
# 
# ds_MbrPrj_mst <- merge(ds_membros_13_16, ds_formacao_mst, 
#                       by.x = c("CD_PROGRAMA_IES", "NM_MEMBRO_PROJETO", "DS_TIPO_MEMBRO"), 
#                       by.y = c("cod_programa", "nome_filtro_cvlattes", "doc_ou_disc"))

# ls(ds_membros_13_16)
# ls(ds_formacao)
# ds_dis <- ds_prj013_dis %>% select(CD_PROGRAMA_IES, NM_MEMBRO_PROJETO)
# ds_frm <- ds_frmdic %>% select(cod_programa, nome_filtro_cvlattes)


##### Quantidade de vinculos por Membro/Formação (Projetos??)

# ds_qt_disprjfrm <- ds_disprjfrm %>% 
#   group_by(AN_BASE.x,
#            NM_AREA_AVALIACAO,
#            nivel_formacao,
#            CD_PROGRAMA_IES, 
#            NM_PROGRAMA_IES, 
#            NM_MEMBRO_PROJETO,
#            nome_curso_formacao) %>% 
#   summarise(QT_VINC = n()) %>% 
#   arrange(desc(QT_VINC))
# 
# write.csv2(ds_qt_disprjfrm, "Qt_Vinculos por membro de projeto.csv")
# write.csv2(ds_disprjfrm, "Membros_projetos_formação.csv")
 
# ls(ds_appprg$NM_FINANCIADOR)
# ds_appprg$NM_FINANCIADOR
# ls(ds_frmdic)
# ls(ds_frmdoc)
# view(names(ds_frmdoc))

# ls(ds_temp)

# ds_temp <- ds_MbrPrjFrm %>% select(ID_PROJETO, AN_BASE.x, NM_MEMBRO_PROJETO, nome_curso_formacao)

f.sum_frm <- function(ds){
  
  ds %<>% group_by(ID_PESSOA,
                   NM_MEMBRO_PROJETO,
                   ID_PROJETO,
                   NM_PROJETO,
                   nome_curso_formacao,  
                   DS_TIPO_MEMBRO,
                   AN_INICIO_PROGRAMA = AN_INICIO_PROGRAMA.x,
                   ano_fim_formacao,
                   ano_inicio_formacao,
                   ANO_PRJ,
                   area_basica,
                   area_curso_formacao,
                   CD_PROGRAMA_IES,
                   cod_area_curso_formacao,
                   DH_INICIO,
                   DH_INICIO_LINHA,
                   DS_CATEGORIA_MEMBRO_PROJETO,
                   DS_PROJETO,
                   DT_SITUACAO_ATUAL,
                   dta_fim,
                   formacao_concluida,
                   grande_area_basica,
                   grande_area_curso_formacao,
                   ID_AREA_AVALIACAO,
                   IN_BOLSA,
                   IN_OUTRO_AUXILIO,
                   nivel_formacao,
                   NM_AREA_AVALIACAO,
                   NM_AREA_CONCENTRACAO,
                   NM_ENTIDADE_ENSINO,
                   NM_LINHA_PESQUISA,
                   NM_MODALIDADE_PROGRAMA,
                   NM_NATUREZA_PROJETO,
                   NM_PAIS_NACIONALIDADE_MEMBRO,
                   NM_PROGRAMA_IES,
                   NM_TIPO_SITUACAO_ATUAL,
                   nme_area_avaliacao,
                   nme_instituicao,
                   nme_programa,
                   nome_cvlattes,
                   nome_ies_formacao,
                   nome_sucupira,
                   nro_id_cnpq,
                   nro_nota_doutorado,
                   nro_nota_mestrado,
                   nro_nota_mestrado_prof,
                   seq_area_basica,
                   seq_pessoa_fisica,
                   seq_tipo_categoria_vinculo,
                   SG_ENTIDADE_ENSINO,
                   sgl_instituicao,
                   sigla_ies_formacao,
                   sigla_pais_ies_formacao,
                   sigla_uf_ies_formacao,
                   TP_SEXO_MEMBRO
                   # DT_FIM_VINCULO,
                   # DT_INICIO_VINCULO,                   
                   # ID_ADD_CONTEXTO = ID_ADD_CONTEXTO.x,
                   # ID_ADD_DISCENTE,
                   # ID_ADD_DOCENTE,
                   # ID_ADD_FOTO_PROGRAMA = ID_ADD_FOTO_PROGRAMA.x,
                   # ID_ADD_FOTO_PROGRAMA_IES = ID_ADD_FOTO_PROGRAMA_IES.x,
                   # ID_ADD_MEMBRO_PROJETO,
                   # ID_ADD_PARTICIPANTE_EXTERNO,                   
                   ) %>% 
          summarise(ULTIMO_ANO = max(AN_BASE.x),
                    IN_RESPONSAVEL_PROJETO = max(IN_RESPONSAVEL_PROJETO))
  
  return(ds)
  
}

rm(ds_Mbr_sum_grd, ds_Mbr_sum_doc, ds_Mbr_sum_mst)

ds_Mbr_sum_grd <- f.sum_frm(ds_MbrPrj_grd)
# ds_Mbr_sum_doc <- f.sum_frm(ds_MbrPrj_doc)
# ds_Mbr_sum_mst <- f.sum_frm(ds_MbrPrj_mst)

# %>%
# filter(ID_PROJETO == 240953)

ds_general <- ds_membros_13_16 %>% 
  group_by(CD_PROGRAMA_IES,
           NM_PROGRAMA_IES,
           DS_TIPO_MEMBRO,
           DS_CATEGORIA_MEMBRO_PROJETO) %>% 
  summarise(QT = n())

f.ind <- function(ds){
  ds %<>% group_by(CD_PROGRAMA_IES,
                   NM_PROGRAMA_IES,
                   DS_TIPO_MEMBRO,
                   #DS_CATEGORIA_MEMBRO_PROJETO,
                   NM_AREA_AVALIACAO,
                   nome_curso_formacao,) %>% 
          summarise(QT = n())
  
  return(ds)
}

ds_ind_grd <- f.ind(ds_Mbr_sum_grd)
# ds_ind_doc <- f.ind(ds_Mbr_sum_doc)
# ds_ind_mst <- f.ind(ds_Mbr_sum_mst)

write.csv2(ds_MbrPrjFrm_grp, "Membros_Projetos_Formação.csv")

view(ls(ds_MbrPrjFrm_grp))

# Id Projeto:
# •	250672
# •	272449
# •	255310
# •	359139
# •	359227
# •	240953 - (3X nome das pessoas)
# ISABELA ALMEIDA PORDEUS
# 173258 172810
# "-"(173258,172810)

ls(ds_membros_13_16)
ls(ds_MbrPrjFrm_grp)
ls(ds_aratdo)

max(str_count(ds_frmdoc$nro_id_cnpq))
min(str_count(ds_frmdoc$nro_id_cnpq))
max(str_count(ds_frmdoc$seq_pessoa_fisica))
min(str_count(ds_frmdoc$seq_pessoa_fisica))

max(str_count(ds_frmdic$nro_id_cnpq))
min(str_count(ds_frmdic$nro_id_cnpq))
max(str_count(ds_frmdic$seq_pessoa_fisica))
min(str_count(ds_frmdic$seq_pessoa_fisica))

max(str_count(ds_aratdo$nro_id_cnpq))
min(str_count(ds_aratdo$nro_id_cnpq))
max(str_count(ds_aratdo$seq_pessoa_fisica))
min(str_count(ds_aratdo$seq_pessoa_fisica))
