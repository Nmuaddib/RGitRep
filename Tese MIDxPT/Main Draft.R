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
# ds_appprg <- read_excel("Dados Aplicativos com Programa.xlsx")
# ds_patprg <- read_excel("Dados Patentes com Programa.xlsx")
# ds_prdprg <- read_excel("Dados Produtos com Programa.xlsx")
ds_prgppg <- read_excel("Programas PPG 2016.xlsx")
ds_prjppg <- read_excel("Projetos PPG 2016.xlsx")

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

# ls(ds_prj013_dis)
# ls(ds_frmdic)
# ls(ds_disprjfrm)
# ?filter

##### Filtra nivel de formação 

#ds_frmdic_grad <- ds_frmdic %>% filter(nivel_formacao == "Graduação")

ds_frmdic <- read_excel("Formação dos discentes 2013-2016 - Tese.xlsx")

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

ds_formacao <- rbind(ds_frmdic, ds_frmdoc) %>% 
  filter(nivel_formacao == "Graduação")


##### Junção de Formação (graduação) com membros de projeto por Nome+ID Programa+tipo de membro

# ds_disprjfrm <- merge(ds_prj013_dis, ds_frmdic_grad, 
#                       by.x = c("CD_PROGRAMA_IES", "NM_MEMBRO_PROJETO"), 
#                       by.y = c("cod_programa", "nome_filtro_cvlattes"))

ds_MbrPrjFrm <- merge(ds_membros_13_16, ds_formacao, 
                      by.x = c("CD_PROGRAMA_IES", "NM_MEMBRO_PROJETO", "DS_TIPO_MEMBRO"), 
                      by.y = c("cod_programa", "nome_filtro_cvlattes", "doc_ou_disc"))

# ls(ds_membros_13_16)
# ls(ds_formacao)
# ds_dis <- ds_prj013_dis %>% select(CD_PROGRAMA_IES, NM_MEMBRO_PROJETO)
# ds_frm <- ds_frmdic %>% select(cod_programa, nome_filtro_cvlattes)


##### Quantidade de vinculos por Membro/Formação (Projetos??)

ds_qt_disprjfrm <- ds_disprjfrm %>% 
  group_by(AN_BASE.x,
           NM_AREA_AVALIACAO,
           nivel_formacao,
           CD_PROGRAMA_IES, 
           NM_PROGRAMA_IES, 
           NM_MEMBRO_PROJETO,
           nome_curso_formacao) %>% 
  summarise(QT_VINC = n()) %>% 
  arrange(desc(QT_VINC))

write.csv2(ds_qt_disprjfrm, "Qt_Vinculos por membro de projeto.csv")
write.csv2(ds_disprjfrm, "Membros_projetos_formação.csv")
 
# ls(ds_appprg$NM_FINANCIADOR)
# ds_appprg$NM_FINANCIADOR
# ls(ds_frmdic)
# ls(ds_frmdoc)
# view(names(ds_frmdoc))



