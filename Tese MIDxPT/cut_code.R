ds_IMI_2 <- merge(ds_IMI1_FCDo[,c(1,15)], ds_IMI3_CC[,c(1,14)], by = "CD_PROGRAMA_IES", all = T) %>% 
   merge(., ds_IMI4_CP_pgr[,c(1,8)], by = "CD_PROGRAMA_IES", all = T)

ds_IMI_2 <- ds_IMI_2[complete.cases(ds_IMI_2),]

ds_IMI_2 %<>% mutate(IMI = (FCDo + CC + CP)/3) %>% 
   merge(ds_prg, ., by = "CD_PROGRAMA_IES", all.y = T)

write.csv2(ds_IMI_2, "~/RGitRep/Tese MIDxPT/Analises/IMI_2.csv")

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
# rm(ds_Mbr_sum_grd, ds_Mbr_sum_doc, ds_Mbr_sum_mst)

 ds_Mbr_sum_grd <- f.sum_frm(ds_MbrPrj_grd)
 ds_Mbr_sum_doc <- f.sum_frm(ds_MbrPrj_doc)
 ds_Mbr_sum_mst <- f.sum_frm(ds_MbrPrj_mst)
# ds_general <- ds_membros_13_16 %>%
   group_by(CD_PROGRAMA_IES,
            NM_PROGRAMA_IES,
            DS_TIPO_MEMBRO,
            DS_CATEGORIA_MEMBRO_PROJETO) %>%
   summarise(QT = n())
# f.ind <- function(ds){
   ds %<>% group_by(CD_PROGRAMA_IES,
                    NM_PROGRAMA_IES,
                    DS_TIPO_MEMBRO,
                    #DS_CATEGORIA_MEMBRO_PROJETO,
                    NM_AREA_AVALIACAO,
                    nome_curso_formacao,) %>%
           summarise(QT = n())

   return(ds)
 }
# ds_ind_grd <- f.ind(ds_Mbr_sum_grd)
 ds_ind_doc <- f.ind(ds_Mbr_sum_doc)
 ds_ind_mst <- f.ind(ds_Mbr_sum_mst)
##### -------------------------------------------------------------------------------------------
# ds_model <- merge(ds_IMI1_FCDo, ds_IMI2_FCDi, by = "cod_programa") %>%
   select(FCDo, FCDi) %>%
   as_tibble() #%>%
   cor(., method = "pearson", use = "complete.obs")

 o_cor <- symnum(ds_model); o_cor

 f.lm_checkin <- function(ds, modl) {
   lm.x <- lm(modl, data = ds)
   print(summary(lm.x))
   print(confint(lm.x, level=0.99))
   for (i in c(1:3,5)) {
     plot(lm.x, which = i)
   }
#   return(lm.x)
 }
#lm.FC <- f.lm_checkin(ds_model, 'FCDi ~ FCDo')
 ds_IMI_Models <- ds_IMI_1 %>%
   mutate(model1 = (FCDo+CC+CP)/3,
          model2 = (FCDo+CC)/2,
          model3 = (FCDo+CP)/2,
          model4 = (CC+CP)/2)

 lm.FC <- f.lm_checkin(ds_IMI_Models, 'FCDi ~ model1')
 ds_IMI_Models %<>% mutate(model = model1)

 lm.FC <- f.lm_checkin(ds_IMI_Models, 'FCDi ~ model2')
 ds_IMI_Models %<>% mutate(model = model2)

 lm.FC <- f.lm_checkin(ds_IMI_Models, 'FCDi ~ model3')
 ds_IMI_Models %<>% mutate(model = model3)

 lm.FC <- f.lm_checkin(ds_IMI_Models, 'FCDi ~ poly(model4, 1, raw = T)')
 ds_IMI_Models %<>% mutate(model = model4)


 lm.FC <- lm(data = ds_IMI_Models, formula = FCDi ~ poly(FCDo, 2, raw = T))
 summary(lm.FC)
 ds_IMI_Models %<>% mutate(model = FCDo)


 lm.FC <- f.lm_checkin(ds_IMI_Models, 'FCDi ~ poly(FCDo, 6, raw = T)')
 lm.FC <- f.lm_checkin(ds_IMI_Models, 'FCDi ~ FCDo')
 ds_IMI_Models %<>% mutate(model = FCDo)

 lm.FC <- f.lm_checkin(ds_IMI_Models, 'FCDi ~ CC')
 ds_IMI_Models %<>% mutate(model = CC)

 lm.FC <- f.lm_checkin(ds_IMI_Models, 'FCDi ~ CP')
 ds_IMI_Models %<>% mutate(model = CP)
##### Incluir conluna com valores previstos ------------------
 ds_IMI_Models %<>% add_predictions(lm.FC) #modelr
#### Gráfico do modelo --------------------------------------
 ggplot(ds_IMI_Models, aes(y = model)) +
   geom_point(aes(x = FCDi), color = "blue", alpha = 0.5, size = 3) +
   geom_line(aes(x = pred),color = "red", size = 1.2) +
   labs(title = 'Modelo proposto',
        x = 'FCDi', y = 'Model')

##### -------------------------------------------------------------------------------------------