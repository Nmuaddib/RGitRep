
t1 <- x_tab_detalhamento %>%
  mutate(ESPECIALIDADE = ifelse(!is.na(match(x_tab_detalhamento$ESPECIALIDADE, x_dp_esp$banco)),
                                x_dp_esp[match(x_tab_detalhamento$ESPECIALIDADE, x_dp_esp$banco),1],
                                ESPECIALIDADE),
         ITEM = ifelse(!is.na(match(x_tab_detalhamento$ITEM, x_dp_prc$banco)),
                       x_dp_prc[match(x_tab_detalhamento$ITEM, x_dp_prc$banco),1],
                       ITEM)) %>%
  filter(str_sub(ANOMES,3,4) == 17) %>% 
  group_by(CODIGO, NOME, ESPECIALIDADE, ITEM, GRUPO) %>% 
  summarise(BN = sum(BN), QUANTIDADE = sum(QUANTIDADE), VALOR = sum(VALOR), REVISAO = sum(REVISAO)) %>% 
  mutate(ANOMES = '2017_TOTAL')%>% 
  select(1:5, 10, 6:9) %>% 
  as.data.frame() %>%
  rbind(., x_tab_detalhamento) %>%
  mutate(ESPECIALIDADE = ifelse(!is.na(match(.$ESPECIALIDADE, x_dp_esp$banco)),
                                x_dp_esp[match(.$ESPECIALIDADE, x_dp_esp$banco),1],
                                ESPECIALIDADE),
         ITEM = ifelse(!is.na(match(.$ITEM, x_dp_prc$banco)),
                       x_dp_prc[match(.$ITEM, x_dp_prc$banco),1],
                       ITEM)) %>%
  gather(IND, VALOR, BN:REVISAO) %>%
  group_by(CODIGO, NOME, ESPECIALIDADE, ITEM, GRUPO, ANOMES, IND) %>%
  summarise(VALOR = sum(VALOR)) %>%
  as.data.frame() %>%   
  mutate(ANOMES = paste0('C_',ANOMES)) %>% 
  unite(ANOMES_IND, c(ANOMES, IND)) %>%
  spread(ANOMES_IND, VALOR) %>% 
  arrange(desc(C_2017_TOTAL_VALOR))

rm(t1,t2,t3,t4)

str(tab_detalhamento)

names(tab_detalhamento)

mean(tab_especialidade$C_TOTAL_2017_VALOR, na.rm = T)

  

