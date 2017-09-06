x_vv_faixa <- read_excel('beneficiarios_faixa.xlsx') %>% 
  mutate(ANO = factor(str_sub(ANOMES, 1, 4)), 
         ANOMES = factor(ANOMES), 
         FAIXA = factor(FAIXA, levels = c("até 18 anos", "19 a 23 anos", 
                                          "24 a 28 anos", "29 a 33 anos", 
                                          "34 a 38 anos", "39 a 43 anos", 
                                          "44 a 48 anos", "49 a 53 anos", 
                                          "54 a 58 anos", "59 anos ou mais"))) %>% 
  select(ANO, ANOMES, FAIXA, BENEFICIARIOS = BT)



vv_faixa_ano <- x_vv_faixa %>% 
  group_by(ANO, FAIXA) %>% 
  summarise(BENEFICIARIOS.md = mean(BENEFICIARIOS)) %>% 
  as.data.frame() %>% 
  mutate(TOTAL_B = vv_ano[match(.$ANO, vv_ano$ANO), 13][[1]])
