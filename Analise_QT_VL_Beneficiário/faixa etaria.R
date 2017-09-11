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
  mutate(TOTAL_B = vv_ano[match(.$ANO, vv_ano$ANO), 13][[1]]) %>% 
  mutate(P_FAIXA = round((BENEFICIARIOS.md/TOTAL_B)*100,3))

ggplot(data = vv_faixa_ano, aes(x = ANO)) +
  geom_bar(aes(y = BENEFICIARIOS.md, group = FAIXA, fill = FAIXA), 
           width = 0.9, alpha = 0.75, 
           stat = 'identity',
           position = 'fill') +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  scale_fill_brewer(palette = 'Spectral',
                    direction = 1) +
  labs(title = 'Percentual de faixa etária por ano',
       x = 'Ano', 
       y = 'Percentual') +
  theme_gray() +
  theme(axis.text.x = element_text(angle = 45, 
                                   vjust = 1, 
                                   size = 9, 
                                   hjust = 1))

ggplot(data = x_vv_faixa , aes(x = ANOMES)) +
  geom_line(aes(y = BENEFICIARIOS, color = FAIXA, group = FAIXA), 
            size = 1, alpha = 1, show.legend = T) +
  scale_y_continuous(breaks = seq(10000, 200000, by = 5000)) +
  scale_colour_manual(values = c('blue',
                                 'yellow',
                                 'green',
                                 'red',
                                 'orange',
                                 'dark green',
                                 'black',
                                 'gray',
                                 'violet',
                                 'light blue')) +
  labs(title = 'Evolução da faixa etária',
       x = 'Ano e mês', 
       y = 'Beneficiários') +
  theme_gray() +
  theme(axis.text.x = element_text(angle = 45, 
                                   vjust = 1, 
                                   size = 9, 
                                   hjust = 1))


f.stats <- function(vec) {
  res <- double()
  res[1] <- min(vec)
  res[2] <- max(vec)
  res[3] <- res[2]-res[1]
  res[4] <- sum(vec)
  res[5] <- round(res[3]/res[1]*100, 4)
  names(res) <- c('min', 'max', 'delta', 'sum', 'p_delta')
  return(res)
}

lt <- x_vv_faixa %>% 
  split(.$FAIXA) %>% 
  map(arrange, ANOMES, FAIXA) %>% 
  map("BENEFICIARIOS") %>% 
  map(f.stats) %>% 
  map('delta') %>% 
  unlist() %>% 
  as.data.frame() %>% 
  cbind(rownames(.), .) %>% 
  as.tibble()

names(lt) <- c('FAIXA','DELTA')

delta.df <- vv_faixa_ano %>% 
  split(.$FAIXA) %>% 
  map(arrange, ANO, FAIXA) %>% 
  map('BENEFICIARIOS.md') %>% 
  map(f.stats) 

delta.df

#%>% 
map('delta') %>% 
  unlist() %>% 
  as.data.frame() %>% 
  cbind(rownames(.), .) %>% 
  as.tibble()

names(delta.df) <- c('FAIXA','DELTA')