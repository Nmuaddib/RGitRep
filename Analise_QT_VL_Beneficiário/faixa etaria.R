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

x_vv_main_c %>% 
  '['(c(17, 4, 5:16))

x_vv_main_c %>% 
  '['(c(3, 17, 4, 5:16)) %>% 
  group_by(ANO, CLASSE, ANOMES) %>% 
  summarise(VT = sum(VT),
            VA = sum(VA),
            VC = sum(VC),
            VI = sum(VI),
            QP = sum(QP),
            QA = sum(QA),
            QC = sum(QC),
            QI = sum(QI),
            VQ = sum(VQ),
            BA = sum(BA),
            CT = sum(CT),
            IT = sum(IT)) %>% 
  as.data.frame()

names(mn_c.ano[[1]])[-1:-3]

t <- x_vv_main_c %>% 
  filter(COMPONENTE != 'NA')


```{r conn, eval=FALSE, include=FALSE}
drv <- dbDriver("Oracle")
con <- dbConnect(drv, "ts_consulta", "ts_consulta", dbname='TS.INTRANET')
```

### Criação de base a partir do banco de dados

```{sql connection=con, eval=FALSE, include=FALSE, output.var="x_vv_main"}
select  substr(fc.ID_TEMPO_MES_ANO_REF,1,4) ano,
substr(fc.ID_TEMPO_MES_ANO_REF,1,6) anomes,
sum(nvl(fc.VAL_APROVADO_ITEM,0)) + sum(nvl(fc.VALOR_PAGO_REVISAO,0))  VT,        
round((sum(nvl(fc.VAL_APROVADO_ITEM,0)) + sum(nvl(fc.VALOR_PAGO_REVISAO,0)))/(tbt.BT),2) VM,
round((sum(nvl(fc.VAL_APROVADO_ITEM,0)) + sum(nvl(fc.VALOR_PAGO_REVISAO,0)))/(count(distinct fc.COD_TS)),2) VA,
round((sum(nvl(fc.VAL_APROVADO_ITEM,0)) + sum(nvl(fc.VALOR_PAGO_REVISAO,0)))/(count(distinct fc.CONTA)),2) VC,
round((sum(nvl(fc.VAL_APROVADO_ITEM,0)) + sum(nvl(fc.VALOR_PAGO_REVISAO,0)))/(count(1)),2) VI,
sum(nvl(fc.QTD_ITEM,0)) - sum(nvl(fc.QTD_GLOSADO,0))  QP,        
round((sum(nvl(fc.QTD_ITEM,0)) - sum(nvl(fc.QTD_GLOSADO,0)))/(tbt.BT),2) QM,        
round((sum(nvl(fc.QTD_ITEM,0)) - sum(nvl(fc.QTD_GLOSADO,0)))/(count(distinct fc.COD_TS)),2) QA,
round((sum(nvl(fc.QTD_ITEM,0)) - sum(nvl(fc.QTD_GLOSADO,0)))/(count(distinct fc.CONTA)),2) QC,
round((sum(nvl(fc.QTD_ITEM,0)) - sum(nvl(fc.QTD_GLOSADO,0)))/(count(1)),2) QI,
(sum(nvl(fc.VAL_APROVADO_ITEM,0)) + sum(nvl(fc.VALOR_PAGO_REVISAO,0)))/(sum(nvl(fc.QTD_ITEM,0)) - sum(nvl(fc.QTD_GLOSADO,0))) VQ,
tbt.BT,
count(distinct fc.COD_TS) BA,
count(distinct fc.CONTA) CT,
count(1) IT,
count(distinct fc.COD_TS)/tbt.BT PA        
from    TS.FAT_ITEM_CONTA fc,
(select  to_char(pcm.mes_ano_ref,'RRRRMM') anomes,
 sum(qtd_ativos) BT
 from    ts.posicao_cadastro_mes pcm           
 where   pcm.mes_ano_ref between to_date ('01/01/2014','dd/mm/yyyy') and to_date ('01/07/2017','dd/mm/yyyy') 
 group by   
 to_char(pcm.mes_ano_ref,'RRRRMM')
 order by 1) tbt
where   substr(fc.ID_TEMPO_MES_ANO_REF,1,6) = tbt.anomes
--and     substr(fc.ID_TEMPO_MES_ANO_REF,1,6) in ('201605')--, '201606', '201705', '201706')
group by substr(fc.ID_TEMPO_MES_ANO_REF,1,4),
substr(fc.ID_TEMPO_MES_ANO_REF,1,6),
tbt.BT
having  sum(nvl(fc.QTD_ITEM,0)) - sum(nvl(fc.QTD_GLOSADO,0)) > 0
or      sum(nvl(fc.VAL_APROVADO_ITEM,0)) + sum(nvl(fc.VALOR_PAGO_REVISAO,0)) > 0
order by 2
```

### Analise de faixa etária

```{sql connection=con, eval=FALSE, include=FALSE, output.var="x_vv_faixa"}
select  to_char(pcm.mes_ano_ref,'RRRRMM') anomes,
fe.DESC_FAIXA faixa,
sum(qtd_ativos) BT
from    ts.posicao_cadastro_mes pcm,
TS.FAIXA_ETARIA fe
where   pcm.mes_ano_ref between to_date ('01/01/2014','dd/mm/yyyy') and to_date ('01/07/2017','dd/mm/yyyy') 
and     pcm.TIPO_FAIXA_ETARIA = fe.TIPO_FAIXA
and     pcm.COD_FAIXA_ETARIA = fe.COD_FAIXA_ETARIA
group by   
to_char(pcm.mes_ano_ref,'RRRRMM'),
fe.DESC_FAIXA
order by 1,2
```

