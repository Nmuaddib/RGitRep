##----------------------------------------------------------------------------
#f.get_last_scale <- function(mtx, idx = 0) {
#  l <- dim(mtx)[1]
#  if (idx > 0) idx <- idx - 1
#  if (idx >= l) idx <- 1
#  i <- l - idx
#  mtx[i:l,1]
#}
##----------------------------------------------------------------------------
#x_fat_ate <- read_excel('Geral2014_17_prestadores_t.xls')
x_fat_ate <- read_excel('Geral2014_17_prestadores.xlsx')

x_fat_ate %<>% mutate(DIF = VA_FT-VA_AT)

FA.lst_scale <- x_fat_ate %>%
  split(.$CODIGO, drop = T) %>%
  map("DIF") %>%
  map(scale, F, T) #%>% 
#  map(f.get_last_scale, 2)

for (i in seq_along(FA.lst_scale)) names(FA.lst_scale[[i]]) <- f.name_lst(FA.lst_scale[i], x_fat_ate)

idx <- unlist(FA.lst_scale)

FA.df_scale <- tibble(prestador = character(), anomes = character(), escala = double())

for (i in seq_along(unlist(FA.lst_scale))) {
  FA.df_scale[i,] <- c(str_sub(names(idx)[i], 1, str_locate(names(idx)[i], ".201")[1]-1),
                       str_sub(names(idx)[i], str_locate(names(idx)[i], ".201")[1]+1),
                       idx[i])
}

FA.df_scale %<>% mutate(escala = as.double(escala))
##----------------------------------------------------------------------------
##----------------------------------------------------------------------------
x_fat_ate <- read_excel('Geral2014_17_prestadores.xlsx')
##----------------------------------------------------------------------------
##as.tibble(x_fat_ate)

f.name_lst <- function(lt, df) {
  prest <- names(lt)
  anomes_vec <- df %>% 
    filter(CODIGO == prest) %>% 
    select(ANOMES)
  idx_f <- length(anomes_vec[[1]])
  idx_i <- idx_f - (length(lt[[1]]) - 1)
  anomes_vec <- anomes_vec[[1]][idx_i:idx_f]
  return(anomes_vec)
}

FA.lst_scale <- FA.df_base %>%
  split(.$CODIGO, drop = T) %>%
  map("DIF") %>%
  map(scale, F, T)

for (i in seq_along(FA.lst_scale)) names(FA.lst_scale[[i]]) <- f.name_lst(FA.lst_scale[i], FA.df_base)

FA.df_scale <- tibble(prestador = character(), anomes = character(), escala = double())

for (i in seq_along(FA.lst_scale)) {
  for (j in seq_along(FA.lst_scale[[i]][,1])) {
    FA.df_scale[dim(FA.df_scale)[1]+1,] <- c(names(FA.lst_scale[i]),
                                             names(FA.lst_scale[[i]][j]),
                                             FA.lst_scale[[i]][j,1])
  }
}

FA.df_scale %<>% mutate(escala = as.double(escala))

##----------------------------------------------------------------------------
##FA.df_scale
##----------------------------------------------------------------------------
##----------------------------------------------------------------------------
### Cálculo de dados padronizados do Planserv

Planilha original importada e resultado final com dados padronizados, total do Planserv.
```{r geral}
x_geral <- read_excel('Geral2014_17.xlsx')

##----------------------------------------------------------------------------
##as.tibble(x_geral)

x_geral <- cbind(x_geral, ID_AM = 1:dim(x_geral)[1])

x_geral %<>% mutate(DIF = VA_FT-VA_AT)

FA.df_scale_geral <- cbind(anomes = x_geral$ANOMES, 
                           escala = scale(x_geral$DIF, 
                                          center = F, 
                                          scale = T)[1:dim(x_geral)[1],1])

FA.df_scale_geral %<>% 
  as.tibble() %>% 
  mutate(escala = as.double(escala))

##----------------------------------------------------------------------------
##FA.df_scale_geral
```

### Planilha final

Junção dos dados das planilhas anteriores, código oculto abaixo.
```{r lookups}
x_fat_ate %<>%
  add_column(., escala = FA.df_scale[match(paste(.$CODIGO,
                                                 .$ANOMES),
                                           paste(FA.df_scale$prestador, 
                                                 FA.df_scale$anomes)), 3][[1]]) %>%
  add_column(., escala_geral = FA.df_scale_geral[match(.$ANOMES, 
                                                       FA.df_scale_geral$anomes), 2][[1]]) %>% 
  add_column(., P_FCR = FA.df_base[match(paste(.$CODIGO,
                                               .$ANOMES),
                                         paste(FA.df_base$CODIGO, 
                                               FA.df_base$ANOMES)), 16]) %>% 
  add_column(., C_FTT = FA.df_base[match(paste(.$CODIGO,
                                               .$ANOMES),
                                         paste(FA.df_base$CODIGO, 
                                               FA.df_base$ANOMES)), 14])

##----------------------------------------------------------------------------
write.csv2(x_fat_ate, 'Prestador_FT_AT_Anomes.csv')
##----------------------------------------------------------------------------
##x_fat_ate

x_fat_ate %<>% mutate(ANOMES = factor(.$ANOMES, levels = x_geral$ANOMES),
                      escala_geral = as.double(escala_geral))

FA.lst_prestador <- x_fat_ate %>% 
  split(.$CODIGO)

##----------------------------------------------------------------------------
##----------------------------------------------------------------------------
##----------------------------------------------------------------------------
##----------------------------------------------------------------------------

FA.df_base <- x_fat_ate_d %>% 
  group_by(CODIGO, PRESTADOR, ANOMES = ANOMES_FT) %>% 
  dplyr::summarise(QTA_FTT = sum(QP, na.rm = T), VA_FTT = sum(VA, na.rm = T),
                   VR_FTT = sum(VR, na.rm = T), C_FTT = sum(CT, na.rm = T)) %>% 
  as.data.frame()

FA.df_base <- x_fat_ate_d %>%
  filter(DELTA_ANM == 0) %>% 
  group_by(CODIGO, PRESTADOR, ANOMES = ANOMES_FT) %>% 
  dplyr::summarise(QTA_FCR = sum(QP, na.rm = T), VA_FCR = sum(VA, na.rm = T),
                   VR_FCR = sum(VR, na.rm = T), C_FCR = sum(CT, na.rm = T)) %>% 
  as.data.frame() %>% 
  merge(FA.df_base, ., by = c('CODIGO', 'PRESTADOR', 'ANOMES'), all = T)

FA.df_base <- x_fat_ate_d %>%
  filter(DELTA_ANM != 0) %>% 
  group_by(CODIGO, PRESTADOR, ANOMES = ANOMES_FT) %>% 
  dplyr::summarise(QTA_FAC = sum(QP, na.rm = T), VA_FAC = sum(VA, na.rm = T),
                   VR_FAC = sum(VR, na.rm = T), C_FAC = sum(CT, na.rm = T)) %>% 
  as.data.frame() %>% 
  merge(FA.df_base, ., by = c('CODIGO', 'PRESTADOR', 'ANOMES'), all = T)

FA.df_base <- x_fat_ate_d %>%
  filter(DELTA_ANM == 1) %>% 
  group_by(CODIGO, PRESTADOR, ANOMES = ANOMES_FT) %>% 
  dplyr::summarise(QTA_FMA = sum(QP, na.rm = T), VA_FMA = sum(VA, na.rm = T),
                   VR_FMA = sum(VR, na.rm = T), C_FMA = sum(CT, na.rm = T)) %>% 
  as.data.frame() %>% 
  merge(FA.df_base, ., by = c('CODIGO', 'PRESTADOR', 'ANOMES'), all = T)

FA.df_base <- x_fat_ate_d %>%
  group_by(CODIGO, PRESTADOR, ANOMES = ANOMES_AT) %>% 
  dplyr::summarise(QTA_ATE = sum(QP, na.rm = T), VA_ATE = sum(VA, na.rm = T),
                   VR_ATE = sum(VR, na.rm = T), C_ATE = sum(CT, na.rm = T)) %>% 
  as.data.frame()%>% 
  merge(FA.df_base, ., by = c('CODIGO', 'PRESTADOR', 'ANOMES'), all.x = T)

##----------------------------------------------------------------------------
##----------------------------------------------------------------------------
##----------------------------------------------------------------------------
##----------------------------------------------------------------------------
FA.df_base_geral <- x_geral_d %>% 
  group_by(ANOMES = ANOMES_FT) %>% 
  dplyr::summarise(QTA_FTT = sum(QP, na.rm = T), VA_FTT = sum(VA, na.rm = T),
                   VR_FTT = sum(VR, na.rm = T), C_FTT = sum(CT, na.rm = T)) %>% 
  as.data.frame()

FA.df_base_geral <- x_geral_d %>%
  filter(DELTA_ANM == 0) %>% 
  group_by(ANOMES = ANOMES_FT) %>% 
  dplyr::summarise(QTA_FCR = sum(QP, na.rm = T), VA_FCR = sum(VA, na.rm = T),
                   VR_FCR = sum(VR, na.rm = T), C_FCR = sum(CT, na.rm = T)) %>% 
  as.data.frame() %>% 
  merge(FA.df_base_geral, ., by = 'ANOMES', all = T)

FA.df_base_geral <- x_geral_d %>%
  filter(DELTA_ANM != 0) %>% 
  group_by(ANOMES = ANOMES_FT) %>% 
  dplyr::summarise(QTA_FAC = sum(QP, na.rm = T), VA_FAC = sum(VA, na.rm = T),
                   VR_FAC = sum(VR, na.rm = T), C_FAC = sum(CT, na.rm = T)) %>% 
  as.data.frame() %>% 
  merge(FA.df_base_geral, ., by = 'ANOMES', all = T)

FA.df_base_geral <- x_geral_d %>%
  filter(DELTA_ANM == 1) %>% 
  group_by(ANOMES = ANOMES_FT) %>% 
  dplyr::summarise(QTA_FMA = sum(QP, na.rm = T), VA_FMA = sum(VA, na.rm = T),
                   VR_FMA = sum(VR, na.rm = T), C_FMA = sum(CT, na.rm = T)) %>% 
  as.data.frame() %>% 
  merge(FA.df_base_geral, ., by = 'ANOMES', all = T)

FA.df_base_geral <- x_geral_d %>%
  group_by(ANOMES = ANOMES_AT) %>% 
  dplyr::summarise(QTA_ATE = sum(QP, na.rm = T), VA_ATE = sum(VA, na.rm = T),
                   VR_ATE = sum(VR, na.rm = T), C_ATE = sum(CT, na.rm = T)) %>% 
  as.data.frame()%>% 
  merge(FA.df_base_geral, ., by = 'ANOMES', all.x = T)

FA.df_base_geral[is.na(FA.df_base_geral)] <- 0

FA.df_base_geral %<>% select(ANOMES, 
                             VA_FTT, VA_ATE, VA_FCR, VA_FAC, VA_FMA,
                             # QTA_FTT, QTA_ATE, QTA_FCR, QTA_FAC, QTA_FMA,
                             VR_FTT, VR_ATE, VR_FCR, VR_FAC, VR_FMA,
                             C_FTT, C_ATE #, C_FCR, C_FAC, C_FMA
) %>% 
  mutate(P_FCR = VA_FCR/VA_FTT, P_FMA = VA_FMA/VA_FTT, DIF = VA_FTT-VA_ATE, ANOMES = factor(ANOMES))

```