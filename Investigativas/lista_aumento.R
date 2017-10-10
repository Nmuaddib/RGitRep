library("stringr", lib.loc="~/R/win-library/3.4")
library("dplyr", lib.loc="~/R/win-library/3.4")
library("ggplot2", lib.loc="~/R/win-library/3.4")
library("readr", lib.loc="~/R/win-library/3.4")
library("tibble", lib.loc="~/R/win-library/3.4")
library("tidyr", lib.loc="~/R/win-library/3.4")
library("magrittr", lib.loc="~/R/win-library/3.4")
library("readxl", lib.loc="~/R/win-library/3.4")
library("purrr", lib.loc="~/R/win-library/3.4")
library("DBI", lib.loc="~/R/win-library/3.4")
library("ROracle", lib.loc="~/R/win-library/3.4")

f.resumo <- function(df) {
  res <- character()
  res[1] <- df[1,1]
  res[2] <- df[1,2]  
  
  smt <- summary(df$valor)
  
  res[3] <- df[which(df$valor == smt[1])[1], 3]
  res[4] <- smt[1]
  res[5] <- df[which(df$valor == smt[6])[1], 3]
  res[6] <- smt[6]  
  res[7] <- smt[2]
  res[8] <- smt[3]
  res[9] <- smt[5]

  res[10] <- smt[4]
  res[11] <- sum(df$valor)
  res[12] <- round(smt[6]/smt[1], 3)
  res[13] <- round(smt[4]/smt[1], 3)
  res[14] <- round(smt[6]/smt[4], 3)
  
  if (nrow(df) >= 10) {
    dft <- tail(df, 5)
    tm <- mean(dft$valor)
    dfh <- head(df, 5)
    hm <- mean(dfh$valor)
  } else {
    dft <- tail(df, 3)
    tm <- mean(dft$valor)
    dfh <- head(df, 3)
    hm <- mean(dfh$valor)    
  }
  
  res[15] <- round(tm/hm, 3)
  
  names(res) <- c('codigo','nome', 'am_min', 'min', 'am_max', 'max', 'q1', 'q2', 'q3',
                  'avg', 'sum', 'mx_mi', 'av_mi', 'mx_av', 'tav_hav')
  return(res)
}

x_prestadores_am <- read_excel('prestadores_2016_17.xlsx') %>% 
  select(codigo = "Identificador do Prestador",
         nome = "Nome do Prestador",
         anomes = "Ano e Mês",
         valor = "Valor Aprovado (calculado)",
         quantidade = "Quantidade Aprovada") %>%
  arrange(codigo, anomes) 

f_codigo <- factor(x_prestadores_am$codigo)

ls_range <- x_prestadores_am %>% 
  split(f_codigo) %>% 
  map(f.resumo)

df_prestadores <-  tibble('codigo' = character(),
                          'nome' = character(),
                          'am_min' = character(),
                          'min' = double(),  
                          'am_max' = character(), 
                          'max' = double(),
                          'q1' = double(), 
                          'q2' = double(), 
                          'q3' = double(),
                          'avg' = double(), 
                          'sum' = double(), 
                          'mx_mi' = double(), 
                          'av_mi' = double(),
                          'mx_av' = double(),
                          'tav_hav' = double())

for(i in seq_along(ls_range)) {
  for(j in seq_along(ls_range[[i]])) {
    df_prestadores[i, j] <- ls_range[[i]][[j]]
  }
}

df_prestadores_att <- df_prestadores %>% 
  filter(#mx_mi > 3, 
         #av_mi < 2, 
         #mx_av < 2,
         tav_hav > 2,         
         str_sub(am_max, 1, 4) == '2017',
         #am_min < '201606',
         avg > 100000)

f.plot <- function(df) {
  
  gg <- ggplot(data = df , aes(x = anomes)) +
    geom_line(aes(y = valor, color = valor, group = 1), 
              size = 1, alpha = 1, show.legend = T) +
    labs(title = paste0('Faturamento do Prestador: ',df[1,2]),
         x = 'Ano e mês', 
         y = 'Valores') +
    theme_gray() +
    theme(axis.text.x = element_text(angle = 45, 
                                     vjust = 1, 
                                     size = 9, 
                                     hjust = 1))
  plot(gg)
  
}

x_prestadores_am %<>% mutate(anomes = factor(anomes))

for (i in 1:nrow(df_prestadores_att)) {
  df_tmp <- x_prestadores_am %>% 
    filter(codigo == df_prestadores_att[[1]][i])
  f.plot(df_tmp)
}

df_tmp <- x_prestadores_am %>% 
  filter(codigo == df_prestadores_att[[1]][1])

f.plot(df_tmp)

df_tmp
