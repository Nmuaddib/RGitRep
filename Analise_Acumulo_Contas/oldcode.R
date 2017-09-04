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