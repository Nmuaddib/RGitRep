f.sel_cai_var <- function(var_n, ind_int) {
  cai_df %>%
    filter(internacao == ind_int) %>% 
    arrange(amq) %>% 
    select(var_n) %>% 
    as.tibble()  
}

f.sel_cai_var(c('taxa', 'consulta'), 'n')

cai_ECn_PTs.corP <- cbind(f.sel_cai_var(c('emergencia', 'consulta'), 'n'),
                          f.sel_cai_var(c('taxa', 'pacote'), 's')) %>% 
  .[1:77,] %>% 
  cor(., method = 'pearson')


topcorS_W <- topcorS %>% 
  add_column(Var1_prct = resumo_valor_S[match(topcorS$Var1, resumo_valor_S$componente),4]) %>% 
  add_column(Var2_prct = resumo_valor_S[match(topcorS$Var2, resumo_valor_S$componente),4]) %>% 
  select("Var1","Var1_prct","Var2","Var2_prct",Corr = "value")

topcorN_W <- topcorN %>% 
  add_column(Var1_prct = resumo_valor_N[match(topcorN$Var1, resumo_valor_N$componente),4]) %>% 
  add_column(Var2_prct = resumo_valor_N[match(topcorN$Var2, resumo_valor_N$componente),4]) %>% 
  select("Var1","Var1_prct","Var2","Var2_prct",Corr = "value")

f.cor_plot <- function(ds, plt, dirc, lab_leg, dig) {
  GGcor <- ggplot(data = melt(ds, na.rm = TRUE), aes(Var2, Var1, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_distiller(palette = plt, 
                        direction = dirc,
                        name=lab_leg) +  
    theme_minimal()+ 
    theme(axis.text.x = element_text(angle = 45, 
                                    vjust = 1, 
                                    size = 12, 
                                    hjust = 1),
          axis.title.x = element_blank(),
          axis.title.y = element_blank())+
    guides(fill = guide_colorbar(barwidth = 1, 
                                barheight = 7,
                                title.position = "bottom")) +
    coord_fixed()
  GGcor + 
    geom_text(aes(Var2, Var1, label = format(value, digits = dig, nsmall= 3)), 
              color = "black", 
              size = 3)
}

f.cor_plot(caiN.corP,"RdYlGn", 1,"Correlação de\nPearson\n|Ambulatório|",1)

f.cor_plot(caiS.corP,"Set2", -1,"Correlação de\nPearson\n|Internação|",2)

f.sel_cai_var(c('taxa', 'consulta'), 'n')

cai_SCs <- cbind(f.sel_cai_var('sadt','s'), f.sel_cai_var('consulta','n'))

cai_SCn <- cbind(f.sel_cai_var('sadt','n'), f.sel_cai_var('consulta','n'))

#### Preparação: Consulta sobre exames sem Internação -------


#### Preparação: Consulta sobre exames totais ---------------

#rm(cai_SCc)

cai_SCc <- cai_df %>% 
  select(amq,sadt) %>% 
  group_by(amq) %>% 
  summarise(sadt=sum(sadt)) %>% 
  as.data.frame() %>% 
  select(sadt) %>% 
  as.tibble() %>% 
  add_column(consulta = f.sel_cai_var('consulta','n'))

## Inclusão de coluna "z", análise de variação por desvios padrão, outliers acima de 3 
zSc <- scale(cai_SCc$sadt, center = TRUE, scale = TRUE) 
cai_SCc <- cai_SCc %>% add_column(z = zSc[1:dim(cai_SCc)[1]])

smr <- summary(lm.cai_SCc)

smr$r.squared
smr$terms
smr$terms[[2]]
smr$terms[[3]]

SCc.CI <- as.tibble(predict.lm(lm.cai_SCc, level=0.95, interval = 'confidence')) %>% 
  add_column(., rn = as.numeric(row.names(SCc.CI)))
cai_SCc <- add_column(cai_SCc, rn = as.numeric(row.names(SCc.CI)))

#----------------------------------------------------------------------------------
rm(vn,x,y)
vn <- f.cor_smst(smst.corP, 'consulta', 'honorario')
vn




ggplot(SCc.CI[25:65,], aes(x = rn, y = fit, col = fit)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymax = upr, ymin = lwr)) +
  #geom_line(color = "light blue", size = 0.4, alpha = 0.5) +
  geom_point(data = cai_SCc[25:65,], aes(y = sadt),col = "red" , size = 1.5) +
  theme_dark() +
  geom_smooth(col = 'white', method = "loess", alpha = 0.2, linetype = 3, size = 0.5) +
  scale_colour_gradient2(name="Variação em\ntorno da média", 
                         low = "white", 
                         mid = "orange",
                         midpoint = 591713,
                         high = "white") +
  labs(title = 'Amostra de assertividade nas previsões', 
       x = 'Observações', y = 'Valor Previsto')

class(sum)
attributes("mutate")
class(print)
.Primitive("mutate")

class(mutate)
??srcref

attr(f.cor_plot, "srcref")
body(mutate)
body(UseMethod)

.Primitive("UseMethod")

sum
mget

objs <- mget(ls("package:base"), inherits = TRUE)
funs <- Filter(is.function, objs)

str(funs)

str(funs$abbreviate)

len(formals(funs$abbreviate))

length(formals(funs$abbreviate))

siz <- lapply(lapply(funs, formals), length)
lenV <- vector()
for (i in seq_along(siz)) {
  lenV[i] <- siz[[i]]
}

siz
  
siz[] 
siz[[ ]]
lenV
max(lenV)

seq_along(siz)

map(siz, '[[', 1)

funs['[[']
