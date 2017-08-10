rm(rec_des)

rec_des <- read_excel('Previsão_2017.xlsx') %>%
  add_column(Diff = .$Despesa - .$Receita) %>%
  mutate(Mês = factor(.$Mês, levels = .$Mês))
  as.tibble()

GGrec_des <- ggplot(data = rec_des, aes(x = Mês)) + # Dados básicos e eixos comuns
  geom_col(aes(y = (Despesa/1000000), fill = Diff/1000000),
           width = 0.9,
           alpha=0.4, 
           #stat = "sum",
           col = "black",
           size = 1) +
  geom_bar(aes(y = (Receita/1000000)),
           width = 0.4, 
           alpha=0.3,
           fill = "blue",
           stat = "sum",
           size = 2) +
  geom_text(aes(y = (Receita/1000000) + 14,
                label = format((Diff/1000000), digits = 2),
                col = Diff/1000000), 
            size = 5) +  
  scale_fill_gradient(name="Delta entre Despesa\ne Receita em Mil, 2017",  
                      low = "green", 
                      high = "red", 
                      space = "Lab",
                      guide = "colourbar") +
  scale_colour_gradient(name="Valor do Delta\nem Mil, 2017", 
                        low = "blue", 
                        high = "orange", 
                        space = "Lab",
                        guide = "colourbar") +
  coord_cartesian(ylim=c(95,150)) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, 
                                   vjust = 1, 
                                   size = 9, 
                                   hjust = 1)) +
  scale_y_continuous(breaks = seq(90, 180, by = 10)) +
  labs(title = 'Despesa X Receita 2017', y = 'Despesa/Receita')
#, limits = 

GGrec_des
  
  