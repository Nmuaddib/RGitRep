#### Importação de dados e inclusão de coluna ---------------
rec_des <- read_excel('Gráfico e Previsão 2017 2.xlsx')
rec_des <- rec_des %>% add_column(Diff = rec_des$Despesa - rec_des$Receita);as.tibble(rec_des)

#### Gráfico base -------------------------------------------
GGrec_des <- ggplot(data = rec_des, aes(x = Ano)) + # Dados básicos e eixos comuns
  # Barras de despesa
  geom_col(aes(y = Despesa/1000000, fill = Diff/1000000), 
           width = 0.6,
           alpha=0.5, 
           #stat = "sum",
           position = "dodge",
           col = "black",
           size = 1) +
  # Efeito gradiente para qualquer preenchimento usado na estética do gráfico
  scale_fill_gradient(name="Delta de Despesa\nem Mil, 2011 - 2017",  
                      low = "green", 
                      high = "red", 
                      space = "Lab",
                      guide = "colourbar") +
  # Controle da quebra da escala no eixo X
  scale_x_continuous(breaks = seq(2011, 2017, by = 1)) +
  # Mudança de orientação do texto da legenda do eico x
  theme(axis.text.x = element_text(angle = 45, 
                                 vjust = 1, 
                                 size = 9, 
                                 hjust = 1)) +
  # Textos de eixos e Título
  labs(title = 'Média Mensal de Despesa X Receita', x = 'Anos', y = 'Despesa/Receita');GGrec_des

#### Gráfico com adição de camada com coluna de receita -----
GGrec_des + 
  # Barras de receita
  geom_bar(aes(y = Receita/1000000),
           width = 0.2, alpha=0.5, 
           fill = "blue", 
           stat = "sum",
           size = 1)

#### Gráfico com adição de camada com área de receita -------
GGrec_des + 
  # Área de receita
  geom_area(aes(y = Receita/1000000),
            col = "blue",
            size = 0.2,
            alpha = 0.3) +
  # Texto com valores
  geom_text(aes(y = (Despesa/1000000) + 13,
                label = format((Diff/1000000), digits = 2),
                col = Diff/1000000), 
            size = 4) +
  # Efeito gradiente para qualquer cor usada na estética do gráfico  
  scale_colour_gradient(name="Valor Delta \nem Mil, 2011 - 2017", 
                    low = "blue", 
                    high = "orange", 
                    space = "Lab",
                    guide = "colourbar")