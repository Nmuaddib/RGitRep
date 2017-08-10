options(scipen=999)

# SADT C,S,N
plot(cai_g)

## check model X scatter
ggplot(cai_SCc, aes(y = consulta)) +
  geom_point(aes(x = sadt), color = "blue", alpha = 0.5, size = 3) +
  geom_line(aes(x = pred),color = "red", size = 1.2)

## check model with the 4 standards
par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(lm.cai_SCc)
par(mfrow = c(1,1), oma = c(0, 0, 0, 0))

## plot model
plot(fitted(lm.cai_SCc))

## CEP to check the model, results are poor in this case
qcc.cai_SCc <- qcc(cai_SCc$sadt, type = 'xbar.one', newdata = fitted(lm.cai_SCc))

## extra hist and density with CEP/qcc
process.capability (qcc.cai_SCc, spec.limits=c(400000,700000))

## ggplot hist
ggplot(cai_SCc,aes(sadt)) +
  stat_bin(aes(y =..density..,
               fill = ..count..), 
           col = "black",
           binwidth = 35000, 
           alpha = 0.8) +
  geom_density(fill = "red",
               color = "red",
               alpha = 0.11) +
  scale_x_continuous(breaks = seq(200000, 800000, by = 100000)) +
  scale_y_continuous(labels = NULL) +
  labs(title = 'Hist', x = 'SADT', y = 'Count') #+ 
#xlim(c(260000,780000))

ggplot(cai_SCc,aes(pred)) +
  stat_bin(aes(y =..density..,
               fill = ..count..), 
           col = "black",
           binwidth = 35000, 
           alpha = 0.8) +
  geom_density(fill = "red",
               color = "red",
               alpha = 0.11) +
  scale_x_continuous(breaks = seq(200000, 800000, by = 100000)) +
  scale_y_continuous(labels = NULL) +
  labs(title = 'Hist', x = 'SADT', y = 'Count') #+ 
#xlim(c(260000,780000))

GGcorC <- ggplot(data = melt(caiC.corP, na.rm = TRUE), aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_distiller(palette = "Spectral", 
                       #trans = "reverse",
                       #space = "Lab", 
                       name="Pearson\nCorrelation") +  
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, 
                                   vjust = 1, 
                                   size = 12, 
                                   hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  guides(fill = guide_colorbar(barwidth = 1, 
                               barheight = 7,
                               title.position = "bottom"))+
  coord_fixed()

GGcorC + 
  geom_text(aes(Var2, Var1, label = format(value, digits = 2, nsmall= 2)), 
            color = "black", 
            size = 3)# +

GGcorC <- ggplot(data = melt(caiN.corP, na.rm = TRUE), aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_distiller(palette = "Spectral", 
                       #trans = "reverse",
                       #space = "Lab", 
                       name="Pearson\nCorrelation") +  
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, 
                                   vjust = 1, 
                                   size = 12, 
                                   hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  guides(fill = guide_colorbar(barwidth = 1, 
                               barheight = 7,
                               title.position = "bottom"))+
  coord_fixed()

GGcorC + 
  geom_text(aes(Var2, Var1, label = format(value, digits = 2, nsmall= 2)), 
            color = "black", 
            size = 3)# +

GGcorC <- ggplot(data = melt(caiS.corP, na.rm = TRUE), aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_distiller(palette = "Spectral", 
                       #trans = "reverse",
                       #space = "Lab", 
                       name="Pearson\nCorrelation") +  
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, 
                                   vjust = 1, 
                                   size = 12, 
                                   hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  guides(fill = guide_colorbar(barwidth = 1, 
                               barheight = 7,
                               title.position = "bottom"))+
  coord_fixed()

GGcorC + 
  geom_text(aes(Var2, Var1, label = format(value, digits = 2, nsmall= 2)), 
            color = "black", 
            size = 3)# +
