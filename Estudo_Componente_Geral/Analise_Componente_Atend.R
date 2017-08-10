###---------------------------------------------------------
library("reshape2", lib.loc="~/R/win-library/3.4")
#library("tidyverse", lib.loc="~/R/win-library/3.4")
library("readxl", lib.loc="~/R/win-library/3.4")
library("qcc", lib.loc="~/R/win-library/3.4")
library("modelr", lib.loc="~/R/win-library/3.4")
library("dplyr", lib.loc="~/R/win-library/3.4")
library("ggplot2", lib.loc="~/R/win-library/3.4")
library("readr", lib.loc="~/R/win-library/3.4")
library("tibble", lib.loc="~/R/win-library/3.4")
library("tidyr", lib.loc="~/R/win-library/3.4")
library("purrr", lib.loc="~/R/win-library/3.4")
#library("psych", lib.loc="~/R/win-library/3.4")
library("forcats", lib.loc="~/R/win-library/3.4")
####---------------------------------------------------------
write.csv2(dataset2, "dataset2.csv")
write.csv2(dataset, "dataset.csv")
save.image("~/Analise_Componente_atend.RData")
####---------------------------------------------------------
CAI_G[,-4] #select(CAI_G, -valor)
View(filter(CAI_GQS,amq>="20140101"))
depara_componente <- read_excel('depara_componente.xlsx')

#rm()

org <- read_excel('CAIv2.xlsx')
####---------------------------------------------------------
cai <- read_excel('CAIv2.xlsx') %>% # readxl: import
  group_by(componente,internacao,amq) %>% # dplyr: group
  summarise(valor = sum(valor), qt = sum(quantidade)) %>% # dplyr: sum by groupped beforehand
  select(-valor) %>% # dplyr: drop a column
  spread(componente,qt) %>% # tidyr: change from "key value" to columns
  arrange(amq, internacao) %>%  # dplyr: re-order to make row selection simpler
  filter(amq>="20140101") # dplyr: select row for analysis

View(cai)
####---------------------------------------------------------
names(cai)
?split
?as.factor

cai_splt <- split(cai, as.factor(cai$internacao))
cai_splt <- cai %>% split(.$internacao)
cai_splt[[1]][1,2]

View(cai_splt)
summary(cai_splt)
names(cai_splt[[1]])
####---------------------------------------------------------
cai

cai_df <- cai %>% 
  as.data.frame(.) %>% 
  as.tibble()

cai_df
####---------------------------------------------------------
cai_SCs <- cai_df %>%
  filter(internacao == "s") %>% 
  arrange(amq) %>% 
  select(sadt) %>% 
  as.tibble()

cai_temp <- cai_df %>%
  filter(internacao == "n") %>% 
  arrange(amq) %>% 
  select(consulta) %>% 
  as.tibble()
  
cai_SCs <- cai_SCs %>% add_column(consulta = cai_temp$consulta)
####---------------------------------------------------------
cai_SCn <- cai_df %>%
  filter(internacao == "n") %>% 
  arrange(amq) %>% 
  select(sadt) %>% 
  as.tibble()

cai_SCn <- cai_SCn %>% add_column(consulta = cai_temp$consulta)
####---------------------------------------------------------
rm(cai_SCc)
                       
cai_SCc <- cai_df %>% 
  select(amq,sadt) %>% 
  group_by(amq) %>% 
  summarise(sadt=sum(sadt)) %>% 
  as.data.frame(.) %>% 
  select(sadt) %>% 
  as.tibble()     

zSc <- scale(cai_SCc$sadt, center = TRUE, scale = TRUE) #z, p for sigmas
cai_SCc <- cai_SCc %>% add_column(consulta = cai_temp$consulta, z = zSc[1:dim(cai_SCc)[1]]) 
#%>% filter(!abs(z) > 2.5)
####---------------------------------------------------------

dim(cai_SCc)[1] # length of a tibble

cai_SCc

cai_g <- cai_SCc %>% 
  select(consulta,sadt) %>% 
  add_column(sadt_n = cai_SCn$sadt) %>% 
  add_column(sadt_s = cai_SCs$sadt)

plot(cai_g)
####---------------------------------------------------------
## check regress
lm(sadt ~ consulta, data = cai_SCc) %>% 
  summary()

## fit model
lm.cai_SCc <- lm(sadt ~consulta, data = cai_SCc)

## add fiited column
cai_SCc <- cai_SCc %>% add_predictions(lm.cai_SCc) #modelr lib

## check model X scatter
ggplot(cai_SCc, aes(y = consulta)) +
  geom_point(aes(x = sadt), color = "blue", alpha = 0.5, size = 3) +
  geom_line(aes(x = pred),color = "red", size = 1.2)

## check model with the 4 standards
par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(lm.cai_SCc)
par(mfrow = c(1,1), oma = c(0, 0, 0, 0))

## limits for fitted
predict.lm(lm.cai_SCc, level=0.95, interval = 'confidence')

rm(SCc.CI)
SCc.CI <- as.tibble(predict.lm(lm.cai_SCc, level=0.99, interval = 'confidence'))
SCc.CI <- add_column(SCc.CI, rn = as.numeric(row.names(SCc.CI)))
cai_SCc <- add_column(cai_SCc, rn = as.numeric(row.names(SCc.CI)))
View(SCc.CI)

ggplot(SCc.CI, aes(x = rn, y = fit, col = fit)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymax = upr, ymin = lwr)) +
  geom_line(color = "red", size = 0.4, alpha = 0.5) +
  geom_point(data = cai_SCc, aes(y = sadt),col = "orange" , size = 2)

cai_SCc

plotCI(SCc.CI$fit[1:3], ui = SCc.CI$upr[1:3], li = SCc.CI$lwr[1:3], ylab = NULL, xlab = NULL)
####---------------------------------------------------------

## check range within 99% of dist
t.test(cai_SCc$sadt, fitted(lm.cai_SCc), conf.level = 0.95)

## check range for the 99% conf. for the model
t.test(fitted(lm.cai_SCc), conf.level = 0.95) #confidence for the model
confint(lm.cai_SCc, level=0.95)

## plot model
plot(fitted(lm.cai_SCc))

## standard hists for model and actual
hist(fitted(lm.cai_SCc))
hist(cai_SCc$sadt)

## CEP to check the model, results are poor in this case
qcc.cai_SCc <- qcc(cai_SCc$sadt, type = 'xbar.one', newdata = fitted(lm.cai_SCc))

## extra hist and density with CEP/qcc
process.capability (qcc.cai_SCc, spec.limits=c(400000,700000))

## default summary to check min and max
summary(cai_SCc$sadt)

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

####---------------------------------------------------------
lm(sadt ~ consulta, data = cai_SCn) %>% 
  summary()

lm.cai_SCn <- lm(sadt ~consulta, data = cai_SCn)

cai_SCn <- cai_SCn %>% add_predictions(lm.cai_SCn)

ggplot(cai_SCn, aes(y = consulta)) +
  geom_point(aes(x = sadt, alpha = consulta, color = sadt), size = 3) +
  geom_line(aes(x = pred),color = "red", size = 1.2)

plot(lm.cai_SCn)

predict.lm(lm.cai_SCn, level=0.95, interval = 'confidence')
####---------------------------------------------------------
filter(org,componente == "sadt", amq == '20140101' | amq == '20140102') %>% 
  group_by(componente,internacao,amq) %>% 
  summarise(sum(quantidade))
####---------------------------------------------------------
names(cai)

caiC.precor <- cai %>%
  arrange(amq, internacao) %>% 
  group_by(amq) %>% 
  summarise(consulta = sum(consulta, na.rm = TRUE), 
            domiciliar = sum(domiciliar, na.rm = TRUE), 
            emergencia = sum(emergencia, na.rm = TRUE), 
            honorario = sum(honorario, na.rm = TRUE), 
            material = sum(material, na.rm = TRUE), 
            medicamento = sum(medicamento, na.rm = TRUE), 
            nulo = sum(nulo, na.rm = TRUE), 
            pacote = sum(pacote, na.rm = TRUE), 
            remocao = sum(remocao, na.rm = TRUE), 
            sadt = sum(sadt, na.rm = TRUE), 
            taxa = sum(taxa, na.rm = TRUE)) %>% 
  as.data.frame(.) %>%
  select(-amq) %>% 
  as.tibble()

caiN.precor <- cai %>%
  filter(internacao == 'n') %>% 
  arrange(amq) %>%   
  group_by(amq) %>% 
  summarise(consulta = sum(consulta, na.rm = TRUE), 
            domiciliar = sum(domiciliar, na.rm = TRUE), 
            emergencia = sum(emergencia, na.rm = TRUE), 
            honorario = sum(honorario, na.rm = TRUE), 
            material = sum(material, na.rm = TRUE), 
            medicamento = sum(medicamento, na.rm = TRUE), 
            nulo = sum(nulo, na.rm = TRUE), 
            pacote = sum(pacote, na.rm = TRUE), 
            remocao = sum(remocao, na.rm = TRUE), 
            sadt = sum(sadt, na.rm = TRUE), 
            taxa = sum(taxa, na.rm = TRUE)) %>% 
  as.data.frame(.) %>%
  select(-amq) %>% 
  as.tibble()

caiS.precor <- cai %>%
  filter(internacao == 's') %>% 
  arrange(amq) %>% 
  group_by(amq) %>% 
  summarise(consulta = sum(consulta, na.rm = TRUE), 
            domiciliar = sum(domiciliar, na.rm = TRUE), 
            emergencia = sum(emergencia, na.rm = TRUE), 
            honorario = sum(honorario, na.rm = TRUE), 
            material = sum(material, na.rm = TRUE), 
            medicamento = sum(medicamento, na.rm = TRUE), 
            nulo = sum(nulo, na.rm = TRUE), 
            pacote = sum(pacote, na.rm = TRUE), 
            remocao = sum(remocao, na.rm = TRUE), 
            sadt = sum(sadt, na.rm = TRUE), 
            taxa = sum(taxa, na.rm = TRUE)) %>% 
  as.data.frame(.) %>%
  select(-amq) %>% 
  as.tibble()

rm(caiC.cor,caiS.cor,caiN.cor)

caiC.corP <- caiC.precor %>% 
  cor(., method = 'pearson');caiC.corP

caiN.corP <- caiN.precor %>% 
  cor(., method = 'pearson');caiN.corP

caiS.corP <- caiS.precor %>% 
  cor(., method = 'pearson');caiS.corP

Ccorgrid <- symnum(caiC.corP)
Ncorgrid <- symnum(caiN.corP)
Scorgrid <- symnum(caiS.corP)

caiC.corP[lower.tri(caiC.corP)] <- NA
caiN.corP[lower.tri(caiN.corP)] <- NA
caiS.corP[lower.tri(caiS.corP)] <- NA

Ccorgrid
Ncorgrid
Scorgrid

####---------------------------------------------------------

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

?format

GGcorC + 
  geom_text(aes(Var2, Var1, label = format(value, digits = 2, nsmall= 2)), 
            color = "black", 
            size = 3)# +
####---------------------------------------------------------
#  theme(
#    axis.title.x = element_blank(),
#    axis.title.y = element_blank(),
#    panel.grid.major = element_blank(),
#    panel.border = element_blank(),
#    panel.background = element_blank(),
#    axis.ticks = element_blank(),
#    legend.justification = c(1, 0),
#    legend.position = c(0.4, 0.77),
#    legend.direction = "horizontal")+
#  guides(fill = guide_colorbar(barwidth = 7, 
#                               barheight = 1,
#                               title.position = "top", 
#                               title.hjust = 0.5))

#scale_fill_gradient2(low = "light blue", 
#                     high = "red", 
#                     mid = "blue", 
#                     midpoint = 0.5, 
#                     limit = c(-0.1,1), 
#                     space = "Lab", 
#                     name="Pearson\nCorrelation") +
####---------------------------------------------------------
GGcorC
####---------------------------------------------------------

topcorS <- melt(caiS.corP, na.rm = TRUE) %>% 
  filter((value >= 0.85 | ((Var1 == "pacote" | Var2 == "pacote") & value >= 0.70)) & value != 1) %>% 
  arrange(desc(value))

topcorN <- melt(caiN.corP, na.rm = TRUE) %>% 
  filter(value >= 0.85 & value != 1) %>% 
  arrange(desc(value))

melt(caiN.corP, na.rm = TRUE) %>% 
  filter(value >= 0.90 & value != 1) %>% 
  arrange(desc(value))

####---------------------------------------------------------
names(dataset)
resumo_valor_N <- dataset %>% 
  select(internacao = "Indicador de Internação", 
         componente = "Componente assistencial",
         valor = "Valor Total",
         percentual =  "Valor Total.1",
         quantidade = "Quantidade Aprovada") %>% 
  mutate(percentual = as.numeric(format(percentual*100, digits = 2)))

resumo_valor_S <- dataset2 %>% 
  select(internacao = "Indicador de Internação", 
         componente = "Componente assistencial",
         valor = "Valor Total",
         percentual =  "Valor Total.1",
         quantidade = "Quantidade Aprovada") %>% 
  mutate(percentual = as.numeric(format(percentual*100, digits = 2)))

#append tibbles
resumo_valor <- rbind(resumo_valor_N,resumo_valor_S) %>% 
  arrange(internacao, desc(valor))

# merge two data frames by ID and Country
# total <- merge(data frameA,data frameB,by=c("ID","Country"))

####---------------------------------------------------------
depara_componente

names(depara_componente)

depara_componente <- depara_componente %>% 
  select(componente = "Componente assistencial", comp = "componente")
####---------------------------------------------------------
resumo_valor_S <- merge(resumo_valor_S, depara_componente, by = "componente") %>% 
  mutate(componente = comp) %>% 
  select(-comp)

resumo_valor_N <- merge(resumo_valor_N, depara_componente, by = "componente") %>% 
  mutate(componente = comp) %>% 
  select(-comp)
####---------------------------------------------------------
topcorS
resumo_valor_S

topcorN
resumo_valor_N

# lookup > lkupTable[match(valuesTable$Var, lkupTable$key),#response_index]
topcorS_W <- add_column(topcorS, var1_prct = resumo_valor_S[match(topcorS$Var1, resumo_valor_S$componente),4])
topcorS_W <- add_column(topcorS_W, var2_prct = resumo_valor_S[match(topcorS$Var2, resumo_valor_S$componente),4])

topcorN_W <- add_column(topcorN, var1_prct = resumo_valor_N[match(topcorN$Var1, resumo_valor_N$componente),4])
topcorN_W <- add_column(topcorN_W, var2_prct = resumo_valor_N[match(topcorN$Var2, resumo_valor_N$componente),4])

names(topcorS_W)
topcorS_W <- select(topcorS_W, "Var1","var1_prct","Var2","var2_prct","value")
topcorN_W <- select(topcorN_W, "Var1","var1_prct","Var2","var2_prct","value")

topcorS_W
topcorN_W
####---------------------------------------------------------

names(cai)

topcorS_W

lm(taxa ~ sadt, data = caiS.precor) %>% 
  summary()
lm(sadt ~ taxa, data = caiS.precor) %>% 
  summary()

View(caiS.precor)

ggplot(caiS.precor[1:77,], aes(taxa)) +
  stat_bin(aes(y =..density..,
               fill = ..count..), 
           col = "black",
           binwidth = 2000, 
           alpha = 0.8) +
  geom_density(fill = "red",
               color = "red",
               alpha = 0.11) +
  scale_x_continuous(breaks = seq(200000, 800000, by = 100000)) +
  scale_y_continuous(labels = NULL) +
  labs(title = 'Hist', x = 'SADT', y = 'Count')

####---------------------------------------------------------
## Correlação Emergencia > Taxas em Internacao 
names(cai_df)

cai_ETs <- cai_df %>%
  filter(internacao == "s") %>% 
  arrange(amq) %>% 
  select(taxa) %>% 
  as.tibble()

cai_temp <- cai_df %>%
  filter(internacao == "n") %>% 
  arrange(amq) %>% 
  select(emergencia) %>% 
  as.tibble()

cai_ETs <- cai_ETs %>% add_column(emergencia = cai_temp$emergencia)
cai_ETs <- cai_ETs[1:77,]

cai_ETs.corP <- cai_ETs %>% 
  cor(., method = 'pearson');cai_ETs.corP

## Correlação Consulta > Pacote em Internação
cai_PsC <- cai_df %>%
  filter(internacao == "s") %>% 
  arrange(amq) %>% 
  select(pacote) %>% 
  as.tibble()

cai_temp <- cai_df %>%
  filter(internacao == "n") %>% 
  arrange(amq) %>% 
  select(consulta) %>% 
  as.tibble()

cai_PsC <- cai_PsC  %>% add_column(consulta = cai_temp$consulta)
cai_PsC <- cai_PsC[1:77,]

cai_PsC.corP <- cai_PsC %>% 
  cor(., method = 'pearson');cai_PsC.corP

cai_PsC
####---------------------------------------------------------
## checking Z
zPsC <- scale(cai_PsC$pacote, center = TRUE, scale = TRUE)
cai_PsC <- cai_PsC %>% add_column(zp = zPsC[1:dim(cai_PsC)[1]])

filter(cai_PsC, abs(zc) > 2.5)

####---------------------------------------------------------
## fit model
lm.ST<- lm(sadt ~ taxa, data = caiS.precor)

## add fiited column
cai_ST <- caiS.precor %>% add_predictions(lm.ST) #modelr lib

## check model X scatter
ggplot(cai_ST, aes(y = taxa)) +
  geom_point(aes(x = sadt), color = "blue", alpha = 0.5, size = 3) +
  geom_line(aes(x = pred),color = "red", size = 1.2)

## check model with the 4 standards
par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(lm.ST)
par(mfrow = c(1,1), oma = c(0, 0, 0, 0))

summary(lm.ST)

anv.ST <- anova(lm.ST)

rm(lm.ST,cai_ST)
## POOR MODEL ##
####---------------------------------------------------------

## fit model
lm.PsC<- lm(pacote ~ consulta, data = cai_PsC)

## add fiited column
cai_PsC <- cai_PsC %>% add_predictions(lm.PsC) #modelr lib

## check model X scatter
ggplot(cai_PsC, aes(y = consulta)) +
  geom_point(aes(x = pacote), color = "blue", alpha = 0.5, size = 3) +
  geom_line(aes(x = pred),color = "red", size = 1.2)

## check model with the 4 standards
par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(lm.PsC)
par(mfrow = c(1,1), oma = c(0, 0, 0, 0))

summary(lm.PsC)

qcc(cai_PsC$pacote, type = 'xbar.one', newdata = fitted(lm.PsC))

ggplot(cai_PsC, aes(pred)) +
  stat_bin(aes(y =..density..,
               fill = ..count..), 
           col = "black",
           binwidth = 700, 
           alpha = 0.8) +
  geom_density(fill = "red",
               color = "red",
               alpha = 0.11) +
  scale_x_continuous(breaks = seq(200000, 800000, by = 100000)) +
  scale_y_continuous(labels = NULL) +
  labs(title = 'Hist', x = 'consulta', y = 'Count')

cai_PsC

####---------------------------------------------------------
resumo_valor %>% 
  group_by(componente) %>% 
  summarise(valor = sum(valor)) %>% 
  arrange(desc(valor))

resumo_valor

####---------------------------------------------------------
mtcars %>%
  split(.$cyl) %>% # from base R
  map(~ lm(mpg ~ wt, data = .)) %>%
  map(summary) %>%
  map_dbl("r.squared")

as.tibble(mtcars)
####---------------------------------------------------------
cai_ECn_PTs <- cai_df %>%
  filter(internacao == "s") %>% 
  arrange(amq) %>% 
  select(taxa) %>% 
  as.tibble()

cai_temp <- cai_df %>%
  filter(internacao == "n") %>% 
  arrange(amq) %>% 
  select(emergencia) %>% 
  as.tibble()

cai_ECn_PTs <- cai_ECn_PTs %>% add_column(emergencia = cai_temp$emergencia)

cai_temp <- cai_df %>%
  filter(internacao == "n") %>% 
  arrange(amq) %>% 
  select(consulta) %>% 
  as.tibble()

cai_ECn_PTs <- cai_ECn_PTs %>% add_column(consulta = cai_temp$consulta)

cai_temp <- cai_df %>%
  filter(internacao == "s") %>% 
  arrange(amq) %>% 
  select(pacote) %>% 
  as.tibble()

cai_ECn_PTs <- cai_ECn_PTs %>% add_column(pacote = cai_temp$pacote)
cai_ECn_PTs <- cai_ECn_PTs[1:77,]

cai_ECn_PTs.corP <- cai_ECn_PTs %>% 
  cor(., method = 'pearson');cai_ECn_PTs.corP

