###---------------------------------------------------------
library("reshape2" )
library("tidyverse" )
library("readxl" )
library("qcc" )
library("modelr" )
library("dplyr" )
library("ggplot2" )
library("readr" )
library("tibble" )
library("tidyr" )
library("purrr" )
#library("psych" )
library("forcats" )
####---------------------------------------------------------
library("reshape2")
library("tidyverse")
library("readxl")
library("qcc")
library("modelr")
####---------------------------------------------------------
`dataset` = read.csv('C:/Users/nelson.junior/Documents/R/REditorWrapper_c0e3f832-cc45-40ce-a15c-5b3b8512888c/input_df_45846562-f894-45e9-8771-e6ee4139d56e.csv', check.names = FALSE, encoding = "UTF-8", blank.lines.skip = FALSE);
write.csv2(dataset3, "external.csv")
save.image("~/Analise_Componente.RData")
RdataCAI <- read_excel('CAI.xlsx')
###---------------------------------------------------------
#plot frames
par(mfrow = c(1,1), oma = c(0, 0, 0, 0))
par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
#sci not, e format
options(scipen=0)
options(scipen=10)
options(scipen=999)

###---------------------------------------------------------
RdataQT
RdataSQT <- RdataQT %>% spread(componente,quantidade)
df10 <- data.frame(RdataQT)
df20 <- data.frame(RdataSQT)
df30 <- select(df20,-(anomes))
row.names(df30) <- df20$anomes
View(df30)
rownames(df30)

###---------------------------------------------------------
plot(df30)
cortabPQT <- cor(df30, method = 'pearson'); cortabPQT
cortabKQT <- cor(df30, method = 'kendall')
cortabSQT <- cor(df30, method = 'spearman')
symnum(cortabPQT)
symnum(cortabKQT)
symnum(cortabSQT)

###---------------------------------------------------------
hist(df30$pacote)
hist(df30$consulta)
hist(df30$sadt,9)
hist(df30$emergencia)

###---------------------------------------------------------
qqnorm(df30$pacote)
qqnorm(df30$consulta)
qqnorm(df30$sadt)
qqnorm(df30$emergencia)


###---------------------------------------------------------
lmodSCqt <- lm(sadt ~ consulta, data = df30)
summary(lmodSCqt)
plot(lmodSCqt)
cor.test(df30$sadt, df30$consulta)
anova(lmodSCqt)
sadtLMt <- data.frame(sadt = df30$sadt, fitt = fitted(lmodSCqt)); sadtLMt
#SC_qcc <- sadtLMt %>% gather(typ, val, sadt:fitt)
#SC_qcc
#attach(SC_qcc)
#qcc(val[1:39], type = 'xbar.one', newdata = val[40:78])
#detach(SC_qcc)
qcc(df30$sadt, type = 'xbar.one', newdata = fitted(lmodSCqt))

###---------------------------------------------------------
df30$sadt
df30$honorarios
plot(sadt ~ honorarios, data = df30)
lmodSHqt <- lm(sadt ~ honorarios, data = df30)
resid(lmodSHqt)
summary(lmodSHqt)
plot(lmodSHqt)
#CEP, model test
qcc(df30$sadt, type = 'xbar.one', newdata = fitted(lmodSHqt))
cor.test(df30$sadt, df30$honorarios)
anova(lmodSHqt)
plot(fitted(lmodSHqt))
qr(lmodSHqt)

###---------------------------------------------------------
xS <- scale(df30$sadt, center = TRUE, scale = TRUE)
xS <- xS[1:39]
xS
xT <- scale(df30$taxas, center = TRUE, scale = TRUE)
xT <- xT[1:39]
xT
df30$sadt
df30$taxas
plot(df30$sadt, type = 'b')
plot(df30$taxas, type = 'b')
plot(xS, type = 'b')
plot(xT, type = 'b')

plot(sadt ~ taxas, data = df30)
lmodSTqt <- lm(sadt ~ taxas, data = df30)
summary(lmodSTqt)
resid(lmodSTqt)
plot(lmodSTqt)
plot(resid(lmodSTqt))
#CEP, model test
qcc(df30$sadt, type = 'xbar.one', newdata = fitted(lmodSTqt))
cor.test(df30$sadt, df30$taxas)
anova(lmodSTqt)
plot(fitted(lmodSTqt))
qr(lmodSTqt)

###---------------------------------------------------------
#std.dv.
sd(df30$sadt)
sd(df30$consulta)

#confidence interval
t.test(df30$sadt, conf.level = 0.9)

###---------------------------------------------------------
#using the model
#lmodSCqt <- lm(sadt ~ consulta, data = df30)
ptx <- data.frame(consulta = c(6490))
fitted(lmodSCqt) # =predict.lm(lmodSCqt), y values predicted by the model
names(lmodSCqt)
sd(resid(lmodSCqt))#actual-fitted = residual err
?qr
qr(lmodSCqt)
?predict.lm
predict.lm(lmodSCqt)
predict.lm(lmodSCqt, ptx, interval = 'predict')
predict.lm(lmodSCqt, ptx, interval = 'confidence')
mean(df30$sadt)
sd(df30$sadt)
df30$sadt
summary(lmodSCqt)
dfSC <- data.frame(sadt = df30$sadt, consul = df30$consulta)
dfSC
#student T 
qt(0.01, df = 38)
qt(0.05, df = 38)
mean(df30$sadt)

###---------------------------------------------------------
#std.err
qt(0.01, df = 38)
ste <- sd(df30$sadt)/(sqrt(length(df30$sadt))); ste
#var coef.
cv <- (sd(df30$sadt)/mean(df30$sadt)*100); cv

# z-scores, checking for outliers, z > 3
xS <- scale(df30$sadt, center = TRUE, scale = TRUE)
xS <- xS[1:39]
xS
xC <- scale(df30$consulta, center = TRUE, scale = TRUE)
xC <- xC[1:39]

xTT <- cbind(consulta = df30$consulta, zC = xC, sadt = df30$sadt, zS = xS)
xTT
txT <- as.tibble(xTT)
txT

plot(xS)
plot(xC)
plot(xS, type = 'b')
plot(xC, type = 'b')


###---------------------------------------------------------
plot(sadt ~ taxas, data = dfmF)
lmodSTadj <- lm(sadt ~ taxas, data = dfmF)
summary(lmodSTadj)
resid(lmodSTadj)
plot(lmodSTadj)
plot(lmodSTqt)
plot(resid(lmodSTadj))
#CEP, model test
qcc(dfmF$sadt, type = 'xbar.one', newdata = fitted(lmodSTadj))
cor.test(dfmF$sadt, dfmF$taxas)
anova(lmodSTadj)
plot(fitted(lmodSTadj))
hist(dfmF$sadt, 12)
hist(fitted(lmodSTadj), 12)
qr(lmodSTadj)

###---------------------------------------------------------
#vector manip
xS <- scale(df30$sadt, center = TRUE, scale = TRUE) #z, p for sigmas
xS <- xS[1:39]
dfm <- data.frame(sadt = df30$sadt, taxas = df30$taxas, v = TRUE, z = xS)
View(dfm)
#abs > -signal, %% > mod
dfmF2 <- dfm[!abs(dfm[,4])>2.5,] # remove outliers, z > 2.5
dfmF2
row.names(dfmF2) <- 1:length(dfmF2[,1]) #ajust rownames

dfm[,3] <- TRUE
dfm[25:26,3] <- FALSE
length (dfm) # cols
length (dfm[,1]) # rows
dfm[25:length(dfm[,1]),3] <- FALSE
i <- c(25,26)
dfmF <- dfm[-26,]
dfmF <- dfm[-i,]
dfmF <- dfm[-c(25:26),]
dfmF

lmodSTadj <- lm(sadt ~ taxas,data = dfmF2)
summary(lmodSTadj)

lmodSTqt <- lm(sadt ~ taxas,data = df30)
summary(lmodSTqt)

qrST <- qr(lmodSTadj)
summary(qrST)

dfmF2[dfmF2[,1]<40000,]

View(dfmF2)
View(dfST)
dfST1 <- data.frame(taxas = dfmF2$taxas)
dfST2 <- data.frame(taxas = df30$taxas)
dfST3 <- data.frame(consulta = df30$consulta)
dfST3
#dfST <- dfmF2 %>% data_grid(taxas)
dfST1 <- dfST1 %>% add_predictions(lmodSTadj)
dfST2 <- dfST2 %>% add_predictions(lmodSTqt)
dfST3 <- dfST3 %>% add_predictions(lmodSCqt)


ggplot(dfmF2, aes(y = taxas)) +
  geom_point(aes(x = sadt)) +
  geom_line(data = dfST1, aes(x = pred), colour = "red", size = 1)

ggplot(df30, aes(y = taxas)) +
  geom_point(aes(x = sadt), shape = 1, size = 2) +
  geom_line(data = dfST2, aes(x = pred), colour = "red", size = 1.5) +
  geom_line(data = dfST1, aes(x = pred), colour = "blue", size = 1.5)

ggplot(df30, aes(y = consulta)) +
  geom_point(aes(x = sadt)) +
  geom_line(data = dfST3, aes(x = pred), colour = "red", size = 1)
