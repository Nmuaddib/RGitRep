####---------------------------------------------------------
dwtest(lm.cai_SCc)
bgtest(lm.cai_SCc, order = 2)

summary.orcutt(lmT.SCc)
summary(lmT.SCc)

names(lmT.SCc)

lmT.SCc["rho"]

####---------------------------------------------------------
library("HoRM", lib.loc="C:/Program Files/R/R-3.4.1/library")
lmT_hil.SCc <- hildreth.lu(x = cai_SCc$consulta, y = cai_SCc$sadt, rho = 0.7097007)

summary(lmT_hil.SCc)

cai_SCc_Hil <- as.tibble(lmT_hil.SCc[["model"]]) %>%
  select(sadt = y, consulta = x)
    
cai_SCc_Hil <- add_predictions(cai_SCc_Hil,lmT_hil.SCc)

plot(p2$consulta,p2$sadt)

ggplot(cai_SCc_Hil, aes(y = consulta)) +
  geom_point(aes(x = sadt), color = "dark blue", alpha = 0.5, size = 3) +
  geom_line(aes(x = pred),color = "orange", size = 1.2) +
  labs(title = 'Dados e Modelo transformados pelo procedimento de Hildreth-Lu', 
       x = 'SADT Transformado', y = 'Consulta Transformada')

ggplot(cai_SCc, aes(y = consulta)) +
  geom_point(aes(x = sadt), color = "blue", alpha = 0.5, size = 3) +
  geom_line(aes(x = pred),color = "red", size = 1.2) +
  geom_line(aes(x = predT),color = "green", size = 1.2) +
  labs(title = 'Modelo original e transformado pelo procedimento de Cochrane Orcutt', 
       x = 'SADT', y = 'Consulta')
####---------------------------------------------------------
par(col = "black")

plot(cai_SCc_Hil$consulta,cai_SCc_Hil$sadt)
abline(lmT_hil.SCc, col = "red", lwd = 2, lty = 2)

plot(cai_SCc$consulta,resid(lm.cai_SCc))
abline(h = 0, col = "red", lwd = 2, lty = 2)

plot(cai_SCc$sadt ~ cai_SCc$consulta)
abline(lm.cai_SCc, col = "red", lwd = 2)
####---------------------------------------------------------

std_r <- resid(lmT.SCc)/sd(resid(lmT.SCc))
std_r <- sqrt(abs(std_r))
fit_r <- fitted(lmT.SCc)
plot(fit_r, std_r, 
     ylab = expression(sqrt("Standarized Residuals")), 
     xlab = "Fitted Values",
     main = "Escala e Localização de Resíduos")
smt = smooth.spline(fit_r, std_r, spar=1)
lines(smt, col='red', lwd=1)

std_r <- resid(lm.cai_SCc)/sd(resid(lm.cai_SCc))
std_r <- sqrt(abs(std_r))
fit_r <- fitted(lmT.SCc)
smt = smooth.spline(fit_r, std_r, spar=1)
plot(fit_r, std_r, ylab = expression(sqrt("Standarized Residuals")), xlab = "Fitted Values")
lines(smt, col='red', lwd=1)
####---------------------------------------------------------
plot(cooks.distance(lm.cai_SCc), std_r)
plot(cooks.distance(lm.cai_SCc))
plot(std_r ~ cooks.distance(lm.cai_SCc))
####---------------------------------------------------------
std_r1 <- resid(lm.cai_SCc)/sd(resid(lm.cai_SCc))
std_r2 <- rstandard(lm.cai_SCc)
std_r3 <- scale(resid(lm.cai_SCc), scale = TRUE, center = FALSE)
std_r3_sqrt <- sqrt(abs(std_r3))
std_r0 <- cbind(std_r1,std_r2,std_r3,std_r3_sqrt)

plot(fitted(lmT.SCc), std_r3)
plot(fitted(lmT.SCc), std_r3_sqrt)
plot(fitted(lmT.SCc), resid(lmT.SCc))
####---------------------------------------------------------
plot(lm.cai_SCc)




