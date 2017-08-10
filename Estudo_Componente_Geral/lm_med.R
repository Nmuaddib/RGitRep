Med_mr <- cai_df %>% 
  filter(internacao == "n") %>% 
  arrange(amq) %>% 
  select(medicamento, honorario, consulta, taxa)

lm01.Med_mr <- lm(data =  Med_mr, medicamento ~ honorario)
lm02.Med_mr <- lm(data =  Med_mr, honorario ~ consulta)

lm03.Med_mr <- lm(data =  Med_mr, medicamento ~ consulta)
lm04.Med_mr <- lm(data =  Med_mr, medicamento ~ honorario + consulta)
lm05.Med_mr <- lm(data =  Med_mr, medicamento ~ honorario + consulta + taxa)
lm06.Med_mr <- lm(data =  Med_mr, medicamento ~ honorario + taxa)

summary(lm01.Med_mr)
summary(lm02.Med_mr)

plot(lm01.Med_mr)
plot(lm02.Med_mr)

plot(Med_mr)

anv <- anova(lm01.Med_mr,lm03.Med_mr,lm04.Med_mr,lm05.Med_mr,lm06.Med_mr)

str(anv)

switch()

cat()

?apply()
