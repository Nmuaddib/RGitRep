qqnorm()

qqnorm(cai_df$sadt ~ cai_df$consulta)

qqnorm(resid(lm.cai_SCc))
qqline(resid(lm.cai_SCc), lty = 2)

plot(lm.cai_SCc, which = 2)

strs <- resid(lm.cai_SCc)/sd(resid(lm.cai_SCc))

qqnorm(strs)
qqline(strs, lty = 2)

     