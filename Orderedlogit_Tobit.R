library(haven)
df <- read_dta("Mesh_data_year_new.dta")

# descriptive statistics
library(stargazer)
df_ds <- df[, -c(1, 2, 4, 5, 10, 11, 12, 13, 14,15, 16, 17, 18, 19, 20, 21, 26, 27, 28, 29, 30, 31, 32, 33, 34, 38, 39, 40, 41, 42, 43)]
df_ds <- as.data.frame(df_ds)
stargazer(df_ds, out="stargazer.descript.tex", title="descriptive statistics", align=F, style="qje")

# Logit Model 
out_logit_L <- glm(LAWSON_Entry ~ LAWSON_existed + LAWSON_existed_2 + Familymart_existed +
                     F_than_L + L_mono + pop + n_employee + super_dummy, data = df, family = binomial("logit"))

out_logit_F <- glm(Familymart_Entry ~ Familymart_existed + Familymart_existed_2 + LAWSON_existed +
                     L_than_F + F_mono + pop + n_employee + super_dummy, data = df, family = binomial("logit"))


# Probit Model
out_probit_L <- glm(LAWSON_Entry ~ LAWSON_existed + LAWSON_existed_2 + Familymart_existed +
                      F_than_L + L_mono + pop + n_employee + super_dummy, data = df, family = binomial("probit"))

out_probit_F <- glm(Familymart_Entry ~ Familymart_existed + Familymart_existed_2 + LAWSON_existed +
                      L_than_F + F_mono + pop + n_employee + super_dummy, data = df, family = binomial("probit"))



# Ordered logit
df$L_Entry_new <- as.ordered(df$L_Entry_new)
df$F_Entry_new <- as.ordered(df$F_Entry_new)

library(MASS)
out_orderd_L <- polr(L_Entry_new ~ LAWSON_existed + LAWSON_existed_2 + Familymart_existed +
                       F_than_L + L_mono + pop + n_employee + super_dummy, data = df, Hess = TRUE, method = "logistic")

out_orderd_F <- polr(F_Entry_new ~ Familymart_existed + Familymart_existed_2 + LAWSON_existed +
                       L_than_F + F_mono + pop + n_employee + super_dummy, data = df, Hess = TRUE, method = "logistic")

# marginal effect / Odds ratio



# Tobit Model
library(AER)
df$L_Entry_new <- as.numeric(df$L_Entry_new)
df$F_Entry_new <- as.numeric(df$F_Entry_new)
out_tobit_L <- tobit(L_Entry_new ~ LAWSON_existed + LAWSON_existed_2 + Familymart_existed +
                       F_than_L + L_mono + pop + n_employee + super_dummy, data = df)

out_tobit_F <- tobit(F_Entry_new ~ Familymart_existed + Familymart_existed_2 + LAWSON_existed +
                       L_than_F + F_mono + pop + n_employee + super_dummy, data = df)

# marginal effect / Odds ratio



# Stargazer
stargazer(out_orderd_L, out_orderd_F, out_tobit_L, out_tobit_F, title="stargazer による回帰分析の結果")
