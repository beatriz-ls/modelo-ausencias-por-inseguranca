library(DHARMa)
library(dplyr)
library(tidyr)
library(pscl)
library(lmtest)
library(sandwich)
library(car)


dados <- readRDS("dados_pense2024_tratados.rds")
dados$ausencia_num <- as.numeric(dados$dias_ausencia_inseguranca_escola) - 1

dados_mod <- dados %>%
  drop_na(all_of(variaveis_modelo)) %>%
  drop_na(ausencia_num) %>%
  mutate(peso_norm = peso_amostral / mean(peso_amostral)) %>%
  mutate(across(where(is.ordered), ~ factor(.x, ordered = FALSE)))

# Ajuste do modelo final (ZINB completo)
fit_zinb_full <- zeroinfl(
  ausencia_num ~ bullying_sofrido + exclusao_social + agressao_fisica_colegas +
    cyberbullying_sofrido + apoio_familiar + escolaridade_mae +
    acao_prevencao_bullying + sexo + raca_cor + faixa_etaria + rede_ensino,
  data = dados_mod,
  weights = peso_norm,
  dist = "negbin"
)

# Inferência com erros-padrão robustos (sandwich) e tabela IRR / OR
res_full <- coeftest(fit_zinb_full, vcov = sandwich)
ic_full  <- coefci(fit_zinb_full, vcov = sandwich)

tabela_final <- data.frame(
  Parametro = rownames(res_full),
  Exp_Coef  = round(exp(res_full[, 1]), 3), # IRR na parte count_ | OR na parte zero_
  IC_inf    = round(exp(ic_full[, 1]), 3),
  IC_sup    = round(exp(ic_full[, 2]), 3),
  p_valor   = round(res_full[, 4], 4)
)

# Diagnóstico: Resíduos quantílicos 
probs_full <- predict(fit_zinb_full, type = "prob", at = 0:5) 

y <- dados_mod$ausencia_num
cdf <- probs_full
for (j in 2:6) cdf[, j] <- cdf[, j - 1] + probs_full[, j]

F_y <- cdf[cbind(seq_along(y), y + 1)]
F_y_minus_1 <- ifelse(y == 0, 0, cdf[cbind(seq_along(y), pmax(y, 1))])

set.seed(123)
u <- runif(length(y), min = F_y_minus_1, max = F_y)
res_q <- qnorm(pmin(pmax(u, 1e-15), 1 - 1e-15))

idx_plot <- sample(seq_along(res_q), 5000)

qqPlot(
  res_q[idx_plot],
  distribution = "norm",
  envelope = 0.95,
  pch = 16, cex = 0.5,
  main = "QQ-plot - Resíduos Quantílicos (ZINB)",
  ylab = "Resíduos Quantílicos Aleatorizados"
)
#################################
################################ RESULTADOS DAS ESTIMATIVAS
res_full <- coeftest(fit_zinb_full, vcov = sandwich)
ic_full  <- coefci(fit_zinb_full, vcov = sandwich)

tabela_completa <- data.frame(
  Parametro = rownames(res_full),
  Estimativa = round(res_full[, 1], 3),
  EP_Robusto = round(res_full[, 2], 3),
  Exp_Coef   = round(exp(res_full[, 1]), 3),
  IC_inf     = round(exp(ic_full[, 1]), 3),
  IC_sup     = round(exp(ic_full[, 2]), 3),
  p_valor    = ifelse(res_full[, 4] < 0.001, "< 0.001", round(res_full[, 4], 3)),
  row.names  = NULL
)

# Separar em Parte de Contagem (IRR) e Parte Zero-Inflada (OR)
tabela_count <- tabela_completa %>%
  filter(grepl("^count_", Parametro)) %>%
  mutate(Parametro = sub("^count_", "", Parametro)) %>%
  rename(IRR = Exp_Coef)

tabela_zero <- tabela_completa %>%
  filter(grepl("^zero_", Parametro)) %>%
  mutate(Parametro = sub("^zero_", "", Parametro)) %>%
  rename(OR = Exp_Coef)

# Visualizar as duas tabelas
tabela_count
tabela_zero


# Pesos normalizados e proporções observadas (ponderadas)
w <- dados_mod$peso_norm / sum(dados_mod$peso_norm)
obs_prop <- sapply(0:5, function(k) sum(w[dados_mod$ausencia_num == k]))

# Proporções previstas pelo modelo ZINB 
p_zinb_mat <- predict(fit_zinb_full, type = "prob")
zinb_prop  <- c(
  colSums(p_zinb_mat[, 1:5] * w),
  sum(p_zinb_mat[, 6:ncol(p_zinb_mat)] * w)
)

#  Organizar matriz para o gráfico
mat_zinb <- rbind(
  "Observado"          = obs_prop,
  "Previsto pela ZINB" = zinb_prop
)
colnames(mat_zinb) <- c("0", "1", "2", "3", "4", "5")


barplot(
  mat_zinb,
  beside = TRUE,
  col    = c("#4682B4", "#FFA500"),
  border = "black",
  ylim   = c(0, max(mat_zinb) * 1.12),
  xlab   = "Dias de ausência",
  ylab   = "Proporção ponderada",
  main   = "Distribuição observada versus prevista (ZINB)",
  legend = rownames(mat_zinb),
  args.legend = list(
    x = "topright",
    inset = c(-0.1, 0),
    cex = 0.5,
    bty = "n"
  )
)

legend(
  "topright",
  legend = rownames(mat_zinb),
  col    = c("#4682B4", "#FFA500"),
  pch    = 15,
  pt.cex = 2,
  bty    = "n",
  cex    = 1.05
)

### estimativas do pi
pi_hat <- predict(fit_zinb_full, type = "zero")
summary(pi_hat)
head(pi_hat)