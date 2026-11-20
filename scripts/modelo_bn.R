
dados <- readRDS(file.choose())

variaveis_modelo <- c(
  "dias_ausencia_inseguranca_escola",
  "bullying_sofrido",
  "exclusao_social",
  "agressao_fisica_colegas",
  "cyberbullying_sofrido",
  "apoio_familiar",
  "escolaridade_mae",
  "acao_prevencao_bullying",
  "sexo",
  "raca_cor",
  "faixa_etaria",
  "rede_ensino",
  "peso_amostral"
)

dados_modelo <- dados[
  complete.cases(dados[, variaveis_modelo]),
]

dados_modelo <- droplevels(dados_modelo)

cat("Número de observações:", nrow(dados_modelo), "\n")

categorias <- c(
  "0 dias",
  "1 dia",
  "2 dias",
  "3 dias",
  "4 dias",
  "5 dias ou mais"
)

dados_modelo$y <- match(
  as.character(dados_modelo$dias_ausencia_inseguranca_escola),
  categorias
) - 1

table(dados_modelo$y, useNA = "ifany")

stopifnot(
  !anyNA(dados_modelo$y),
  all(dados_modelo$y %in% 0:5)
)


variaveis_explicativas <- c(
  "bullying_sofrido",
  "exclusao_social",
  "agressao_fisica_colegas",
  "cyberbullying_sofrido",
  "apoio_familiar",
  "escolaridade_mae",
  "acao_prevencao_bullying",
  "sexo",
  "raca_cor",
  "faixa_etaria",
  "rede_ensino"
)

dados_modelo[variaveis_explicativas] <- lapply(
  dados_modelo[variaveis_explicativas],
  function(x) factor(x, ordered = FALSE)
)

formula_modelo <- as.formula(
  paste(
    "y ~",
    paste(variaveis_explicativas, collapse = " + ")
  )
)


# ============================================================
# Pesos amostrais
# ============================================================

dados_modelo$peso_normalizado <-
  dados_modelo$peso_amostral /
  mean(dados_modelo$peso_amostral)

peso <- dados_modelo$peso_normalizado

summary(peso)

mean(peso)

stopifnot(
  all(is.finite(peso)),
  all(peso > 0)
)


# ============================================================
# Análise descritiva
# ============================================================

y <- dados_modelo$y

barplot(
  table(factor(y, levels = 0:5)),
  names.arg = 0:5,
  main = "Frequência dos dias de ausência",
  xlab = "Número de dias de ausência",
  ylab = "Frequência",
  col = "lightblue",
  border = "white"
)

proporcao_y <- sapply(
  0:5,
  function(k) weighted.mean(y == k, w = peso)
)

barplot(
  proporcao_y,
  names.arg = 0:5,
  main = "Distribuição ponderada dos dias de ausência",
  xlab = "Número de dias de ausência",
  ylab = "Proporção ponderada",
  col = "steelblue",
  border = "white"
)

par(mfrow = c(2, 2))

for (variavel in variaveis_explicativas) {
  
  categorias_var <- levels(dados_modelo[[variavel]])
  
  proporcoes <- sapply(
    categorias_var,
    function(cat) {
      weighted.mean(
        dados_modelo[[variavel]] == cat,
        w = peso)})
  
  barplot(
    proporcoes,
    names.arg = categorias_var,
    las = 2,
    cex.names = 0.7,
    main = paste("Distribuição:", variavel),
    ylab = "Proporção ponderada",
    col = "lightblue",
    border = "white")}

par(mfrow = c(1, 1))


# ============================================================
# Ajuste do modelos Binomial Negativo _ NB
# ============================================================

library(MASS)

modelo_nb <- MASS::glm.nb(
  formula = formula_modelo,
  data = dados_modelo,
  weights = peso_normalizado
)

summary(modelo_nb)

set.seed(123)

mu <- fitted(modelo_nb)
theta <- modelo_nb$theta

limite_inferior <- pnbinom(y - 1,size = theta,mu = mu)

limite_superior <- pnbinom(y,size = theta,mu = mu)

u <- runif(
  length(y),
  min = limite_inferior,
  max = limite_superior)

u <- pmin(pmax(u, 1e-10), 1 - 1e-10)

residuos_q <- qnorm(u)

summary(residuos_q)

par(mfrow = c(2, 2))

hist(
  residuos_q,
  probability = TRUE,
  breaks = 40,
  main = "Histograma dos resíduos quantílicos",
  xlab = "Resíduos quantílicos",
  col = "lightblue",
  border = "white")

curve(dnorm(x),add = TRUE,col = "red",lwd = 2)

qqnorm(
  residuos_q,
  main = "QQ-plot dos resíduos quantílicos",
  pch = 16,
  cex = 0.5)

qqline(residuos_q,col = "red",lwd = 2)

plot(
  mu,
  residuos_q,
  main = "Resíduos versus valores ajustados",
  xlab = "Valores ajustados",
  ylab = "Resíduos quantílicos",
  pch = 16,
  cex = 0.4,
  col = rgb(0, 0, 0, 0.15))

abline(h = 0, col = "red", lwd = 2)

plot(modelo_nb,which = 2,main = "Diagnóstico do modelo NB")

par(mfrow = c(1, 1))

proporcao_zeros_observada <- weighted.mean(y == 0,w = peso)

prob_zero_nb <- dnbinom(0,size = theta,mu = mu)

proporcao_zeros_prevista <- weighted.mean(prob_zero_nb,w = peso)

diferenca_zeros <- proporcao_zeros_observada -proporcao_zeros_prevista

cat("Proporção observada de zeros:",proporcao_zeros_observada, "\n")

cat("Proporção prevista de zeros pela NB:",proporcao_zeros_prevista, "\n")

cat("Diferença:",diferenca_zeros, "\n")

barplot(
  c(proporcao_zeros_observada, proporcao_zeros_prevista),
  names.arg = c("Observado", "Previsto pela NB"),
  main = "Comparação das proporções de zeros",
  ylab = "Proporção ponderada",
  col = c("steelblue", "orange"),
  border = "white")

categorias_y <- 0:5

proporcao_observada <- sapply(categorias_y,
  function(k) weighted.mean(y == k, w = peso))

probabilidades_nb <- sapply(categorias_y,
  function(k) {dnbinom(k, size = theta, mu = mu)})

proporcao_prevista <- sapply(seq_along(categorias_y),
  function(j) {weighted.mean(probabilidades_nb[, j], w = peso)})

comparacao_nb <- data.frame(
  Dias = categorias_y,
  Observado = proporcao_observada,
  Previsto_NB = proporcao_prevista
)

comparacao_nb$Diferenca <-
  comparacao_nb$Observado - comparacao_nb$Previsto_NB

print(comparacao_nb)

barplot(
  rbind(
    comparacao_nb$Observado,
    comparacao_nb$Previsto_NB
  ),
  beside = TRUE,
  names.arg = comparacao_nb$Dias,
  col = c("steelblue", "orange"),
  main = "Distribuição observada versus prevista",
  xlab = "Dias de ausência",
  ylab = "Proporção ponderada"
)

legend(
  "topright",
  legend = c("Observado", "Previsto pela NB"),
  fill = c("steelblue", "orange"),
  bty = "n"
)

coeficientes_nb <- coef(modelo_nb)

ic_coeficientes <- confint.default(
  modelo_nb,
  level = 0.95
)

tabela_irr_ic <- data.frame(
  Variavel = names(coeficientes_nb),
  Coeficiente = coeficientes_nb,
  IRR = exp(coeficientes_nb),
  Limite_inferior = exp(ic_coeficientes[, 1]),
  Limite_superior = exp(ic_coeficientes[, 2])
)

print(tabela_irr_ic)

# Gráfico das IRRs, sem o intercepto
dados_irr <- subset(
  tabela_irr_ic,
  Variavel != "(Intercept)"
)

dados_irr <- dados_irr[order(dados_irr$IRR), ]

par(mar = c(5, 12, 4, 2))

plot(
  dados_irr$IRR,
  seq_len(nrow(dados_irr)),
  xlim = range(
    c(
      dados_irr$Limite_inferior,
      dados_irr$Limite_superior,
      1
    ),
    finite = TRUE
  ),
  ylim = c(0.5, nrow(dados_irr) + 0.5),
  yaxt = "n",
  pch = 19,
  col = "steelblue",
  xlab = "Razão de taxas (IRR)",
  ylab = "",
  main = "IRRs e intervalos de confiança de 95%"
)

axis(
  2,
  at = seq_len(nrow(dados_irr)),
  labels = dados_irr$Variavel,
  las = 2,
  cex.axis = 0.7
)

segments(
  dados_irr$Limite_inferior,
  seq_len(nrow(dados_irr)),
  dados_irr$Limite_superior,
  seq_len(nrow(dados_irr)),
  lwd = 2
)

abline(v = 1, col = "red", lty = 2, lwd = 2)

par(mar = c(5, 4, 4, 2))

log_verossimilhanca_nb <- as.numeric(logLik(modelo_nb))

numero_parametros_nb <- attr(
  logLik(modelo_nb),
  "df")

resultados_nb <- data.frame(
  Modelo = "Binomial Negativa",
  LogVerossimilhanca = log_verossimilhanca_nb,
  Parametros = numero_parametros_nb,
  Observacoes = nobs(modelo_nb),
  AIC = AIC(modelo_nb),
  BIC = BIC(modelo_nb))

print(resultados_nb)


cat("Theta estimado:", modelo_nb$theta, "\n")
cat("Erro padrão de theta:", modelo_nb$SE.theta, "\n")


write.csv(
  comparacao_nb,
  "comparacao_distribuicao_nb.csv",
  row.names = FALSE
)

write.csv(
  tabela_irr_ic,
  "irr_binomial_negativa_com_ic.csv",
  row.names = FALSE
)

write.csv(
  resultados_nb,
  "criterios_binomial_negativa.csv",
  row.names = FALSE
)

estimativas <- coef(summary(modelo_nb))
ic_beta <- confint.default(modelo_nb)

tabela_estimativas <- data.frame(
  Variavel = rownames(estimativas),
  Estimativa = estimativas[, "Estimate"],
  Erro_padrao = estimativas[, "Std. Error"],
  Valor_z = estimativas[, "z value"],
  Valor_p = estimativas[, "Pr(>|z|)"],
  IRR = exp(estimativas[, "Estimate"]),
  IC95_inferior = exp(ic_beta[, 1]),
  IC95_superior = exp(ic_beta[, 2]),
  row.names = NULL
)

print(tabela_estimativas, digits = 4)

library(gt)
library(dplyr)

tabela_visual <- tabela_estimativas %>%
  dplyr::mutate(
    `IC 95%` = sprintf(
      "%.3f – %.3f",
      IC95_inferior,
      IC95_superior
    ),
    Valor_p = ifelse(
      Valor_p < 0.001,
      "< 0.001",
      sprintf("%.3f", Valor_p)
    )
  ) %>%
  dplyr::select(
    Variavel,
    Estimativa,
    Erro_padrao,
    Valor_z,
    Valor_p,
    IRR,
    `IC 95%`
  ) %>%
  gt::gt() %>%
  gt::cols_label(
    Variavel = "Variável",
    Estimativa = "Coeficiente",
    Erro_padrao = "Erro-padrão",
    Valor_z = "Estatística Z",
    Valor_p = "Valor-p",
    IRR = "IRR",
    `IC 95%` = "IC de 95%"
  ) %>%
  gt::fmt_number(
    columns = c(
      Estimativa,
      Erro_padrao,
      Valor_z,
      IRR
    ),
    decimals = 3
  ) %>%
  gt::tab_header(
    title = "Estimativas do modelo binomial negativo",
    subtitle = "Coeficientes, razões entre médias e intervalos de confiança"
  ) %>%
  gt::tab_options(
    table.font.size = 12,
    heading.title.font.size = 16
  )

tabela_visual

library(dplyr)
library(ggplot2)

calcular_probabilidades_nb <- function(variavel) {
  
  categorias <- unique(as.character(dados_modelo[[variavel]]))
  
  resultados <- lapply(categorias, function(categoria) {
    
    novos_dados <- dados_modelo
    
    novos_dados[[variavel]] <- factor(categoria,
      levels = levels(dados_modelo[[variavel]]))
    
    mu <- predict(
      modelo_nb,
      newdata = novos_dados,
      type = "response")
    
    probabilidades <- sapply(0:5, function(dias) {
      
      weighted.mean(
        dnbinom(
          dias,
          size = modelo_nb$theta,
          mu = mu),
        w = novos_dados$peso_normalizado)})
    
    
    prob_maior_5 <- weighted.mean(
      pnbinom(
        5,
        size = modelo_nb$theta,
        mu = mu,
        lower.tail = FALSE
      ),
      w = novos_dados$peso_normalizado
    )
    
    data.frame(
      Variavel = variavel,
      Categoria = categoria,
      Dias = c("0", "1", "2", "3", "4", "5", "Acima de 5"),
      Probabilidade = c(probabilidades, prob_maior_5))})
  
  do.call(rbind, resultados)}

probabilidades_nb <- do.call(rbind,
  lapply(variaveis_explicativas, calcular_probabilidades_nb))

print(probabilidades_nb, digits = 4)

estimativas <- coef(summary(modelo_nb))

ic_beta <- confint.default(modelo_nb)

tabela_nb_completa <- data.frame(
  Coeficiente = rownames(estimativas),
  Estimativa = estimativas[, "Estimate"],
  Erro_padrao = estimativas[, "Std. Error"],
  Valor_p = estimativas[, "Pr(>|z|)"],
  IRR = exp(estimativas[, "Estimate"]),
  IC95_inferior = exp(ic_beta[, 1]),
  IC95_superior = exp(ic_beta[, 2]),
  row.names = NULL)

tabela_nb_completa <- tabela_nb_completa %>%
  mutate(
    Interpretacao = case_when(
      IRR > 1 ~ "Maior média esperada",
      IRR < 1 ~ "Menor média esperada",
      TRUE ~ "Mesma média esperada"))

class(tabela_nb_completa)
head(tabela_nb_completa)

base::print(
  tabela_nb_completa,
  row.names = FALSE)
View(tabela_nb_completa)

install.packages("hnp")
library(hnp)

set.seed(123)

n_sim <- 20
n_obs <- nrow(dados_modelo)

sim_residuos <- matrix(NA, nrow = n_obs, ncol = n_sim)

mu_hat <- fitted(modelo_nb)
theta_hat <- modelo_nb$theta

for (s in 1:n_sim) {
  dados_sim <- dados_modelo
  dados_sim$y_sim <- rnbinom(n_obs, size = theta_hat, mu = mu_hat)
  

  mod_sim <- MASS::glm.nb(
    formula = update(formula_modelo, y_sim ~ .),
    data = dados_sim,
    weights = peso_normalizado
  )
  
  mu_s <- fitted(mod_sim)
  th_s <- mod_sim$theta
  
  u_s <- runif(
    n_obs,
    min = pnbinom(dados_sim$y_sim - 1, size = th_s, mu = mu_s),
    max = pnbinom(dados_sim$y_sim, size = th_s, mu = mu_s)
  )
  u_s <- pmin(pmax(u_s, 1e-10), 1 - 1e-10)
  
  sim_residuos[, s] <- sort(qnorm(u_s))}

# Calcula limites do envelope (mínimo, mediana e máximo das 20 simulações)
env_inf <- apply(sim_residuos, 1, min)
env_sup <- apply(sim_residuos, 1, max)
env_med <- apply(sim_residuos, 1, median)
quantis_teoricos <- qnorm(ppoints(n_obs))

ord <- order(residuos_q)
res_ord <- residuos_q[ord]

plot(
  quantis_teoricos, res_ord,
  type = "n",
  main = "QQ-plot - Resíduos Quantílicos (NB)",
  xlab = "norm quantiles",
  ylab = "Resíduos Quantílicos Aleatorizados",
  xlim = range(quantis_teoricos) * 1.05,
  ylim = range(c(res_ord, env_inf, env_sup)) * 1.05
)

grid(col = "gray88", lty = 1)

polygon(
  c(quantis_teoricos, rev(quantis_teoricos)),
  c(env_inf, rev(env_sup)),
  col = rgb(0, 0, 1, alpha = 0.18),
  border = rgb(0, 0, 1, alpha = 0.45))

lines(quantis_teoricos, env_med, col = "blue", lwd = 1.5)
points(quantis_teoricos, res_ord, pch = 16, cex = 0.5, col = "black")

extremos <- order(abs(res_ord - env_med), decreasing = TRUE)[1:2]
text(
  quantis_teoricos[extremos],
  res_ord[extremos],
  labels = ord[extremos],
  pos = ifelse(quantis_teoricos[extremos] > 0, 2, 4),
  cex = 0.75,
  col = "gray20")