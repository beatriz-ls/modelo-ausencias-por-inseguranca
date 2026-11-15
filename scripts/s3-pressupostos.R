# Script 3: Avaliando adequações dos modelos

library(tidyverse)

################################################################################
# 1. BINOMIAL NEGATIVA CENSURADA
################################################################################

# Carregar os dados tratados
dados <- readRDS("data/dados_pense2024_tratados.rds")

# Criar pasta dos resultados
dir.create(
  "modelos/nb_censurada",
  recursive = TRUE,
  showWarnings = FALSE
)

pasta_nb <- "modelos/nb_censurada"

# Cor fixa da variável resposta
cor_ausencia <- "#31688E"

# Preparação da resposta e dos pesos
dados_nb <- dados |>
  filter(
    !is.na(ausencia_limite_inferior),
    !is.na(peso_amostral),
    peso_amostral > 0
  ) |>
  mutate(
    y = ausencia_limite_inferior,
    peso_normalizado = peso_amostral / mean(peso_amostral)
  )

# Verificar o tamanho da amostra utilizada
resumo_amostra_nb <- dados_nb |>
  summarise(
    n = n(),
    n_escolas = n_distinct(id_escola),
    n_estratos = n_distinct(estrato),
    peso_minimo = min(peso_amostral),
    peso_maximo = max(peso_amostral),
    soma_pesos = sum(peso_amostral)
  )

print(resumo_amostra_nb)

# Distribuição observada, ponderada e não ponderada
distribuicao_nb <- dados_nb |>
  group_by(y) |>
  summarise(
    n = n(),
    soma_pesos = sum(peso_amostral),
    .groups = "drop"
  ) |>
  arrange(y) |>
  mutate(
    categoria = factor(
      y,
      levels = 0:5,
      labels = c("0", "1", "2", "3", "4", "5+")
    ),
    observado_nao_ponderado = n / sum(n),
    observado_ponderado = soma_pesos / sum(soma_pesos)
  )

print(distribuicao_nb)

# Diagnóstico inicial da concentração de zeros
resumo_zeros_nb <- dados_nb |>
  summarise(
    percentual_zeros = 100 * weighted.mean(
      y == 0, peso_amostral
    ),
    percentual_positivos = 100 * weighted.mean(
      y > 0, peso_amostral
    ),
    percentual_censurados = 100 * weighted.mean(
      y == 5, peso_amostral
    ),
    percentual_censurados_positivos =
      100 * sum(peso_amostral[y == 5]) /
      sum(peso_amostral[y > 0])
  )

print(resumo_zeros_nb)

# Função de pseudolog-verossimilhança ponderada
log_verossimilhanca_nb <- function(parametros) {
  
  mu <- exp(parametros[1])
  theta <- exp(parametros[2])
  
  log_prob <- ifelse(
    dados_nb$y == 5,
    pnbinom(
      4,
      mu = mu,
      size = theta,
      lower.tail = FALSE,
      log.p = TRUE
    ),
    dnbinom(
      pmin(dados_nb$y, 4),
      mu = mu,
      size = theta,
      log = TRUE
    )
  )
  
  -sum(dados_nb$peso_normalizado * log_prob)
}

# Ajustar a NB censurada ponderada
ajuste_nb <- optim(
  par = log(c(0.4, 0.1)),
  fn = log_verossimilhanca_nb,
  method = "L-BFGS-B",
  lower = log(c(1e-5, 1e-5)),
  upper = log(c(30, 1e5))
)

# Estimativas dos parâmetros
mu_nb <- exp(ajuste_nb$par[1])
theta_nb <- exp(ajuste_nb$par[2])

estimativas_nb <- tibble(
  parametro = c("mu", "theta"),
  estimativa = c(mu_nb, theta_nb)
)

print(estimativas_nb)

cat("Convergência:", ajuste_nb$convergence, "\n")

# Dispersão estimada sob a NB
diagnostico_dispersao_nb <- tibble(
  media_latente = mu_nb,
  variancia_latente = mu_nb + mu_nb^2 / theta_nb,
  razao_variancia_media = 1 + mu_nb / theta_nb,
  parametro_dispersao = theta_nb
)

print(diagnostico_dispersao_nb)

# Probabilidades previstas para as seis categorias
probabilidades_nb <- c(
  dnbinom(
    0:4,
    mu = mu_nb,
    size = theta_nb
  ),
  pnbinom(
    4,
    mu = mu_nb,
    size = theta_nb,
    lower.tail = FALSE
  )
)

# Comparação entre observado e esperado
diagnostico_nb <- distribuicao_nb |>
  mutate(
    esperado = probabilidades_nb,
    diferenca_pp =
      100 * (observado_ponderado - esperado),
    frequencia_esperada =
      sum(soma_pesos) * esperado,
    diferenca_relativa =
      100 * (observado_ponderado - esperado) / esperado
  )

print(diagnostico_nb)

# Diferenças absolutas das probabilidades
resumo_adequacao_nb <- diagnostico_nb |>
  summarise(
    erro_absoluto_maximo_pp =
      max(abs(diferenca_pp)),
    erro_absoluto_medio_pp =
      mean(abs(diferenca_pp)),
    erro_absoluto_total_pp =
      sum(abs(diferenca_pp))
  )

print(resumo_adequacao_nb)

# Gráfico: distribuição observada e esperada
grafico_nb <- diagnostico_nb |>
  select(
    categoria,
    observado_ponderado,
    esperado
  ) |>
  pivot_longer(
    cols = c(observado_ponderado, esperado),
    names_to = "tipo",
    values_to = "probabilidade"
  ) |>
  mutate(
    tipo = factor(
      tipo,
      levels = c("observado_ponderado", "esperado"),
      labels = c("Observado ponderado", "NB censurada")
    )
  ) |>
  ggplot(
    aes(
      x = categoria,
      y = probabilidade,
      fill = tipo
    )
  ) +
  geom_col(
    position = "dodge",
    width = 0.7
  ) +
  scale_fill_manual(
    values = c(
      "Observado ponderado" = cor_ausencia,
      "NB censurada" = "#35B779"
    )
  ) +
  scale_y_continuous(
    labels = scales::percent_format()
  ) +
  labs(
    title = "Binomial negativa censurada",
    subtitle = "Distribuição observada ponderada e prevista",
    x = "Dias de ausência",
    y = "Percentual de estudantes",
    fill = NULL
  ) +
  theme_minimal()

print(grafico_nb)

# Gráfico: diferenças em pontos percentuais
grafico_diferencas_nb <- diagnostico_nb |>
  ggplot(
    aes(
      x = categoria,
      y = diferenca_pp
    )
  ) +
  geom_col(
    fill = cor_ausencia,
    width = 0.7
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  labs(
    title = "Discrepâncias da binomial negativa censurada",
    subtitle = "Observado ponderado menos previsto",
    x = "Dias de ausência",
    y = "Diferença em pontos percentuais"
  ) +
  theme_minimal()

print(grafico_diferencas_nb)

# Estabilidade numérica: diferentes valores iniciais
ajuste_nb_inicio_1 <- optim(
  par = log(c(0.2, 0.05)),
  fn = log_verossimilhanca_nb,
  method = "L-BFGS-B",
  lower = log(c(1e-5, 1e-5)),
  upper = log(c(30, 1e5))
)

ajuste_nb_inicio_2 <- optim(
  par = log(c(1, 1)),
  fn = log_verossimilhanca_nb,
  method = "L-BFGS-B",
  lower = log(c(1e-5, 1e-5)),
  upper = log(c(30, 1e5))
)

estabilidade_nb <- tibble(
  inicializacao = c("Principal", "Alternativa 1", "Alternativa 2"),
  convergencia = c(
    ajuste_nb$convergence,
    ajuste_nb_inicio_1$convergence,
    ajuste_nb_inicio_2$convergence
  ),
  mu = c(
    exp(ajuste_nb$par[1]),
    exp(ajuste_nb_inicio_1$par[1]),
    exp(ajuste_nb_inicio_2$par[1])
  ),
  theta = c(
    exp(ajuste_nb$par[2]),
    exp(ajuste_nb_inicio_1$par[2]),
    exp(ajuste_nb_inicio_2$par[2])
  ),
  objetivo = c(
    ajuste_nb$value,
    ajuste_nb_inicio_1$value,
    ajuste_nb_inicio_2$value
  )
)

print(estabilidade_nb)

# Salvar gráficos
ggsave(
  file.path(pasta_nb, "01_distribuicao_nb.png"),
  grafico_nb,
  width = 9,
  height = 5,
  dpi = 300
)

ggsave(
  file.path(pasta_nb, "02_diferencas_nb.png"),
  grafico_diferencas_nb,
  width = 9,
  height = 5,
  dpi = 300
)

# Salvar tabelas
write_csv(
  distribuicao_nb,
  file.path(pasta_nb, "distribuicao_nb.csv")
)

write_csv(
  resumo_zeros_nb,
  file.path(pasta_nb, "resumo_zeros_nb.csv")
)

write_csv(
  estimativas_nb,
  file.path(pasta_nb, "estimativas_nb.csv")
)

write_csv(
  diagnostico_dispersao_nb,
  file.path(pasta_nb, "diagnostico_dispersao_nb.csv")
)

write_csv(
  diagnostico_nb,
  file.path(pasta_nb, "diagnostico_nb.csv")
)

write_csv(
  resumo_adequacao_nb,
  file.path(pasta_nb, "resumo_adequacao_nb.csv")
)

write_csv(
  estabilidade_nb,
  file.path(pasta_nb, "estabilidade_nb.csv")
)

# Salvar objetos do ajuste
saveRDS(
  ajuste_nb,
  file.path(pasta_nb, "ajuste_nb.rds")
)

################################################################################
# 2. HURDLE POISSON CENSURADA
################################################################################

# Criar pasta dos resultados
dir.create(
  "modelos/hurdle_poisson",
  recursive = TRUE,
  showWarnings = FALSE
)

pasta_hp <- "modelos/hurdle_poisson"

# Utilizar a mesma amostra da NB censurada
dados_hp <- dados_nb |>
  select(
    y,
    peso_amostral,
    peso_normalizado
  )

# Probabilidade ponderada de alguma ausência
pi_hp <- weighted.mean(
  dados_hp$y > 0,
  dados_hp$peso_amostral
)

# Frequências ponderadas das categorias positivas
frequencias_hp <- dados_hp |>
  filter(y > 0) |>
  group_by(y) |>
  summarise(
    peso = sum(peso_normalizado),
    .groups = "drop"
  ) |>
  arrange(y)

# Pseudolog-verossimilhança da Poisson truncada em zero
log_verossimilhanca_hp <- function(parametro) {
  
  lambda <- exp(parametro)
  
  log_prob <- c(
    dpois(
      1:4,
      lambda = lambda,
      log = TRUE
    ),
    ppois(
      4,
      lambda = lambda,
      lower.tail = FALSE,
      log.p = TRUE
    )
  )
  
  # Condicionar a distribuição aos valores positivos
  log_prob <- log_prob - log(-expm1(-lambda))
  
  -sum(frequencias_hp$peso * log_prob)
}

# Ajuste do componente positivo
ajuste_hp <- optim(
  par = log(2),
  fn = log_verossimilhanca_hp,
  method = "Brent",
  lower = log(1e-4),
  upper = log(100)
)

lambda_hp <- exp(ajuste_hp$par)

# Estimativas
estimativas_hp <- tibble(
  parametro = c("pi", "lambda"),
  estimativa = c(pi_hp, lambda_hp)
)

print(estimativas_hp)

cat("Convergência:", ajuste_hp$convergence, "\n")

# Probabilidades previstas para as seis categorias
probabilidades_hp <- c(
  1 - pi_hp,
  pi_hp * dpois(
    1:4,
    lambda = lambda_hp
  ) / (-expm1(-lambda_hp)),
  pi_hp * ppois(
    4,
    lambda = lambda_hp,
    lower.tail = FALSE
  ) / (-expm1(-lambda_hp))
)

# Comparação com as proporções observadas
diagnostico_hp <- distribuicao_nb |>
  mutate(
    previsto = probabilidades_hp,
    observado = observado_ponderado,
    diferenca_pp = 100 * (observado - previsto),
    diferenca_relativa = 100 * (observado - previsto) / previsto
  ) |>
  select(
    categoria,
    n,
    observado,
    previsto,
    diferenca_pp,
    diferenca_relativa
  )

print(diagnostico_hp, n = Inf)

# Medidas descritivas de adequação
resumo_adequacao_hp <- diagnostico_hp |>
  summarise(
    erro_maximo_pp = max(abs(diferenca_pp)),
    erro_medio_pp = mean(abs(diferenca_pp)),
    erro_total_pp = sum(abs(diferenca_pp))
  )

print(resumo_adequacao_hp)

# Comparação inicial com a NB censurada
comparacao_nb_hp <- tibble(
  modelo = c(
    "NB censurada",
    "Hurdle Poisson censurada"
  ),
  erro_maximo_pp = c(
    max(abs(diagnostico_nb$diferenca_pp)),
    max(abs(diagnostico_hp$diferenca_pp))
  ),
  erro_medio_pp = c(
    mean(abs(diagnostico_nb$diferenca_pp)),
    mean(abs(diagnostico_hp$diferenca_pp))
  ),
  erro_total_pp = c(
    sum(abs(diagnostico_nb$diferenca_pp)),
    sum(abs(diagnostico_hp$diferenca_pp))
  )
)

print(comparacao_nb_hp)

# Gráfico de distribuição observada e prevista
grafico_hp <- diagnostico_hp |>
  select(categoria, observado, previsto) |>
  pivot_longer(
    cols = c(observado, previsto),
    names_to = "tipo",
    values_to = "probabilidade"
  ) |>
  ggplot(
    aes(
      x = categoria,
      y = probabilidade,
      fill = tipo
    )
  ) +
  geom_col(
    position = "dodge",
    width = 0.7
  ) +
  scale_fill_manual(
    values = c(
      observado = "#31688E",
      previsto = "#35B779"
    ),
    labels = c(
      observado = "Observado ponderado",
      previsto = "Hurdle Poisson"
    )
  ) +
  scale_y_continuous(
    labels = scales::percent_format()
  ) +
  labs(
    title = "Hurdle Poisson censurada",
    subtitle = "Distribuição observada ponderada e prevista",
    x = "Dias de ausência",
    y = "Proporção",
    fill = NULL
  ) +
  theme_minimal()

print(grafico_hp)

# Gráfico das discrepâncias
grafico_diferencas_hp <- diagnostico_hp |>
  ggplot(
    aes(
      x = categoria,
      y = diferenca_pp
    )
  ) +
  geom_col(
    fill = "#31688E",
    width = 0.7
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  labs(
    title = "Discrepâncias do Hurdle Poisson",
    subtitle = "Observado ponderado menos previsto",
    x = "Dias de ausência",
    y = "Diferença em pontos percentuais"
  ) +
  theme_minimal()

print(grafico_diferencas_hp)

# Comparar apenas as categorias positivas
diagnostico_positivos_hp <- diagnostico_hp |>
  filter(categoria != "0") |>
  mutate(
    observado_condicional =
      observado / sum(observado),
    previsto_condicional =
      previsto / sum(previsto),
    diferenca_condicional_pp =
      100 * (observado_condicional - previsto_condicional)
  )

print(diagnostico_positivos_hp, n = Inf)

# Salvar gráficos
ggsave(
  file.path(pasta_hp, "01_distribuicao_hp.png"),
  grafico_hp,
  width = 9,
  height = 5,
  dpi = 300
)

ggsave(
  file.path(pasta_hp, "02_diferencas_hp.png"),
  grafico_diferencas_hp,
  width = 9,
  height = 5,
  dpi = 300
)

# Salvar tabelas
write_csv(
  estimativas_hp,
  file.path(pasta_hp, "estimativas_hp.csv")
)

write_csv(
  diagnostico_hp,
  file.path(pasta_hp, "diagnostico_hp.csv")
)

write_csv(
  resumo_adequacao_hp,
  file.path(pasta_hp, "resumo_adequacao_hp.csv")
)

write_csv(
  diagnostico_positivos_hp,
  file.path(pasta_hp, "diagnostico_positivos_hp.csv")
)

write_csv(
  comparacao_nb_hp,
  file.path(pasta_hp, "comparacao_nb_hp.csv")
)

# Salvar o ajuste
saveRDS(
  ajuste_hp,
  file.path(pasta_hp, "ajuste_hp.rds")
)

################################################################################
# 3. ZINB CENSURADA
################################################################################

# Criar pasta dos resultados
dir.create(
  "modelos/zinb_censurada",
  recursive = TRUE,
  showWarnings = FALSE
)

pasta_zinb <- "modelos/zinb_censurada"

# Frequências ponderadas normalizadas
frequencias_zinb <- dados_nb |>
  group_by(y) |>
  summarise(
    peso = sum(peso_normalizado),
    .groups = "drop"
  ) |>
  arrange(y)

# Função de pseudolog-verossimilhança
log_verossimilhanca_zinb <- function(parametros) {
  
  mu <- exp(parametros[1])
  theta <- exp(parametros[2])
  omega <- plogis(parametros[3])
  
  prob_nb <- c(
    dnbinom(
      0:4,
      mu = mu,
      size = theta
    ),
    pnbinom(
      4,
      mu = mu,
      size = theta,
      lower.tail = FALSE
    )
  )
  
  probabilidades <- c(
    omega + (1 - omega) * prob_nb[1],
    (1 - omega) * prob_nb[2:6]
  )
  
  -sum(
    frequencias_zinb$peso *
      log(pmax(probabilidades, 1e-300))
  )
}

# Ajuste principal
ajuste_zinb <- optim(
  par = c(
    log(mu_nb),
    log(theta_nb),
    qlogis(0.05)
  ),
  fn = log_verossimilhanca_zinb,
  method = "L-BFGS-B",
  lower = c(
    log(1e-5),
    log(1e-5),
    -20
  ),
  upper = c(
    log(30),
    log(1e5),
    10
  ),
  control = list(maxit = 1000)
)

# Recuperar estimativas
mu_zinb <- exp(ajuste_zinb$par[1])
theta_zinb <- exp(ajuste_zinb$par[2])
omega_zinb <- plogis(ajuste_zinb$par[3])

estimativas_zinb <- tibble(
  parametro = c("mu", "theta", "omega"),
  estimativa = c(mu_zinb, theta_zinb, omega_zinb)
)

print(estimativas_zinb)

cat("Convergência:", ajuste_zinb$convergence, "\n")
cat("Função objetivo:", ajuste_zinb$value, "\n")

# Probabilidade de zero por componente
resumo_zeros_zinb <- tibble(
  componente = c(
    "Zeros adicionais",
    "Zeros da binomial negativa",
    "Zeros totais previstos",
    "Zeros observados ponderados"
  ),
  probabilidade = c(
    omega_zinb,
    (1 - omega_zinb) * dnbinom(
      0,
      mu = mu_zinb,
      size = theta_zinb
    ),
    omega_zinb +
      (1 - omega_zinb) * dnbinom(
        0,
        mu = mu_zinb,
        size = theta_zinb
      ),
    weighted.mean(
      dados_nb$y == 0,
      dados_nb$peso_amostral
    )
  )
)

print(resumo_zeros_zinb)

# Probabilidades previstas
probabilidades_zinb <- c(
  omega_zinb +
    (1 - omega_zinb) * dnbinom(
      0,
      mu = mu_zinb,
      size = theta_zinb
    ),
  
  (1 - omega_zinb) * dnbinom(
    1:4,
    mu = mu_zinb,
    size = theta_zinb
  ),
  
  (1 - omega_zinb) * pnbinom(
    4,
    mu = mu_zinb,
    size = theta_zinb,
    lower.tail = FALSE
  )
)

cat(
  "Soma das probabilidades:",
  sum(probabilidades_zinb),
  "\n"
)

# Distribuição observada versus prevista
diagnostico_zinb <- distribuicao_nb |>
  mutate(
    previsto = probabilidades_zinb,
    observado = observado_ponderado,
    diferenca_pp = 100 * (observado - previsto),
    diferenca_relativa =
      100 * (observado - previsto) / previsto
  ) |>
  select(
    categoria,
    n,
    observado,
    previsto,
    diferenca_pp,
    diferenca_relativa
  )

print(diagnostico_zinb, n = Inf)

# Resumo de adequação
resumo_adequacao_zinb <- diagnostico_zinb |>
  summarise(
    erro_maximo_pp = max(abs(diferenca_pp)),
    erro_medio_pp = mean(abs(diferenca_pp)),
    erro_total_pp = sum(abs(diferenca_pp))
  )

print(resumo_adequacao_zinb)

# Verificar estabilidade com diferentes inicializações
ajuste_zinb_inicio_1 <- optim(
  par = c(log(0.5), log(0.1), qlogis(0.01)),
  fn = log_verossimilhanca_zinb,
  method = "L-BFGS-B",
  lower = c(log(1e-5), log(1e-5), -20),
  upper = c(log(30), log(1e5), 10),
  control = list(maxit = 1000)
)

ajuste_zinb_inicio_2 <- optim(
  par = c(log(1), log(0.5), qlogis(0.30)),
  fn = log_verossimilhanca_zinb,
  method = "L-BFGS-B",
  lower = c(log(1e-5), log(1e-5), -20),
  upper = c(log(30), log(1e5), 10),
  control = list(maxit = 1000)
)

estabilidade_zinb <- tibble(
  inicializacao = c(
    "Principal",
    "Alternativa 1",
    "Alternativa 2"
  ),
  convergencia = c(
    ajuste_zinb$convergence,
    ajuste_zinb_inicio_1$convergence,
    ajuste_zinb_inicio_2$convergence
  ),
  mu = exp(c(
    ajuste_zinb$par[1],
    ajuste_zinb_inicio_1$par[1],
    ajuste_zinb_inicio_2$par[1]
  )),
  theta = exp(c(
    ajuste_zinb$par[2],
    ajuste_zinb_inicio_1$par[2],
    ajuste_zinb_inicio_2$par[2]
  )),
  omega = plogis(c(
    ajuste_zinb$par[3],
    ajuste_zinb_inicio_1$par[3],
    ajuste_zinb_inicio_2$par[3]
  )),
  objetivo = c(
    ajuste_zinb$value,
    ajuste_zinb_inicio_1$value,
    ajuste_zinb_inicio_2$value
  )
)

print(estabilidade_zinb)

# Comparação com os modelos anteriores
comparacao_modelos <- tibble(
  modelo = c(
    "NB censurada",
    "Hurdle Poisson censurada",
    "ZINB censurada"
  ),
  parametros = c(2, 2, 3),
  erro_maximo_pp = c(
    max(abs(diagnostico_nb$diferenca_pp)),
    max(abs(diagnostico_hp$diferenca_pp)),
    max(abs(diagnostico_zinb$diferenca_pp))
  ),
  erro_medio_pp = c(
    mean(abs(diagnostico_nb$diferenca_pp)),
    mean(abs(diagnostico_hp$diferenca_pp)),
    mean(abs(diagnostico_zinb$diferenca_pp))
  ),
  erro_total_pp = c(
    sum(abs(diagnostico_nb$diferenca_pp)),
    sum(abs(diagnostico_hp$diferenca_pp)),
    sum(abs(diagnostico_zinb$diferenca_pp))
  )
)

print(comparacao_modelos)

# Gráfico observado versus previsto
grafico_zinb <- diagnostico_zinb |>
  select(categoria, observado, previsto) |>
  pivot_longer(
    cols = c(observado, previsto),
    names_to = "tipo",
    values_to = "probabilidade"
  ) |>
  ggplot(
    aes(
      x = categoria,
      y = probabilidade,
      fill = tipo
    )
  ) +
  geom_col(
    position = "dodge",
    width = 0.7
  ) +
  scale_fill_manual(
    values = c(
      observado = "#31688E",
      previsto = "#35B779"
    ),
    labels = c(
      observado = "Observado ponderado",
      previsto = "ZINB censurada"
    )
  ) +
  scale_y_continuous(
    labels = scales::percent_format()
  ) +
  labs(
    title = "ZINB censurada",
    subtitle = "Distribuição observada ponderada e prevista",
    x = "Dias de ausência",
    y = "Proporção",
    fill = NULL
  ) +
  theme_minimal()

print(grafico_zinb)

# Gráfico das discrepâncias
grafico_diferencas_zinb <- diagnostico_zinb |>
  ggplot(
    aes(
      x = categoria,
      y = diferenca_pp
    )
  ) +
  geom_col(
    fill = "#31688E",
    width = 0.7
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  labs(
    title = "Discrepâncias da ZINB censurada",
    subtitle = "Observado ponderado menos previsto",
    x = "Dias de ausência",
    y = "Diferença em pontos percentuais"
  ) +
  theme_minimal()

print(grafico_diferencas_zinb)

# Salvar gráficos
ggsave(
  file.path(pasta_zinb, "01_distribuicao_zinb.png"),
  grafico_zinb,
  width = 9,
  height = 5,
  dpi = 300
)

ggsave(
  file.path(pasta_zinb, "02_diferencas_zinb.png"),
  grafico_diferencas_zinb,
  width = 9,
  height = 5,
  dpi = 300
)

# Salvar tabelas
write_csv(
  estimativas_zinb,
  file.path(pasta_zinb, "estimativas_zinb.csv")
)

write_csv(
  resumo_zeros_zinb,
  file.path(pasta_zinb, "resumo_zeros_zinb.csv")
)

write_csv(
  diagnostico_zinb,
  file.path(pasta_zinb, "diagnostico_zinb.csv")
)

write_csv(
  resumo_adequacao_zinb,
  file.path(pasta_zinb, "resumo_adequacao_zinb.csv")
)

write_csv(
  estabilidade_zinb,
  file.path(pasta_zinb, "estabilidade_zinb.csv")
)

write_csv(
  comparacao_modelos,
  file.path(pasta_zinb, "comparacao_modelos.csv")
)

# Salvar ajuste
saveRDS(
  ajuste_zinb,
  file.path(pasta_zinb, "ajuste_zinb.rds")
)

################################################################################
# 4. MISTURA DE DUAS POISSON CENSURADAS
################################################################################

# Criar pasta dos resultados
dir.create(
  "modelos/mistura_poisson",
  recursive = TRUE,
  showWarnings = FALSE
)

pasta_mp <- "modelos/mistura_poisson"

# Frequências ponderadas normalizadas
frequencias_mp <- dados_nb |>
  group_by(y) |>
  summarise(
    peso = sum(peso_normalizado),
    .groups = "drop"
  ) |>
  arrange(y)

# Função de pseudolog-verossimilhança
log_verossimilhanca_mp <- function(parametros) {
  
  lambda1 <- exp(parametros[1])
  lambda2 <- lambda1 + exp(parametros[2])
  pi <- plogis(parametros[3])
  
  prob1 <- c(
    dpois(0:4, lambda = lambda1),
    ppois(
      4,
      lambda = lambda1,
      lower.tail = FALSE
    )
  )
  
  prob2 <- c(
    dpois(0:4, lambda = lambda2),
    ppois(
      4,
      lambda = lambda2,
      lower.tail = FALSE
    )
  )
  
  probabilidades <- pi * prob1 + (1 - pi) * prob2
  
  -sum(
    frequencias_mp$peso *
      log(pmax(probabilidades, 1e-300))
  )
}

# Ajuste principal
ajuste_mp <- optim(
  par = c(
    log(0.05),
    log(3),
    qlogis(0.85)
  ),
  fn = log_verossimilhanca_mp,
  method = "L-BFGS-B",
  lower = c(log(1e-5), log(1e-5), -20),
  upper = c(log(30), log(30), 20),
  control = list(maxit = 2000)
)

# Ajustes com diferentes valores iniciais
ajuste_mp_inicio_1 <- optim(
  par = c(
    log(0.2),
    log(5),
    qlogis(0.70)
  ),
  fn = log_verossimilhanca_mp,
  method = "L-BFGS-B",
  lower = c(log(1e-5), log(1e-5), -20),
  upper = c(log(30), log(30), 20),
  control = list(maxit = 2000)
)

ajuste_mp_inicio_2 <- optim(
  par = c(
    log(0.01),
    log(8),
    qlogis(0.95)
  ),
  fn = log_verossimilhanca_mp,
  method = "L-BFGS-B",
  lower = c(log(1e-5), log(1e-5), -20),
  upper = c(log(30), log(30), 20),
  control = list(maxit = 2000)
)

# Recuperar parâmetros dos três ajustes
estabilidade_mp <- tibble(
  inicializacao = c(
    "Principal",
    "Alternativa 1",
    "Alternativa 2"
  ),
  convergencia = c(
    ajuste_mp$convergence,
    ajuste_mp_inicio_1$convergence,
    ajuste_mp_inicio_2$convergence
  ),
  lambda1 = exp(c(
    ajuste_mp$par[1],
    ajuste_mp_inicio_1$par[1],
    ajuste_mp_inicio_2$par[1]
  )),
  lambda2 = c(
    exp(ajuste_mp$par[1]) + exp(ajuste_mp$par[2]),
    exp(ajuste_mp_inicio_1$par[1]) +
      exp(ajuste_mp_inicio_1$par[2]),
    exp(ajuste_mp_inicio_2$par[1]) +
      exp(ajuste_mp_inicio_2$par[2])
  ),
  pi = plogis(c(
    ajuste_mp$par[3],
    ajuste_mp_inicio_1$par[3],
    ajuste_mp_inicio_2$par[3]
  )),
  objetivo = c(
    ajuste_mp$value,
    ajuste_mp_inicio_1$value,
    ajuste_mp_inicio_2$value
  )
)

print(estabilidade_mp, n = Inf)

# Selecionar o melhor ajuste convergido
melhor_inicio_mp <- estabilidade_mp |>
  mutate(indice = row_number()) |>
  filter(convergencia == 0) |>
  slice_min(objetivo, n = 1, with_ties = FALSE)

if (nrow(melhor_inicio_mp) == 0) {
  stop("Nenhum ajuste Mix-Poisson convergiu.")
}

indice_melhor_mp <- melhor_inicio_mp$indice

ajuste_mp_final <- switch(
  indice_melhor_mp,
  ajuste_mp,
  ajuste_mp_inicio_1,
  ajuste_mp_inicio_2
)

# Estimativas finais
lambda1_mp <- exp(ajuste_mp_final$par[1])

lambda2_mp <- lambda1_mp +
  exp(ajuste_mp_final$par[2])

pi_mp <- plogis(ajuste_mp_final$par[3])

estimativas_mp <- tibble(
  parametro = c("lambda1", "lambda2", "pi"),
  estimativa = c(lambda1_mp, lambda2_mp, pi_mp)
)

print(estimativas_mp)

cat(
  "Melhor inicialização:",
  melhor_inicio_mp$inicializacao,
  "\n"
)

cat(
  "Convergência:",
  ajuste_mp_final$convergence,
  "\n"
)

cat(
  "Função objetivo:",
  ajuste_mp_final$value,
  "\n"
)

# Probabilidades previstas por cada componente
prob_componente1 <- c(
  dpois(0:4, lambda = lambda1_mp),
  ppois(
    4,
    lambda = lambda1_mp,
    lower.tail = FALSE
  )
)

prob_componente2 <- c(
  dpois(0:4, lambda = lambda2_mp),
  ppois(
    4,
    lambda = lambda2_mp,
    lower.tail = FALSE
  )
)

probabilidades_mp <- pi_mp * prob_componente1 +
  (1 - pi_mp) * prob_componente2

cat(
  "Soma das probabilidades:",
  sum(probabilidades_mp),
  "\n"
)

# Contribuição dos componentes para a distribuição
componentes_mp <- tibble(
  categoria = factor(
    0:5,
    levels = 0:5,
    labels = c("0", "1", "2", "3", "4", "5+")
  ),
  componente1 = pi_mp * prob_componente1,
  componente2 = (1 - pi_mp) * prob_componente2,
  mistura = probabilidades_mp
)

print(componentes_mp, n = Inf)

# Distribuição observada versus prevista
diagnostico_mp <- distribuicao_nb |>
  mutate(
    previsto = probabilidades_mp,
    observado = observado_ponderado,
    diferenca_pp = 100 * (observado - previsto),
    diferenca_relativa =
      100 * (observado - previsto) / previsto
  ) |>
  select(
    categoria,
    n,
    observado,
    previsto,
    diferenca_pp,
    diferenca_relativa
  )

print(diagnostico_mp, n = Inf)

# Resumo da adequação
resumo_adequacao_mp <- diagnostico_mp |>
  summarise(
    erro_maximo_pp = max(abs(diferenca_pp)),
    erro_medio_pp = mean(abs(diferenca_pp)),
    erro_total_pp = sum(abs(diferenca_pp))
  )

print(resumo_adequacao_mp)

# Comparação com os modelos anteriores
comparacao_modelos <- tibble(
  modelo = c(
    "NB censurada",
    "Hurdle Poisson censurada",
    "ZINB censurada",
    "Mix-Poisson censurada"
  ),
  parametros = c(2, 2, 3, 3),
  erro_maximo_pp = c(
    max(abs(diagnostico_nb$diferenca_pp)),
    max(abs(diagnostico_hp$diferenca_pp)),
    max(abs(diagnostico_zinb$diferenca_pp)),
    max(abs(diagnostico_mp$diferenca_pp))
  ),
  erro_medio_pp = c(
    mean(abs(diagnostico_nb$diferenca_pp)),
    mean(abs(diagnostico_hp$diferenca_pp)),
    mean(abs(diagnostico_zinb$diferenca_pp)),
    mean(abs(diagnostico_mp$diferenca_pp))
  ),
  erro_total_pp = c(
    sum(abs(diagnostico_nb$diferenca_pp)),
    sum(abs(diagnostico_hp$diferenca_pp)),
    sum(abs(diagnostico_zinb$diferenca_pp)),
    sum(abs(diagnostico_mp$diferenca_pp))
  )
)

print(comparacao_modelos, n = Inf)

# Gráfico observado versus previsto
grafico_mp <- diagnostico_mp |>
  select(categoria, observado, previsto) |>
  pivot_longer(
    cols = c(observado, previsto),
    names_to = "tipo",
    values_to = "probabilidade"
  ) |>
  ggplot(
    aes(
      x = categoria,
      y = probabilidade,
      fill = tipo
    )
  ) +
  geom_col(
    position = "dodge",
    width = 0.7
  ) +
  scale_fill_manual(
    values = c(
      observado = "#31688E",
      previsto = "#35B779"
    ),
    labels = c(
      observado = "Observado ponderado",
      previsto = "Mix-Poisson"
    )
  ) +
  scale_y_continuous(
    labels = scales::percent_format()
  ) +
  labs(
    title = "Mistura de duas Poisson censuradas",
    subtitle = "Distribuição observada ponderada e prevista",
    x = "Dias de ausência",
    y = "Proporção",
    fill = NULL
  ) +
  theme_minimal()

print(grafico_mp)

# Gráfico das discrepâncias
grafico_diferencas_mp <- diagnostico_mp |>
  ggplot(
    aes(
      x = categoria,
      y = diferenca_pp
    )
  ) +
  geom_col(
    fill = "#31688E",
    width = 0.7
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  labs(
    title = "Discrepâncias da mistura de Poisson",
    subtitle = "Observado ponderado menos previsto",
    x = "Dias de ausência",
    y = "Diferença em pontos percentuais"
  ) +
  theme_minimal()

print(grafico_diferencas_mp)

# Gráfico das contribuições dos componentes
grafico_componentes_mp <- componentes_mp |>
  select(
    categoria,
    componente1,
    componente2
  ) |>
  pivot_longer(
    cols = c(componente1, componente2),
    names_to = "componente",
    values_to = "probabilidade"
  ) |>
  ggplot(
    aes(
      x = categoria,
      y = probabilidade,
      fill = componente
    )
  ) +
  geom_col(width = 0.7) +
  scale_fill_manual(
    values = c(
      componente1 = "#31688E",
      componente2 = "#35B779"
    ),
    labels = c(
      componente1 = "Componente 1",
      componente2 = "Componente 2"
    )
  ) +
  scale_y_continuous(
    labels = scales::percent_format()
  ) +
  labs(
    title = "Contribuição dos componentes Mix-Poisson",
    subtitle = "Probabilidades ponderadas pela mistura",
    x = "Dias de ausência",
    y = "Probabilidade",
    fill = NULL
  ) +
  theme_minimal()

print(grafico_componentes_mp)

# Salvar gráficos
ggsave(
  file.path(pasta_mp, "01_distribuicao_mp.png"),
  grafico_mp,
  width = 9,
  height = 5,
  dpi = 300
)

ggsave(
  file.path(pasta_mp, "02_diferencas_mp.png"),
  grafico_diferencas_mp,
  width = 9,
  height = 5,
  dpi = 300
)

ggsave(
  file.path(pasta_mp, "03_componentes_mp.png"),
  grafico_componentes_mp,
  width = 9,
  height = 5,
  dpi = 300
)

# Salvar tabelas
write_csv(
  estimativas_mp,
  file.path(pasta_mp, "estimativas_mp.csv")
)

write_csv(
  estabilidade_mp,
  file.path(pasta_mp, "estabilidade_mp.csv")
)

write_csv(
  componentes_mp,
  file.path(pasta_mp, "componentes_mp.csv")
)

write_csv(
  diagnostico_mp,
  file.path(pasta_mp, "diagnostico_mp.csv")
)

write_csv(
  resumo_adequacao_mp,
  file.path(pasta_mp, "resumo_adequacao_mp.csv")
)

write_csv(
  comparacao_modelos,
  file.path(pasta_mp, "comparacao_modelos.csv")
)

# Salvar ajuste
saveRDS(
  ajuste_mp_final,
  file.path(pasta_mp, "ajuste_mp_final.rds")
)

################################################################################
# 5. MISTURA DE DUAS BINOMIAIS NEGATIVAS CENSURADAS
################################################################################

library(tidyverse)

# Criar pasta dos resultados
dir.create(
  "modelos/mistura_nb",
  recursive = TRUE,
  showWarnings = FALSE
)

pasta_mnb <- "modelos/mistura_nb"

# Frequências ponderadas normalizadas
frequencias_mnb <- dados_nb |>
  group_by(y) |>
  summarise(
    peso = sum(peso_normalizado),
    .groups = "drop"
  ) |>
  arrange(y)

# Função de pseudolog-verossimilhança
log_verossimilhanca_mnb <- function(parametros) {
  
  mu1 <- exp(parametros[1])
  theta1 <- exp(parametros[2])
  
  mu2 <- mu1 + exp(parametros[3])
  theta2 <- exp(parametros[4])
  
  pi <- plogis(parametros[5])
  
  prob1 <- c(
    dnbinom(
      0:4,
      mu = mu1,
      size = theta1
    ),
    pnbinom(
      4,
      mu = mu1,
      size = theta1,
      lower.tail = FALSE
    )
  )
  
  prob2 <- c(
    dnbinom(
      0:4,
      mu = mu2,
      size = theta2
    ),
    pnbinom(
      4,
      mu = mu2,
      size = theta2,
      lower.tail = FALSE
    )
  )
  
  probabilidades <- pi * prob1 +
    (1 - pi) * prob2
  
  -sum(
    frequencias_mnb$peso *
      log(pmax(probabilidades, 1e-300))
  )
}

# Limites para os parâmetros transformados
limite_inferior_mnb <- c(
  log(1e-5),
  log(1e-4),
  log(1e-5),
  log(1e-4),
  -15
)

limite_superior_mnb <- c(
  log(30),
  log(1e4),
  log(30),
  log(1e4),
  15
)

# Ajuste principal
ajuste_mnb <- optim(
  par = c(
    log(0.05),
    log(0.1),
    log(3),
    log(1),
    qlogis(0.85)
  ),
  fn = log_verossimilhanca_mnb,
  method = "L-BFGS-B",
  lower = limite_inferior_mnb,
  upper = limite_superior_mnb,
  control = list(maxit = 3000)
)

# Primeira inicialização alternativa
ajuste_mnb_inicio_1 <- optim(
  par = c(
    log(0.2),
    log(0.5),
    log(2),
    log(0.3),
    qlogis(0.70)
  ),
  fn = log_verossimilhanca_mnb,
  method = "L-BFGS-B",
  lower = limite_inferior_mnb,
  upper = limite_superior_mnb,
  control = list(maxit = 3000)
)

# Segunda inicialização alternativa
ajuste_mnb_inicio_2 <- optim(
  par = c(
    log(0.01),
    log(0.05),
    log(5),
    log(2),
    qlogis(0.95)
  ),
  fn = log_verossimilhanca_mnb,
  method = "L-BFGS-B",
  lower = limite_inferior_mnb,
  upper = limite_superior_mnb,
  control = list(maxit = 3000)
)

# Terceira inicialização alternativa
ajuste_mnb_inicio_3 <- optim(
  par = c(
    log(0.4),
    log(0.2),
    log(1),
    log(0.1),
    qlogis(0.50)
  ),
  fn = log_verossimilhanca_mnb,
  method = "L-BFGS-B",
  lower = limite_inferior_mnb,
  upper = limite_superior_mnb,
  control = list(maxit = 3000)
)

# Comparar os ajustes obtidos
estabilidade_mnb <- tibble(
  inicializacao = c(
    "Principal",
    "Alternativa 1",
    "Alternativa 2",
    "Alternativa 3"
  ),
  
  convergencia = c(
    ajuste_mnb$convergence,
    ajuste_mnb_inicio_1$convergence,
    ajuste_mnb_inicio_2$convergence,
    ajuste_mnb_inicio_3$convergence
  ),
  
  mu1 = exp(c(
    ajuste_mnb$par[1],
    ajuste_mnb_inicio_1$par[1],
    ajuste_mnb_inicio_2$par[1],
    ajuste_mnb_inicio_3$par[1]
  )),
  
  theta1 = exp(c(
    ajuste_mnb$par[2],
    ajuste_mnb_inicio_1$par[2],
    ajuste_mnb_inicio_2$par[2],
    ajuste_mnb_inicio_3$par[2]
  )),
  
  mu2 = c(
    exp(ajuste_mnb$par[1]) +
      exp(ajuste_mnb$par[3]),
    
    exp(ajuste_mnb_inicio_1$par[1]) +
      exp(ajuste_mnb_inicio_1$par[3]),
    
    exp(ajuste_mnb_inicio_2$par[1]) +
      exp(ajuste_mnb_inicio_2$par[3]),
    
    exp(ajuste_mnb_inicio_3$par[1]) +
      exp(ajuste_mnb_inicio_3$par[3])
  ),
  
  theta2 = exp(c(
    ajuste_mnb$par[4],
    ajuste_mnb_inicio_1$par[4],
    ajuste_mnb_inicio_2$par[4],
    ajuste_mnb_inicio_3$par[4]
  )),
  
  pi = plogis(c(
    ajuste_mnb$par[5],
    ajuste_mnb_inicio_1$par[5],
    ajuste_mnb_inicio_2$par[5],
    ajuste_mnb_inicio_3$par[5]
  )),
  
  objetivo = c(
    ajuste_mnb$value,
    ajuste_mnb_inicio_1$value,
    ajuste_mnb_inicio_2$value,
    ajuste_mnb_inicio_3$value
  )
)

print(estabilidade_mnb, n = Inf, width = Inf)

# Selecionar o melhor ajuste convergido
melhor_inicio_mnb <- estabilidade_mnb |>
  mutate(indice = row_number()) |>
  filter(
    convergencia == 0,
    is.finite(objetivo)
  ) |>
  slice_min(
    objetivo,
    n = 1,
    with_ties = FALSE
  )

if (nrow(melhor_inicio_mnb) == 0) {
  stop("Nenhum ajuste Mix-NB convergiu.")
}

indice_melhor_mnb <- melhor_inicio_mnb$indice

ajuste_mnb_final <- switch(
  indice_melhor_mnb,
  ajuste_mnb,
  ajuste_mnb_inicio_1,
  ajuste_mnb_inicio_2,
  ajuste_mnb_inicio_3
)

# Recuperar os parâmetros do melhor ajuste
mu1_mnb <- exp(ajuste_mnb_final$par[1])

theta1_mnb <- exp(ajuste_mnb_final$par[2])

mu2_mnb <- mu1_mnb +
  exp(ajuste_mnb_final$par[3])

theta2_mnb <- exp(ajuste_mnb_final$par[4])

pi_mnb <- plogis(ajuste_mnb_final$par[5])

estimativas_mnb <- tibble(
  parametro = c(
    "mu1",
    "theta1",
    "mu2",
    "theta2",
    "pi"
  ),
  estimativa = c(
    mu1_mnb,
    theta1_mnb,
    mu2_mnb,
    theta2_mnb,
    pi_mnb
  )
)

print(estimativas_mnb)

cat(
  "Melhor inicialização:",
  melhor_inicio_mnb$inicializacao,
  "\n"
)

cat(
  "Convergência:",
  ajuste_mnb_final$convergence,
  "\n"
)

cat(
  "Função objetivo:",
  ajuste_mnb_final$value,
  "\n"
)

# Distribuição prevista pelo primeiro componente
prob_componente1_mnb <- c(
  dnbinom(
    0:4,
    mu = mu1_mnb,
    size = theta1_mnb
  ),
  pnbinom(
    4,
    mu = mu1_mnb,
    size = theta1_mnb,
    lower.tail = FALSE
  )
)

# Distribuição prevista pelo segundo componente
prob_componente2_mnb <- c(
  dnbinom(
    0:4,
    mu = mu2_mnb,
    size = theta2_mnb
  ),
  pnbinom(
    4,
    mu = mu2_mnb,
    size = theta2_mnb,
    lower.tail = FALSE
  )
)

# Probabilidades da mistura
probabilidades_mnb <-
  pi_mnb * prob_componente1_mnb +
  (1 - pi_mnb) * prob_componente2_mnb

cat(
  "Soma das probabilidades:",
  sum(probabilidades_mnb),
  "\n"
)

# Contribuições dos componentes
componentes_mnb <- tibble(
  categoria = factor(
    0:5,
    levels = 0:5,
    labels = c("0", "1", "2", "3", "4", "5+")
  ),
  
  componente1 = pi_mnb * prob_componente1_mnb,
  
  componente2 = (1 - pi_mnb) * prob_componente2_mnb,
  
  mistura = probabilidades_mnb
)

print(componentes_mnb, n = Inf)

# Comparar frequências observadas e previstas
diagnostico_mnb <- distribuicao_nb |>
  mutate(
    previsto = probabilidades_mnb,
    observado = observado_ponderado,
    
    diferenca_pp =
      100 * (observado - previsto),
    
    diferenca_relativa =
      100 * (observado - previsto) / previsto
  ) |>
  select(
    categoria,
    n,
    observado,
    previsto,
    diferenca_pp,
    diferenca_relativa
  )

print(diagnostico_mnb, n = Inf)

# Medidas descritivas de adequação
resumo_adequacao_mnb <- diagnostico_mnb |>
  summarise(
    erro_maximo_pp =
      max(abs(diferenca_pp)),
    
    erro_medio_pp =
      mean(abs(diferenca_pp)),
    
    erro_total_pp =
      sum(abs(diferenca_pp))
  )

print(resumo_adequacao_mnb)

# Diagnóstico exploratório da curvatura da função objetivo
hessiana_mnb <- optimHess(
  par = ajuste_mnb_final$par,
  fn = log_verossimilhanca_mnb
)

autovalores_hessiana_mnb <- eigen(
  hessiana_mnb,
  symmetric = TRUE,
  only.values = TRUE
)$values

diagnostico_hessiana_mnb <- tibble(
  parametro = seq_along(autovalores_hessiana_mnb),
  autovalor = autovalores_hessiana_mnb
)

print(diagnostico_hessiana_mnb)

cat(
  "Menor autovalor:",
  min(autovalores_hessiana_mnb),
  "\n"
)

cat(
  "Razão entre autovalores extremos:",
  max(abs(autovalores_hessiana_mnb)) /
    min(abs(autovalores_hessiana_mnb)),
  "\n"
)

# Comparação com os modelos anteriores
comparacao_modelos <- tibble(
  modelo = c(
    "NB censurada",
    "Hurdle Poisson censurada",
    "ZINB censurada",
    "Mix-Poisson censurada",
    "Mix-NB censurada"
  ),
  
  parametros = c(2, 2, 3, 3, 5),
  
  erro_maximo_pp = c(
    max(abs(diagnostico_nb$diferenca_pp)),
    max(abs(diagnostico_hp$diferenca_pp)),
    max(abs(diagnostico_zinb$diferenca_pp)),
    max(abs(diagnostico_mp$diferenca_pp)),
    max(abs(diagnostico_mnb$diferenca_pp))
  ),
  
  erro_medio_pp = c(
    mean(abs(diagnostico_nb$diferenca_pp)),
    mean(abs(diagnostico_hp$diferenca_pp)),
    mean(abs(diagnostico_zinb$diferenca_pp)),
    mean(abs(diagnostico_mp$diferenca_pp)),
    mean(abs(diagnostico_mnb$diferenca_pp))
  ),
  
  erro_total_pp = c(
    sum(abs(diagnostico_nb$diferenca_pp)),
    sum(abs(diagnostico_hp$diferenca_pp)),
    sum(abs(diagnostico_zinb$diferenca_pp)),
    sum(abs(diagnostico_mp$diferenca_pp)),
    sum(abs(diagnostico_mnb$diferenca_pp))
  )
)

print(comparacao_modelos, n = Inf)

# Gráfico da distribuição observada e prevista
grafico_mnb <- diagnostico_mnb |>
  select(
    categoria,
    observado,
    previsto
  ) |>
  pivot_longer(
    cols = c(observado, previsto),
    names_to = "tipo",
    values_to = "probabilidade"
  ) |>
  ggplot(
    aes(
      x = categoria,
      y = probabilidade,
      fill = tipo
    )
  ) +
  geom_col(
    position = "dodge",
    width = 0.7
  ) +
  scale_fill_manual(
    values = c(
      observado = "#31688E",
      previsto = "#35B779"
    ),
    labels = c(
      observado = "Observado ponderado",
      previsto = "Mix-NB censurada"
    )
  ) +
  scale_y_continuous(
    labels = scales::percent_format()
  ) +
  labs(
    title = "Mistura de duas binomiais negativas censuradas",
    subtitle = "Distribuição observada ponderada e prevista",
    x = "Dias de ausência",
    y = "Proporção",
    fill = NULL
  ) +
  theme_minimal()

print(grafico_mnb)

# Gráfico das discrepâncias
grafico_diferencas_mnb <- diagnostico_mnb |>
  ggplot(
    aes(
      x = categoria,
      y = diferenca_pp
    )
  ) +
  geom_col(
    fill = "#31688E",
    width = 0.7
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  labs(
    title = "Discrepâncias da mistura de NB",
    subtitle = "Observado ponderado menos previsto",
    x = "Dias de ausência",
    y = "Diferença em pontos percentuais"
  ) +
  theme_minimal()

print(grafico_diferencas_mnb)

# Gráfico das contribuições dos componentes
grafico_componentes_mnb <- componentes_mnb |>
  select(
    categoria,
    componente1,
    componente2
  ) |>
  pivot_longer(
    cols = c(componente1, componente2),
    names_to = "componente",
    values_to = "probabilidade"
  ) |>
  ggplot(
    aes(
      x = categoria,
      y = probabilidade,
      fill = componente
    )
  ) +
  geom_col(width = 0.7) +
  scale_fill_manual(
    values = c(
      componente1 = "#31688E",
      componente2 = "#35B779"
    ),
    labels = c(
      componente1 = "Componente 1",
      componente2 = "Componente 2"
    )
  ) +
  scale_y_continuous(
    labels = scales::percent_format()
  ) +
  labs(
    title = "Componentes da mistura de NB censuradas",
    subtitle = "Contribuições ponderadas pela mistura",
    x = "Dias de ausência",
    y = "Probabilidade",
    fill = NULL
  ) +
  theme_minimal()

print(grafico_componentes_mnb)

# Salvar os gráficos
ggsave(
  file.path(pasta_mnb, "01_distribuicao_mnb.png"),
  grafico_mnb,
  width = 9,
  height = 5,
  dpi = 300
)

ggsave(
  file.path(pasta_mnb, "02_diferencas_mnb.png"),
  grafico_diferencas_mnb,
  width = 9,
  height = 5,
  dpi = 300
)

ggsave(
  file.path(pasta_mnb, "03_componentes_mnb.png"),
  grafico_componentes_mnb,
  width = 9,
  height = 5,
  dpi = 300
)

# Salvar as tabelas
write_csv(
  estimativas_mnb,
  file.path(pasta_mnb, "estimativas_mnb.csv")
)

write_csv(
  estabilidade_mnb,
  file.path(pasta_mnb, "estabilidade_mnb.csv")
)

write_csv(
  componentes_mnb,
  file.path(pasta_mnb, "componentes_mnb.csv")
)

write_csv(
  diagnostico_mnb,
  file.path(pasta_mnb, "diagnostico_mnb.csv")
)

write_csv(
  resumo_adequacao_mnb,
  file.path(pasta_mnb, "resumo_adequacao_mnb.csv")
)

write_csv(
  diagnostico_hessiana_mnb,
  file.path(pasta_mnb, "diagnostico_hessiana_mnb.csv")
)

write_csv(
  comparacao_modelos,
  file.path(pasta_mnb, "comparacao_modelos.csv")
)

# Salvar o ajuste
saveRDS(
  ajuste_mnb_final,
  file.path(pasta_mnb, "ajuste_mnb_final.rds")
)

################################################################################
# DIAGNÓSTICO DISTRIBUCIONAL DOS MODELOS CENSURADOS
################################################################################

library(tidyverse)

dir.create(
  "modelos/comparacao_distribucional",
  recursive = TRUE,
  showWarnings = FALSE
)

pasta_comparacao <- "modelos/comparacao_distribucional"

# Probabilidades observadas ponderadas
prob_observada <- distribuicao_nb$observado_ponderado

categorias <- c("0", "1", "2", "3", "4", "5+")

# NB censurada
probabilidades_nb <- c(
  dnbinom(0:4, mu = mu_nb, size = theta_nb),
  pnbinom(
    4,
    mu = mu_nb,
    size = theta_nb,
    lower.tail = FALSE
  )
)

# Reunir probabilidades dos modelos já ajustados
probabilidades_modelos <- tibble(
  categoria = categorias,
  valor_agrupado = 0:5,
  observado = prob_observada,
  `NB censurada` = probabilidades_nb,
  `Hurdle Poisson` = probabilidades_hp,
  `ZINB censurada` = probabilidades_zinb,
  `Mix-Poisson` = probabilidades_mp,
  `Mix-NB` = probabilidades_mnb
)

# Formato longo para comparação
probabilidades_longas <- probabilidades_modelos |>
  pivot_longer(
    cols = -c(categoria, valor_agrupado, observado),
    names_to = "modelo",
    values_to = "previsto"
  ) |>
  mutate(
    diferenca_pp = 100 * (observado - previsto),
    erro_absoluto_pp = abs(diferenca_pp)
  )

# Média e variância da variável observada agrupada
# O valor 5 representa a categoria 5+, não cinco dias exatos
momentos_observados <- probabilidades_modelos |>
  summarise(
    media = sum(valor_agrupado * observado),
    variancia = sum(
      valor_agrupado^2 * observado
    ) - media^2
  ) |>
  mutate(
    razao_variancia_media = variancia / media
  )

print(momentos_observados)

# Momentos previstos para a mesma variável agrupada
momentos_previstos <- probabilidades_longas |>
  group_by(modelo) |>
  summarise(
    media = sum(valor_agrupado * previsto),
    variancia = sum(
      valor_agrupado^2 * previsto
    ) - media^2,
    .groups = "drop"
  ) |>
  mutate(
    razao_variancia_media = variancia / media,
    diferenca_media = media - momentos_observados$media,
    diferenca_variancia =
      variancia - momentos_observados$variancia
  )

print(momentos_previstos, n = Inf)

# Diagnóstico dos zeros e da cauda
diagnostico_extremos <- probabilidades_longas |>
  filter(categoria %in% c("0", "5+")) |>
  select(
    modelo,
    categoria,
    observado,
    previsto,
    diferenca_pp
  )

print(diagnostico_extremos, n = Inf)

# Adequação das contagens positivas
diagnostico_positivos <- probabilidades_longas |>
  filter(categoria != "0") |>
  group_by(modelo) |>
  mutate(
    observado_condicional =
      observado / sum(observado),
    previsto_condicional =
      previsto / sum(previsto),
    diferenca_condicional_pp =
      100 * (
        observado_condicional -
          previsto_condicional
      )
  ) |>
  ungroup()

print(
  diagnostico_positivos |>
    select(
      modelo,
      categoria,
      observado_condicional,
      previsto_condicional,
      diferenca_condicional_pp
    ),
  n = Inf
)

# Comparação padronizada do ajuste marginal
resumo_modelos <- probabilidades_longas |>
  group_by(modelo) |>
  summarise(
    erro_medio_pp = mean(erro_absoluto_pp),
    erro_maximo_pp = max(erro_absoluto_pp),
    erro_total_pp = sum(erro_absoluto_pp),
    log_score_ponderado = -sum(
      observado * log(pmax(previsto, 1e-300))
    ),
    .groups = "drop"
  ) |>
  mutate(
    parametros = case_when(
      modelo == "NB censurada" ~ 2L,
      modelo == "Hurdle Poisson" ~ 2L,
      modelo == "ZINB censurada" ~ 3L,
      modelo == "Mix-Poisson" ~ 3L,
      modelo == "Mix-NB" ~ 5L
    )
  ) |>
  arrange(erro_medio_pp)

print(resumo_modelos, n = Inf)

# Gráfico das probabilidades positivas condicionais
grafico_positivos <- diagnostico_positivos |>
  select(
    modelo,
    categoria,
    observado_condicional,
    previsto_condicional
  ) |>
  pivot_longer(
    cols = c(
      observado_condicional,
      previsto_condicional
    ),
    names_to = "tipo",
    values_to = "probabilidade"
  ) |>
  ggplot(
    aes(
      x = categoria,
      y = probabilidade,
      fill = tipo
    )
  ) +
  geom_col(
    position = "dodge",
    width = 0.7
  ) +
  facet_wrap(~ modelo) +
  scale_fill_manual(
    values = c(
      observado_condicional = "#31688E",
      previsto_condicional = "#35B779"
    ),
    labels = c(
      observado_condicional = "Observado",
      previsto_condicional = "Previsto"
    )
  ) +
  scale_y_continuous(
    labels = scales::percent_format()
  ) +
  labs(
    title = "Adequação das ausências positivas",
    subtitle = "Distribuições condicionais a pelo menos uma ausência",
    x = "Dias de ausência",
    y = "Probabilidade condicional",
    fill = NULL
  ) +
  theme_minimal()

print(grafico_positivos)

# Gráfico das discrepâncias
grafico_discrepancias <- probabilidades_longas |>
  ggplot(
    aes(
      x = categoria,
      y = diferenca_pp
    )
  ) +
  geom_col(
    fill = "#31688E",
    width = 0.7
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  facet_wrap(~ modelo) +
  labs(
    title = "Discrepâncias por categoria",
    subtitle = "Observado ponderado menos previsto",
    x = "Dias de ausência",
    y = "Diferença em pontos percentuais"
  ) +
  theme_minimal()

print(grafico_discrepancias)

# Salvar resultados
write_csv(
  probabilidades_longas,
  file.path(pasta_comparacao, "probabilidades_modelos.csv")
)

write_csv(
  momentos_observados,
  file.path(pasta_comparacao, "momentos_observados.csv")
)

write_csv(
  momentos_previstos,
  file.path(pasta_comparacao, "momentos_previstos.csv")
)

write_csv(
  diagnostico_extremos,
  file.path(pasta_comparacao, "diagnostico_extremos.csv")
)

write_csv(
  diagnostico_positivos,
  file.path(pasta_comparacao, "diagnostico_positivos.csv")
)

write_csv(
  resumo_modelos,
  file.path(pasta_comparacao, "resumo_modelos.csv")
)

ggsave(
  file.path(pasta_comparacao, "01_positivos.png"),
  grafico_positivos,
  width = 11,
  height = 7,
  dpi = 300
)

ggsave(
  file.path(pasta_comparacao, "02_discrepancias.png"),
  grafico_discrepancias,
  width = 11,
  height = 7,
  dpi = 300
)