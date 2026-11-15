################################################################################
# SCRIPT 2 - ANÁLISE DESCRITIVA | PeNSE 2024
# Saídas: plot/descritiva
################################################################################

library(tidyverse)
library(survey)
library(gtsummary)
library(gt)

# Leitura da base tratada no Script 1
dados <- readRDS("data/dados_pense2024_tratados.rds")

# Diretório dos resultados
diretorio <- "plot/descritiva"
dir.create(diretorio, recursive = TRUE, showWarnings = FALSE)

# Cor fixa para cada variável (paleta com contraste sobre fundo branco)
cores_variaveis <- c(
  dias_ausencia_inseguranca_escola = "#31688E",
  bullying_sofrido = "#D1495B",
  exclusao_social = "#7B2CBF",
  agressao_fisica_colegas = "#E76F51",
  cyberbullying_sofrido = "#C77D00",
  apoio_familiar = "#26867A",
  escolaridade_mae = "#648FFF",
  acao_prevencao_bullying = "#009E73",
  sexo = "#CC79A7",
  raca_cor = "#8C6D31",
  faixa_etaria = "#2A9D8F",
  rede_ensino = "#5E6472"
)

rotulos_variaveis <- c(
  bullying_sofrido = "Bullying sofrido",
  exclusao_social = "Exclusão social",
  agressao_fisica_colegas = "Agressão física por colegas",
  cyberbullying_sofrido = "Cyberbullying sofrido",
  apoio_familiar = "Apoio familiar",
  escolaridade_mae = "Escolaridade materna",
  acao_prevencao_bullying = "Ações escolares de prevenção ao bullying",
  sexo = "Sexo",
  raca_cor = "Raça/cor",
  faixa_etaria = "Faixa etária",
  rede_ensino = "Rede de ensino"
)

variaveis_explicativas <- names(rotulos_variaveis)

# Tema único para todos os gráficos
tema_descritiva <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "grey40"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(color = "grey25"),
    axis.text.y = element_text(color = "grey25")
  )

# Conferência da elegibilidade para estimativas ponderadas
resumo_elegibilidade <- dados |>
  summarise(
    n_total = n(),
    n_com_resposta = sum(!is.na(ausencia_limite_inferior)),
    n_com_peso_positivo = sum(!is.na(peso_amostral) & peso_amostral > 0),
    n_com_resposta_e_peso = sum(
      !is.na(ausencia_limite_inferior) &
        !is.na(peso_amostral) & peso_amostral > 0
    )
  )
readr::write_csv(resumo_elegibilidade, file.path(diretorio, "01_elegibilidade.csv"))

# Não imputar pesos ausentes; restringir estimativas ponderadas ao domínio elegível.
dados_ponderados <- dados |>
  filter(
    !is.na(peso_amostral),
    is.finite(peso_amostral),
    peso_amostral > 0,
    !is.na(estrato),
    !is.na(id_escola)
  ) |>
  mutate(
    ausencia_binaria = factor(
      ausencia_alguma,
      levels = c(FALSE, TRUE),
      labels = c("Nenhuma ausência", "Alguma ausência")
    ),
    # Fatores ordinais como categorias (sem tendência linear imposta)
    across(all_of(variaveis_explicativas), as.factor)
  )

# ATENÇÃO: especificação provisória do desenho, a confirmar na documentação
# IBGE PeNSE 2024 (PSU, estratos, peso final e possíveis ajustes de variância).
# A opção de PSU solitária deve ser definida de forma fundamentada.
options(survey.lonely.psu = "adjust")
desenho <- survey::svydesign(
  ids = ~id_escola,
  strata = ~estrato,
  weights = ~peso_amostral,
  data = dados_ponderados,
  nest = TRUE
)

################################################################################
# ANÁLISE DESCRITIVA DA RESPOSTA
################################################################################

# Mesma subamostra na coluna amostral e na coluna ponderada
dados_resposta <- dados_ponderados |>
  filter(!is.na(dias_ausencia_inseguranca_escola)) |>
  mutate(
    y = as.integer(dias_ausencia_inseguranca_escola) - 1L
  )

distribuicao <- dados_resposta |>
  group_by(dias_ausencia_inseguranca_escola, y) |>
  summarise(
    n_amostral = n(),
    total_ponderado = sum(peso_amostral),
    .groups = "drop"
  ) |>
  arrange(y) |>
  mutate(
    percentual_amostral = 100 * n_amostral / sum(n_amostral),
    percentual_ponderado = 100 * total_ponderado / sum(total_ponderado),
    percentual_acumulado_ponderado = cumsum(percentual_ponderado)
  )

readr::write_csv(distribuicao, file.path(diretorio, "02_distribuicao_resposta.csv"))

resumo_contagens <- dados_resposta |>
  summarise(
    n_validos = n(),
    n_zeros = sum(y == 0),
    n_positivos = sum(y > 0),
    n_censurados = sum(y == 5),
    percentual_zeros_ponderado = 100 * weighted.mean(y == 0, peso_amostral),
    percentual_positivos_ponderado = 100 * weighted.mean(y > 0, peso_amostral),
    percentual_censurados_ponderado = 100 * weighted.mean(y == 5, peso_amostral),
    percentual_censurados_entre_positivos_ponderado =
      100 * sum(peso_amostral[y == 5]) / sum(peso_amostral[y > 0])
  )
readr::write_csv(resumo_contagens, file.path(diretorio, "03_resumo_contagens.csv"))

distribuicao_positivos <- dados_resposta |>
  filter(y > 0) |>
  group_by(dias_ausencia_inseguranca_escola, y) |>
  summarise(
    n_amostral = n(),
    total_ponderado = sum(peso_amostral),
    .groups = "drop"
  ) |>
  arrange(y) |>
  mutate(percentual_ponderado = 100 * total_ponderado / sum(total_ponderado))
readr::write_csv(distribuicao_positivos, file.path(diretorio, "04_distribuicao_positivos.csv"))

grafico_geral <- ggplot(
  distribuicao,
  aes(x = dias_ausencia_inseguranca_escola, y = percentual_ponderado)
) +
  geom_col(fill = cores_variaveis[["dias_ausencia_inseguranca_escola"]], width = 0.72) +
  geom_text(aes(label = sprintf("%.1f%%", percentual_ponderado)),
            vjust = -0.4, size = 3.4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Ausência escolar por insegurança",
       subtitle = "PeNSE 2024 · proporções ponderadas",
       x = "Dias de ausência", y = "Percentual (%)") +
  tema_descritiva +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

ggsave(file.path(diretorio, "01_ausencia_distribuicao.png"),
       grafico_geral, width = 9, height = 5, dpi = 300)

grafico_positivos <- ggplot(
  distribuicao_positivos,
  aes(x = dias_ausencia_inseguranca_escola, y = percentual_ponderado)
) +
  geom_col(fill = cores_variaveis[["dias_ausencia_inseguranca_escola"]], width = 0.72) +
  geom_text(aes(label = sprintf("%.1f%%", percentual_ponderado)),
            vjust = -0.4, size = 3.4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Ausências entre estudantes que faltaram",
       subtitle = "PeNSE 2024 · distribuição ponderada condicional a alguma ausência",
       x = "Dias de ausência", y = "Percentual (%)") +
  tema_descritiva +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

ggsave(file.path(diretorio, "02_ausencia_entre_positivos.png"),
       grafico_positivos, width = 9, height = 5, dpi = 300)

grafico_acumulado <- ggplot(
  distribuicao,
  aes(x = dias_ausencia_inseguranca_escola,
      y = percentual_acumulado_ponderado, group = 1)
) +
  geom_line(color = cores_variaveis[["dias_ausencia_inseguranca_escola"]], linewidth = 1) +
  geom_point(color = cores_variaveis[["dias_ausencia_inseguranca_escola"]], size = 2.6) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  labs(title = "Distribuição acumulada das ausências",
       subtitle = "PeNSE 2024 · proporções ponderadas · 5+ é categoria aberta",
       x = "Dias de ausência", y = "Percentual acumulado (%)") +
  tema_descritiva +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

ggsave(file.path(diretorio, "03_ausencia_acumulada.png"),
       grafico_acumulado, width = 9, height = 5, dpi = 300)

################################################################################
# COVARIÁVEIS - DISTRIBUIÇÕES E GRÁFICOS
################################################################################

# Formato longo: conserva rótulos e usa os pesos para frequências populacionais
dados_longos <- dados_ponderados |>
  select(peso_amostral, all_of(variaveis_explicativas)) |>
  mutate(across(all_of(variaveis_explicativas), as.character)) |>
  pivot_longer(
    cols = all_of(variaveis_explicativas),
    names_to = "variavel",
    values_to = "categoria"
  ) |>
  filter(!is.na(categoria))

distribuicao_covariaveis <- dados_longos |>
  group_by(variavel, categoria) |>
  summarise(
    n_amostral = n(),
    total_ponderado = sum(peso_amostral),
    .groups = "drop"
  ) |>
  group_by(variavel) |>
  mutate(
    percentual_amostral = 100 * n_amostral / sum(n_amostral),
    percentual_ponderado = 100 * total_ponderado / sum(total_ponderado)
  ) |>
  ungroup()

readr::write_csv(distribuicao_covariaveis,
                 file.path(diretorio, "05_distribuicao_covariaveis.csv"))

# Associação descritiva: proporção de alguma ausência em cada categoria
associacoes <- dados_ponderados |>
  filter(!is.na(ausencia_alguma)) |>
  select(peso_amostral, ausencia_alguma, ausencia_censurada,
         all_of(variaveis_explicativas)) |>
  mutate(across(all_of(variaveis_explicativas), as.character)) |>
  pivot_longer(
    cols = all_of(variaveis_explicativas),
    names_to = "variavel",
    values_to = "categoria"
  ) |>
  filter(!is.na(categoria)) |>
  group_by(variavel, categoria) |>
  summarise(
    n_amostral = n(),
    n_ausencias = sum(ausencia_alguma),
    percentual_alguma_ausencia = 100 * weighted.mean(ausencia_alguma, peso_amostral),
    percentual_cinco_mais = 100 * weighted.mean(ausencia_censurada, peso_amostral),
    percentual_cinco_mais_entre_positivos =
      if_else(sum(peso_amostral[ausencia_alguma]) > 0,
              100 * sum(peso_amostral[ausencia_censurada]) /
                sum(peso_amostral[ausencia_alguma]), NA_real_),
    .groups = "drop"
  )

readr::write_csv(associacoes, file.path(diretorio, "06_ausencia_por_covariavel.csv"))

# Gera um gráfico por variável, preservando sua cor em todos os gráficos.
# A ordem das categorias segue a ordem dos fatores definida no Script 1.
purrr::walk(variaveis_explicativas, function(nome) {
  niveis <- levels(as.factor(dados_ponderados[[nome]]))

  tabela_dist <- distribuicao_covariaveis |>
    filter(variavel == nome) |>
    mutate(categoria = factor(categoria, levels = niveis))

  grafico_dist <- ggplot(tabela_dist,
                         aes(x = categoria, y = percentual_ponderado)) +
    geom_col(fill = cores_variaveis[[nome]], width = 0.72) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.09))) +
    labs(title = rotulos_variaveis[[nome]],
         subtitle = "PeNSE 2024 · distribuição ponderada",
         x = NULL, y = "Percentual (%)") +
    tema_descritiva +
    coord_flip()

  ggsave(file.path(diretorio, paste0("distribuicao_", nome, ".png")),
         grafico_dist, width = 9, height = 5.5, dpi = 300)

  tabela_assoc <- associacoes |>
    filter(variavel == nome) |>
    mutate(categoria = factor(categoria, levels = niveis))

  grafico_assoc <- ggplot(tabela_assoc,
                          aes(x = categoria, y = percentual_alguma_ausencia)) +
    geom_col(fill = cores_variaveis[[nome]], width = 0.72) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.09))) +
    labs(title = paste("Alguma ausência segundo", tolower(rotulos_variaveis[[nome]])),
         subtitle = "PeNSE 2024 · prevalências ponderadas, não ajustadas",
         x = NULL, y = "Estudantes com alguma ausência (%)") +
    tema_descritiva +
    coord_flip()

  ggsave(file.path(diretorio, paste0("ausencia_por_", nome, ".png")),
         grafico_assoc, width = 9, height = 5.5, dpi = 300)
})

################################################################################
# TABELAS FORMATADAS E TESTES EXPLORATÓRIOS
################################################################################

# Tabela descritiva ponderada das covariáveis (sem separar pela resposta)
tabela_perfil <- desenho |>
  gtsummary::tbl_svysummary(
    include = all_of(variaveis_explicativas),
    statistic = all_categorical() ~ "{p}%",
    digits = all_categorical() ~ 1,
    missing = "ifany",
    label = list(
      bullying_sofrido ~ "Bullying sofrido",
      exclusao_social ~ "Exclusão social",
      agressao_fisica_colegas ~ "Agressão física por colegas",
      cyberbullying_sofrido ~ "Cyberbullying sofrido",
      apoio_familiar ~ "Apoio familiar",
      escolaridade_mae ~ "Escolaridade materna",
      acao_prevencao_bullying ~ "Ações escolares de prevenção ao bullying",
      sexo ~ "Sexo",
      raca_cor ~ "Raça/cor",
      faixa_etaria ~ "Faixa etária",
      rede_ensino ~ "Rede de ensino"
    )
  ) |>
  gtsummary::modify_caption("**PeNSE 2024 — Perfil dos estudantes (percentuais ponderados)**")

# Para associação, usar somente estudantes com resposta conhecida;
# survey::subset preserva a estrutura de desenho para os domínios analisados.
desenho_com_resposta <- subset(desenho, !is.na(ausencia_binaria))

tabela_associacoes <- desenho_com_resposta |>
  gtsummary::tbl_svysummary(
    by = ausencia_binaria,
    include = all_of(variaveis_explicativas),
    statistic = all_categorical() ~ "{p}%",
    digits = all_categorical() ~ 1,
    missing = "ifany",
    label = list(
      bullying_sofrido ~ "Bullying sofrido",
      exclusao_social ~ "Exclusão social",
      agressao_fisica_colegas ~ "Agressão física por colegas",
      cyberbullying_sofrido ~ "Cyberbullying sofrido",
      apoio_familiar ~ "Apoio familiar",
      escolaridade_mae ~ "Escolaridade materna",
      acao_prevencao_bullying ~ "Ações escolares de prevenção ao bullying",
      sexo ~ "Sexo",
      raca_cor ~ "Raça/cor",
      faixa_etaria ~ "Faixa etária",
      rede_ensino ~ "Rede de ensino"
    )
  ) |>
  gtsummary::add_p(test = all_categorical() ~ "svy.chisq.test") |>
  gtsummary::modify_caption(
    "**PeNSE 2024 — Características segundo ausência por insegurança (percentuais ponderados)**"
  )

# HTML para apresentação e CSV para conferência dos resultados
gt::gtsave(gtsummary::as_gt(tabela_perfil),
           file.path(diretorio, "07_tabela_perfil.html"))
gt::gtsave(gtsummary::as_gt(tabela_associacoes),
           file.path(diretorio, "08_tabela_associacoes_testes.html"))

readr::write_csv(gtsummary::as_tibble(tabela_perfil, col_labels = FALSE),
                 file.path(diretorio, "07_tabela_perfil.csv"))
readr::write_csv(gtsummary::as_tibble(tabela_associacoes, col_labels = FALSE),
                 file.path(diretorio, "08_tabela_associacoes_testes.csv"))

# Conservar objetos dos resultados para análises posteriores
saveRDS(
  list(
    distribuicao_resposta = distribuicao,
    resumo_contagens = resumo_contagens,
    distribuicao_positivos = distribuicao_positivos,
    distribuicao_covariaveis = distribuicao_covariaveis,
    associacoes = associacoes,
    tabela_perfil = tabela_perfil,
    tabela_associacoes = tabela_associacoes
  ),
  file.path(diretorio, "resultados_descritiva.rds")
)

cat("Resultados exportados para:", diretorio, "\n")
