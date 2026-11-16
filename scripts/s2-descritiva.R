################################################################################
# SCRIPT 2 - ANÁLISE DESCRITIVA | PeNSE 2024
# Saídas: plot/descritiva_ver2
################################################################################

library(tidyverse)
library(survey)
library(gtsummary)
library(gt)

################################################################################
# 0. PREPARAÇÃO DOS DADOS
################################################################################

# Leitura da base tratada no Script 1
dados <- readRDS("data/dados_pense2024_tratados.rds")

# Diretório dos resultados
diretorio <- "plot/descritiva_ver2"
dir.create(diretorio, recursive = TRUE, showWarnings = FALSE)

# Variável resposta
variavel_resposta <- "dias_ausencia_inseguranca_escola"

# Variáveis explicativas
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

# Todas as variáveis utilizadas na análise
variaveis_analise <- c(
  variavel_resposta,
  variaveis_explicativas
)

################################################################################
# 1. QUALIDADE DOS DADOS
################################################################################

# 1.1. Completude das variáveis -----------------------------------------------

qualidade_dados <- dados |>
  select(all_of(variaveis_analise)) |>
  summarise(
    across(
      everything(),
      list(
        n_total = ~length(.x),
        n_validos = ~sum(!is.na(.x)),
        n_ausentes = ~sum(is.na(.x)),
        percentual_validos = ~100 * mean(!is.na(.x)),
        percentual_ausentes = ~100 * mean(is.na(.x))
      )
    )
  ) |>
  pivot_longer(
    cols = everything(),
    names_to = c("variavel", ".value"),
    names_pattern = paste0(
      "^(.*)_(n_total|n_validos|n_ausentes|",
      "percentual_validos|percentual_ausentes)$"
    )
  ) |>
  arrange(desc(percentual_ausentes))

# Visualizar os resultados
print(qualidade_dados, n = Inf)

# Exportar tabela
readr::write_csv(
  qualidade_dados,
  file.path(diretorio, "01_qualidade_dados.csv")
)


# 1.2. RESUMO DA COMPLETUDE DA AMOSTRA -----------------------------------------

resumo_qualidade <- dados |>
  summarise(
    n_total = n(),
    
    n_resposta_valida = sum(
      !is.na(dias_ausencia_inseguranca_escola)
    ),
    
    percentual_resposta_valida =
      100 * mean(!is.na(dias_ausencia_inseguranca_escola)),
    
    n_casos_completos = sum(
      complete.cases(pick(all_of(variaveis_analise)))
    ),
    
    percentual_casos_completos =
      100 * mean(
        complete.cases(pick(all_of(variaveis_analise)))
      )
  )

# Visualizar os resultados
print(resumo_qualidade, width = 200)

# Exportar resumo
readr::write_csv(
  resumo_qualidade,
  file.path(diretorio, "02_resumo_qualidade.csv")
)


################################################################################
# 2. DISTRIBUIÇÃO DA VARIÁVEL RESPOSTA
################################################################################

# Cor da variável resposta
cor_resposta <- "#31688E"

# Preparação dos dados
dados_resposta <- dados |>
  filter(
    !is.na(peso_amostral),
    is.finite(peso_amostral),
    peso_amostral > 0,
    !is.na(estrato),
    !is.na(id_escola)
  ) |>
  mutate(
    # Recodificação: categorias 1 a 6 correspondem a 0, 1, 2, 3, 4, 5+
    y = as.integer(dias_ausencia_inseguranca_escola) - 1L,
    
    categoria_geral = factor(
      y,
      levels = 0:5,
      labels = c("0 dias", "1 dia", "2 dias", "3 dias",
                 "4 dias", "5 dias ou mais")
    ),
    
    categoria_positivos = factor(
      y,
      levels = 1:5,
      labels = c("1 dia", "2 dias", "3 dias",
                 "4 dias", "5 dias ou mais")
    )
  )

# Desenho amostral - especificação provisória
options(survey.lonely.psu = "adjust")

desenho <- survey::svydesign(
  ids = ~id_escola,
  strata = ~estrato,
  weights = ~peso_amostral,
  data = dados_resposta,
  nest = TRUE
)

# Domínios de análise
desenho_geral <- subset(desenho, !is.na(y))
desenho_positivos <- subset(desenho, !is.na(y) & y > 0)

################################################################################
# 2.1. DISTRIBUIÇÃO GERAL - INCLUINDO ZERO
################################################################################

# Estimativas ponderadas e intervalos de confiança
estimativa_geral <- survey::svymean(
  ~categoria_geral,
  desenho_geral,
  na.rm = TRUE
)

ic_geral <- confint(estimativa_geral)

distribuicao_geral <- tibble(
  categoria = levels(dados_resposta$categoria_geral),
  percentual = 100 * as.numeric(coef(estimativa_geral)),
  ic_inferior = pmax(0, 100 * ic_geral[, 1]),
  ic_superior = pmin(100, 100 * ic_geral[, 2])
)

# Gráfico
grafico_geral <- ggplot(
  distribuicao_geral,
  aes(x = categoria, y = percentual)
) +
  geom_col(
    fill = cor_resposta,
    width = 0.7
  ) +
  geom_errorbar(
    aes(ymin = ic_inferior, ymax = ic_superior),
    width = 0.15,
    linewidth = 0.5
  ) +
  geom_text(
    aes(
      y = ic_superior + 1,
      label = sprintf("%.1f%%", percentual)
    ),
    size = 3.7
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    title = "Ausência escolar por insegurança",
    subtitle = "PeNSE 2024 | Percentuais ponderados e IC 95%",
    x = "Dias de ausência",
    y = "Estudantes (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

ggsave(
  file.path(diretorio, "03_ausencia_distribuicao_geral.png"),
  grafico_geral,
  width = 9,
  height = 5,
  dpi = 300
)

################################################################################
# 2.2. DISTRIBUIÇÃO CONDICIONAL - SEM ZERO
################################################################################

# Estimativas ponderadas e intervalos de confiança
estimativa_positivos <- survey::svymean(
  ~categoria_positivos,
  desenho_positivos,
  na.rm = TRUE
)

ic_positivos <- confint(estimativa_positivos)

distribuicao_positivos <- tibble(
  categoria = levels(dados_resposta$categoria_positivos),
  percentual = 100 * as.numeric(coef(estimativa_positivos)),
  ic_inferior = pmax(0, 100 * ic_positivos[, 1]),
  ic_superior = pmin(100, 100 * ic_positivos[, 2])
)

# Gráfico
grafico_positivos <- ggplot(
  distribuicao_positivos,
  aes(x = categoria, y = percentual)
) +
  geom_col(
    fill = cor_resposta,
    width = 0.7
  ) +
  geom_errorbar(
    aes(ymin = ic_inferior, ymax = ic_superior),
    width = 0.15,
    linewidth = 0.5
  ) +
  geom_text(
    aes(
      y = ic_superior + 1,
      label = sprintf("%.1f%%", percentual)
    ),
    size = 3.7
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    title = "Ausências entre estudantes que faltaram",
    subtitle = "PeNSE 2024 | Percentuais ponderados e IC 95%",
    x = "Dias de ausência",
    y = "Estudantes com alguma ausência (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

ggsave(
  file.path(diretorio, "04_ausencia_distribuicao_positivos.png"),
  grafico_positivos,
  width = 9,
  height = 5,
  dpi = 300
)

################################################################################
# 2.3. EXPORTAÇÃO DAS ESTIMATIVAS
################################################################################

readr::write_csv(
  distribuicao_geral,
  file.path(diretorio, "03_distribuicao_geral.csv")
)

readr::write_csv(
  distribuicao_positivos,
  file.path(diretorio, "04_distribuicao_positivos.csv")
)

# Visualizar gráficos
print(grafico_geral)
print(grafico_positivos)


################################################################################
# 3. PERFIL DOS ESTUDANTES E ASSOCIAÇÕES BIVARIADAS
################################################################################

################################################################################
# 3.0. PREPARAÇÃO DOS DADOS
################################################################################

# Rótulos das variáveis
rotulos_variaveis <- c(
  bullying_sofrido = "Bullying",
  exclusao_social = "Exclusão social",
  agressao_fisica_colegas = "Agressão física por colegas",
  cyberbullying_sofrido = "Cyberbullying",
  apoio_familiar = "Apoio familiar",
  escolaridade_mae = "Escolaridade materna",
  acao_prevencao_bullying = "Ações escolares de prevenção ao bullying",
  sexo = "Sexo",
  raca_cor = "Raça/cor",
  faixa_etaria = "Faixa etária",
  rede_ensino = "Rede de ensino"
)

# Organização temática das variáveis
grupos_variaveis <- list(
  sociodemograficas = c(
    "sexo",
    "raca_cor",
    "faixa_etaria",
    "escolaridade_mae",
    "rede_ensino"
  ),
  
  inseguranca = c(
    "bullying_sofrido",
    "exclusao_social",
    "agressao_fisica_colegas",
    "cyberbullying_sofrido"
  ),
  
  protecao = c(
    "apoio_familiar",
    "acao_prevencao_bullying"
  )
)

# Verificar as variáveis necessárias
stopifnot(
  exists("desenho_geral"),
  all(variaveis_explicativas %in% names(desenho_geral$variables)),
  "y" %in% names(desenho_geral$variables)
)

# Utilizar o desenho da Parte 2, com resposta válida
desenho_analise <- desenho_geral

# Preparar fatores e categorias da resposta
desenho_analise$variables <- desenho_analise$variables |>
  mutate(
    across(
      all_of(variaveis_explicativas),
      as.factor
    ),
    
    ausencia_binaria = factor(
      y > 0,
      levels = c(FALSE, TRUE),
      labels = c("Nenhuma ausência", "Alguma ausência")
    ),
    
    alguma_ausencia = as.numeric(y > 0),
    
    faixa_ausencia = factor(
      case_when(
        y == 0 ~ "Nenhuma ausência",
        y >= 1 & y <= 4 ~ "1 a 4 dias",
        y == 5 ~ "5 dias ou mais"
      ),
      levels = c(
        "Nenhuma ausência",
        "1 a 4 dias",
        "5 dias ou mais"
      )
    )
  )

################################################################################
# 3.1. TABELA DE CARACTERÍSTICAS SEGUNDO AUSÊNCIA ESCOLAR
################################################################################

tabela_caracteristicas <- desenho_analise |>
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
      acao_prevencao_bullying ~
        "Ações escolares de prevenção ao bullying",
      sexo ~ "Sexo",
      raca_cor ~ "Raça/cor",
      faixa_etaria ~ "Faixa etária",
      rede_ensino ~ "Rede de ensino"
    )
  ) |>
  gtsummary::add_overall(
    last = FALSE,
    col_label = "**Total**"
  ) |>
  gtsummary::add_p(
    test = all_categorical() ~ "svy.chisq.test"
  ) |>
  gtsummary::bold_labels() |>
  gtsummary::modify_caption(
    "**PeNSE 2024 — Características dos estudantes segundo a ocorrência de ausência escolar**"
  )

# Visualizar tabela
tabela_caracteristicas

# Exportar HTML
gt::gtsave(
  gtsummary::as_gt(tabela_caracteristicas),
  file.path(diretorio, "05_tabela_caracteristicas.html")
)

# Exportar CSV
readr::write_csv(
  gtsummary::as_tibble(
    tabela_caracteristicas,
    col_labels = FALSE
  ),
  file.path(diretorio, "05_tabela_caracteristicas.csv")
)

################################################################################
# 3.2. PREVALÊNCIA DE AUSÊNCIA POR COVARIÁVEL - FOREST PLOTS
################################################################################

# Estimar prevalências e IC 95% por categoria

calcula_prevalencias <- function(nome) {
  
  estimativas <- survey::svyby(
    formula = ~alguma_ausencia,
    by = stats::reformulate(nome),
    design = desenho_analise,
    FUN = survey::svymean,
    na.rm = TRUE,
    na.rm.by = TRUE,
    vartype = c("se", "ci"),
    drop.empty.groups = TRUE,
    keep.names = FALSE
  )
  
  intervalos <- confint(estimativas)
  
  tibble(
    variavel = nome,
    categoria = as.character(estimativas[[nome]]),
    percentual = 100 * as.numeric(coef(estimativas)),
    ic_inferior = pmax(0, 100 * intervalos[, 1]),
    ic_superior = pmin(100, 100 * intervalos[, 2])
  )
}


# Calcular estimativas para todas as covariáveis
prevalencias <- purrr::map_dfr(
  variaveis_explicativas,
  calcula_prevalencias
) |>
  mutate(
    variavel_rotulo = unname(rotulos_variaveis[variavel]),
    
    grupo = case_when(
      variavel %in% grupos_variaveis$sociodemograficas ~
        "sociodemograficas",
      
      variavel %in% grupos_variaveis$inseguranca ~
        "inseguranca",
      
      variavel %in% grupos_variaveis$protecao ~
        "protecao"
    ),
    
    # Identificador único para ordenar categorias nos painéis
    categoria_plot = paste(variavel, categoria, sep = "___"),
    
    categoria_plot = forcats::fct_reorder(
      categoria_plot,
      percentual
    )
  )

# Exportar estimativas
readr::write_csv(
  prevalencias |>
    select(
      variavel,
      categoria,
      grupo,
      percentual,
      ic_inferior,
      ic_superior
    ),
  file.path(diretorio, "06_prevalencias_ic95.csv")
)


################################################################################
# 3.2.1. FUNÇÃO PARA CONSTRUIR OS FOREST PLOTS
################################################################################

grafico_forest <- function(grupo_selecionado, titulo, arquivo) {
  
  dados_grafico <- prevalencias |>
    filter(grupo == grupo_selecionado) |>
    mutate(
      variavel_rotulo = factor(
        variavel_rotulo,
        levels = unname(
          rotulos_variaveis[
            grupos_variaveis[[grupo_selecionado]]
          ]
        )
      )
    )
  
  # Limite dinâmico do eixo X, com espaço para os rótulos
  limite_x <- min(
    100,
    ceiling(
      (max(dados_grafico$ic_superior, na.rm = TRUE) + 5) / 5
    ) * 5
  )
  
  grafico <- ggplot(
    dados_grafico,
    aes(
      x = percentual,
      y = categoria_plot
    )
  ) +
    geom_segment(
      aes(
        x = ic_inferior,
        xend = ic_superior,
        yend = categoria_plot
      ),
      color = "#31688E",
      linewidth = 0.9
    ) +
    geom_point(
      color = "#31688E",
      size = 3
    ) +
    geom_text(
      aes(
        x = ic_superior + 0.8,
        label = sprintf("%.1f%%", percentual)
      ),
      hjust = 0,
      size = 3.2,
      color = "#333333"
    ) +
    facet_wrap(
      ~variavel_rotulo,
      ncol = 1,
      scales = "free_y"
    ) +
    scale_y_discrete(
      labels = function(x) {
        stringr::str_remove(x, "^.*___")
      }
    ) +
    scale_x_continuous(
      breaks = seq(0, 100, by = 5),
      labels = function(x) paste0(x, "%"),
      expand = expansion(mult = c(0.01, 0))
    ) +
    coord_cartesian(
      xlim = c(0, limite_x)
    ) +
    labs(
      title = titulo,
      subtitle = "PeNSE 2024 | Prevalências ponderadas e IC 95%",
      x = "Estudantes com alguma ausência (%)",
      y = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(
        face = "bold",
        size = 13
      ),
      plot.subtitle = element_text(
        color = "grey40"
      ),
      strip.text = element_text(
        face = "bold",
        hjust = 0
      ),
      strip.background = element_rect(
        fill = "grey95",
        color = NA
      ),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(
        color = "grey88",
        linewidth = 0.3
      )
    )
  
  ggsave(
    file.path(diretorio, arquivo),
    grafico,
    width = 10,
    height = max(
      5,
      1.8 * length(grupos_variaveis[[grupo_selecionado]]) + 1.5
    ),
    dpi = 300
  )
  
  return(grafico)
}


################################################################################
# 3.2.2. FOREST PLOT - CARACTERÍSTICAS SOCIODEMOGRÁFICAS
################################################################################

forest_sociodemograficas <- grafico_forest(
  grupo_selecionado = "sociodemograficas",
  titulo = "Ausência escolar segundo características sociodemográficas",
  arquivo = "06_forest_sociodemograficas.png"
)

################################################################################
# 3.2.3. FOREST PLOT - EXPERIÊNCIAS DE INSEGURANÇA
################################################################################

forest_inseguranca <- grafico_forest(
  grupo_selecionado = "inseguranca",
  titulo = "Ausência escolar segundo experiências de violência",
  arquivo = "07_forest_inseguranca.png"
)

################################################################################
# 3.2.4. FOREST PLOT - FATORES DE PROTEÇÃO
################################################################################

forest_protecao <- grafico_forest(
  grupo_selecionado = "protecao",
  titulo = "Ausência escolar segundo fatores de proteção",
  arquivo = "08_forest_protecao.png"
)

# Visualizar os três gráficos
print(forest_sociodemograficas)
print(forest_inseguranca)
print(forest_protecao)

################################################################################
# 3.3. DISTRIBUIÇÃO DOS DIAS DE AUSÊNCIA - BARRAS EMPILHADAS
################################################################################

# Selecionar as quatro variáveis de violência e insegurança
variaveis_inseguranca <- grupos_variaveis$inseguranca

# Calcular distribuições ponderadas dentro de cada categoria
calcula_distribuicao <- function(nome) {
  
  # Tabela ponderada pelo desenho amostral
  tabela <- survey::svytable(
    stats::reformulate(c(nome, "faixa_ausencia")),
    design = desenho_analise
  )
  
  # Percentuais condicionais à categoria da covariável
  distribuicao <- prop.table(tabela, margin = 1)
  
  as.data.frame(distribuicao) |>
    as_tibble() |>
    transmute(
      variavel = nome,
      categoria = as.character(.data[[nome]]),
      faixa_ausencia = as.character(faixa_ausencia),
      percentual = 100 * Freq
    ) |>
    filter(is.finite(percentual))
}

# Calcular para as quatro variáveis
# Selecionar as quatro variáveis de violência e insegurança
variaveis_inseguranca <- grupos_variaveis$inseguranca

# Calcular distribuições ponderadas dentro de cada categoria
calcula_distribuicao <- function(nome) {
  
  tabela <- survey::svytable(
    stats::reformulate(c(nome, "faixa_ausencia")),
    design = desenho_analise
  )
  
  distribuicao <- prop.table(tabela, margin = 1)
  
  as.data.frame(distribuicao) |>
    as_tibble() |>
    transmute(
      variavel = nome,
      categoria = as.character(.data[[nome]]),
      faixa_ausencia = as.character(faixa_ausencia),
      percentual = 100 * Freq
    ) |>
    filter(is.finite(percentual))
}

distribuicao_inseguranca <- purrr::map_dfr(
  variaveis_inseguranca,
  calcula_distribuicao
) |>
  mutate(
    variavel_rotulo = unname(rotulos_variaveis[variavel]),
    
    faixa_ausencia = factor(
      faixa_ausencia,
      levels = c(
        "Nenhuma ausência",
        "1 a 4 dias",
        "5 dias ou mais"
      )
    )
  )

# Definir ordem desejada das categorias
ordem_categorias <- c(
  "Não",
  "Sim",
  "Nenhuma vez",
  "1 vez",
  "2 vezes",
  "3 ou mais vezes"
)

distribuicao_inseguranca <- distribuicao_inseguranca |>
  mutate(
    categoria = factor(categoria, levels = ordem_categorias)
  ) |>
  arrange(variavel_rotulo, categoria) |>
  mutate(
    categoria_plot = paste(variavel_rotulo, categoria, sep = "___")
  )

# Preservar a ordem dentro de cada painel
distribuicao_inseguranca <- distribuicao_inseguranca |>
  mutate(
    categoria_plot = factor(
      categoria_plot,
      levels = rev(unique(categoria_plot))
    )
  )

# Exportar tabela
readr::write_csv(
  distribuicao_inseguranca |>
    select(
      variavel,
      categoria,
      faixa_ausencia,
      percentual
    ),
  file.path(diretorio, "07_distribuicao_inseguranca.csv")
)



################################################################################
# 3.3.1. GRÁFICO DE BARRAS EMPILHADAS
################################################################################

cores_ausencia <- c(
  "Nenhuma ausência" = "#31688E",
  "1 a 4 dias" = "#35A79B",
  "5 dias ou mais" = "#D18A35"
)

grafico_barras_empilhadas <- ggplot(
  distribuicao_inseguranca,
  aes(
    x = percentual,
    y = categoria_plot,
    fill = faixa_ausencia
  )
) +
  geom_col(
    width = 0.7
  ) +
  geom_text(
    aes(
      label = if_else(
        percentual >= 5,
        sprintf("%.1f%%", percentual),
        ""
      )
    ),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 3.2
  ) +
  facet_grid(
    variavel_rotulo ~ .,
    scales = "free_y",
    space = "free_y"
  ) +
  scale_y_discrete(
    labels = function(x) {
      stringr::str_remove(x, "^.*___")
    }
  ) +
  scale_fill_manual(
    values = cores_ausencia,
    drop = FALSE
  ) +
  scale_x_continuous(
    breaks = seq(0, 100, 25),
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "Distribuição dos dias de ausência segundo experiências de violência",
    subtitle = "PeNSE 2024 | Percentuais ponderados por categoria",
    x = "Estudantes (%)",
    y = NULL,
    fill = "Dias de ausência"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "grey40"),
    strip.text.y = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  )

ggsave(
  file.path(diretorio, "09_ausencia_barras_empilhadas.png"),
  grafico_barras_empilhadas,
  width = 10,
  height = 8,
  dpi = 300
)

print(grafico_barras_empilhadas)

################################################################################
# 3.4. SALVAR OBJETOS DOS RESULTADOS
################################################################################

saveRDS(
  list(
    tabela_caracteristicas = tabela_caracteristicas,
    prevalencias = prevalencias,
    distribuicao_inseguranca = distribuicao_inseguranca,
    forest_sociodemograficas = forest_sociodemograficas,
    forest_inseguranca = forest_inseguranca,
    forest_protecao = forest_protecao,
    grafico_barras_empilhadas = grafico_barras_empilhadas
  ),
  file.path(diretorio, "resultados_perfil_associacoes.rds")
)

cat(
  "Análises de perfil e associações concluídas.\n",
  "Resultados exportados para:", diretorio, "\n"
)

