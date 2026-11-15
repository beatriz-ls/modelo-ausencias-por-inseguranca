################################################################################
# Script1 - Tratamento dos dados
# Base original: PENSE2024_MICRODADOS
# Dicionario: Dicionario Microdados PeNSE2024.xlsx (IBGE)
################################################################################


library(tidyverse)

################################################################################
# 1. Leitura e seleção das variáveis
################################################################################

dados <- read.csv("data-raw/PENSE2024_MICRODADOS.csv") |>
  select(
    # Desenho amostral
    UF, ESCOLA, TURMA,
    IND_EXPANSAO, PESO_ALUNO_FREQ,
    ESTRATO_EXP, POSEST,
    
    # Variável resposta
    B09002,
    
    # Violência entre colegas
    B07007B, B07011A, B07012A, B07013,
    
    # Contexto familiar e socioeconômico
    B07004, B01008B,
    
    # Ambiente escolar
    E01P8510,
    
    # Características individuais e escolares
    B01001A, B01002, IDADE_AGREG, DEP_ADMIN
  ) |>
  rename(
    peso_amostral = PESO_ALUNO_FREQ,
    estrato = ESTRATO_EXP,
    escola = ESCOLA,
    turma = TURMA,
    uf = UF,
    indicador_expansao = IND_EXPANSAO,
    pos_estrato = POSEST,
    
    dias_ausencia_inseguranca_escola = B09002,
    bullying_sofrido = B07007B,
    exclusao_social = B07011A,
    agressao_fisica_colegas = B07012A,
    cyberbullying_sofrido = B07013,
    apoio_familiar = B07004,
    escolaridade_mae = B01008B,
    acao_prevencao_bullying = E01P8510,
    sexo = B01001A,
    raca_cor = B01002,
    faixa_etaria = IDADE_AGREG,
    rede_ensino = DEP_ADMIN
  )

################################################################################
# 2. Tratamento dos códigos especiais
################################################################################

# -1 = pulo do questionário
# -2 = abandono
# -9 = sem resposta

dados <- dados |>
  mutate(
    across(
      c(
        dias_ausencia_inseguranca_escola,
        bullying_sofrido,
        exclusao_social,
        agressao_fisica_colegas,
        cyberbullying_sofrido,
        apoio_familiar,
        escolaridade_mae,
        acao_prevencao_bullying,
        sexo,
        raca_cor,
        faixa_etaria,
        rede_ensino
      ),
      ~ replace(.x, .x %in% c(-1, -2, -9), NA_real_)
    )
  )

################################################################################
# 3. Variável resposta
################################################################################

# Códigos:
# 1 = 0 dias
# 2 = 1 dia
# 3 = 2 dias
# 4 = 3 dias
# 5 = 4 dias
# 6 = 5 dias ou mais

dados <- dados |>
  mutate(
    ausencia_limite_inferior = case_when(
      dias_ausencia_inseguranca_escola %in% 1:6 ~
        as.integer(dias_ausencia_inseguranca_escola) - 1L,
      TRUE ~ NA_integer_
    ),
    
    ausencia_censurada = case_when(
      dias_ausencia_inseguranca_escola == 6 ~ TRUE,
      dias_ausencia_inseguranca_escola %in% 1:5 ~ FALSE,
      TRUE ~ NA
    ),
    
    ausencia_alguma = case_when(
      dias_ausencia_inseguranca_escola == 1 ~ FALSE,
      dias_ausencia_inseguranca_escola %in% 2:6 ~ TRUE,
      TRUE ~ NA
    ),
    
    dias_ausencia_inseguranca_escola = factor(
      dias_ausencia_inseguranca_escola,
      levels = 1:6,
      labels = c(
        "0 dias",
        "1 dia",
        "2 dias",
        "3 dias",
        "4 dias",
        "5 dias ou mais"
      ),
      ordered = TRUE
    )
  )

################################################################################
# 4. Variáveis ordinais
################################################################################

dados <- dados |>
  mutate(
    bullying_sofrido = factor(
      bullying_sofrido,
      levels = 1:4,
      labels = c(
        "Nenhuma vez",
        "1 vez",
        "2 vezes",
        "3 ou mais vezes"
      ),
      ordered = TRUE
    ),
    
    exclusao_social = factor(
      exclusao_social,
      levels = 1:4,
      labels = c(
        "Nenhuma vez",
        "1 vez",
        "2 vezes",
        "3 ou mais vezes"
      ),
      ordered = TRUE
    ),
    
    agressao_fisica_colegas = factor(
      agressao_fisica_colegas,
      levels = 1:4,
      labels = c(
        "Nenhuma vez",
        "1 vez",
        "2 vezes",
        "3 ou mais vezes"
      ),
      ordered = TRUE
    ),
    
    apoio_familiar = factor(
      apoio_familiar,
      levels = 1:5,
      labels = c(
        "Nunca",
        "Raramente",
        "Às vezes",
        "Na maioria das vezes",
        "Sempre"
      ),
      ordered = TRUE
    ),
    
    faixa_etaria = factor(
      faixa_etaria,
      levels = 1:4,
      labels = c(
        "Menos de 13 anos",
        "13 a 15 anos",
        "16 e 17 anos",
        "18 anos ou mais"
      ),
      ordered = TRUE
    )
  )

################################################################################
# 5. Variáveis categóricas nominais
################################################################################

dados <- dados |>
  mutate(
    cyberbullying_sofrido = factor(
      cyberbullying_sofrido,
      levels = c(2, 1),
      labels = c("Não", "Sim")
    ),
    
    acao_prevencao_bullying = factor(
      acao_prevencao_bullying,
      levels = c(0, 1),
      labels = c("Não", "Sim")
    ),
    
    escolaridade_mae = factor(
      escolaridade_mae,
      levels = 1:8,
      labels = c(
        "Não estudou",
        "Fundamental incompleto",
        "Fundamental completo",
        "Médio incompleto",
        "Médio completo",
        "Superior incompleto",
        "Superior completo",
        "Não sabe"
      )
    ),
    
    sexo = factor(
      sexo,
      levels = c(1, 2),
      labels = c("Homem", "Mulher")
    ),
    
    raca_cor = factor(
      raca_cor,
      levels = 1:5,
      labels = c(
        "Branca",
        "Preta",
        "Amarela",
        "Parda",
        "Indígena"
      )
    ),
    
    rede_ensino = factor(
      rede_ensino,
      levels = c(1, 2),
      labels = c("Pública", "Privada")
    )
  )

# Identificação dos agrupamentos escolares
dados <- dados |>
  mutate(
    id_escola = interaction(
      uf,
      escola,
      drop = TRUE
    )
  )

################################################################################
# 6. Conferência do tratamento
################################################################################

dim(dados)

glimpse(dados)

# Distribuição da resposta
dados |>
  count(dias_ausencia_inseguranca_escola) |>
  mutate(
    percentual = round(100 * n / sum(n), 2)
  )

# Percentual de valores ausentes
resumo_na <- dados |>
  summarise(
    across(
      everything(),
      ~ mean(is.na(.x)) * 100
    )
  ) |>
  pivot_longer(
    everything(),
    names_to = "variavel",
    values_to = "percentual_na"
  ) |>
  arrange(desc(percentual_na))

print(resumo_na, n = Inf)

# Verificar a relação entre indicador de expansão e pesos ausentes
dados |>
  count(
    indicador_expansao,
    peso_ausente = is.na(peso_amostral)
  )

# Distribuição dos pesos amostrais
dados |>
  filter(!is.na(peso_amostral)) |>
  summarise(
    n = n(),
    peso_minimo = min(peso_amostral),
    peso_mediano = median(peso_amostral),
    peso_medio = mean(peso_amostral),
    peso_maximo = max(peso_amostral),
    soma_pesos = sum(peso_amostral)
  )

# Verificar os agrupamentos escolares e estratos
dados |>
  summarise(
    n_estudantes = n(),
    n_escolas = n_distinct(id_escola, na.rm = TRUE),
    n_estratos = n_distinct(estrato, na.rm = TRUE),
    n_pos_estratos = n_distinct(pos_estrato, na.rm = TRUE)
  )

# Verificar a quantidade de respostas válidas com pesos disponíveis
dados |>
  summarise(
    n_respostas_validas = sum(!is.na(ausencia_limite_inferior)),
    n_respostas_com_peso = sum(
      !is.na(ausencia_limite_inferior) &
        !is.na(peso_amostral) &
        peso_amostral > 0
    )
  )

################################################################################
# 7. Exportar dados tratados
################################################################################

dir.create("data", showWarnings = FALSE)

saveRDS(
  dados,
  "data/dados_pense2024_tratados.rds"
)