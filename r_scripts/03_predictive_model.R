library(DBI)
library(duckdb)
library(tidyverse)

# 1. Conectar ao DuckDB e criar features diretamente via SQL sobre as vagas reais
con <- dbConnect(duckdb::duckdb(), dbdir = "data/skills_warehouse.duckdb")

query_vagas_modelo <- "
SELECT 
    id_vaga,
    COALESCE(NULLIF(TRIM(pais), ''), 'Não informado') AS pais,
    COALESCE(NULLIF(TRIM(modelo_trabalho), ''), 'Não informado') AS modelo_trabalho,
    -- Features extraídas via SQL (busca textual case-insensitive)
    CASE WHEN LOWER(descricao_texto) LIKE '%sql%' THEN 1 ELSE 0 END AS pede_sql,
    CASE WHEN LOWER(descricao_texto) LIKE '%python%' THEN 1 ELSE 0 END AS pede_python,
    CASE WHEN REGEXP_MATCHES(LOWER(descricao_texto), '\\br\\b') THEN 1 ELSE 0 END AS pede_r,
    CASE WHEN LOWER(descricao_texto) LIKE '%excel%' THEN 1 ELSE 0 END AS pede_excel,
    -- Variável Alvo (Target): A vaga exige IA, Machine Learning ou NLP?
    CASE WHEN LOWER(descricao_texto) LIKE '%machine learning%' 
           OR LOWER(descricao_texto) LIKE '%artificial intelligence%'
           OR REGEXP_MATCHES(LOWER(descricao_texto), '\\b(ai|ml|nlp|llm)\\b') 
         THEN 1 ELSE 0 END AS exige_ia
FROM fato_demanda_vagas
"

df_vagas_modelo <- dbGetQuery(con, query_vagas_modelo)
dbDisconnect(con, shutdown = TRUE)

# 2. Preparar variáveis categóricas
df_vagas_modelo <- df_vagas_modelo %>%
  mutate(
    pais = as.factor(pais),
    modelo_trabalho = as.factor(modelo_trabalho)
  )

# 3. Separar em Treino (80%) e Teste (20%)
set.seed(42)
idx <- sample(1:nrow(df_vagas_modelo), size = 0.8 * nrow(df_vagas_modelo))
treino <- df_vagas_modelo[idx, ]
teste  <- df_vagas_modelo[-idx, ]

# 4. Montar a fórmula dinamicamente (evita erro se algum fator tiver apenas 1 nível)
variaveis <- c("pede_sql", "pede_python", "pede_r", "pede_excel")
if (n_distinct(treino$pais) >= 2) variaveis <- c(variaveis, "pais")
if (n_distinct(treino$modelo_trabalho) >= 2) variaveis <- c(variaveis, "modelo_trabalho")

formula_modelo <- as.formula(paste("exige_ia ~", paste(variaveis, collapse = " + ")))

# Treinar Regressão Logística
modelo_vagas_ia <- glm(
  formula_modelo,
  data = treino,
  family = binomial(link = "logit")
)

print("--- RESUMO DO MODELO PREDITIVO (VAGAS REAIS LINKEDIN) ---")
print(summary(modelo_vagas_ia))

# 5. Avaliar Acurácia na base de Teste
teste <- teste %>%
  mutate(
    prob_ia = predict(modelo_vagas_ia, newdata = teste, type = "response"),
    pred_ia = ifelse(prob_ia >= 0.5, 1, 0)
  )

acuracia <- mean(teste$pred_ia == teste$exige_ia)
print(paste0("Acurácia na base de teste: ", round(acuracia * 100, 2), "%"))

# 6. Simular cenários de stacks técnicas (usando o nível mais frequente de país/modelo)
pais_padrao <- names(sort(table(treino$pais), decreasing = TRUE))[1]
modelo_padrao <- names(sort(table(treino$modelo_trabalho), decreasing = TRUE))[1]

cenarios <- tibble(
  cenario = c(
    "Apenas Excel",
    "SQL + Excel",
    "SQL + R",
    "SQL + Python",
    "Stack Completa (SQL + Python + R)"
  ),
  pais = factor(pais_padrao, levels = levels(df_vagas_modelo$pais)),
  modelo_trabalho = factor(modelo_padrao, levels = levels(df_vagas_modelo$modelo_trabalho)),
  pede_excel = c(1, 1, 0, 0, 0),
  pede_sql   = c(0, 1, 1, 1, 1),
  pede_r     = c(0, 0, 1, 0, 1),
  pede_python= c(0, 0, 0, 1, 1)
) %>%
  mutate(
    prob_exigir_ia = predict(modelo_vagas_ia, newdata = ., type = "response")
  )

# 7. Gerar e salvar o gráfico para o README do GitHub
dir.create("assets", showWarnings = FALSE)

grafico_preditivo <- ggplot(cenarios, aes(x = reorder(cenario, prob_exigir_ia), y = prob_exigir_ia)) +
  geom_col(fill = "#2E8B57", width = 0.6) +
  geom_text(aes(label = scales::percent(prob_exigir_ia, accuracy = 0.1)), hjust = -0.15, size = 4) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  labs(
    title = "Probabilidade de uma Vaga Exigir Skills de IA por Stack Técnica",
    subtitle = "Previsão via Regressão Logística treinada com vagas reais do LinkedIn",
    x = NULL,
    y = "Probabilidade Prevista de Exigência de IA"
  ) +
  theme_minimal(base_size = 12)

print(grafico_preditivo)
ggsave("assets/probabilidade_ia_por_stack.png", plot = grafico_preditivo, width = 8, height = 4.5, dpi = 300)


