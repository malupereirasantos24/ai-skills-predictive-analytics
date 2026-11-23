library(DBI)
library(duckdb)
library(tidyverse)
library(tidytext)

# 1. Conectar ao DuckDB e puxar as vagas reais do LinkedIn
con <- dbConnect(duckdb::duckdb(), dbdir = "data/skills_warehouse.duckdb")
df_vagas <- dbGetQuery(con, "SELECT id_vaga, cargo, pais, modelo_trabalho, salario_medio, descricao_texto FROM fato_demanda_vagas")
dbDisconnect(con, shutdown = TRUE)

# 2. Stopwords em inglês + termos genéricos de RH para limpar o ruído
data("stop_words")
ruido_rh <- tibble(word = c(
  "experience", "years", "work", "working", "team", "ability", "skills",
  "required", "preferred", "including", "strong", "knowledge", "role",
  "opportunity", "equal", "employer", "status", "sexual", "orientation",
  "gender", "identity", "national", "origin", "veteran", "disability",
  "applicants", "qualifications", "responsibilities", "benefits", "job",
  "company", "business", "support", "provide", "using", "based", "related"
))
stopwords_en <- bind_rows(stop_words, ruido_rh)

# 3. Extração de Bigramas Reais das Vagas do LinkedIn
bigramas_reais <- df_vagas %>%
  unnest_tokens(bigrama, descricao_texto, token = "ngrams", n = 2) %>%
  separate(bigrama, into = c("palavra1", "palavra2"), sep = " ") %>%
  # Remove números e stopwords
  filter(!str_detect(palavra1, "^[0-9]+$"),
         !str_detect(palavra2, "^[0-9]+$"),
         !palavra1 %in% stopwords_en$word,
         !palavra2 %in% stopwords_en$word) %>%
  unite(bigrama, palavra1, palavra2, sep = " ") %>%
  count(bigrama, sort = TRUE)

print("--- TOP 20 BIGRAMAS REAIS NAS VAGAS DO LINKEDIN ---")
print(head(bigramas_reais, 20))

# 4. Detecção via Regex de Skills Específicas (SQL, Python, R, ML, AI, NLP) nas descrições
skills_detectadas <- df_vagas %>%
  mutate(
    SQL = str_detect(descricao_texto, regex("\\bsql\\b", ignore_case = TRUE)),
    Python = str_detect(descricao_texto, regex("\\bpython\\b", ignore_case = TRUE)),
    R = str_detect(descricao_texto, regex("\\br\\b", ignore_case = TRUE)),
    Excel = str_detect(descricao_texto, regex("\\bexcel\\b", ignore_case = TRUE)),
    `Machine Learning` = str_detect(descricao_texto, regex("machine learning|\\bml\\b", ignore_case = TRUE)),
    `Artificial Intelligence` = str_detect(descricao_texto, regex("artificial intelligence|\\bai\\b", ignore_case = TRUE)),
    NLP = str_detect(descricao_texto, regex("natural language processing|\\bnlp\\b", ignore_case = TRUE))
  ) %>%
  summarise(across(SQL:NLP, ~ mean(.x, na.rm = TRUE) * 100)) %>%
  pivot_longer(everything(), names_to = "Skill", values_to = "Percentual_Vagas") %>%
  arrange(desc(Percentual_Vagas))

print("--- % DE VAGAS REAIS DO LINKEDIN QUE PEDEM CADA SKILL ---")
print(skills_detectadas)

# 5. Gráfico dos Top 15 Bigramas Reais
grafico_nlp <- bigramas_reais %>%
  slice_max(n, n = 15) %>%
  ggplot(aes(x = reorder(bigrama, n), y = n)) +
  geom_col(fill = "#0A66C2") + # Azul oficial do LinkedIn
  coord_flip() +
  labs(
    title = "Skills requisitadas em vagas reais - Linkedin",
    subtitle = paste0("Base: ", nrow(df_vagas), " vagas  analisadas via NLP (USA, Canadá e África)"),
    x = NULL,
    y = "Frequência nas Descrições"
  ) +
  theme_minimal(base_size = 12)

print(grafico_nlp)

