library(DBI)
library(duckdb)
library(tidyverse)
library(lubridate)

set.seed(42)

# 1. Remover banco antigo para recriar limpo com os dados reais
if (file.exists("data/skills_warehouse.duckdb")) {
  file.remove("data/skills_warehouse.duckdb")
}

con <- dbConnect(duckdb::duckdb(), dbdir = "data/skills_warehouse.duckdb")

# 2. Ler e executar o schema SQL
ddl_sql <- read_file("sql/01_ddl_schema.sql")
comandos <- str_split(ddl_sql, ";")[[1]] %>% str_trim()
comandos <- comandos[comandos != ""]

for (cmd in comandos) {
  dbExecute(con, cmd)
}

# 3. Adicionar colunas extras na tabela fato_demanda_vagas
dbExecute(con, "ALTER TABLE fato_demanda_vagas ADD COLUMN empresa VARCHAR;")
dbExecute(con, "ALTER TABLE fato_demanda_vagas ADD COLUMN modelo_trabalho VARCHAR;")
dbExecute(con, "ALTER TABLE fato_demanda_vagas ADD COLUMN pais VARCHAR;")

# 4. Carregar e unificar os 3 datasets reais do LinkedIn
ler_vagas <- function(caminho, nome_pais) {
  read_csv(caminho, show_col_types = FALSE) %>%
    mutate(
      pais = nome_pais,
      salary = as.character(salary)
    )
}

vagas_brutas <- bind_rows(
  ler_vagas("data/archive/linkedin-jobs-usa.csv", "USA"),
  ler_vagas("data/archive/linkedin-jobs-canada.csv", "Canada"),
  ler_vagas("data/archive/linkedin-jobs-africa.csv", "Africa")
)

fato_demanda_vagas <- vagas_brutas %>%
  filter(!is.na(description), description != "") %>%
  distinct(title, company, description, .keep_all = TRUE) %>%
  transmute(
    id_vaga = row_number(),
    data_publicacao = as.Date(posted_date),
    cargo = title,
    salario_medio = parse_number(salary),
    descricao_texto = description,
    empresa = company,
    modelo_trabalho = onsite_remote,
    pais = pais
  )

# 5. Popular dim_skills
dim_skills <- tibble(
  id_skill = 1:10,
  nome_skill = c(
    "SQL", "Python", "R", "Statistics", "Excel",
    "Machine Learning", "AI / Artificial Intelligence",
    "NLP", "LLM / Generative AI", "Deep Learning"
  ),
  categoria = c(
    "Dados", "Programação", "Programação", "Análise", "Análise",
    "IA & ML", "IA & ML", "IA & ML", "IA & ML", "IA & ML"
  ),
  eh_skill_ia = c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE)
)

# 6. Gerar dim_profissionais
n_prof <- 1500
dim_profissionais <- tibble(
  id_profissional = 1:n_prof,
  area_atuacao = sample(
    c("Digital Analytics", "Data Science", "Computational Linguistics / NLP", "Data Engineering", "Business Intelligence"),
    n_prof, replace = TRUE, prob = c(0.30, 0.25, 0.15, 0.15, 0.15)
  ),
  senioridade = sample(
    c("Júnior", "Pleno", "Sênior", "Especialista"),
    n_prof, replace = TRUE, prob = c(0.25, 0.40, 0.25, 0.10)
  ),
  anos_experiencia = case_when(
    senioridade == "Júnior" ~ sample(1:2, n_prof, replace = TRUE),
    senioridade == "Pleno" ~ sample(3:5, n_prof, replace = TRUE),
    senioridade == "Sênior" ~ sample(6:9, n_prof, replace = TRUE),
    TRUE ~ sample(8:15, n_prof, replace = TRUE)
  ),
  formacao = sample(c("Exatas/TI", "Humanas/Letras", "Negócios"), n_prof, replace = TRUE)
)

# 7. Gerar fato_historico_skills
datas_possiveis <- seq(as.Date("2022-01-01"), as.Date("2026-10-31"), by = "day")

fato_historico_skills <- expand_grid(
  id_profissional = 1:n_prof,
  id_skill = 1:10
) %>%
  mutate(
    prob_ter = ifelse(id_skill <= 5, 0.60, 0.28),
    tem_skill = rbinom(n(), 1, prob_ter)
  ) %>%
  filter(tem_skill == 1) %>%
  mutate(
    id_registro = row_number(),
    data_aquisicao = if_else(
      id_skill > 5,
      sample(seq(as.Date("2023-06-01"), as.Date("2026-10-31"), by = "day"), n(), replace = TRUE),
      sample(datas_possiveis, n(), replace = TRUE)
    ),
    nivel_proficiencia = sample(1:5, n(), replace = TRUE)
  ) %>%
  select(id_registro, id_profissional, id_skill, data_aquisicao, nivel_proficiencia)

# 8. Gravar todas as tabelas no DuckDB
dbWriteTable(con, "dim_skills", dim_skills, append = TRUE)
dbWriteTable(con, "dim_profissionais", dim_profissionais, append = TRUE)
dbWriteTable(con, "fato_historico_skills", fato_historico_skills, append = TRUE)
dbWriteTable(con, "fato_demanda_vagas", fato_demanda_vagas, append = TRUE)

print(paste("Total de vagas reais carregadas do LinkedIn:", nrow(fato_demanda_vagas)))
dbDisconnect(con, shutdown = TRUE)
