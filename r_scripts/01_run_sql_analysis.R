library(DBI)
library(duckdb)
library(tidyverse)

# 1. Conectar ao banco existente
con <- dbConnect(duckdb::duckdb(), dbdir = "data/skills_warehouse.duckdb")

# 2. Ler o arquivo SQL e separar as duas queries
sql_file <- read_file("sql/02_feature_engineering.sql")
queries <- str_split(sql_file, ";")[[1]] %>% str_trim()
queries <- queries[queries != ""]

# 3. Executar Query 1: Crescimento Trimestral
df_crescimento <- dbGetQuery(con, queries[1])
print("--- CRESCIMENTO TRIMESTRAL DE SKILLS DE IA ---")
print(head(df_crescimento, 15))

# 4. Executar Query 2: Coocorrência (Skills Ponte)
df_coocorrencia <- dbGetQuery(con, queries[2])
print("--- SKILLS TRADICIONAIS QUE PUXAM SKILLS DE IA ---")
print(head(df_coocorrencia, 15))

# 5. Desconectar
dbDisconnect(con, shutdown = TRUE)