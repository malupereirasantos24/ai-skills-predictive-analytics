-- 1. Tabela Dimensão: Profissionais
CREATE TABLE IF NOT EXISTS dim_profissionais (
    id_profissional INTEGER PRIMARY KEY,
    area_atuacao VARCHAR,
    senioridade VARCHAR,
    anos_experiencia INTEGER,
    formacao VARCHAR
);

-- 2. Tabela Dimensão: Skills
CREATE TABLE IF NOT EXISTS dim_skills (
    id_skill INTEGER PRIMARY KEY,
    nome_skill VARCHAR,
    categoria VARCHAR,
    eh_skill_ia BOOLEAN
);

-- 3. Tabela Fato: Histórico de Aquisição de Skills
CREATE TABLE IF NOT EXISTS fato_historico_skills (
    id_registro INTEGER PRIMARY KEY,
    id_profissional INTEGER,
    id_skill INTEGER,
    data_aquisicao DATE,
    nivel_proficiencia INTEGER,
    FOREIGN KEY (id_profissional) REFERENCES dim_profissionais(id_profissional),
    FOREIGN KEY (id_skill) REFERENCES dim_skills(id_skill)
);

-- 4. Tabela Fato: Demanda de Vagas 
CREATE TABLE IF NOT EXISTS fato_demanda_vagas (
    id_vaga INTEGER PRIMARY KEY,
    data_publicacao DATE,
    cargo VARCHAR,
    salario_medio DECIMAL(10, 2),
    descricao_texto VARCHAR
);
