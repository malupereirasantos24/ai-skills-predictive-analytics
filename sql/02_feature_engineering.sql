-- QUERY 1: Evolução Trimestral e Taxa de Crescimento (Window Functions)
WITH aquisicoes_trimestrais AS (
  SELECT 
  s.nome_skill,
  s.categoria,
  DATE_TRUNC('quarter', f.data_aquisicao) AS trimestre,
  COUNT(DISTINCT f.id_profissional) AS novos_profissionais
  FROM fato_historico_skills f
  INNER JOIN dim_skills s ON f.id_skill = s.id_skill
  WHERE s.eh_skill_ia = TRUE
  GROUP BY 1, 2, 3
)
SELECT 
nome_skill,
trimestre,
novos_profissionais,
-- Pega o valor do trimestre anterior para a mesma skill
LAG(novos_profissionais) OVER (PARTITION BY nome_skill ORDER BY trimestre) AS trimestre_anterior,
-- Calcula o crescimento percentual trimestre contra trimestre (QoQ)
ROUND(
  ((novos_profissionais - LAG(novos_profissionais) OVER (PARTITION BY nome_skill ORDER BY trimestre)) * 100.0) 
  / NULLIF(LAG(novos_profissionais) OVER (PARTITION BY nome_skill ORDER BY trimestre), 0), 
  2) AS crescimento_qoq_pct,
-- Acumulado histórico de profissionais com aquela skill
SUM(novos_profissionais) OVER (PARTITION BY nome_skill ORDER BY trimestre) AS total_acumulado
FROM aquisicoes_trimestrais
ORDER BY nome_skill, trimestre;


-- QUERY 2: Coocorrência de Skills (Self-Join para identificar "Skills Ponte")
WITH base_skills AS (
  SELECT 
  f.id_profissional,
  s.nome_skill,
  s.eh_skill_ia,
  f.data_aquisicao
  FROM fato_historico_skills f
  INNER JOIN dim_skills s ON f.id_skill = s.id_skill
)
SELECT 
b1.nome_skill AS skill_base,
b2.nome_skill AS skill_ia_adquirida,
COUNT(DISTINCT b1.id_profissional) AS qtd_profissionais,
ROUND(AVG(DATE_DIFF('day', b1.data_aquisicao, b2.data_aquisicao)), 0) AS media_dias_para_aprender
FROM base_skills b1
INNER JOIN base_skills b2 
ON b1.id_profissional = b2.id_profissional
AND b1.eh_skill_ia = FALSE      -- Começa com uma skill tradicional (ex: SQL, R, Python)
AND b2.eh_skill_ia = TRUE       -- Adquire uma skill de IA
AND b2.data_aquisicao >= b1.data_aquisicao -- Garante ordem cronológica
GROUP BY 1, 2
ORDER BY qtd_profissionais DESC;