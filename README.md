# Sustentabilidade Fiscal e Regras de Endividamento
### Painel de Estados Brasileiros (2002–2024)

Replicação adaptada de **Abubakar, McCausland & Theodossiou (2025)**
para o contexto das finanças estaduais brasileiras.

> *Do debt relief and fiscal rules improve public debt sustainability?
> The experience of sub-Sahara African countries.*
> Journal of Policy Modeling, 47, 166–186.

Metodologia completa: [`docs/metodologia_tcc_v8.pdf`](docs/metodologia_tcc_v8.pdf)

---

## Sobre o projeto

Este trabalho aplica o framework de sustentabilidade fiscal de Bohn (1998)
a um painel de 25 estados brasileiros (2002–2024), investigando:

1. Se a dívida pública estadual é sustentável (Modelo I)
2. Quais fatores determinam a trajetória da dívida (Modelo II)
3. Como os tetos contratuais da Lei 9.496/97 condicionam o ajuste fiscal

A variável de identificação central é o **teto de comprometimento da RCL**
definido nos contratos de refinanciamento com a União (Lei 9.496/97),
que varia entre 12%, 13% e 15% entre estados — criando variação
exógena na intensidade da restrição fiscal.

---

## Resultados principais

| Coeficiente | Valor | Interpretação |
|---|---|---|
| β₁ (DCL/RCL, 2SLS) | +0.653*** | Condição de Bohn satisfeita — dívida é sustentável |
| β₂ (DCL×Teto, 2SLS) | −0.041*** | Teto mais alto atenua o ajuste fiscal |
| θ (Primário/RCL, LSDVC) | −0.933*** | Superávit reduz dívida em ~0.93pp por 1pp |
| ρ (DCL lag, LSDVC) | +0.984*** | Alta persistência da dívida estadual |

Todos os resultados são robustos à exclusão do período COVID (2020–2021)
e à substituição do crescimento do PIB pelo hiato do produto.

---

## Estrutura do projeto

```
tcc/
├── data/
│   ├── raw/                          # SICONFI, IBGE, BCB (coletados via API)
│   └── processed/
│       ├── panel_final_v5.csv        # Painel completo (575 obs × 45 vars)
│       └── panel_slim.csv            # Painel analítico (575 obs × 13 vars)
├── scripts/
│   ├── 01_collect_data.R             # Coleta: SICONFI, IBGE/SIDRA, BCB
│   ├── 02_build_panel.R              # Construção do painel analítico
│   ├── 03_model1_2sls.R              # Modelo I: MQO-EF + 2SLS
│   ├── 04_model2_lsdvc.R             # Modelo II: LSDVC + EF+DK
│   ├── 05_robustness.R               # Robustez: sem COVID, yvar
│   ├── 05b_robustness_binding.R      # Robustez: binding (Rob. 3)
│   ├── 06_tables.R                   # Tabela principal de resultados
│   ├── 07_figures.R                  # Figuras 1–4 (AER-style)
│   ├── 08_descriptive_stats.R        # Tabela 2: estatísticas descritivas
│   └── 09_pre_trend.R                # Figuras 5–6: análise de pré-tendência
├── output/
│   ├── tables/
│   │   ├── tabela_final.html         # Tabela 1 — resultados (7 colunas)
│   │   ├── tabela_final.tex          # Versão LaTeX
│   │   ├── tabela_final.txt          # Versão texto
│   │   ├── estatisticas_descritivas.html  # Tabela 2 — descritivas
│   │   └── estatisticas_descritivas.tex
│   └── figures/
│       ├── fig1_dcl_por_teto.{pdf,png}
│       ├── fig2_coef_mqo_vs_2sls.{pdf,png}
│       ├── fig3_primario_por_teto.{pdf,png}
│       ├── fig4_forest_robustez.{pdf,png}
│       ├── fig5_pre_trend.{pdf,png}
│       └── fig6_event_study.{pdf,png}
├── docs/
│   └── metodologia_tcc_v8.pdf        # Metodologia completa
├── run_all.R                         # Pipeline completo (9 etapas)
└── README.md
```

---

## Dados

| Variável | Fonte | Período |
|---|---|---|
| DCL, RCL, encargos | STN/SICONFI (API) | 2015–2024 |
| DCL, RCL histórica | STN/SISTN | 2002–2014 |
| Resultado primário histórico | FINBRA (manual) | 2002–2014 |
| PIB estadual real | IBGE/SCN (SIDRA) | 2002–2023 |
| Tetos contratuais | STN/DOU (manual) | Fixo (1997) |

25 estados — excluindo AP e TO (sem contrato Lei 9.496/97).

---

## Modelos

**Modelo I — Função de Reação Fiscal (2SLS)**

$$s_{it} = \alpha_i + \lambda_t + \beta_1 d_{i,t-1} + \beta_2 (d_{i,t-1} \times teto_i) + \gamma\, yvar_{it} + \varepsilon_{it}$$

Endógenas: $d_{i,t-1}$ e $d_{i,t-1} \times teto_i$
Instrumentos: $d_{i,t-2}$ e $d_{i,t-2} \times teto_i$

**Modelo II — Dinâmica da Dívida (LSDVC)**

$$d_{it} = \alpha_i + \rho\, d_{i,t-1} + \theta\, s_{it} + \phi\, gdp_{it} + u_{it}$$

Estimador: LSDVC (Bruno, 2005) — corrige viés de Nickell
System-GMM two-way FE inviável com N=25 (Arellano & Bond, 1991)

---

## Análises complementares

**Tabela 2 — Estatísticas descritivas** (`scripts/08_descriptive_stats.R`)

Três painéis: (A) amostra completa, (B) por grupo de teto (12/13/15%) com
testes t de Welch entre grupos, (C) matriz de correlação das variáveis do modelo.

**Figuras 5–6 — Análise de pré-tendência** (`scripts/09_pre_trend.R`)

- Figura 5: DCL/RCL média por grupo de teto com bandas de confiança (2002–2008)
- Figura 6: Event study completo (2002–2024), interação `year × teto`, referência em 2002

A identificação 2SLS baseia-se em variação cross-sectional do teto (fixo desde 1997),
não em variação temporal. O teste de pré-tendência é evidência adicional de comparabilidade
entre grupos antes do período de ajuste.

---

## Pacotes R

```r
install.packages(c("fixest", "plm", "xtlvr2",
                   "mFilter", "modelsummary",
                   "tidyverse", "sidrar", "rbcb",
                   "ggplot2", "patchwork", "scales"))
```

---

## Referências

- Abubakar, A.B., McCausland, W.D. & Theodossiou, I. (2025). *Journal of Policy Modeling*, 47, 166–186.
- Bohn, H. (1998). *Quarterly Journal of Economics*, 113(3), 949–963.
- Bruno, G. (2005). *Stata Journal*, 5(4), 473–500.
- Arellano, M. & Bond, S. (1991). *Review of Economic Studies*, 58(2), 277–297.
