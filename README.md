# Sustentabilidade Fiscal e Regras de Endividamento
### Painel de Estados Brasileiros (2002–2024)

Replicação adaptada de **Abubakar, McCausland & Theodossiou (2025)**
para o contexto das finanças estaduais brasileiras.

> *Do debt relief and fiscal rules improve public debt sustainability?
> The experience of sub-Sahara African countries.*
> Journal of Policy Modeling, 47, 166–186.

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
├── output/
│   ├── panel_final_v5.csv        # Painel completo (575 obs × 45 vars, PIB 2002–2023)
│   ├── tabela_final.html         # Tabela de resultados (6 colunas)
│   ├── tabela_final.tex          # Versão LaTeX
│   └── tabela_final.txt          # Versão texto
├── collect_*.R                   # Scripts de coleta de dados
├── robustness.R                  # Verificações de robustez
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

## Pacotes R

```r
install.packages(c("fixest", "plm", "xtlvr2",
                   "mFilter", "modelsummary",
                   "tidyverse", "sidrar", "rbcb"))
```

---

## Referências

- Abubakar, A.B., McCausland, W.D. & Theodossiou, I. (2025). *Journal of Policy Modeling*, 47, 166–186.
- Bohn, H. (1998). *Quarterly Journal of Economics*, 113(3), 949–963.
- Bruno, G. (2005). *Stata Journal*, 5(4), 473–500.
- Arellano, M. & Bond, S. (1991). *Review of Economic Studies*, 58(2), 277–297.
