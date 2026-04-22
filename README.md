# Sustentabilidade Fiscal e Regras de Endividamento
## Painel de Estados Brasileiros (2002–2024)

Replicação adaptada de Abubakar, McCausland & Theodossiou (2025) para finanças estaduais brasileiras.

### Modelos estimados
- Modelo I: Função de Reação Fiscal (2SLS)
- Modelo II: Dinâmica da Dívida (LSDVC)

### Resultados principais
- β₁ = +0.711 (p<0.001) — Condição de Bohn satisfeita
- β₂ = -0.046 (p<0.001) — Teto contratual atenua ajuste
- θ  = -0.937 (p<0.001) — Primário reduz dívida

### Fontes de dados
- STN/SICONFI, STN/SISTN, FINBRA, IBGE/SCN
- Tetos contratuais Lei 9.496/97 (STN/DOU)

### Pacotes R
fixest, plm, xtlvr2, mFilter, modelsummary
