# market_risk_model
# 📊 Análise de Risco de Mercado com GARCH e DCC-GARCH

Projeto de modelagem quantitativa para mensuração de risco de mercado em um portfólio de ações brasileiras utilizando R.

## 📌 Objetivo

Aplicar modelos econométricos e simulação para estimar:
- Volatilidade
- Correlação dinâmica
- Value-at-Risk
- Expected Shortfall
- Stress Testing

## 📌 Portfólio

Ativos analisados:
- PETR4
- ITUB4
- VALE3

Período: 2016–2025

## 📌 Metodologia

- ARMA-GARCH(1,1) com distribuição t
- DCC-GARCH multivariado
- VaR: Paramétrico, Histórico e Monte Carlo
- Expected Shortfall
- Testes de estresse

## 📌 Principais Resultados

- VaR histórico subestima risco em crises
- Modelos condicionais reagem rapidamente
- Monte Carlo valida a modelagem
- Benefícios da diversificação diminuem sob estresse

## 📌 Tecnologias

- R
- rugarch
- rmgarch
- mvtnorm
- tidyverse

## 📌 Aplicações

- Risco de Mercado
- Asset Management
- Tesouraria
- Controle de Capital

---


