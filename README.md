# market_risk_model
Este trabalho teve como objetivo analisar o risco de mercado de um portfólio composto por ações brasileiras por meio de modelos econométricos e técnicas de simulação.

Inicialmente, foram estimados modelos GARCH univariados para capturar a dinâmica da volatilidade individual dos ativos, cujos diagnósticos indicaram adequação na remoção de autocorrelação e heterocedasticidade condicional. Em seguida, utilizou-se o modelo DCC-GARCH para modelar a dependência dinâmica entre os retornos, permitindo a obtenção de matrizes de covariância condicionais ao longo do tempo.

A mensuração do risco foi realizada por meio do Value-at-Risk utilizando três abordagens: paramétrica, histórica e por simulação Monte Carlo. Observou-se que o VaR histórico apresentou menor sensibilidade a períodos de estresse, evidenciando resposta defasada a choques extremos. Por outro lado, o VaR paramétrico e o VaR via Monte Carlo apresentaram resultados bastante próximos, indicando consistência interna do modelo.

Adicionalmente, o Expected Shortfall foi estimado como medida complementar, evidenciando perdas médias significativamente superiores ao VaR em cenários extremos, o que reforça a importância da análise de cauda na gestão de risco.

Os testes de estresse, tanto históricos quanto hipotéticos, revelaram que, em cenários severos, as perdas do portfólio podem superar substancialmente aquelas estimadas pelos modelos probabilísticos, destacando a limitação de medidas baseadas exclusivamente em distribuições condicionais.

De forma geral, os resultados indicam que a diversificação proporciona redução significativa do risco em períodos normais, porém tal benefício se reduz substancialmente em momentos de estresse sistêmico, quando as correlações entre ativos se elevam.

Conclui-se que a utilização conjunta de modelos de volatilidade condicional, medidas probabilísticas de risco e testes de estresse constitui uma abordagem robusta para a gestão de risco de mercado, sendo fundamental para subsidiar decisões estratégicas em ambientes financeiros caracterizados por elevada incerteza.
