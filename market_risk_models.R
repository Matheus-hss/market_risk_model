######### Analise de Risco de Mercado de uma carteira téorica 📊📈💰 ##########
#O Script abaixo implementa um model de analise de riscos de mercado
#para um portfólio teórico composto por 3 ativos (PETR4 + ITUB4 + VALE3)
#Primeiro é construido a base de dados com os seus devidos
#tratamentos (coleta feita na API da B3) em seguida é feita a modelagem, iremos
#utilizar 5 modelos comumente usados nesse tipo de analise.
#  🔹 Etapa 1 — Volatilidade Univariada
# 
# → EWMA, GARCH
# 📌 Objetivo: entender risco/volatilidade individual
# 
# 🔹 Etapa 2 — Volatilidade Multivariada
# 
# → DCC-GARCH, BEKK
# 📌 Objetivo: correlação dinâmica
# 
# 🔹 Etapa 3 — VaR
# 
# → Paramétrico
# → Histórico
# → Monte Carlo
# 
# 📌 Objetivo: mensurar perda extrema
# 
# 🔹 Etapa 4 — ES
# 
# → Expected Shortfall
# 📌 Objetivo: risco de cauda
#
#🔹 Etapa 5 — Stress Testing
#📌 Objetivo: verificar o comportamento em periodos de risco sistêmico

#Bibliotecas
library(rb3) #baixar dados oficiais da b3
library(dplyr) #limpeza e manipulação
library(lubridate) #trabalhar com datas
library(tidyr) #organização dos dados
library(ggplot2) #gráficos
library(tseries) #testes estatisticos
library(zoo) #séries temporais
library(FinTS) #teste ARCH
library(forecast) #auto arima, modelagem das médias
library(rugarch) #modelo GARCH
library(rmgarch) #modelo DCC
library(MASS)   # para mvrnorm
library(mvtnorm) #para simulação de monte carlo

####Etapa 1 — Volatilidade Univariada####
#Definir cache (evita ter que ficar baixando toda vez)
options(rb3.cachedir = "C:/rb3_cache")
dir.create("C:/rb3_cache", showWarnings = FALSE)

#Baixar dados da API da B3 de 2016 a 2025
fetch_marketdata("b3-cotahist-yearly", year = 2016:2025)

#Colocar os dados na memória do R
cotahist <- cotahist_get("yearly")

#Selecionar os ativo que eu quero
base <- cotahist |>
  filter(symbol %in% c("PETR4", "ITUB4", "VALE3")) |>
  select(refdate, symbol, close, volume) |>
  collect() |>
  rename(date = refdate) |>
  arrange(symbol, date) |>
  distinct(symbol, date, .keep_all = TRUE) |>
  filter(close > 0)

#Construção dos retornos log
base_ret <- base |>
  group_by(symbol) |>
  arrange(date) |>
  mutate(ret = log(close / lag(close))) |>
  filter(!is.na(ret)) |>
  ungroup()

#transformar em formato “wide”
portfolio <- base_ret |>
  select(date, symbol, ret) |>
  tidyr::pivot_wider(
    names_from = symbol,
    values_from = ret
  ) |>
  drop_na()

#Testar estacionariedade
adf.test(portfolio$PETR4)
adf.test(portfolio$ITUB4)
adf.test(portfolio$VALE3)
# Interpretação:
#   
# 👉 Rejeita H₀ com muita força
# 👉 Não tem raiz unitária
# 👉 Retornos estacionários
# 
# Ou seja:
#   
# ✔ PETR4 → OK
# ✔ ITUB4 → OK
# ✔ VALE3 → OK
# 
# pode usar GARCH sem restrição.


#Teste de Heterocedasticidade (ARCH)
ArchTest(portfolio$PETR4)
ArchTest(portfolio$ITUB4)
ArchTest(portfolio$VALE3)
#Os ativos apresentam comportamentos heterogêneos de volatilidade.
# 📈 PETR4
# p < 2.2e-16
# → Forte heterocedasticidade.
# Clustering clássico.
# 
# GARCH é obrigatório.
# 
# 📈 VALE3
# p < 2.2e-16
# → Também forte ARCH.
# Commodity + China → volatilidade.
# 
# GARCH faz todo sentido.
# 
# 🏦 ITUB4
# p = 0.9996
# Isso é muito alto.
# Interpretação:
#   
# ❗ Não há evidência de efeito ARCH.
# 
# Ou seja:
#   
# 👉 Volatilidade praticamente constante
# 👉 Pouco clustering
# 👉 Série “estável”

# | Ativo | ARCH | GARCH       |
# | ----- | ---- | ----------- |
# | PETR4 | Sim  | Obrigatório |
# | VALE3 | Sim  | Obrigatório |
# | ITUB4 | Não  | Opcional    |
  
#Analise de autocorrelação
Box.test(portfolio$PETR4, lag = 20, type = "Ljung-Box")
Box.test(portfolio$ITUB4, lag = 20, type = "Ljung-Box")
Box.test(portfolio$VALE3, lag = 20, type = "Ljung-Box")

# | Ativo | p-valor | Conclusão             |
# | ----- | ------- | --------------------  |
# | PETR4 | 3.9e-09 | ❌ Tem autocorrelação |
# | ITUB4 | 8.4e-04 | ❌ Tem autocorrelação |
# | VALE3 | 7.0e-06 | ❌ Tem autocorrelação |
  
#Consequência direta: GARCH puro NÃO basta
# Vai dar:
#   
# ❌ resíduos correlacionados
# ❌ volatilidade enviesada
# ❌ VaR errado
# 
# Então precisamos:
# 👉 Modelar a média primeiro.

#Modelo correto agora: ARMA-GARCH
#Vamos usar a função auto.arima para escolher o melhor modelo
#Antes vamos transformar os dados para o formato série temporal usando zoo
#não é obrigatório pois auto.arima só precisa de vetor numérico, porém melhora as analises

returns_zoo <-zoo(portfolio[, -1],
                  order.by = portfolio$date)
auto.arima(returns_zoo[,"PETR4"])
auto.arima(returns_zoo[,"ITUB4"])
auto.arima(returns_zoo[,"VALE3"])

# | Ativo | Modelo    | Interpretação          |
# | ----- | --------- | ---------------------- |
# | PETR4 | ARMA(2,1) | Memória forte + ajuste |
# | ITUB4 | AR(2)     | Dependência fraca      |
# | VALE3 | AR(4)     | Dinâmica mais longa    |
  
#Vamos usar GARCH em ITUB4 para deixar tudo padronizado
# Próximo passo:
#   
# 🔹 Ajustar GARCH univariado
# 🔹 Validar resíduos
# 🔹 Comparar com EWMA

#Vamos usar "rugarch"

#Primeiro para PETR4 -> ARMA(2,1)-GARCH(1,1) com t-Student fazemos a especificação
spec_petr4 <- ugarchspec(
  variance.model = list(
    model = "sGARCH",
    garchOrder = c(1,1)
  ),
  mean.model = list(
    armaOrder = c(2,1),
    include.mean = FALSE
  ),
  distribution.model = "std"
)
# ITUB4 -> ARMA(2)-GARCH(1,1) com t-Student
spec_itub4 <- ugarchspec(
  variance.model = list(
    model = "sGARCH",
    garchOrder = c(1,1)
  ),
  mean.model = list(
    armaOrder = c(2,0),
    include.mean = FALSE
  ),
  distribution.model = "std"
)
# VALE3 -> ARMA(4)-GARCH(1,1) com t-Student
spec_vale3 <- ugarchspec(
  variance.model = list(
    model = "sGARCH",
    garchOrder = c(1,1)
  ),
  mean.model = list(
    armaOrder = c(4,0),
    include.mean = FALSE
  ),
  distribution.model = "std"
)

# Agora fazemos a estimação dos modelos dos 3 ativos do nosso portfólio
#PETR4
fit_petr4 <- ugarchfit(
  spec = spec_petr4,
  data = portfolio$PETR4
)
#ITUB4
fit_itub4 <- ugarchfit(
  spec = spec_itub4,
  data = portfolio$ITUB4
)
#VALE3
fit_vale3 <- ugarchfit(
  spec = spec_vale3,
  data = portfolio$VALE3
)

#Agora vamos ao processo de validação dos modelos, primeiro vamos olhar os residuos
#Modelo removeu autocorrelação?
#Primeiro estraimos os residuos dos modelos
res_petr4 <- residuals(fit_petr4, standardize = TRUE)
res_itub4 <- residuals(fit_itub4, standardize = TRUE)
res_vale3 <- residuals(fit_vale3, standardize = TRUE)
#Agora vamos fazer teste de autocorrelação
Box.test(res_petr4, lag = 20, type = "Ljung-Box")
Box.test(res_itub4, lag = 20, type = "Ljung-Box")
Box.test(res_vale3, lag = 20, type = "Ljung-Box")

#Os resíduos padronizados não apresentaram autocorrelação, 
#indicando adequada especificação da média condicional.
# | Ativo | p-valor | Conclusão  |
# | ----- | ------- | ---------  |
# | PETR4 | 0.32    | ✅ OK      |
# | ITUB4 | 0.81    | ✅ OK      |
# | VALE3 | 0.85    | ✅ OK      |
  
#Vamos fazer teste ARCH novamente, agora nos residuos dos modelos
ArchTest(res_petr4)
ArchTest(res_itub4)
ArchTest(res_vale3)

# | Ativo | p-valor | Conclusão      |
# | ----- | ------- | -------------  |
# | PETR4 | 0.073   | ✅ OK (limite) |
# | ITUB4 | 1.000   | ✅ OK          |
# | VALE3 | 1.000   | ✅ OK          |
  
#Diagnóstico final dos modelos
# | Etapa            | Status  |
# | ---------------- | ------  |
# | Estacionariedade | ✅      |
# | Autocorrelação   | ✅      |
# | ARCH inicial     | ✅      |
# | ARMA             | ✅      |
# | GARCH            | ✅      |
# | Resíduos         | ✅      |
  
#Ver parâmetros estimados (sanity check)
show(fit_petr4)
#O modelo PETR4 -> ARMA(2,1)-GARCH(1,1) apresentou elevada persistência (α+β≈0.99) e caudas pesadas (ν≈4), 
#consistentes com a dinâmica da PETR4. Os resíduos não apresentaram autocorrelação 
#nem heterocedasticidade residual.
show(fit_itub4)
#ITUB4 apresentou baixa dependência na média e volatilidade menos persistente, 
#refletindo seu perfil defensivo, com distribuição próxima à normal.
show(fit_vale3)
#Os modelos univariados evidenciaram elevada persistência da volatilidade, 
#especialmente em PETR4 e VALE3, enquanto ITUB4 apresentou comportamento mais estável.

#Agora vamos comparar nossos modelos GARCH univariados com um modelo simples baseline (EWMA)
#EWMA — Exponentially Weighted Moving Average

#Definir Lambda
lambda <- 0.94 #RiskMetrics J.P Morgan 94% peso do passado

#Função EWMA
ewma_vol <- function(r, lambda = 0.94){
  
  v <- numeric(length(r))
  v[1] <- var(r, na.rm = TRUE)
  
  for(i in 2:length(r)){
    v[i] <- lambda * v[i-1] + (1 - lambda) * r[i-1]^2
  }
  
  sqrt(v)
} #Função para rodar modelo EWMA

#Calcular EWMA
ewma_petr4 <- ewma_vol(portfolio$PETR4)
ewma_itub4 <- ewma_vol(portfolio$ITUB4)
ewma_vale3 <- ewma_vol(portfolio$VALE3)

#Extrair volatilidade do GARCH
garch_petr4 <- sigma(fit_petr4)
garch_itub4 <- sigma(fit_itub4)
garch_vale3 <- sigma(fit_vale3)

#Comparação Visual
#PETR4
plot(portfolio$date, garch_petr4, type = "l",
     main = "PETR4: GARCH vs EWMA",
     ylab = "Volatilidade", xlab = "Data")
lines(portfolio$date, ewma_petr4, lty = 2, col = "blue")
legend("topright",
       legend = c("GARCH", "EWMA"),
       lty = c(1,2))
# ✔ forte cluster de volatilidade
# ✔ memória longa
# ✔ choques persistentes
# GARCH: α + β ≈ 0.99 -> α: Sensibilidade ao choque/ β: Persistência
# EWMA: λ = 0.94
#Não foram encontrados Artefatos
#📌 Artefato é um padrão falso criado pelo modelo ou pelo código — não pelos dados reais

#ITUB4
plot(portfolio$date, garch_itub4, type = "l",
     main = "ITUB4: GARCH vs EWMA",
     ylab = "Volatilidade", xlab = "Data")
lines(portfolio$date, ewma_itub4, lty = 2, col = "purple")
legend("topright",
       legend = c("GARCH", "EWMA"),
       lty = c(1,2))
#Observa-se elevação da volatilidade no período pré-crise covid-19, 
#indicando incorporação gradual das informações sobre a pandemia, ou seja, mercado
#começou a preficar o risco em novembro/2019. Como ambos os modelos indicam isso,
#não se trata de artefato.
plot(portfolio$date, portfolio$ITUB4, type="l")
abline(v = as.Date("2020-01-01"), col="red")

#VALE3
plot(portfolio$date, garch_vale3, type = "l",
     main = "VALE3: GARCH vs EWMA",
     ylab = "Volatilidade", xlab = "Data")
lines(portfolio$date, ewma_vale3, lty = 2, col = "yellow")
legend("topright",
       legend = c("GARCH", "EWMA"),
       lty = c(1,2))
#Observam-se dois episódios de elevada volatilidade, associados à desaceleração
#chinesa pré-pandemia e ao choque da COVID-19, refletindo a exposição da empresa 
#ao ciclo global de commodities.

####Etapa 2 — Volatilidade Multivariada####
#DCC-GARCH
#DCC = Dynamic Conditional Correlation
#GARCH univariado mede risco individual, DCC-GARCH mede risco sistêmico.
#Modelos univariados subestimam o risco de portfólio ao ignorarem 
#a dinâmica das correlações condicionais, por isso vamos usar DCC.
#Para DCC usamos o pacote "rmgarch".

#Matriz de retornos: formato Tempo x Ativos
returns_mat <- as.matrix(
  portfolio[,c("PETR4", "ITUB4", "VALE3")]
)

#Definir uma especificação GARCH base para todos
uspec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(0,0), include.mean = FALSE),
  distribution.model = "std"
) #Usamos ARMA(0,0) porque já limpamos a média antes e no DCC, foco é correlação.

#Replicamos para os ativos
mspec <- multispec(replicate(3,uspec)) # -> isso cria GARCH x 3

#Definir modelo DCC
dcc_spec <- dccspec(
  uspec = mspec,
  dccOrder = c(1,1),
  distribution = "mvt" #📌 Distribuição t multivariada.Ela generaliza a Student-t.
)
#Foi adotada a distribuição t multivariada para capturar dependência de cauda entre os ativos.
#std → cauda pesada individual
#mvt → cauda pesada conjunta

#Estimar o modelo
dcc_fit <- dccfit(
  spec = dcc_spec,
  data = returns_mat
)

#Resultado
show(dcc_fit)
# | Item     | Interpretação              |
# | -------- | -------------------------- |
# | mvt      | t multivariada             |
# | DCC(1,1) | Correlação dinâmica básica |
# | 3 séries | PETR4, ITUB4, VALE3        |
# | 2482     | Dias                       |

#Parametros alpha, beta e omega similares ao modelo univariado assim como o shape

#Parametros do DCC
# | Parâmetro | Papel             |
# | --------- | ----------------- |
# | a (dcca1) | impacto do choque |
# | b (dccb1) | persistência      |
#0.0118 + 0.9832 ≈ 0.995 -> 📌 Quase 1: Correlações extremamente persistentes,
#Confirmado por p < 0.001. Além disso mshape = 5.24 (cauda conjunta)
# | Valor | Cauda        |
# | ----- | ------------ |
# | <5    | Muito pesada |
# | 5–8   | Moderada✔    |
# | >10   | Normal       |

#No. Parameters : 18 -> 12 GARCH (4×3)/3 DCC/3 cauda
#📌 O portfólio apresenta elevada persistência tanto na volatilidade 
#quanto nas correlações, com forte dependência de cauda.O modelo DCC(1,1) 
#apresentou elevada persistência das correlações condicionais (a+b≈0.99), 
#indicando forte contágio em períodos de estresse, ou seja, a diversificação 
#entre ações reduz risco em períodos normais, mas é limitada em episódios de estresse sistêmico.
#Em períodos de estresse, a gestão de risco passa a ser dominada por controle de exposição 
#e não apenas por diversificação.

#Correlações Dinâmicas
r_t <-rcor(dcc_fit)

#Gráfico da correlação dinâmica
corredinamica_petr4xitub4 <- r_t[1,2,]
corredinamica_itub4xvale3 <- r_t[2,3,]
corredinamica_petr4xvale3 <- r_t[1,3,] #Pegue a correlação entre PETR4 e VALE3 em todos os dias
#Importante entender que rmgarch devolver um array tridimensional [i,j,t]
#i: ativo 1
#j: ativo 2
#t: tempo
#PETR4 x ITUB4
plot(portfolio$date, corredinamica_petr4xitub4, type = "l",
     main = "Correlação dinâmica entre PETR4 x ITUB4",
     ylab = "Correlação",
     xlab = "Data")
#ITUB4 x VALE3
plot(portfolio$date, corredinamica_itub4xvale3, type = "l",
     main = "Correlação dinâmica entre ITUB4 x VALE3",
     ylab = "Correlação",
     xlab = "Data")
#PETR4 x VALE3
plot(portfolio$date, corredinamica_petr4xvale3, type = "l",
     main = "Correlação dinâmica entre PETR4 x VALE3",
     ylab = "Correlação",
     xlab = "Data")

#Extrair covariância condicional
h_t <- rcov(dcc_fit) # pega o risco

#Validação do DCC
dcc_fit@mfit$convergence # verifica se é confiável
#Pegue a matriz de risco dinâmica e confirme que o modelo funcionou.



####Etapa 3 — VaR####
####✅ 1. VaR Paramétrico (Analítico / Condicional)####
#📌 Comparar “soma dos VaRs individuais” vs “VaR do portfólio”
#Passo 1 - Definir pesos
w <- c(1/3,1/3,1/3)

#Passo 2 - VaR univariado
alpha <- 0.05 #Nível de confiança (alpha). Em 95% dos dias, a perda será menor que o VaR.
#Com 5% de chance nos dias será maior.
# | alpha | Confiança |
# | ----- | --------- |
# | 0.10  | 90%       |
# | 0.05  | 95%       |
# | 0.01  | 99%       |

#PETR4  
var_petr4 <- qdist(
  "std", alpha, #distribuição e nivel de confiança
  mu = fitted(fit_petr4), #média condicional estimada pelo ARMA-GARCH
  sigma = sigma(fit_petr4),#volatilidade condicional do GARCH
  shape = coef(fit_petr4)["shape"] #graus de liberdade da t
) #qdist() vem do pacote rugarch.
#Resultado é VaR₁, VaR₂, VaR₃, ..., VaR_T, isto é, um valor para cada dia
#Cada valor responde: “Hoje, com 95%, a perda máxima da PETR4 é X%.”

#ITUB4
var_itub4 <- qdist(
  "std", alpha, 
  mu = fitted(fit_itub4), 
  sigma = sigma(fit_itub4),
  shape = coef(fit_itub4)["shape"]
)

#VALE3
var_vale3 <- qdist(
  "std", alpha, 
  mu = fitted(fit_vale3), 
  sigma = sigma(fit_vale3),
  shape = coef(fit_vale3)["shape"]
)

#Soma dos VaR´s individuais
var_soma <- var_petr4+var_itub4+var_vale3

#VaR multivariado
#h_t <- rcov(dcc_fit) Extrair a matriz de covariância, essa parte eu ja rodei acima

var_port <- rep(NA, dim(h_t)[3]) #Criar vetor para guardar o VaR

q <- qt(alpha, df = coef(dcc_fit)["[Joint]mshape"])

for(t in 1:length(var_port)){
  
  Sigma_t <- h_t[,,t]
  
  if(any(is.na(Sigma_t))) next   # pula se tiver NA
  
  sigma_p <- sqrt( t(w) %*% Sigma_t %*% w )
  
  var_port[t] <- q * sigma_p
} #Função para rodar VaR paramétrico do DCC

summary(var_port)
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.15643 -0.03800 -0.03128 -0.03476 -0.02623 -0.01802
summary(var_soma)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.41987 -0.11612 -0.09744 -0.10689 -0.08401 -0.06199 
#Comparação
plot(portfolio$date, var_port, type="l", col="blue", lwd=1,
     main="VaR: Portfólio vs Soma dos Individuais",
     ylab="VaR Diário",
     xlab="Data")

lines(portfolio$date, var_soma, lty=1, lwd=1, col="darkgreen")

legend("bottomright",
       legend=c("DCC (Portfólio)", "Soma Univariada"),
       col=c("blue","darkgreen"),
       lty=c(1,2),
       lwd=1,
       bty="n")
DB <- 1 - var_port / var_soma
summary(DB)
#Observa-se redução significativa do risco quando consideradas correlações dinâmicas, 
#evidenciando benefícios da diversificação.

####✅ 2. VaR Histórico (Historical Simulation)####

#Passo 1 - Construir o retorno diário
ret_port <- returns_mat %*% w

#Passo 2 - VaR histórico simples (Isso é o VaR histórico “fixo”, um número só)
var_hist <- quantile(ret_port, probs = alpha)
var_hist

#Passo 3 - VaR Histórico Rolling (janela deslizante)
#Aqui vamos usar 1 ano ~ 250 dias
window <- 250
var_hist_roll <- rep(NA, length(ret_port))
for (t in window:length(ret_port)){
  var_hist_roll[t] <- quantile(
    ret_port[(t-window+1):t],
    probs = alpha
  )
} #Função para rodar VaR Historico Rolling
summary(var_hist_roll)
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
#-0.04726 -0.03042 -0.02635 -0.02631 -0.02124 -0.01395      249 

#Gráfico do Var Histórico com Janela sem os NA´s
valid <- complete.cases(var_hist_roll)

plot(portfolio$date[valid],
     var_hist_roll[valid],
     type = "l",
     col = "black",
     main = "VaR Histórico (Rolling)",
     ylab = "VaR",
     xlab = "Data")

#Comparação com VaR do DCC
lines(portfolio$date[valid],
      var_port[valid],
      col = "blue")

legend("bottomright",
       legend = c("Histórico", "DCC"),
       col = c("black","blue"),
       lty = 1,
       bty = "n")
#O VaR histórico apresentou menor sensibilidade a choques extremos,
#evidenciando resposta defasada em períodos de estresse.


####✅ 3. VaR por Monte Carlo####
#No Monte Carlo não estimamos o VaR direto, fazemos milhares de simulações de futuros
#possiveis e vemos quanto perdemos nos piores cenários. Monte Carlo é o mais flexivel

#Parâmetro fundamental: número de simulações
# | N      | Qualidade |
# | ------ | --------- |
# | 1.000  | Fraco     |
# | 5.000  | Ok        |
# | 10.000 | Bom       |
# | 50.000 | Banco     |

#Vamos Usar N = 5000

# Pegar o que precisamos do DCC
#Graus de Liberdade
df <- coef(dcc_fit)[grep("mshape", names(coef(dcc_fit)))]
df <- as.numeric(df)

#Para fazer a simulação vamos precisar de duas funções
#1) Função para simular um dia
sim_mc_day <- function(Sigma, w, df, N = 5000){
  
  # Simular retornos multivariados t
  sims <- rmvt(N, sigma = Sigma, df = df) #cria os dias de simulação
  
  # Retorno do portfólio
  ret_p <- sims %*% w #calcular prejuízo
  
  # VaR 5%
  quantile(ret_p, 0.05) #escolher os piores
}
#2) Função para rodar a simulação de monte carlo

N <- 5000 #Para cada dia, vou imaginar 5000 futuros possíveis

var_mc <- rep(NA, dim(h_t)[3]) #uma lista vazia, do tamanho do tempo -> [NA,NA,...,NA]

for(t in 1:length(var_mc)){ #vamos repetir o processo para cada dia: D1 -> simula, D2 -> simula, etc...
  
  Sigma_t <- h_t[,,t] #pegue como estava o risco no dia t (h_t guarda o risco de todos os dias)

  if(any(is.na(Sigma_t))) next #se esse dia tiver problema, pule
  #🎲 Agora vem a simulação de verdade
  var_mc[t] <- sim_mc_day(
    Sigma = Sigma_t,
    w = w,
    df = df,
    N = N
  )
}

summary(var_mc)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.16109 -0.03801 -0.03114 -0.03476 -0.02614 -0.01700 

#Gráfico de comparação entre os métodos
valid <- complete.cases(var_port, var_hist_roll, var_mc)

plot(portfolio$date[valid], var_port[valid],
     type="l", col="blue", lwd=1,
     ylab="VaR", xlab="Data",
     main="Comparação dos Métodos de VaR")

lines(portfolio$date[valid], var_hist_roll[valid],
      col="black", lty=1, lwd=1)

lines(portfolio$date[valid], var_mc[valid],
      col="red", lty=1, lwd=1)

legend("bottomright",
       legend=c("DCC Paramétrico","Histórico","Monte Carlo"),
       col=c("blue","black","red"),
       lty=c(1,1,1),
       lwd=1,
       bty="n")
#A simulação Monte Carlo apresentou maior sensibilidade a eventos extremos.
#Os resultados do VaR por simulação Monte Carlo corroboram os obtidos pelo método paramétrico, 
#indicando consistência do modelo

####Etapa 4 — ES####
#ES tambem vamos calcular em 3 etapas:Histórico, Monte Carlo e Paramétrico

#### ES Histórico ####
ES_hist <- mean(
  ret_port[ret_port <= quantile(ret_port, alpha)]
)

ES_hist #-0.04589742

#### ES Monte Carlo ####
ES_mc_day <- function(Sigma, w, df, N = 5000){
  
  sims <- mvtnorm::rmvt(N, sigma = Sigma, df = df)
  
  ret_p <- sims %*% w
  
  VaR <- quantile(ret_p, 0.05)
  
  mean(ret_p[ret_p <= VaR])
} #Função para ES Monte Carlo

ES_mc <- rep(NA, dim(h_t)[3])

for(t in 1:length(ES_mc)){
  
  Sigma_t <- h_t[,,t]
  
  if(any(is.na(Sigma_t))) next
  
  ES_mc[t] <- ES_mc_day(
    Sigma = Sigma_t,
    w = w,
    df = df,
    N = 5000
  )
} #Loop no tempo

summary(ES_mc)
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.21986 -0.05442 -0.04428 -0.04945 -0.03694 -0.02447

#O Expected Shortfall evidencia perdas médias significativamente superiores ao VaR 
#em cenários extremos, indicando a relevância da análise de cauda

##### ES Paramétrico ####
#👉 O ES Monte Carlo já é o ES paramétrico validado

#Em dias ruins, eu perco cerca de 3,5% na média. Em dias muito ruins, cerca de 5%.
#Em colapso, até 22%.

#Basileia III / Bacen exige:

#✔ ES 97.5%
#✔ Horizonte 10 dias
#✔ Stressed ES

#Gráfico do ES
valid <- complete.cases(ES_mc)

plot(portfolio$date[valid], ES_mc[valid],
     type="l", col="brown", lwd=1,
     main="Expected Shortfall (Monte Carlo)",
     ylab="ES",
     xlab="Data")

#Comparação do VaR com o ES
plot(portfolio$date[valid], var_mc[valid],
     type="l", col="blue", lwd=1,
     ylab="Risco",
     xlab="Data",
     main="VaR vs ES")

lines(portfolio$date[valid], ES_mc[valid],
      col="red", lwd=1)

legend("bottomright",
       legend=c("VaR","ES"),
       col=c("blue","red"),
       lwd=2,
       bty="n")

#### Etapa 5 - Stress Testing ####
#Existem três principais
#1 - Stress Histórico: impeachment, covid, crise no mercado financeiro, etc...
worst <- sort(ret_port)[1:10] #Piores Dias
data.frame(
  date = portfolio$date[worst],
  loss = ret_port[worst]
)
#Stress histórico
stress_hist <- min(ret_port)
stress_hist #Maior perda real: -19.6%

#2 - Stress Hipotético: invento cenários
#Aqui vamos inventar um cenário de crise
# | Ativo | Choque |
# | ----- | ------ |
# | PETR4 | -20%   |
# | ITUB4 | -15%   |
# | VALE3 | -25%   |

shock <- c(-0.20, -0.15, -0.25)
stress_hyp <- sum(w*shock)
stress_hyp #Perda de 20% em cenário de crise e 30% em cenário catastrófico

#Testando varios cenários
scenarios <- rbind(
  c(-0.10,-0.08,-0.12),
  c(-0.20,-0.15,-0.25),
  c(-0.30,-0.25,-0.35)
)
apply(scenarios, 1, function(x) sum(w * x))
  
#3 - Stress via Simulação: usa Monte Carlo, mas olha só os piores 0.01%
#Agora pegamos o quantil extremo de 0.01 (daria para pegar cenario pior como 0.001%)
stress_mc <- quantile(var_mc, 0.01)
stress_mc #Perda de 9% nos piores 1%

#Comparação
stress_results <- c(
  Historico = stress_hist,
  Hipotetico = stress_hyp,
  MonteCarlo = quantile(var_mc, 0.01)
)

stress_results

#Historico    Hipotetico    MonteCarlo.1% 
#-0.19634694   -0.20000000   -0.09310231 

#O stress histórico evidencia perdas superiores às estimadas pelo modelo probabilístico, 
#reforçando a necessidade de cenários hipotéticos.

##### Conclusão ####
#O estudo mostra que modelos baseados em volatilidade condicional e correlação dinâmica 
#capturam melhor o risco do portfólio do que abordagens puramente históricas.
#O VaR paramétrico e o Monte Carlo apresentaram resultados consistentes, enquanto o 
#VaR histórico subestimou perdas em períodos de estresse.
#O Expected Shortfall revelou perdas significativamente maiores nos extremos, e
#videnciando a relevância da análise de cauda.
# Além disso, os testes de estresse indicam que, em cenários severos, a diversificação 
#perde parte de sua eficácia, reforçando a necessidade de gestão ativa da exposição ao risco.