# IMPORTAÇÃO DOS DADOS
setwd("SEU_DIRETORIO_AQUI")
dados <- read.table("base_dados.txt", h = TRUE, dec = ",")
str(dados)
attach(dados)
names(dados)

#CARREGAR PACOTES 
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  dplyr,
  hnp,
  lmtest,     # Testes de diagnostico em séries temporais
  tidyr,
  sandwich,
  tidyverse,   # Manipulação de dados e plots
  corrplot,    # Matriz de correlação visual
  MASS,        # Função stepAIC
  car,         # Teste VIF para multicolinearidade
  caret,     # Avaliaçao de modelos
  glmnet,    # LASSO, Ridge e Elastic Net
  tseries,     # Testes de séries temporais
  skimr        # Estatística descritiva completa
)

#ANÁLISE DESCRITIVA

skim(dados)

#saldo (resultado - variável de interesse)

hist(saldo)

boxplot(saldo)
boxplot(saldo~ano) #deu erro
# Boxplot do saldo original em níveis por ano
boxplot(saldo ~ ano, data = dados, main = "Boxplot do Saldo por Ano", xlab = "Ano", ylab = "Saldo")

plot(saldo)
lines(saldo)

ggplot(dados, aes(x = ano, y = saldo)) +
  geom_line(color = "darkblue", size = 1) +
  geom_point(color = "darkblue") +
  theme_minimal() +
  labs(title = "Evolução do Resultado Previdenciário (2000-2024)",
       x = "Ano", y = "Saldo Previdenciário")

#CORRELAÇÃO

#se os dados forem normais, usamos correlação de pearson
#se os dados forem não normais, usamos correlação de spearman

#Teste de normalidade para todas as colunas numéricas
resultados_normalidade <- dados %>%
  dplyr::select(where(is.numeric)) %>% 
  pivot_longer(cols = everything(), names_to = "Variavel", values_to = "Valor") %>%
  filter(!is.na(Valor)) %>% 
  group_by(Variavel) %>%
  summarise(
    N = n(),
    Estatistica_W = shapiro.test(Valor)$statistic,
    p_valor = shapiro.test(Valor)$p.value,
    .groups = "drop"
  ) %>%
  mutate(
    Normal = if_else(p_valor > 0.05, "Sim (Normal)", "Não (Não Normal)")
  ) %>%
  arrange(p_valor)

print(resultados_normalidade, n = Inf)

#Como Saldo não é normal e será confrontada com todas as outras variáveis, 
#usaremos a correlação de spearman nesta etapa da análise

base <- data.frame(tx_f,env,exp_65,di,ipca,selic,selic_real,pib_nom,pib_real,ppc,ppc_real,rmdpc,rmdpc_real,tx_d,num_cont,nmmc,num_ben,receita,despesa,saldo)

cor_matrix <- cor(base, use = "complete.obs", method = "spearman")

corrplot(cor_matrix, 
         method = "color", 
         type = "upper", 
         tl.col = "black", 
         tl.srt = 45,
         addCoef.col = "black", 
         number.cex = 0.6,
         title = "Matriz de Correlação das Variáveis", 
         mar = c(0, 0, 2, 0))

#saldo com todas as outras
#não normal (spearman): saldo
#ipca apresenta empates
cor.test(saldo,tx_f, method = "spearman") #relação forte e positiva
cor.test(saldo,env, method = "spearman") #relação forte e negativa
cor.test(saldo,exp_65, method = "spearman") #relação moderada e negativa
cor.test(saldo,di, method = "spearman") #relação forte e negativa
cor.test(saldo,ipca, method = "spearman") #relação moderada e positiva
cor.test(saldo,selic, method = "spearman") #relação moderada e positiva
cor.test(saldo,selic_real, method = "spearman") #relação moderada e positiva
cor.test(saldo,pib_nom, method = "spearman") #relação forte e negativa
cor.test(saldo,pib_real, method = "spearman") #relação forte e negativa
cor.test(saldo,ppc, method = "spearman") #relação forte e negativa
cor.test(saldo,ppc_real, method = "spearman") #relação forte e negativa
cor.test(saldo,rmdpc, method = "spearman") #relação forte e negativa
cor.test(saldo,rmdpc_real, method = "spearman") #relação forte e negativa
cor.test(saldo,tx_d, method = "spearman") #relação moderada e positiva
cor.test(saldo,num_cont, method = "spearman") #relação forte e negativa
cor.test(saldo,nmmc, method = "spearman") #relação forte e negativa
cor.test(saldo,num_ben, method = "spearman") #relação forte e negativa
cor.test(saldo,receita, method = "spearman") #relação forte e negativa
cor.test(saldo,despesa, method = "spearman") #relação forte e negativa

#demográficas
#não normal (spearman): di
demograficas <- data.frame(tx_f,env,exp_65,di)
cor(demograficas, method = "spearman")
cor.test(tx_f,env) #relação forte e negativa
cor.test(tx_f,exp_65) #relação moderada e negativa
cor.test(tx_f,di, method = "spearman") #relação forte e negativa
cor.test(env,exp_65) #relação moderada e positiva
cor.test(env,di, method = "spearman") #relação forte e positiva
cor.test(exp_65,di, method = "spearman") #relação forte e positiva

#previdenciarias
#não normal (spearman): tx_d, nmmc, num_cont, despesa
previdenciarias <- data.frame(tx_d,num_cont,nmmc,num_ben,receita,despesa)
cor(previdenciarias, method = "spearman")
cor.test(tx_d,num_cont, method = "spearman") #relação forte e negativa
cor.test(tx_d,nmmc, method = "spearman") #relação moderada e negativa
cor.test(tx_d,num_ben, method = "spearman") #relação moderada e negativa
cor.test(tx_d,receita, method = "spearman") #relação moderada e negativa
cor.test(tx_d,despesa, method = "spearman") #relação moderada e negativa
cor.test(num_cont,nmmc, method = "spearman") #relação forte e positiva
cor.test(num_cont,num_ben, method = "spearman") #relação forte e positiva
cor.test(num_cont,receita, method = "spearman") #relação forte e positiva
cor.test(num_cont,despesa, method = "spearman") #relação forte e positiva
cor.test(nmmc,num_ben, method = "spearman") #relação forte e positiva
cor.test(nmmc,receita, method = "spearman") #relação forte e positiva
cor.test(nmmc,despesa, method = "spearman") #relação forte e positiva
cor.test(num_ben,receita) #relação forte e positiva
cor.test(num_ben,despesa, method = "spearman") #relação forte e positiva
cor.test(receita,despesa, method = "spearman") #relação forte e positiva

#economicas
#não normal (spearman): ipca, rmdpc, rmdpc_real
economicas <- data.frame(ipca,selic,selic_real,pib_nom,pib_real,ppc,ppc_real,rmdpc_real)
cor(economicas)

cor.test(ipca,selic, method = "spearman") #moderada e positiva
cor.test(ipca,selic_real, method = "spearman") #sem relação
cor.test(ipca,pib_nom, method = "spearman") #sem relação
cor.test(ipca,pib_real, method = "spearman") #sem relação
cor.test(ipca,ppc, method = "spearman") #sem relação
cor.test(ipca,ppc_real, method = "spearman") #sem relação
cor.test(ipca,rmdpc, method = "spearman") #sem relação
cor.test(ipca,rmdpc_real, method = "spearman") #sem relação 

cor.test(selic,selic_real) #relação forte e positiva
cor.test(selic,pib_nom) #relação moderada e negativa
cor.test(selic,pib_real) #relação moderada e negativa
cor.test(selic,ppc) #relação moderada e negativa
cor.test(selic,ppc_real) #relação moderada e negativa
cor.test(selic,rmdpc, method = "spearman") #relação moderada e negativa
cor.test(selic,rmdpc_real, method = "spearman") #relação moderada e negativa

cor.test(selic_real,pib_nom) #relação moderada e negativa
cor.test(selic_real,pib_real) #relação moderada e negativa
cor.test(selic_real,ppc) #relação moderada e negativa
cor.test(selic_real,ppc_real) #relação moderada e negativa
cor.test(selic_real,rmdpc, method = "spearman") #relação moderada e negativa
cor.test(selic_real,rmdpc_real, method = "spearman") #relação moderada e negativa

cor.test(pib_nom,pib_real) #relação forte e negativa
cor.test(pib_nom,ppc) #relação forte e positiva
cor.test(pib_nom,ppc_real) #relação forte e positiva
cor.test(pib_nom,rmdpc, method = "spearman") #relação forte e positiva
cor.test(pib_nom,rmdpc_real, method = "spearman") #relação forte e positiva

cor.test(pib_real,ppc) #relação forte e positiva
cor.test(pib_real,ppc_real) #relação forte e positiva
cor.test(pib_real,rmdpc, method = "spearman") #relação forte e positiva
cor.test(pib_real,rmdpc_real, method = "spearman") #relação forte e positiva

cor.test(ppc,ppc_real) #relação forte e positiva
cor.test(ppc,rmdpc, method = "spearman") #relação forte e positiva
cor.test(ppc,rmdpc_real, method = "spearman") #relação forte e positiva

cor.test(ppc_real,rmdpc, method = "spearman") #relação forte e positiva
cor.test(ppc_real,rmdpc_real, method = "spearman") #relação forte e positiva

cor.test(rmdpc,rmdpc_real, method = "spearman") #relação forte e positiva

######################################

#MODELOS DE REGRESSÃO

#Modelo completo (com todas as variáveis)
dados_sentido <- data.frame(saldo,di,tx_f,exp_65,num_ben,num_cont,tx_d,num_cont,nmmc,num_ben,ipca,selic,pib_real,ppc_real,rmdpc_real) 
modelo_completo <- lm(saldo ~ ., data = dados_sentido)
summary(modelo_completo)
#R² = 1, regressão espúria

#Seleção stepwise via AIC (Bidirecional)
modelo_step_aic <- stepAIC(modelo_completo, direction = "both", trace = FALSE)
#Como R² quase igual 1, regressão espúria

#Seleção Stepwise via BIC (Mais penalizador, recomendado para N pequeno)
n <- nrow(dados)
modelo_step_bic <- stepAIC(modelo_completo, direction = "both", k = log(n), trace = FALSE)
#Como R² quase igual 1, regressão espúria

#Teste de normalidade e shapiro
shapiro.test((residuals(modelo_step_bic))) #resíduos normais

#Teste de multicolinearidade (vif > 5-10 indica problema)
vif(modelo_step_bic) #há multicolinearidade

#Teste de autocorrelação de durbin-watson
dwtest(modelo_step_bic) #não há autocorrelação

#Gráficos diagnósticos dos resíduos
par(mfrow = c(2, 2))
plot(modelo_step_aic)
par(mfrow = c(1, 1))
plot(modelo_step_bic)

#Para resolver o problema de multicolinearidade, o número de variáveis no modelo
#foi reduzido, mantendo-se o critério de permanecer no modelo ao menos uma 
#variável de cada grupo de variáveis (demográfica, econômica e previdenciária)

#Mais modelos de regressão linear parcimoniosos
#Selecionando no máximo 3 a 4 variáveis chave para respeitar N=25

#Modelo 1:
modelo1 <- lm(saldo ~ di + num_ben + num_cont + tx_d + ppc_real + selic + ipca, data = dados_sentido)
summary(modelo1)
#R² quase igual 1, regressão espúria

#Teste de normalidade e shapiro
shapiro.test((residuals(modelo1))) #resíduos normais

#Teste de multicolinearidade (vif > 5-10 indica problema)
vif(modelo1) #há multicolinearidade

#Teste de autocorrelação de durbin-watson
dwtest(modelo1) #não há autocorrelação

#Modelo 2:
modelo2 <- lm(saldo ~ di + num_cont + ipca, data = dados_sentido)
summary(modelo2)
#R² quase igual 1, regressão espúria

#Teste de normalidade e shapiro
shapiro.test((residuals(modelo2))) #resíduos normais

# Teste de multicolinearidade (VIF > 5-10 indica problema)
vif(modelo2) #não há multicolinearidade

# Teste de autocorrelação de durbin-atson
dwtest(modelo2) #há autocorrelação

#Modelo 3:
modelo3 <- lm(saldo ~ di + tx_d + ipca, data = dados_sentido)
summary(modelo3)
#R² quase igual 1, regressão espúria

#Teste de normalidade e shapiro
shapiro.test((residuals(modelo3))) #resíduos normais

# Teste de multicolinearidade (vif > 5-10 indica problema)
vif(modelo3) #há multicolinearidade

# Teste de autocorrelação de durbin-watson
dwtest(modelo3) #há autocorrelação

#Por causa da autocorrelação, tentaremos um modelo de regressão com correção 
#de Newey-West

#Testar os coeficientes do modelo com a matriz HAC (Newey-West)
coeftest(modelo3, vcov = vcovHAC)

#Entendendo a Mudança
#Sem a correção (summary(modelo_mco2)): O R usa a variância tradicional do MQO.
#Com a autocorrelação positiva que identificamos ($\text{DW} \approx 1.00$), 
#Os erros-padrão ficam subestimados e os p-valores ficam falsamente baixos
#(aumentando o risco de falso positivo / Erro Tipo I).
#Com a correção (coeftest(modelo_mco2, vcov = vcovHAC)): Os coeficientes 
#($\hat{\beta}$) permanecem exatamente os mesmos, mas os erros-padrão aumentam 
#para refletir a incerteza real provocada pela autocorrelação. 
#Isso torna os testes de hipótese ($t$-test) estatisticamente válidos.

coeftest(modelo3, vcov = NeweyWest)

#Um ponto de atenção urgente no modelo:$R^2 = 0,9739$ muito elevado + DW low ($\approx 1,0032$):
#Quando um modelo de séries temporais apresenta um $R^2$ extremamente alto acompanhado de um teste de Durbin-Watson baixo, há um fortíssimo indício de regressão espúria (variáveis não estacionárias com tendência comum).
#Como resolver/melhorar a modelagem? Reescalar as variáveis: Dividir o saldo por $1.000.000$ (para rodar em "Milhões de R$"). Isso deixará os coeficientes em uma escala legível e fácil de interpretar como elasticidade.

#Criar a variável de saldo em Milhões de Reais
dados_sentido$saldo_milhoes <- dados_sentido$saldo / 1e6

#Re-estimar o modelo MCO com a nova escala
modelo_milhoes <- lm(saldo_milhoes ~ di + tx_d + pib_real, data = dados_sentido)

#Visualizar o novo resumo
summary(modelo_milhoes)

#Teste de normalidade e shapiro
shapiro.test((residuals(modelo_milhoes))) #resídudos não normais

# Teste de multicolinearidade (vif > 5-10 indica problema)
vif(modelo_milhoes)  #há multicolinearidade

# Teste de autocorrelação de durbin-watson
dwtest(modelo_milhoes) #há autocorrelação

#Como os problemas não foram resolvidos, vamos ajustar o modelo em primeiras
#diferenças (usando diff) para corrigir a autocorrelação

#Garantir que a variável saldo esteja em milhões
dados_sentido$saldo_milhoes <- dados_sentido$saldo / 1e6

#Estimar o modelo em primeiras diferenças
modelo_diff1 <- lm(diff(saldo_milhoes) ~ diff(di) + diff(nmmc) + diff(ipca), data = dados_sentido)
modelo_diff2 <- lm(diff(saldo_milhoes) ~ diff(di) + diff(num_cont) + diff(ipca), data = dados_sentido)

#Visualizar o resumo dos coeficientes
summary(modelo_diff1)
summary(modelo_diff2)

#Interpretação do modelo
#A transição para o modelo em primeiras diferenças mudou drasticamente (e para melhor) a dinâmica e a veracidade da sua regressão.1. Resumo da Interpretação GeralO modelo anterior ($R^2 = 0,97$) sofria de uma regressão espúria (um falso ajuste perfeito gerado pelas tendências das séries). Agora, o novo $R^2 = 0,3165$ ($31,65\%$) é muito mais realista para dados macroeconômicos em variação, indicando que as mudanças nas taxas explicam cerca de $31,6\%$ das variações anuais do saldo.2. Interpretação Detalhada dos CoeficientesComo as variáveis foram diferenciadas ($\Delta$) e o saldo está em Milhões de Reais:Intercepto ($1.446$, $p = 0.8522$):
#Sem relevância estatística. Indica que, na ausência de alterações nas taxas, a variação média anual do saldo é nula.diff(di) ($-47.883$, $p = 0.0577$):Marginalmente significante a $10\%$ ($p < 0,10$).Interpretação: A cada aumento de 1 ponto percentual na variação da taxa DI de um ano para o outro (ex: de $10\%$ para $11\%$), a variação do saldo reduz em R$ 47,88 milhões.diff(tx_d) ($-71.092$, $p = 0.8230$):Não significante ($p > 0,10$).Interpretação: Ao isolar o efeito da taxa DI e do IPCA, a variação da taxa D não tem um efeito estatisticamente relevante na variação do saldo. Além disso, o erro-padrão elevado ($313.673$) sugere forte multicolinearidade com o DI.diff(ipca) ($277.094$, $p = 0.0531$):Marginalmente significante a $10\%$ ($p < 0,10$), quase atingindo $5\%$.Interpretação: A cada aumento de 1 ponto percentual na variação do IPCA de um ano para o outro, a variação do saldo aumenta em R$ 277,09 milhões.3. Estatísticas Globais do ModeloResidual Standard Error ($16.66$): O erro médio das previsões de variação anual do saldo é de R$ 16,66 milhões (uma escala infinitamente mais controlável do que os $17,6$ milhões originais não diferenciados).F-statistic ($3.087$, $p = 0.05054$): O modelo é globalmente significante ao nível de $10\%$ (quase $5\%$), confirmando que as variáveis juntas ajudam a explicar a variação do saldo.

#Teste de normalidade e shapiro
shapiro.test((residuals(modelo_diff1))) #resíduos normais
shapiro.test((residuals(modelo_diff2))) #resíduos normais

#Teste de multicolinearidade (vif > 5-10 indica problema)
vif(modelo_diff1) #não há multicolinearidade
vif(modelo_diff2) #não há multicolinearidade

#Reavaliar o teste de durbin-watson
dwtest(modelo_diff1) #sem autocorrelação
dwtest(modelo_diff2) #sem autocorrelação

#Pronto, resolveu os problemas das premissas do modelo

#A substituição da variável pela num_cont (número de contratos) melhorou consideravelmente o seu modelo. A regressão passou de um ajuste mediano para uma modelagem estatisticamente robusta e com excelente capacidade explicativa.1. Diagnóstico dos Pressupostos (O Modelo é Válido?)Antes de interpretar os coeficientes, os testes de diagnóstico confirmam que o modelo atende aos pressupostos do MQO:Autocorrelação (Durbin-Watson = 2.3798, $p = 0,7345$): Como o $p\text{-valor} > 0,05$, não rejeitamos a hipótese nula. Não há autocorrelação nos resíduos (o problema original foi 100% resolvido com as primeiras diferenças).Normalidade dos Resíduos (Shapiro-Wilk W = 0.935, $p = 0,1262$): Como $p > 0,05$, os resíduos seguem distribuição normal, validando os testes $t$ e $F$.Multicolinearidade (VIF $\approx 1,03$ a $1,08$): Valores de VIF bem abaixo de 5 (praticamente próximos de 1) indicam ausência de multicolinearidade entre as variáveis explicativas.2. Ajuste Global do Modelo$R^2$ Ajustado = $0,6334$ ($63,34\%$): O modelo salta de $21,4\%$ (modelo anterior) para $63,34\%$ na capacidade de explicar a variação anual do saldo.Estatística F ($14.25$, $p = 0,0000338$): O conjunto das variáveis é altamente significante ($p < 0,001$).Erro Padrão dos Resíduos ($11,38$): O erro médio de estimativa caiu de $16,66$ para R$ 11,38 milhões.3. Interpretação dos CoeficientesIntercepto ($-14,65$, $p = 0,0263$): Estatisticamente significante a $5\%$. Indica que, mantendo as variações de DI, número de contratos e IPCA constantes (nulas), há uma tendência média de redução do saldo de R$ 14,65 milhões por ano.diff(num_cont) ($7,553 \times 10^{-6} = 0,000007553$, $p = 0,00011$):Altamente significante a $0,1\%$ ($p < 0,001$). É o principal motor explicativo do modelo.Interpretação: Para cada 100.000 contratos novos adicionados na variação anual ($\Delta \text{num\_cont} = +100.000$), a variação do saldo aumenta em R$ 0,755 milhão (ou ~ R$ 755,3 mil). Se a variação for de 1 milhão de contratos, o impacto no saldo é de +R$ 7,55 milhões.diff(ipca) ($200,7$, $p = 0,0401$):Estatisticamente significante a $5\%$ ($p < 0,05$).Interpretação: A cada 1 ponto percentual de aumento na variação do IPCA de um ano para o outro ($\Delta \text{IPCA} = +1\%$), a variação do saldo aumenta em R$ 200,70 milhões.diff(di) ($-32,13$, $p = 0,0625$):Marginalmente significante a $10\%$ ($p < 0,10$).Interpretação: A cada 1 ponto percentual de aumento na variação da taxa DI ($\Delta \text{DI} = +1\%$), a variação do saldo reduz em R$ 32,13 milhões.ConclusãoEste modelo em primeiras diferenças está pronto e bem especificado. O número de contratos capturou a expansão de escala da operação, enquanto o IPCA e a DI capturam os efeitos macroeconômicos de inflação e taxa de juros sobre o saldo.

#Teste de Breusch-Pagan para homocedasticidade dos resíduos
bptest(modelo_diff1) #resíduos não homocedásticos
bptest(modelo_diff2) #resíduos não homocedásticos
#O teste de Breusch-Pagan apresentou um $p\text{-valor} = 0,04148$ (abaixo do limiar padrão de $0,05$), o que significa que rejeita-se a hipótese nula de homocedasticidade ao nível de 5% de significância. Há indício de heterocedasticidade nos resíduos (a variância dos erros não é perfeitamente constante ao longo do tempo).O que isso altera no seu modelo?Os coeficientes permanecem não viesados: As estimativas pontuais dos parâmetros ($\hat{\beta}$) continuam corretas e imparciais.Os erros-padrão originais perdem a validade: A heterocedasticidade afeta a estimativa dos erros-padrão do MQO, o que pode alterar os valores de $t$ e os $p\text{-valores}$ de significância das variáveis.Como corrigir perfeitamente (Solução Definitiva)A solução padrão na literatura econométrica para corrigir a heterocedasticidade sem alterar os coeficientes do modelo é aplicar a matriz de erros-padrão robustos de White / HC3 (ou HAC / Newey-West):

#Por que usar o type = "HC3"? Em amostras pequenas ($N \approx 24$ observações), o ajustador HC3 de White é o mais recomendado, pois corrige pequenas distorções de alavancagem em amostras reduzidas.O que observar: Veja se as variáveis diff(num_cont) e diff(ipca) continuam estatisticamente significantes após a correção dos erros-padrão.  
# Erros-padrão robustos à heterocedasticidade (White / HC3)
coeftest(modelo_diff1, vcov = vcovHC(modelo_diff1, type = "HC3"))
coeftest(modelo_diff2, vcov = vcovHC(modelo_diff2, type = "HC3"))

#Análise Detalhada dos Coeficientes Robustosdiff(num_cont) ($7,5527 \times 10^{-6}$, $p = 0,000486$):Altamente significante a $0,1\%$ ($p < 0,001$).Interpretação: É o efeito mais sólido do modelo. Para cada 100.000 contratos adicionados na variação anual, a variação do saldo aumenta em R$ 0,755 milhão (~ R$ 755,3 mil). Para cada 1 milhão de contratos novos, o saldo aumenta em +R$ 7,55 milhões.diff(ipca) ($200,71$, $p = 0,01415$):Estatisticamente significante a $5\%$ ($p < 0,05$).Interpretação: Com a correção robusta, o IPCA se fortaleceu no modelo (seu $p$-valor caiu de $0,040$ para $0,014$). A cada 1 ponto percentual de aumento na variação do IPCA ($\Delta \text{IPCA} = +1\%$), a variação do saldo aumenta em R$ 200,71 milhões.diff(di) ($-32,129$, $p = 0,31469$):Não significante ($p > 0,10$).Interpretação: Após controlar para o número de contratos e o IPCA — e corrigir a heterocedasticidade —, as variações na taxa DI não apresentam impacto estatisticamente relevante na variação do saldo.Intercepto ($-14,65$, $p = 0,11891$):Não significante a $5\%$ ($p > 0,05$).Interpretação: O termo constante perdeu a significância com os erros robustos, o que indica que não há uma tendência autônoma de queda do saldo quando as variações das explicativas são nulas.Conclusão do ModeloVocê possui agora um modelo ajustado em primeiras diferenças, com $R^2 \approx 68,1\%$, sem autocorrelação ($\text{DW} \approx 2,38$), com resíduos normais, sem multicolinearidade e 100% protegido contra heterocedasticidade. Os resultados provam que a dinâmica do saldo é movida fundamentalmente pelo volume de contratos e pela variação do IPCA.

#Teste de Erro de Especificação (RESET Test de Ramsey)
#Avalia se o modelo omitiu variáveis não lineares importantes (ex: termos quadráticos) ou se a forma funcional linear é a ideal.

resettest(modelo_diff1, power = 2:3, type = "regressor")
resettest(modelo_diff2, power = 2:3, type = "regressor")

# Gráfico dos pontos influentes
plot(modelo_diff1, which = 4)
plot(modelo_diff2, which = 4)

# Identificar observações com Distância de Cook > 0.5 ou 1
cooks.distance(modelo_diff1)[cooks.distance(modelo_diff1) > 0.5] #o modelo tem outliers
cooks.distance(modelo_diff2)[cooks.distance(modelo_diff2) > 0.5] #o modelo tem outliers

#1. Resumo da Interpretação do modelo com outliers

#Como os p-valores são maiores do que o nível de significância padrão de 0,05 (45,39% > 5% e 13,88% > 5%), não rejeitamos a hipótese nula (H0).
#Isso indica que não há evidência de erro de especificação: a forma funcional linear é adequada e não há omissão de variáveis não lineares importantes (como potências quadráticas ou cúbicas das variáveis explicativas).

#2. Detalhamento do Teste

#2.1 Hipóteses:
#H0 (Hipótese Nula): O modelo está corretamente especificado (a relação linear entre as variáveis é adequada).
#H1 (Hipótese Alternativa): O modelo sofre de erro de especificação (omitiu termos não lineares ou combinações das variáveis).

#2.2 Parâmetros do Teste:
#power = 2:3: O teste incluiu termos quadráticos (X²) e cúbicos (X³ dos regressores para verificar se eles teriam poder explicativo oculto.
#RESET = 1.9701 e 1.061: Estatística do teste baseada na distribuição F.
#p-value = 0.1388 e 0.4539: Probabilidade de obter esse resultado sob H0. Como p>0,05, o modelo passa no teste com folga.

#3. Diagnóstico final dos modelos econométricos

#Com este último resultado, a bateria de diagnósticos do modelo em primeiras diferenças está 100% concluída:
  
#Autocorrelação (Durbin-Watson): Aprovado (DW≈2,38, p=0,7345 e DW≈2,48, p=0,8109).

#Normalidade dos Resíduos (Shapiro-Wilk): Aprovado (p=0,1262 e 0,06455).

#Multicolinearidade (VIF): Aprovado (VIF<1,08 e 1,18).

#Forma Funcional (Ramsey RESET): Aprovado (p=0,1388 e 0,4539).

#Heterocedasticidade (Breusch-Pagan / HC3): Identificada e corrigida com sucesso via matriz de erros-padrão robustos de White/HC3.

#Os modelos estão econometricamente perfeitos para ser apresentado ou defendido.

#Retirando outliers para realizar análise de sensibilidade:

# 1. Criar a base filtrada sem a linha 25 (ano de 2024)
dados_sem_2024 <- dados_sentido[-25, ]

# 2. Rodar o modelo com o novo data frame
modelo1_sem_24 <- lm(
  diff(saldo_milhoes) ~ diff(di) + diff(nmmc) + diff(ipca), 
  data = dados_sem_2024
)

modelo2_sem_24 <- lm(
  diff(saldo_milhoes) ~ diff(di) + diff(num_cont) + diff(ipca), 
  data = dados_sem_2024
)

# 3. Visualizar o resumo do modelo
summary(modelo1_sem_24)
summary(modelo2_sem_24)

# 4. Avaliar com os erros-padrão robustos (HC3)
coeftest(modelo1_sem_24, vcov = vcovHC(modelo1_sem_24, type = "HC3"))
coeftest(modelo2_sem_24, vcov = vcovHC(modelo2_sem_24, type = "HC3"))

#1. Resumo da Comparação (Com vs. Sem 2024)
#Ao remover a observação influente 24 (ano de 2024), o modelo ficou substancialmente mais forte e consistente:
#R² Ajustado: Saltou de 63,3% para 77,6% (o poder explicativo aumentou significativamente).
#Erro-Padrão dos Resíduos: Caiu de 11,38 para 8,83 milhões (previsões muito mais precisas).
#Todas as 3 variáveis ficaram estatisticamente significantes a 5% (mesmo após a correção robusta HC3).

#2. Análise dos Coeficientes Robustos (Sem 2024)diff(num_cont) ($7,0229 \times 10^{-6}$, $p = 0,000316$):Altamente significante a $0,1\%$ ($p < 0,001$).Para cada 100.000 novos contratos, a variação anual do saldo aumenta em R$ 0,702 milhão (~ R$ 702,3 mil).Para 1 milhão de novos contratos, o ganho no saldo é de +R$ 7,02 milhões.diff(ipca) ($191,66$, $p = 0,0273$):Estatisticamente significante a $5\%$ ($p < 0,05$).A cada 1 ponto percentual de aumento na variação do IPCA ($\Delta \text{IPCA} = +1\%$), a variação do saldo aumenta em R$ 191,66 milhões.diff(di) ($-55,542$, $p = 0,02336$):Agora é estatisticamente significante a $5\%$ ($p < 0,05$) (no modelo completo, essa variável perdia significância devido ao ruído do ano de 2024).A cada 1 ponto percentual de aumento na variação da taxa DI ($\Delta \text{DI} = +1\%$), a variação do saldo reduz em R$ 55,54 milhões.

#Por fim, sugere-se uma Abordagem B (Manter 2000–2024 com Dummy de Impulso para 2024):
#Se você precisar obrigatoriamente incluir o ano de 2024 na amostra, crie uma variável dummy (d_24 = 1 para 2024 e 0 para os outros anos) no modelo completo. Isso isolará o choque atípico de 2024 sem distorcer o efeito do DI.

# 1. Definir a dummy de impulso (1 em 2024, 0 nos outros anos)
dados_sentido$d_2024 <- 0
dados_sentido$d_2024[25] <- 1

# 2. Estimar o modelo (aplicando diff apenas nas variáveis contínuas)
modelo1_dummy <- lm(
  diff(saldo_milhoes) ~ diff(di) + diff(nmmc) + diff(ipca) + d_2024[-1], 
  data = dados_sentido
)

modelo2_dummy <- lm(
  diff(saldo_milhoes) ~ diff(di) + diff(num_cont) + diff(ipca) + d_2024[-1], 
  data = dados_sentido
)

# 3. Testar os erros-padrão robustos com HC1 ou HC2 (que evitam a instabilidade do HC3 para dummies isoladas)
coeftest(modelo1_dummy, vcov = NeweyWest(modelo1_dummy, prewhite = FALSE))
coeftest(modelo2_dummy, vcov = NeweyWest(modelo2_dummy, prewhite = FALSE))

# Gerar o gráfico de envelopes simulados para o modelo
hnp(modelo1_dummy, verb = FALSE, print.on = TRUE,
    main = "Gráfico Half-Normal com Envelope Simulado",
    xlab = "Quantis Teóricos",
    ylab = "Resíduos Absolutos Ordenados")

hnp(modelo2_dummy, verb = FALSE, print.on = TRUE,
    main = "Gráfico Half-Normal com Envelope Simulado",
    xlab = "Quantis Teóricos",
    ylab = "Resíduos Absolutos Ordenados")

#ANÁLISE

#1. Diagnóstico dos Resíduos e Avaliação dos Gráficos Half-Normal
#A comparação dos gráficos hnp (Half-Normal Plot com envelope de simulação a $95\%$) revela o impacto direto da inclusão da dummy no controle do ruído do ano de 2024:
#Gráfico 1 (modelo1_dummy):Resultado: 0 pontos fora do envelope ($0\%$). Interpretação: Todos os 23 pontos residuais estão estritamente contidos dentro das bandas do envelope de confiança. Isso atesta uma adequação perfeita da suposição de distribuição Gaussiana (normalidade) e ausência total de outliers não controlados nos resíduos quando se utiliza a variável nmmc juntamente com a dummy.
#Gráfico 2 (modelo2_dummy):Resultado: 3 pontos fora do envelope ($13,04\%$).Interpretação: Embora a esmagadora maioria dos pontos acompanhe a tendência teórica, três observações saem levemente das bandas. Em pequenas amostras ($N = 23$), pequenos desvios nas extremidades são toleráveis, mas o comportamento do modelo1_dummy mostra-se visualmente e estatisticamente superior em termos de adesão à distribuição de referência.

#2. Comparação dos Modelos com Dummy (Newey-West)
#Ambas as especificações corrigidas com a dummy de impulso para 2024 (d_2024[-1]) e erros-padrão de Newey-West recuperam a significância estatística de todas as variáveis explicativas: (no zap)

#3. Interpretação Econômica dos Resultados Finalizados
#Com a dummy capturando o choque de 2024, as estimativas são estatisticamente consistentes em ambas as opções:
#Taxa de Juros ($\Delta \text{DI}$):Apresenta um efeito negativo e estatisticamente significante ($p < 0,01$ no Modelo 1 e $p < 0,05$ no Modelo 2). Cada aumento de 1 ponto percentual na variação da taxa DI reduz a variação do saldo entre R$ 39,94 milhões e R$ 55,54 milhões.
#Inflação ($\Delta \text{IPCA}$):Apresenta um efeito positivo altamente significante ($p < 0,001$). A cada aumento de 1 ponto percentual na variação do IPCA, a variação do saldo aumenta entre R$ 182,17 milhões e R$ 191,66 milhões.
#Volume de Operações ($\Delta \text{nmmc}$ / $\Delta \text{num\_cont}$):Permanece como o principal regressor estrutural do modelo ($p < 0,001$). Para cada 1 milhão de novos contratos adicionados no período, o incremento no saldo fica entre +R$ 6,19 milhões e +R$ 7,02 milhões.
#Choque do Ano de 2024 (d_2024):A variável dummy é altamente significante ($p < 0,001$) em ambas as versões, isolando um aumento atípico no saldo entre R$ 32,15 milhões e R$ 37,91 milhões ocorrido estritamente em 2024.

#4. Conclusão da Análise
#O modelo1_dummy destaca-se como a especificação econométrica ideal:
#Todas as variáveis mantêm alta significância estatística com correção de erros-padrão Newey-West/HAC.
#O envelope simulado Half-Normal resultou em 0% de pontos fora dos limites, confirmando perfeita calibração dos resíduos e aderência total aos pressupostos do MQO.
