library(urca)
library(fUnitRoots)
library(kableExtra)

#### Declarar dados Séries de tempo
dados_ts <- ts(Dados_Agricultura_2004, start = c(2004,1), frequency = 4)


###Teste de Raiz Unitária ADF

unit_root_Renda <- adfTest(dados_ts[,5],lags=3, type=c("c"))
unit_root_Agri <- adfTest(dados_ts[,6],lags=3, type=c("c"))
unit_root_XCAFE <- adfTest(dados_ts[,7],lags=3, type=c("c"))
unit_root_REER <- adfTest(dados_ts[,8],lags=3, type=c("c"))

###Teste de Raiz Unitária PP
pp_root_Renda <- PP.test(dados_ts[,5])
pp_root_Agri <- PP.test(dados_ts[,6])
pp_root_XCAFE <- PP.test(dados_ts[,7])
pp_root_REER <- PP.test(dados_ts[,8])


###Criar tabela de resultados de raiz unitária

adf_Renda  = c(unit_root_Renda@test$statistic,  unit_root_Renda@test$p.value)
adf_Agri = c(unit_root_Agri@test$statistic, unit_root_Agri@test$p.value)
adf_XCAFE = c(unit_root_XCAFE@test$statistic, unit_root_XCAFE@test$p.value)
adf_REER  = c(unit_root_REER@test$statistic,  unit_root_REER@test$p.value)


resultADF = cbind(adf_Renda, adf_Agri, adf_XCAFE, adf_REER)
colnames(resultADF) = c("Indice de Renda", "Indice Agricola", "Exportações de café", "TCRE")
rownames(resultADF) = c("Estatística do Teste ADF - Nível", "p-valor")

kable(round(resultADF, digits = 4)) %>%
  kable_styling(bootstrap_options = c("striped","hover"))




library(dLagM)
library(permutations)


##Estimar ARDL
Dadosdf <- data.frame(dados_ts)

Report_ARDL <- ardlBound(data = Dadosdf,
                         formula = diff(x_cafe) ~ AGRI_INDEX + RENDA_INDEX + REER,
                         ic = "AIC", max.p = 6, max.q = 6, ECM = TRUE,
                         stability = TRUE)


Report_ARDL$p