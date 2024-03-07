#########################################################################
#Análise de dados do experimento de Alelopatia de Leucaena leucocephala##
#sobre sementes nativas do Brasil                                      ##
#########################################################################
#
#
################################################################
#GLMMM modelo binomial, para a espécie Schinus terebinthifolius#
################################################################
#
#
#carregando pacotes#
library(lme4)
library(DHARMa)
#Abrindo os dados
data<- read.csv(here::here("Data/processed", "Schinus_germination.csv"), sep = ";", header=TRUE)
data
#tratamentos como fator
data$Tratamento<-factor(data$Tratamento,levels = (c("agua","peg","trat25","trat50")))
data$Tratamento
#Criando a variável resposta do modelo, combinação de sucessos e falhas
#por tratamento, por bandeja#
prop<-cbind(success = data$Sucesso, fail = data$Fracasso)
prop
#modelo com o termo simplificado do fator aleatório
mod.scag2<-glmer(prop ~ Tratamento + (1|Bandeja), data = data, family = "binomial")
mod.scag2
summary(mod.scag2)
#Seleção comparando o modelo com variável tratamento com modelo nulo
#
mod.scag3<- glmer(prop ~ 1 + (1|Bandeja), data = data, family = "binomial")
mod.scag3
summary(mod.scag3)
#
anova(mod.scag2, mod.scag3)
#seleciono o modelo cheio,criando os residous
residuo <- simulateResiduals(fittedModel = mod.scag2, n = 1000)
plotSimulatedResiduals(residuo)
#preditos
proppred<-(predict(mod.scag2, type="response")) 
proppred
prop
comidos <- ggplot(data = data, aes(x = Tratamento, y = proppred, fill=Tratamento))+
          geom_boxplot() +
          geom_jitter(size = .8)+
          theme_minimal()
agua<-proppred[c(1,2,3,4,5,6,7,8,9,10)]
mean(agua)
comidos
#modelo cheio com peg como nivel basal
#
#Primeiro coloco o peg como nivel basal
data$Tratamento<-factor(data$Tratamento,levels = (c("peg","agua","trat25","trat50")))
data$Tratamento
#modelo com o termo simplificado do fator aleatório,nivel basal peg
mod.scag2<-glmer(prop ~ Tratamento + (1|Bandeja), data = data, family = "binomial")
mod.scag2
summary(mod.scag2)
#Seleção comparando o modelo com variável tratamento com modelo nulo
#
mod.scag3<- glmer(prop ~ 1 + (1|Bandeja), data = data, family = "binomial")
mod.scag3
summary(mod.scag3)
#
anova(mod.scag2, mod.scag3)
AIC(mod.scag1, mod.scag2, mod.scag3)

#exponenciando os coeficientes para interpretar melhor o resultado,modelo cheio
## coeficientes da estrutura aleatória
randcoef <- coef(mod.scag)$Bandeja
randcoef
## coeficientes da estrutura fixa do modelo
fixcoef  <- fixef(mod.scag)
fixcoef
randcoefexp<-exp(randcoef)
fixcoefexp<-exp(fixcoef)
fixcoefexp
fixcoeexp<-(c(0.2582851,0.9598174,0.6008590))
#grafico dados, para exploração
boxplot(Sucesso~ Tratamento, data = data)
stripchart(Sucesso ~ Tratamento,
           data = data,
           method = "jitter",
           pch = 19, col= 1:4,
           vertical = TRUE,
           add = TRUE)
library(ggplot2)
ggplot(data = data, aes(y = Sucesso,
                            x = Tratamento,
                            fill = Tratamento)) +
  geom_boxplot() +
  geom_jitter(size = .8)+
  theme_minimal()
proppred=(predict(model3, type="response")) 