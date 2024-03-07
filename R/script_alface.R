#Analise tamanho alface nos diferentes tratamentos
##
##
#carregando pacotes#
library(lme4)
library(DHARMa)
help("lme4")
#Abrindo os dados
data<- read.csv(here::here("Data/processed", "primeira_tamanho_alface.csv"), sep = ";", header=TRUE)
data
#tratamentos como fator
data$tratamento<-factor(data$tratamento,levels = (c("agua","peg","trat25","trat50")))
data$tratamento
data$placa<-as.factor(data$placa)
#grafico dados, para exploração
boxplot(tamanho~ tratamento, data = data)
stripchart(tamanho ~ tratamento,
           data = data,
           method = "jitter",
           pch = 19, col= 1:4,
           vertical = TRUE,
           add = TRUE)
library(ggplot2)
ggplot(data = data, aes(y = tamanho,
                        x = tratamento,
                        fill = tratamento)) +
  geom_boxplot() +
  geom_jitter(size = .8)+
  theme_minimal()
#Modelo cheio
mod.alf1<-lmer(tamanho ~ tratamento + (1| placa), data = data)
summary(mod.alf1)
#Comparação com modelo nulo
mod.alf2<- lmer(tamanho ~ 1 + (1| placa), data = data)
#Seleção de modelo
anova(mod.alf1,mod.alf2)
#Modelo cheio selecionado

