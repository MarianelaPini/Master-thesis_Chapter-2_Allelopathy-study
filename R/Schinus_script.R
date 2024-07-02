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
#Tratamentos como fator
data$Tratamento<-factor(data$Tratamento,levels = (c("agua","peg","ext2.5","ext5.0")))
data$Tratamento
#Criando a variável resposta do modelo, combinação de sucessos e falhas
#por Tratamento, por bandeja#
prop<-cbind(success = data$Sucesso, fail = data$Fracasso)
prop
#proporção dos dados de sementes germinadas
data$propor<-data$Sucesso/data$Numerototal
data$propor
mean_propor<- aggregate(propor ~ Tratamento, data = data, FUN = mean)
mean_propor
data
#modelo com o termo simplificado do fator aleatório
mod.scag2<-glmer(prop ~ Tratamento + (1|Bandeja), data = data, family = "binomial")
mod.scag2
summary(mod.scag2)
#Seleção comparando o modelo com variável Tratamento com modelo nulo
#
mod.scag3<- glmer(prop ~ 1 + (1|Bandeja), data = data, family = "binomial")
mod.scag3
summary(mod.scag3)
#
anova(mod.scag2, mod.scag3)
data
anovasc<-aov(propor ~ Tratamento, data)
summary(anovasc)
#teste tukey par a par
TukeyHSD(anovasc)
#preditos
proppred<-(predict(mod.scag2, type="response")) 
proppred
##transformando odds em proportion
proportions<-proppred/(1+ proppred)
proportions
#including data
data$predicted_prob <- proportions
#Calculando as medias por nível
mean_predictions <- aggregate(predicted_prob ~ Tratamento, data = data, FUN = mean)
mean_predictions
sd_predictions<-aggregate(predicted_prob ~ Tratamento, data = data, FUN = sd)
sd_predictions
dfgrafico<-merge(mean_predictions,sd_predictions,by="Tratamento")
dfgrafico
#nomeando colunas
colnames(dfgrafico)<-c("Tratamento","media","desvio")
dfgrafico
str(dfgrafico)
dfgrafico$ymin<-dfgrafico$media-dfgrafico$desvio
dfgrafico$ymax<-dfgrafico$media+dfgrafico$desvio
dfgrafico
#grafico
library(ggbeeswarm)
library(ggplot2)

ps <- ggplot(data = data, aes(x = Tratamento, y = propor, colour=Tratamento))+
  scale_y_continuous(name = "Proporção de sementes germinadas")+
  scale_x_discrete(name = "Tipo de Solução")+
  geom_beeswarm(size=5, alpha=0.1)+theme_classic()+
  theme(axis.title.x = element_text(size = 17),  # X-axis title size
        axis.title.y = element_text(size = 17),  # Y-axis title size
        axis.text.x = element_text(size = 15),   # X-axis text size
        axis.text.y = element_text(size = 15),   # Y-axis text size
        legend.title = element_text(size = 15),  # Legend title size
        legend.text = element_text(size = 14))
  
help("ggplot")                      
ps
# Add segments
psc <- ps +
  geom_segment(data = dfgrafico, aes(x = Tratamento, y = ymin, xend = Tratamento, yend = ymax), 
               color = "black",alpha=0.6, size = 0.6)+
  geom_text(data = dfgrafico, aes(x = Tratamento, y = ymin, label = "-"), 
            color = "black", size = 6)+
  geom_text(data = dfgrafico, aes(x = Tratamento, y = ymax, label = "-"), 
            color = "black", size = 6,vjust=0.1)
plotschinus<-psc+geom_point(data=dfgrafico,aes(x=Tratamento,y=media),size=2.5,alpha=1)+
  geom_text(data = data, aes(x = 1, y = 0.3, label = "a"), 
            color = "black", size = 5)+
geom_text(data = data, aes(x = 2, y = 0.1, label = "b"), 
          color = "black", size = 5)+
geom_text(data = data, aes(x = 3, y = 0.3, label = "a"), 
          color = "black", size = 5)+
geom_text(data = data, aes(x = 4, y = 0.21, label = "ab"), 
          color = "black", size = 5)
plotschinus
ggsave("plot_schinus.jpg", width=6, height=4.5, dpi=600)
####
####
####

plotschinus

#Tentando com grafico base
plot(x=NULL,y=NULL,
     type = "n",xaxt="n",xlim= c(1,4),ylim=c(0,0.6), xlab = "", 
     ylab = "")
par (mar = c(3, 6, 2, 3), cex.axis = 1, bty = "l")
dfgrafico
dfgrafico$Tratamento<-factor(c("agua","peg","trat25","trat50"))
dfgrafico$Tratamento
dev.off()
#cores
cores<-c("coral","turquoise","purple","green")
#Pontos das medias
points(x = dfgrafico$Tratamento, y = dfgrafico$media, pch = 21, bg = cores, cex = 1.2, lwd = 1.3)
title (xlab ="agua                     peg                  trat25                    trat 50", line = 0.4, cex.lab = 1)
# Intervalos de confiança
segments (x0 = c(1,2,3,4), y0 = dfgrafico$media-dfgrafico$desvio, 
          x1 = c(1,2,3,4), y1 = dfgrafico$media+dfgrafico$desvio)
text (x = c(1,2,3,4), y =dfgrafico$media-dfgrafico$desvio, labels = "_") # traço horizontal (bigodinho)
text (x = c(1,2,3,4), y =dfgrafico$media+dfgrafico$desvio, labels = "_")
xyplot(jitter(propor) ~ Tratamento, jitter = TRUE,data=data , pch=20 , cex=1 , col=cores,alpha=0.2,
       
       panel.xyplot(x =dfgrafico$Tratamento,y = dfgrafico$media, bg = cores, pch = 21)  # Additional points in red with different symbol
       )
data
help("xyplot")
library("lattice")
data  
#modelo cheio com peg como nivel basal
#
#Primeiro coloco o peg como nivel basal
data$Tratamento<-factor(data$Tratamento,levels = (c("peg","agua","trat25","trat50")))
data$Tratamento
#modelo com o termo simplificado do fator aleatório,nivel basal peg
mod.scag2<-glmer(prop ~ Tratamento + (1|Bandeja), data = data, family = "binomial")
mod.scag2
summary(mod.scag2)
#Seleção comparando o modelo com variável Tratamento com modelo nulo
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
par
par (mar = c(3, 6, 2, 3), cex.axis = 1, bty = "l")
plot(odds~ Tratamento, data = data,ylab = "razão de chance
(germinadas/não germinadas)")
text(x = c(1, 2, 3, 4), y = c(1.3,0.3,1.3,1.3), labels = c("a", "b", "a","a"),cex = 0.8)
stripchart(odds ~ Tratamento,
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


# Create a sample data frame
df <- data.frame(
  A = 1:5,
  B = 6:10,
  C = 11:15
)
df
# Change the position of row 3 to the first position
row_to_move <- df[3, ]
df <- rbind(row_to_move, df[-3, ])
df


