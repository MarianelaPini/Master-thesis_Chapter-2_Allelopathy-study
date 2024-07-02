#Analise tamanho alface nos diferentes tratamentos
##
##
#carregando pacotes#
library(lme4)
library(DHARMa)
help("lme4")
#Abrindo os dados
#Primeiro tamanho alface
data<- read.csv(here::here("Data/processed", "primeira_tamanho_alface.csv"), sep = ";", header=TRUE)
data
#tratamentos como fator
data$tratamento<-factor(data$tratamento,levels = (c("agua","peg","ext2.5","ext5.0")))
data$tratamento
data$placa<-as.factor(data$placa)
#Médias dos dados
mediasdata <- aggregate(tamanho ~ tratamento, data = data, FUN = mean)
mediasdata
desviodata<-aggregate(tamanho ~ tratamento, data = data, FUN = sd)
desviodata
#Modelo cheio
mod.alf1<-lmer(tamanho ~ tratamento + (1| placa), data = data)
summary(mod.alf1)
#Comparação com modelo nulo
mod.alf2<- lmer(tamanho ~ 1 + (1| placa), data = data)
#Seleção de modelo
anova(mod.alf1,mod.alf2)
#Modelo cheio selecionado
#Comparações das medias, testes par a par, para saber se são diferentes
anova<-aov(tamanho~tratamento, data)
anova
summary(anova)
TukeyHSD(anova)
#Preditos do modelo
preditosmod.alf1<-predict(mod.alf1)
#Incluindo os preditos nos dados
data$preditos <- preditosmod.alf1
data$preditos
#Calculando as medias por nível
mean_predictions <- aggregate(preditos ~ tratamento, data = data, FUN = mean)
mean_predictions
sd_predictions<-aggregate(preditos ~ tratamento, data = data, FUN = sd)
sd_predictions
#resolver, desvio todos iguiais??
dfgrafico<-merge(mean_predictions,sd_predictions,by="tratamento")
dfgrafico
#nomeando colunas
colnames(dfgrafico)<-c("tratamento","media","desvio")
dfgrafico
str(dfgrafico)
dfgrafico$ymin<-dfgrafico$media-dfgrafico$desvio
dfgrafico$ymax<-dfgrafico$media+dfgrafico$desvio
dfgrafico

#grafico para exploração
library(ggplot2)
library(ggbeeswarm)
ggplot(data = data, aes(y = tamanho,
                        x = tratamento,
                        fill = tratamento)) +
  geom_boxplot() +
  geom_jitter(size = .8)+
  theme_minimal()
#grafico com as médias e os dados
plot <- ggplot(data = data, aes(x = tratamento, y = tamanho, colour=tratamento))+
  scale_y_continuous(name = "Tamanho das plántulas (mm)",limits = c(0,100))+
  scale_x_discrete(name = "Tipo de Solução")+
  geom_beeswarm(size=4, alpha=0.1)+
  theme_classic()+
  theme(axis.title.x = element_text(size = 15),  # X-axis title size
        axis.title.y = element_text(size = 15),  # Y-axis title size
        axis.text.x = element_text(size = 14),   # X-axis text size
        axis.text.y = element_text(size = 14),   # Y-axis text size
        legend.title = element_text(size = 15),  # Legend title size
        legend.text = element_text(size = 14))
plot
dfgrafico
# Adicionar segmentos e letras das médias
plott1 <- plot +
  geom_segment(data = dfgrafico, aes(x = tratamento, y = ymin, xend = tratamento, yend = ymax), 
               color = "black",alpha=0.8, size = 0.7)+
  geom_text(data = dfgrafico, aes(x = 2, y = 90, label = "a"),
                                  color = "black", size = 5)+
  geom_text(data = dfgrafico, aes(x = 1, y = 80, label = "a"),
            color = "black", size = 5)+
  geom_text(data = dfgrafico, aes(x = 3, y = 60, label = "b"),
            color = "black", size = 5)+
  geom_text(data = dfgrafico, aes(x = 4, y = 60, label = "b"),
            color = "black", size = 5)
plott1
plott1alface<-plott1+geom_point(data=dfgrafico,aes(x=tratamento,y=media),size=2.5,alpha=1)
plott1alface
ggsave("plot_alface_1.jpg", width=6, height=4.5, dpi=600)
#######
####
###
#Calculando as reduções de tamanho em porcentagem para o texto da dissertação
##
#agua é 53.344mm, ext 2.5 é -12.016mm, ext 5% -13.838
#agua ext 2,5%
porcext2.5agua<-100/53.344*12.016
porcext5.agua<-100/53.344*13.838
porcext5.agua
#peg, coloco como nivel basal
data$tratamento<-factor(data$tratamento,levels = (c("peg","agua","ext2.5","ext5.0")))
data$tratamento
mod.alfpeg<-lmer(tamanho ~ tratamento + (1| placa), data = data)
mod.alfpeg
summary(mod.alfpeg)
#calculo as porcentagens comparado com PEG para o texto da dissertação
#peg é 49.922, ext 2.5 -8.594, ext 5 -10.415
porcext2.5peg<-100/49.922*8.594
porcext2.5peg
porcext5peg<-100/49.922*10.415
porcext5peg
#Segundo tamanho alface
data<- read.csv(here::here("Data/processed", "segunda_tamanho_alface.csv"), sep = ";", header=TRUE)
data
#tratamentos como fator
data$tratamento<-factor(data$tratamento,levels = (c("agua","peg","ext2.5","ext5.0")))
data$tratamento
data$placa<-as.factor(data$placa)
#Médias dos dados
mediasdata <- aggregate(tamanho ~ tratamento, data = data, FUN = mean)
mediasdata
desviodata<-aggregate(tamanho ~ tratamento, data = data, FUN = sd)
desviodata
#Modelo cheio
mod.alf1<-lmer(tamanho ~ tratamento + (1| placa), data = data)
summary(mod.alf1)
#Comparação com modelo nulo
mod.alf2<- lmer(tamanho ~ 1 + (1| placa), data = data)
#Seleção de modelo
anova(mod.alf1,mod.alf2)
#Modelo cheio selecionado
#Comparações das medias, testes par a par, para saber se são diferentes
anova<-aov(tamanho~tratamento, data)
anova
summary(anova)
TukeyHSD(anova)
#Preditos do modelo
preditosmod.alf1<-predict(mod.alf1)
#Incluindo os preditos nos dados
data$preditos <- preditosmod.alf1
data$preditos
#Calculando as medias por nível
mean_predictions <- aggregate(preditos ~ tratamento, data = data, FUN = mean)
mean_predictions
sd_predictions<-aggregate(preditos ~ tratamento, data = data, FUN = sd)
sd_predictions
dfgrafico2<-merge(mean_predictions,sd_predictions,by="tratamento")
dfgrafico2
#nomeando colunas
colnames(dfgrafico2)<-c("tratamento","media","desvio")
dfgrafico2
str(dfgrafico2)
dfgrafico2$ymin<-dfgrafico2$media-dfgrafico2$desvio
dfgrafico2$ymax<-dfgrafico2$media+dfgrafico2$desvio
dfgrafico2
#grafico para exploração
library(ggplot2)
library(ggbeeswarm)

#grafico com as médias e os dados
plot <- ggplot(data = data, aes(x = tratamento, y = tamanho, colour=tratamento))+
  scale_y_continuous(name = "Tamanho das plántulas (mm)", limits = c(0,100))+
  scale_x_discrete(name = "Tipo de Solução")+
  geom_beeswarm(size=4, alpha=0.1)+
  theme_classic()+
  theme(axis.title.x = element_text(size = 15),  # X-axis title size
        axis.title.y = element_text(size = 15),  # Y-axis title size
        axis.text.x = element_text(size = 14),   # X-axis text size
        axis.text.y = element_text(size = 14),   # Y-axis text size
        legend.title = element_text(size = 15),  # Legend title size
        legend.text = element_text(size = 14))
plot
dfgrafico2
# Adicionar segmentos e letras das médias
plott2 <- plot +
  geom_segment(data = dfgrafico2, aes(x = tratamento, y = ymin, xend = tratamento, yend = ymax), 
               color = "black",alpha=0.8, size = 0.7)+
  geom_text(data = dfgrafico2, aes(x = 2, y = 100, label = "a"),
            color = "black", size = 5)+
  geom_text(data = dfgrafico2, aes(x = 1, y = 100, label = "a"),
            color = "black", size = 5)+
  geom_text(data = dfgrafico2, aes(x = 3, y = 60, label = "b"),
            color = "black", size = 5)+
  geom_text(data = dfgrafico2, aes(x = 4, y = 62, label = "b"),
            color = "black", size = 5)
plott2
plott2alface<-plott2+geom_point(data=dfgrafico2,aes(x=tratamento,y=media),size=2.5,alpha=1)
plott2alface
ggsave("plot_alface_2.jpg", width=6, height=4.5, dpi=600)
#####
#Calculando as reduções de tamanho em porcentagem para o texto da dissertação
###
#agua é 57.778mm, ext 2.5 é -18.872mm, ext 5% -14.782
#agua ext 2,5%
porcext2.5agua<-100/57.778*18.872
porcext2.5agua
porcext5.agua<-100/57.778*14.782
porcext5.agua
#peg, coloco como nivel basal
data$tratamento<-factor(data$tratamento,levels = (c("peg","agua","ext2.5","ext5.0")))
data$tratamento
mod.alfpeg<-lmer(tamanho ~ tratamento + (1| placa), data = data)
mod.alfpeg
summary(mod.alfpeg)
#calculo as porcentagens comparado com PEG para o texto da dissertação
#peg é 63, ext 2.5 -24.094, ext 5 -20.004
porcext2.5peg<-100/63*24.094
porcext2.5peg
porcext5peg<-100/63*20.004
porcext5peg
