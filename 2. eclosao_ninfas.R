#-------------------------------------------------------------------------------
#PRELIMINARES
ls()            #requests all objects in R's brain
rm(list=ls())    #we use two functions at once, rm an ls. rm stands for 

getwd()			    #"get working directory", where R in "currently" loonking...
setwd("C:/Users/Fadini/Documents/0. publicacoes/1. em preparação/1. Samuel_Ms/4. qualificacao")
getwd()			    #use getwd to confirm that R is now looking here

detach(data)
rm(data)


#-------------------------------------------------------------------------------
#03.jun.2025

#Experimento de qualificação de Samuel Campos Abreu

#Coletou ovos de P. nigrispinus com máximo de 2 dias de postura. Foram individualizados
#em 10 ovos por placa de petri de vidro (diametro) como repetição. As placas de
#petri com solo no fundo, peneirado, com umidade de 1 para meio água. Foram 30
#repticoes por tratamento. As placas foram cobertas com organza com gominha;

#Os tratamentos foram o controle (30 placas com os 10 ovos) na BOD, 1, 2 e 3 dias
#no campo.

#A cada dia recolha-se 30 placa e levava para a BOD e marca como dia 1, 2 e 3.
#Depois da última coleta, deixou mais um e foi contada o número de nínfas encontradas
#em cada planca.


#Carregando dados --------------------------------------------------------------
data <- read.table ("1. dados_eclosao.csv", h=T, sep=";", dec=",")

data

str(data)
data$trat                      <- as.factor   (data$trat)
data$n_eclosoes_de_ninfas      <- as.numeric  (data$n_eclosoes_de_ninfas)
str(data)

summary(data)

data$n_eclosoes_de_ninfas 

tapply(data$n_eclosoes_de_ninfas, data$trat, median)


#-------------------------------------------------------------------------------
#Carregando pacotes para graficos
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(hrbrthemes)
library(viridis)

library(hnp)       ## Visualização da qualidade de ajuste dos modelos
library(multcomp)  ## Contraste de modelos


# Carregamento dos pacotes -----------------------------------------------------
if(!require("pacman")){install.packages("pacman")}
pacman::p_load(tidyverse, readxl, AER, car, statmod, sjPlot, emmeans)


#-------------------------------------------------------------------------------
ggplot(data=data, aes(x=trat, y=n_eclosoes_de_ninfas, fill=trat)) +
  
  geom_errorbar(stat = "boxplot", width = 0.05) +
  
  geom_boxplot(notch = FALSE, width = 0.7) +
  
  geom_jitter(color="grey60", size=1, alpha=1, position = position_jitter(width =0.15)) +
  
  ggtitle(" ") +
  
  xlab("Treatments") +
  
  ylab("Number of hatched nymphs") +
  
  theme_classic() + #fundo branco
  
  scale_fill_manual(values = c(
    "Dia 1"      = "#FDD9A0",  # laranja claro
    "Dia 2"      = "#F4A259",  # laranja médio
    "Dia 3"      = "#D96C06",  # laranja escuro
    "Testemunha" = "darkgreen")) +
  
  scale_x_discrete(labels = c("Day 1","Day 2","Day 3", "Control")) +
  
  theme(legend.position="none",axis.title = element_text(size = 14)) +
  (theme(axis.title.x = element_text(size = 15, face = "bold", margin = margin(t = 20)),  #eixo x
         axis.title.y = element_text(size = 15, face = "bold", margin = margin(r = 20)))) +
  annotate("text", x=1.45, y=7.2, label = "a", col="grey40", size= 7) +
  annotate("text", x=2.45, y=0.2, label = "b", col="grey40", size= 7) +
  annotate("text", x=3.45, y=0.2, label = "b", col="grey40", size= 7) +
  annotate("text", x=4.45, y=7.2, label = "a", col="grey40", size= 7) 

 
ggsave("3. numero_de_ninfas_eclodidas.tiff", height = 4.5, width = 6, units = "in", dpi = 600)
ggsave("3. numero_de_ninfas_eclodidas.jpg",  height = 4.5, width = 6, units = "in", dpi = 300)



#-------------------------------------------------------------------------------
ggplot(data = data, aes(x = trat, 
                        y = n_eclosoes_de_ninfas, 
                        fill = trat)) +
  
  geom_boxplot(notch = FALSE, width = 0.7) +
  
  geom_jitter(color = "grey60",
              size = 1,
              alpha = 1,
              position = position_jitter(width = 0.15)) +
  
  scale_fill_manual(values = c(
    "Day 1"  = "#FDD9A0",  # laranja claro
    "Day 2"  = "#F4A259",  # laranja médio
    "Day 3"  = "#D96C06",  # laranja escuro
    "Control" = "grey70"
  )) +
  
  xlab("Treatments") +
  ylab("Number of hatched nymphs") +
  
  theme_classic() +
  
  theme(
    legend.position = "none",
    
    axis.title.x = element_text(size = 15,
                                face = "bold",
                                margin = margin(t = 20)),
    
    axis.title.y = element_text(size = 15,
                                face = "bold",
                                margin = margin(r = 20))
  )





#-------------------------------------------------------------------------------
#10.jun.2025
#Ajuste de modelos

#Mudanca da categoria de referencia para "Testemunha" --------------------------
data$trat <- relevel(data$trat, 
                     ref = "Testemunha") #mudou a categoria de referencia - IMPORTANTE!


#Poisson -----------------------------------------------------------------------
summary(model_poisson <- glm (n_eclosoes_de_ninfas ~ trat,     
                      data = data,
                      family = poisson))

shapiro.test(model_poisson$residuals)

hnp(model_poisson, print.on="T")

anova(model_poisson, test="Chi")


#QuasiPoisson ------------------------------------------------------------------
summary(model_quasipoisson <- glm (n_eclosoes_de_ninfas ~ trat,     
                      data = data,
                      "quasipoisson"))

shapiro.test(model_quasipoisson$residuals)

hnp(model_quasipoisson, print.on="T")

anova(model_quasipoisson, test="Chi")


#Binomial negativa -------------------------------------------------------------
library(MASS)

model_nb <- MASS::glm.nb(n_eclosoes_de_ninfas ~ trat,     #pacote MASS
                       data = data)

summary(model_nb)

anova(model_nb, test="Chi")

shapiro.test(model_nb$residuals)

hnp(model_nb, print.on="T")


#-------------------------------------------------------------------------------
#Teste de comparações múltiplas usando o pacote multcomp

#Utilizei o modelo model_poisson, pois foi o que melhor se ajustou conforme o
#o envelope hnp

pc.eclosoes <- summary(glht(model_poisson, linfct = mcp(trat="Tukey")))
pc.eclosoes

pc.eclosoes.cld <-cld(pc.eclosoes, decreasing = TRUE)
pc.eclosoes.cld

