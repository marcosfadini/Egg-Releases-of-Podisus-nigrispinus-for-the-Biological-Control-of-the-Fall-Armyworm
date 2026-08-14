#-------------------------------------------------------------------------------
#PRELIMINARES
ls()            #requests all objects in R's brain
rm(list=ls())    #we use two functions at once, rm an ls. rm stands for 

getwd()			    #"get working directory", where R in "currently" loonking...
setwd("C:/Users/Fadini/Documents/2. orientações e defesas/1. em andamento/2024_MS_Samuel Campos Abreu/4. qualificacao")
getwd()			    #use getwd to confirm that R is now looking here

detach(data)
rm(data)


#-------------------------------------------------------------------------------
#24.jun.2025

#Experimento implantado as 7:00 da manhã do dia 19.jun.2025 com 6 tratamentos (a cada 
#2 horas coleta de 20 placas de petri com 10 ovos em cada placa). 

#As placas coletadas foram deixadas no laboratorio e contou-se numero de ovos e 
#numero de ninfas eclodidas no dia 23.jun.2025.


#Carregando dados --------------------------------------------------------------
data <- read.table ("1. predacao_de_ovos.csv", h=T, sep=";", dec=",")

data

str(data)

data$tempo     <- as.numeric   (data$tempo)
data$n_ovos    <- as.numeric   (data$n_ovos)
data$n_ninfas  <- as.numeric   (data$n_ninfas)

str(data)

summary(data)


#-------------------------------------------------------------------------------
#Carregando pacotes para graficos
library(ggplot2)
library(scales)
library(ggpubr)
library(tidyverse)
library(hrbrthemes)
library(viridis)

library(hnp)       ## Visualização da qualidade de ajuste dos modelos
library(multcomp)  ## Contraste de modelos


#-------------------------------------------------------------------------------
#Numero de ovos
fig_2a <- ggplot(data=data, aes(x=tempo_exposicao, y=n_ovos)) +
  
  ggtitle("A)") +
  
  geom_point(position = position_jitter(width = 0.3, height = 0.2), 
             shape = 21, color = "darkred", fill = "white", size = 2) +
  
  xlab("Time (hours)") +
  scale_x_continuous(labels = label_number(accuracy = 1), limits = c(0,25)) +
  
  ylab("Number of eggs") +
  scale_y_continuous(labels = label_number(accuracy = 1), limits = c(0,10)) +
  
  theme_classic() + #fundo branco
  
  theme(legend.position="none",axis.title = element_text(size = 14)) +
  
  (theme(axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 20)),  #eixo x
         axis.title.y = element_text(size = 14, face = "bold", margin = margin(r = 10))))

fig_2a

ggsave("3. numero_de_ovos.tiff", height = 4.5, width = 6, units = "in", dpi = 600)
ggsave("3. numero_de_ovos.jpg",  height = 4.5, width = 6, units = "in", dpi = 300)


################################################################################
#28.jun.2025 - adicao de pontos medios

fig_2a <- ggplot(data=data, aes(x=tempo_exposicao, y=n_ovos)) +
  
  ggtitle("A)") +
  
  geom_point(data=data, aes(x=tempo_exposicao, y=n_ovos),
             position = position_jitter(width = 0.3, height = 0.2), 
             shape = 21, color = "darkred", fill = "white", size = 2) +
  
  
  geom_point(data=data, aes(x=tempo_exposicao, y=n_ovos),
             stat = "summary", 
             fun = "mean",
             shape = 21, color = "black", fill = "black", size = 3)+
  
  geom_line(data=data, aes(x=tempo_exposicao, y=n_ovos),
            stat = "summary", 
            fun = "mean",
            color = "black") +

  xlab("Time (hours)") +
  scale_x_continuous(labels = label_number(accuracy = 1), limits = c(0,25)) +
  
  ylab("Number of eggs") +
  scale_y_continuous(labels = label_number(accuracy = 1), limits = c(0,10)) +
  
  theme_classic() + #fundo branco
  
  theme(legend.position="none",axis.title = element_text(size = 14)) +
  
  (theme(axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 20)),  #eixo x
         axis.title.y = element_text(size = 14, face = "bold", margin = margin(r = 10))))

fig_2a

ggsave("3. numero_de_ovos.tiff", height = 4.5, width = 6, units = "in", dpi = 600)
ggsave("3. numero_de_ovos.jpg",  height = 4.5, width = 6, units = "in", dpi = 300)


#-------------------------------------------------------------------------------
#Ajuste de modelos para Numero de ovos por tempo de exposicao

#Modelo linear -----------------------------------------------------------------
summary(model_n_ovos_lm <-      lm (n_ovos ~ tempo_exposicao, data = data))

shapiro.test(model_n_ovos_lm$residuals) #W= 0.76, p= 9.752e-13, erros nao normais

# Pacote hnp#model_n_ovos_lm Pacote hnp
hnp(model_n_ovos_lm, plotit=TRUE)


#Modelo linear generalizado Poisson --------------------------------------------
summary(model_n_ovos_glm_quasipoisson <- glm (n_ovos ~ tempo_exposicao, 
                                 data = data,
                                 "quasipoisson"))

shapiro.test(model_n_ovos_glm_quasipoisson$residuals) #W= 0.76, p= 1.205e-12, erros nao normais
hist(model_n_ovos_glm_quasipoisson$residuals)

# Pacote hnp
hnp(model_n_ovos_glm_quasipoisson, plotit=TRUE)


#Modelo linear generalizado Binomia negativo -----------------------------------
library(MASS)
summary(model_n_ovos_glm_nb <- MASS::glm.nb (n_ovos ~ tempo_exposicao, 
                                 data = data))

anova(model_n_ovos_glm_nb, test="Chi")

shapiro.test(model_n_ovos_glm_nb$residuals) #W = 0.76, p= 1.205e-12, erros nao normais

# Pacote hnp
hnp(model_n_ovos_glm_nb, plotit=TRUE)

plot(model_n_ovos_glm_nb$residuals)


#-------------------------------------------------------------------------------
#Numero de ninfas

fig_2b <- ggplot(data=data, aes(x=tempo_exposicao, y=n_ninfas_eclodidas)) +
  
  ggtitle("B)") +
  
  geom_point(position = position_jitter(width = 0.2, height = 0.2), 
             shape = 21, color = "blue", fill = "white", size = 2) +
  
  geom_smooth(method = "lm", size = 0.7) +
  
  xlab("Time (hours)") +
  scale_x_continuous(labels = label_number(accuracy = 1), limits = c(0,25)) +
  
  ylab("Number of hatched nymphs") +
  scale_y_continuous(labels = label_number(accuracy = 1), limits = c(0,10)) +
  
  theme_classic() + #fundo branco
  
  theme(legend.position="none",axis.title = element_text(size = 14)) +
  
  (theme(axis.title.x = element_text(size = 13, face = "bold", margin = margin(t = 20)),  #eixo x
         axis.title.y = element_text(size = 13, face = "bold", margin = margin(r = 10))))

fig_2b

ggsave("3. numero_de_ninfas.tiff", height = 4.5, width = 6, units = "in", dpi = 600)
ggsave("3. numero_de_ninfas.jpg",  height = 4.5, width = 6, units = "in", dpi = 300)

################################################################################
#28.jun.205
#Numero de ninfas - adicao de pontos medios

fig_2b <- ggplot() +
  
  ggtitle("B)") +
  
  geom_point(data=data, aes(x=tempo_exposicao, y=n_ninfas_eclodidas),
             position = position_jitter(width = 0.2, height = 0.2), 
             shape = 21, color = "blue", fill = "white", size = 2) +
  
  geom_point(data=data, aes(x=tempo_exposicao, y=n_ninfas_eclodidas),
             stat = "summary", 
             fun = "mean",
             color = "black",
             size = 3) +
  
  geom_smooth(data=data, aes(x=tempo_exposicao, y=n_ninfas_eclodidas),
              method = "lm", color = "black", size = 0.7) +
  
  xlab("Time (hours)") +
  scale_x_continuous(labels = label_number(accuracy = 1), limits = c(0,25)) +
  
  ylab("Number of hatched nymphs") +
  scale_y_continuous(labels = label_number(accuracy = 1), limits = c(0,10)) +
  
  theme_classic() + #fundo branco
  
  theme(legend.position="none",axis.title = element_text(size = 14)) +
  
  (theme(axis.title.x = element_text(size = 13, face = "bold", margin = margin(t = 20)),  #eixo x
         axis.title.y = element_text(size = 13, face = "bold", margin = margin(r = 10))))

fig_2b

ggsave("3. numero_de_ninfas.tiff", height = 4.5, width = 6, units = "in", dpi = 600)
ggsave("3. numero_de_ninfas.jpg",  height = 4.5, width = 6, units = "in", dpi = 300)






#-------------------------------------------------------------------------------
#Ajuste de modelos para Numero de ninfas eclodidas por tempo de exposicao

#Modelo linear -----------------------------------------------------------------
summary(model_n_ninfas_lm <- lm (n_ninfas ~ tempo_exposicao, data = data))

shapiro.test(model_n_ninfas_lm$residuals) #W = 0.99, p = 0.575, erros normais

# Pacote hnp
hnp(model_n_ninfas_lm, plotit=TRUE)


#Modelo linear generalizado Poisson --------------------------------------------
summary(model_n_ninfas_glm_poisson <- glm (n_ninfas ~ tempo_exposicao, 
                                              data = data,
                                              "quasipoisson"))

anova(model_n_ninfas_glm_poisson, test="Chi")

shapiro.test(model_n_ninfas_glm_poisson$residuals) #W = 0.98, p = 0.499, erros normais

# Pacote hnp
hnp(model_n_ninfas_glm_poisson, plotit=TRUE)

anova(model_n_ninfas_glm_poisson, test="Chi")


#-------------------------------------------------------------------------------
#24.jun.2025
#Criando painel com a figura 2

library(patchwork)         #https://blog.curso-r.com/posts/2021-05-19-patchwork/
library(magrittr)


fig_2a + fig_2b


ggsave("3. Figure_2_panel.tiff", height = 3.5, width = 8, units = "in", dpi = 600)
ggsave("3. Figure_5_panel.jpg" , height = 3.5, width = 8, units = "in", dpi = 300)

