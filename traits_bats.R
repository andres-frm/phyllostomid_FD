
bt <- read.csv("rasgos_spp.csv.", header = T, sep = ";", dec = ",")
bt$LugarF <- as.factor(bt$Lugar)
str(bt)


#organizar el DF

library(tidyverse)
library(ggplot2)
library(reshape2)
library(reshape)

# Reorder following the value of another column:

Total <- ggplot(bt, aes(x= fct_reorder(Especie, Aspect.ratio),y=Aspect.ratio))
Total+geom_boxplot(position = position_dodge(2), fill="gray")+geom_jitter(size= 1, colour="black",alpha=0.45) + theme_classic()+
  stat_summary(fun.y = "mean", geom = "point", colour = "red", size=1)+
  theme(legend.position = "none", axis.text.x = element_text(
  angle = 45, hjust = 1, vjust = 1, face = "italic", family = "Arial", size = 10), text = element_text(size = 12))+
  labs(x= "", y="Wing aspect ratio")
#ggsave("Fig_S2.tiff", units = "cm", height = 10, width = 15, dpi = 300)
# 

b <- ggplot(bt, aes(x=reorder(Especie,MC),y=MC))
b+geom_boxplot(position = position_dodge(2), fill= "gray")+geom_jitter(size= 1, alpha=0.45) + theme_classic()+
  stat_summary(fun.y = "mean", geom = "point", colour = "red", size=1)+
  theme(legend.position = "none", axis.text.x = element_text(
    angle = 90, hjust = 1, vjust = 0.5, face = "italic", family = "Arial", size = 10), text = element_text(size = 12))+
  labs(x= "", y="Body mass (g)") 
#ggsave("Fig_S3.tiff", units = "cm", height = 10, width = 15, dpi = 300) 

c <- ggplot(bt, aes(x=reorder(Especie,Envergadura),y=Envergadura))
c+geom_boxplot(position = position_dodge(2), fill= "gray")+geom_jitter(size= 1, alpha=0.45) + theme_classic()+
  stat_summary(fun.y = "mean", geom = "point", colour = "red", size=1)+
  theme(legend.position = "none", axis.text.x = element_text(
    angle = 90, hjust = 1, vjust = 0.5, face = "italic", family = "Arial", size = 10), text = element_text(size = 12))+
  labs(x= "", y="Wingspan (cm)") 
#ggsave("Fig_S4.tiff", units = "cm", height = 10, width = 15, dpi = 300) 

d <- ggplot(bt, aes(x=reorder(Especie,Carga.alar),y=Carga.alar))
d+geom_boxplot(position = position_dodge(2), fill= "gray")+geom_jitter(size= 1, alpha=0.45) + theme_classic()+
  stat_summary(fun.y = "mean", geom = "point", colour = "red", size=1)+
  theme(legend.position = "none", axis.text.x = element_text(
    angle = 90, hjust = 1, vjust = 0.5, face = "italic", family = "Arial", size = 10), text = element_text(size = 12))+
  labs(x= "", y="Wing loading") 
#ggsave("Fig_S5.tiff", units = "cm", height = 10, width = 15, dpi = 300) 
