DF <- read.csv("data.actualizado.csv", header = T, sep = ";", dec = ",")
str(DF)

levels(DF$site)

library(doBy)
summaryBy(Carollia.perspicillata~site, data = DF, FUN = c(mean, sd, median))

#explorar correlaciones entre variables explicativas a diferentes escalas
require(GGally)
require(ggplot2)
ggpairs(DF[,c(23:43)], columns = c(1:3,8:10,15:17),
        upper = list(continuous = wrap("cor", size = 3)))
ggsave("COR_ba-g-f.tiff", dpi = 300, width = 25, height = 25, units = "cm")
#Variables a retener en el an?lisis: F% en todas las escalas, grass% a 0.5 y grass% a 1.25 o 2 (no ambas).
#el porcentaje de ?rea construida se correlacion? consigomisma en todas las escalas y tambi?n con el porcentaje de potreros

ggpairs(DF[,c(23:43)], columns = c(4:7,11:14,18:21),
        upper = list(continuous = wrap("cor", size = 3)))
ggsave("COR_pno-mps-pssd-pd.tiff", dpi = 300, width = 25, height = 25, units = "cm")
#variables a retener para el an?lsisi: pssd en todas las escalas, pno a 0.5 y 1.25.
#el resto de variabes explicativas estuvieron altamente correlacionadas entre si en todas las escalas espaciales

ggpairs(DF[,c(22, 44:50)], columns = c(1:8),
        upper = list(continuous = wrap("cor", size = 3)))
ggsave("COR_FD-indices.tiff", dpi = 100, width = 25, height = 25, units = "cm")
#todos los indices de CWM estuvieron altamente correlacionados. Dada la pregunta, me inclino por retener la carga alar o el aspect ratio

ggpairs(DF, columns = c(23,24,27,28,30,31,33,35,37,38,40,42),
        upper = list(continuous = wrap("cor", size = 3)))
ggsave("COR_RETEINED_VARIABLES.tiff", dpi = 300, width = 30, height = 30, units = "cm")
#despu?s de revisar las correlaciones de las variables retenidas, para el an?lsis solo se considerar?n: F% 0.5 y 1.25, G% 0.5 y 2, pno 0.5, y pssd 0.5

#reorganizar el DF de wide a long format (en caso de que el ingreso de datos en el modelo lo requiera)
DF.1 <- DF[, c(1:25,37, 27:29,44:50)]
str(DF.1)
DF1 <- DF[,c(1:22)]
str(DF1)
DF2 <- DF[,c(23,24,27,28,30,38)]
str(DF2)
DF3 <-  DF[,c(44:50)]
str(DF3)
DF1 <- stack(DF, select = c("Carollia.perspicillata", "Carollia.castanea", "Artibeus.lituratus", "Artibeus.planirostris",
                             "Sturnira.lilium", "Platyrrhinus.sp1", "Platyrrhinus.sp2", "Uroderma.bilobatum",
                             "Mesophylla.maconnelli", "Desmodus.rotundus", "Glossophaga.soricina", "Anoura.sp",
                            "Phyllostomus.discolor", "Phyllostomus.elongatus", "Phyllostomus.hastatus",
                            "Gardnerycteris.crenulatum", "Tonatia.saurophila", "Micronicteris.sp", "Lophostoma.brasiliense",
                            "Rhinophylla.sp", "S"))
str(DF1)
DF2 <- stack(DF, select = c("forest.perc.05", "forest.perc.125", "grass.perc.05", "grass.perc.2", "pssd.05",
                            "p.no.05"))
str(DF2)
DF3 <- stack(DF, select = c("CWM.body_mass", "CWM.wing_span", "CWM.wing_loading", "CWM.aspect_ratio",
                            "FRic", "FEve", "FDiv"))
str(DF3)
DF4 <- cbind(DF2, DF1, DF3)
names(DF4) <- c("lands.metr.value", "lands.metr", "abundance", "bat.species", "FD.index.value", "FD.index")
library(utils)
View(DF4)
str(DF4)
summary(DF4)
#explorando los datos

library(doBy)
summaryBy(DF[,c(2:22)]~site, data = DF, FUN = c(mean, sd, median), na.rm = T)
summary(DF[,c(2:22)])

require(ggplot2)

#paper DT
Sforest0.5 <- ggplot(DF, aes(forest.perc.05, S))
Sforest0.5 + geom_point(na.rm = T) + geom_smooth(method = "glm", se = T, na.rm = T)

Sforest1.25 <- ggplot(DF, aes(forest.perc.125, S))
Sforest1.25 + geom_point(na.rm = T) + geom_smooth(method = "glm", se = T, na.rm = T)

Sgrass0.5 <- ggplot(DF, aes(grass.perc.05, S))
Sgrass0.5 + geom_point(na.rm = T) + geom_smooth(method = "glm", se = T, na.rm = T)

Sgrass1.25 <- ggplot(DF, aes(grass.perc.125, S))
Sgrass1.25 + geom_point(na.rm = T) + geom_smooth(method = "glm", se = T, na.rm = T)

Spssd <- ggplot(DF, aes(pssd.05, S))
Spssd + geom_point(na.rm = T) + geom_smooth(method = "glm", se = T, na.rm = T)

Spno <- ggplot(DF, aes(p.no.05, S))
Spno + geom_point(na.rm = T) + geom_smooth(method = "glm", se = T, na.rm = T)

#paper FD
batWL <- ggplot(DF, aes(site, CWM.wing_loading), na.rm= T)
batWL + stat_summary(fun.y = mean, geom = "bar", colour = "Black", na.rm = T) + stat_summary(fun.data = mean_cl_boot, 
                                                                                             geom = "errorbar", position = position_dodge(width = 0.9), width = 0.2, na.rm = T)
batFR <- ggplot(DF, aes(site, FRic), na.rm= T)
batFR + stat_summary(fun.y = mean, geom = "bar", colour = "Black", na.rm = T) + stat_summary(fun.data = mean_cl_boot, 
                                                                                             geom = "errorbar", position = position_dodge(width = 0.9), width = 0.2, na.rm = T)

batFD <- ggplot(DF, aes(site, FDiv), na.rm= T)
batFD + stat_summary(fun.y = mean, geom = "bar", colour = "Black", na.rm = T) + stat_summary(fun.data = mean_cl_boot, 
                                                                                             geom = "errorbar", position = position_dodge(width = 0.9), width = 0.2, na.rm = T)

batFE <- ggplot(DF, aes(site, FEve), na.rm= T)
batFE + stat_summary(fun.y = mean, geom = "bar", colour = "Black", na.rm = T) + stat_summary(fun.data = mean_cl_boot, 
                                                                                             geom = "errorbar", position = position_dodge(width = 0.9), width = 0.2, na.rm = T)
WLforest0.5 <- ggplot(DF, aes(forest.perc.05, CWM.wing_loading))
WLforest0.5 + geom_point(na.rm = T) + geom_smooth(method = "glm", se = T, na.rm = T)

WLforest1.25 <- ggplot(DF, aes(forest.perc.125, CWM.wing_loading))
WLforest1.25 + geom_point(na.rm = T) + geom_smooth(method = "glm", se = T, na.rm = T)

WLgrass0.5 <- ggplot(DF, aes(grass.perc.05, CWM.wing_loading))
WLgrass0.5 + geom_point(na.rm = T) + geom_smooth(method = "glm", se = T, na.rm = T)

WLgrass1.25 <- ggplot(DF, aes(grass.perc.125, CWM.wing_loading))
WLgrass1.25 + geom_point(na.rm = T) + geom_smooth(method = "glm", se = T, na.rm = T)

WLpssd <- ggplot(DF, aes(pssd.05, CWM.wing_loading))
WLpssd + geom_point(na.rm = T) + geom_smooth(method = "glm", se = T, na.rm = T)

WLpno <- ggplot(DF, aes(p.no.05, CWM.wing_loading))
WLpno + geom_point(na.rm = T) + geom_smooth(method = "glm", se = T, na.rm = T)

# elecci?n de la funci?n de distribuci?n de probabilidad

summary(DF[,c(2:22)])

#Carollia
require(fitdistrplus)
poisson=fitdist(DF$Carollia.perspicillata,"pois")
lognormal=fitdist(DF$Carollia.perspicillata,"lnorm")
negbinom=fitdist(DF$Carollia.perspicillata,"nbinom")#esta
normal=fitdist(DF$Carollia.perspicillata, "norm")
par(mfrow=c(1,2), mai=c(1.5,1.5,0.25,0.25), cex.lab=1.3, cex.axis=1.2, cex=1.5)
cdfcomp(list(negbinom),addlegend=T)
qqcomp(list(negbinom),addlegend=T)
cdfcomp(list(poisson),addlegend=T)
qqcomp(list(poisson),addlegend=T)
cdfcomp(list(poisson,negbinom,normal),addlegend=T,main="",
        legendtext=c("Poisson","NegBinom", "Normal"))
qqcomp(list(poisson,negbinom, normal),addlegend=T, main="",
       legendtext=c("Poisson","NegBinom", "Normal"))
par(mfrow=c(1,1),cex.lab=1, cex.axis=1, cex=1)
gofstat(list(negbinom, poisson,normal))

#A. planirostris
poisson=fitdist(DF$Artibeus.planirostris,"pois")
lognormal=fitdist(DF$Artibeus.planirostris,"lnorm")
negbinom=fitdist(DF$Artibeus.planirostris,"nbinom")#esta o lnorm
normal=fitdist(DF$Artibeus.planirostris, "norm")
par(mfrow=c(1,2), mai=c(1.5,1.5,0.25,0.25), cex.lab=1.3, cex.axis=1.2, cex=1.5)
cdfcomp(list(negbinom),addlegend=T)
qqcomp(list(negbinom),addlegend=T)
cdfcomp(list(poisson),addlegend=T)
qqcomp(list(poisson),addlegend=T)
cdfcomp(list(poisson,negbinom,lognormal,normal),addlegend=T,main="",
        legendtext=c("Poisson","NegBinom", "logN", "Norm"))
qqcomp(list(poisson,negbinom,lognormal,normal),addlegend=T, main="",
       legendtext=c("Poisson","NegBinom", "logN", "Norm"))
par(mfrow=c(1,1),cex.lab=1, cex.axis=1, cex=1)
gofstat(list(poisson,negbinom,lognormal,normal))

#sturnira
poisson=fitdist(DF$Sturnira.lilium,"pois")
#lognormal=fitdist(DF$Sturnira.lilium,"lnorm")
negbinom=fitdist(DF$Sturnira.lilium,"nbinom")#esta, aunque no ajusta del todo bien. Muchos ceros. Usar modelo de mezcla.
normal=fitdist(DF$Sturnira.lilium, "norm")
par(mfrow=c(1,2), mai=c(1.5,1.5,0.25,0.25), cex.lab=1.3, cex.axis=1.2, cex=1.5)
cdfcomp(list(negbinom),addlegend=T)
qqcomp(list(negbinom),addlegend=T)
cdfcomp(list(poisson),addlegend=T)
qqcomp(list(poisson),addlegend=T)
cdfcomp(list(poisson,negbinom,normal),addlegend=T,main="",
        legendtext=c("Poisson","NegBinom","norml"))
qqcomp(list(poisson,negbinom, normal),addlegend=T, main="",
       legendtext=c("Poisson","NegBinom","normal"))
par(mfrow=c(1,1),cex.lab=1, cex.axis=1, cex=1)
gofstat(list(poisson,negbinom,normal))

(table(DF$Sturnira.lilium==0)*100)/26

#P. discolor
poisson=fitdist(DF$Phyllostomus.discolor,"pois")
lognormal=fitdist(DF$Phyllostomus.discolor,"lnorm")
negbinom=fitdist(DF$Phyllostomus.discolor,"nbinom")#esta, aunque no ajusta del todo bien. Muchos ceros
normal=fitdist(DF$Phyllostomus.discolor, "norm")
par(mfrow=c(1,2), mai=c(1.5,1.5,0.25,0.25), cex.lab=1.3, cex.axis=1.2, cex=1.5)
cdfcomp(list(negbinom),addlegend=T)
qqcomp(list(negbinom),addlegend=T)
cdfcomp(list(poisson),addlegend=T)
qqcomp(list(poisson),addlegend=T)
cdfcomp(list(poisson,negbinom,normal),addlegend=T,main="",
        legendtext=c("Poisson","NegBinom","norm"))
qqcomp(list(poisson,negbinom,normal),addlegend=T, main="",
       legendtext=c("Poisson","NegBinom","norm"))
par(mfrow=c(1,1),cex.lab=1, cex.axis=1, cex=1)

(table(DF$Phyllostomus.discolor==0)*100)/26

#S
poisson=fitdist(DF$S, "pois")
normmal=fitdist(DF$S, "norm")
bneg=fitdist(DF$S, "nbinom")
logN=fitdist(DF$S, "lnorm")
par(mfrow=c(1,2), mar=c(4,4,0.25,0.25), cex.lab=1.3, cex.axis=1.2, cex=1.5)
cdfcomp(list(poisson, normmal,bneg,logN), addlegend = T, main = "", 
        legendtext = c("Pois","Norm","BNeg","LogN"))
qqcomp(list(poisson, normmal,bneg,logN), addlegend = T, main="",
       legendtext = c("Pois","Norm","BNeg","LogN"))
par(mfrow=c(1,1), cex.lab=1, cex.axis=1, cex=1)

# mismo grafico cdf en "formato" ggplot2 -> se puede usar para cualquiera de los casos anteriores
cdf=cdfcomp(list(poisson,negbinom),addlegend=T,main="",
            legendtext=c("Poisson","Lognormal","NegBinom"), plotstyle = "ggplot")
require(ggplot2)
cdf+geom_point(size=2.5) + geom_line(size=1.5)+
  theme(axis.title=element_text(size=20),axis.text=element_text(size=14),
        legend.text=element_text(size=14), legend.position=c(0.8,0.25))

#paper DF funci?n de distribuci?n de probabilidad

#wing loading
DF5 <- na.omit(DF)
gamma=fitdist(DF5$CWM.wing_loading,"gamma")
lognormal=fitdist(DF5$CWM.wing_loading,"lnorm")
negbinom=fitdist(DF5$CWM.wing_loading,"nbinom")
normal=fitdist(DF5$CWM.wing_loading, "norm")#mejor ajuste
par(mfrow=c(1,2), mai=c(1.5,1.5,0.25,0.25), cex.lab=1.3, cex.axis=1.2, cex=1.5)
cdfcomp(list(gamma, lognormal,normal),addlegend=T,
        legendtext = c("Gamma","LogNorm","Normal"))
qqcomp(list(gamma, lognormal,normal),addlegend=T,
       legendtext = c("Gamma","LogNorm","Normal"))
cdfcomp(list(lognormal),addlegend=T)
qqcomp(list(lognormal),addlegend=T)
cdfcomp(list(normal),addlegend=T)
qqcomp(list(normal),addlegend=T)

cdf=cdfcomp(list(gamma,normal,lognormal),addlegend=T,main="",
            legendtext=c("Gamma", "Normal","Lognormal"), plotstyle = "ggplot")
cdf+geom_point(size=2.5) + geom_line(size=1)+
  theme(axis.title=element_text(size=20),axis.text=element_text(size=14),
        legend.text=element_text(size=14), legend.position=c(0.8,0.25))
par(mfrow=c(1,1),cex.lab=1, cex.axis=1, cex=1)

gofstat(list(gamma,lognormal,normal))

#FRic
DF5 <- na.omit(DF)
gamma=fitdist(DF5$FRic,"gamma")#mejor 
lognormal=fitdist(DF5$FRic,"lnorm")
negbinom=fitdist(DF5$FRic,"nbinom")
normal=fitdist(DF5$FRic, "norm")
par(mfrow=c(1,2), mai=c(1.5,1.5,0.25,0.25), cex.lab=1.3, cex.axis=1.2, cex=1.5)
cdfcomp(list(gamma, lognormal,normal),addlegend=T,
        legendtext = c("Gamma","LogNorm","Normal"))
qqcomp(list(gamma, lognormal,normal),addlegend=T,
       legendtext = c("Gamma","LogNorm","Normal"))
cdfcomp(list(gamma),addlegend=T)
qqcomp(list(gamma),addlegend=T)
cdfcomp(list(lognormal),addlegend=T)
qqcomp(list(lognormal),addlegend=T)
cdfcomp(list(normal),addlegend=T)
qqcomp(list(normal),addlegend=T)

cdf=cdfcomp(list(gamma,normal,lognormal),addlegend=T,main="",
            legendtext=c("Gamma", "Normal","Lognormal"), plotstyle = "ggplot")
cdf+geom_point(size=2.5) + geom_line(size=1)+
  theme(axis.title=element_text(size=20),axis.text=element_text(size=14),
        legend.text=element_text(size=14), legend.position=c(0.8,0.25))

par(mfrow=c(1,1),cex.lab=1, cex.axis=1, cex=1)
gofstat(list(gamma,lognormal,normal))

#FEve
gamma=fitdist(DF5$FEve,"gamma")
lognormal=fitdist(DF5$FEve,"lnorm")
negbinom=fitdist(DF5$FEve,"nbinom")
normal=fitdist(DF5$FEve, "norm")#mejor ajuste
par(mfrow=c(1,2), mai=c(1.5,1.5,0.25,0.25), cex.lab=1.3, cex.axis=1.2, cex=1.5)
cdfcomp(list(gamma, lognormal,normal),addlegend=T,
        legendtext = c("Gamma","LogNorm","Normal"))
qqcomp(list(gamma, lognormal,normal),addlegend=T,
       legendtext = c("Gamma","LogNorm","Normal"))

cdfcomp(list(gamma),addlegend=T)
qqcomp(list(gamma),addlegend=T)
cdfcomp(list(lognormal),addlegend=T)
qqcomp(list(lognormal),addlegend=T)
cdfcomp(list(normal),addlegend=T)
qqcomp(list(normal),addlegend=T)

cdf=cdfcomp(list(gamma,normal,lognormal),addlegend=T,main="",
            legendtext=c("Gamma", "Normal","Lognormal"), plotstyle = "ggplot")
cdf+geom_point(size=2.5) + geom_line(size=1)+
  theme(axis.title=element_text(size=20),axis.text=element_text(size=14),
        legend.text=element_text(size=14), legend.position=c(0.8,0.25))

par(mfrow=c(1,1),cex.lab=1, cex.axis=1, cex=1)
gofstat(list(gamma,lognormal,normal))

#FDiv
gamma=fitdist(DF5$FDiv,"gamma")
lognormal=fitdist(DF5$FDiv,"lnorm")
negbinom=fitdist(DF5$FDiv,"nbinom")
normal=fitdist(DF5$FDiv, "norm")#mejor ajuste
par(mfrow=c(1,2), mai=c(1.5,1.5,0.25,0.25), cex.lab=1.3, cex.axis=1.2, cex=1.5)
cdfcomp(list(gamma, lognormal,normal),addlegend=T,
        legendtext = c("Gamma","LogNorm","Normal"))
qqcomp(list(gamma, lognormal,normal),addlegend=T,
       legendtext = c("Gamma","LogNorm","Normal"))
cdfcomp(list(gamma),addlegend=T)
qqcomp(list(gamma),addlegend=T)
cdfcomp(list(lognormal),addlegend=T)
qqcomp(list(lognormal),addlegend=T)
cdfcomp(list(normal),addlegend=T)
qqcomp(list(normal),addlegend=T)

cdf=cdfcomp(list(gamma,normal,lognormal),addlegend=T,main="",
            legendtext=c("Gamma", "Normal","Lognormal"), plotstyle = "ggplot")
cdf+geom_point(size=2.5) + geom_line(size=1)+
  theme(axis.title=element_text(size=20),axis.text=element_text(size=14),
        legend.text=element_text(size=14), legend.position=c(0.8,0.25))

par(mfrow=c(1,1),cex.lab=1, cex.axis=1, cex=1)

gofstat(list(lognormal,gamma, normal))
