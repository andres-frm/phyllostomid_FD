#Data analisis 

#bat functional diversity


#CWM WL----

DF <- read.csv("data.actualizado.csv", header = T, sep = ";", dec = ",")
str(DF)

for (i in seq_along(DF)) {
  if (grepl("^f(.*)*\\.05$", colnames(DF)[i]) ||
      grepl("^g(.*)*\\.05$", colnames(DF[i])) ||
      grepl("^f(.*)*\\.125$", colnames(DF[i])) ||
      grepl("^g(.*)*\\.2$", colnames(DF[i]))) {
    DF[i] <- as.vector(scale(DF[[i]], center = T, scale = T))
  }
}


library(fitdistrplus)
DF5 <- na.omit(DF)
gamma=fitdist(DF5$CWM.wing_loading,"gamma")
lognormal=fitdist(DF5$CWM.wing_loading,"lnorm")
normal=fitdist(DF5$CWM.wing_loading, "norm")#mejor ajuste
par(mfrow=c(1,2))
cdfcomp(list(gamma, lognormal,normal),addlegend=T,
        legendtext = c("Gamma","LogNorm","Normal"))
qqcomp(list(gamma, lognormal,normal),addlegend=T,
       legendtext = c("Gamma","LogNorm","Normal"))
gofstat(list(gamma,lognormal,normal))
par(mfrow=c(1,1))


CWMm1 <- glm(CWM.wing_loading ~ forest.perc.05 + grass.perc.05 +
            forest.perc.125 + grass.perc.2, data = DF)

summary(CWMm1)

anova(CWMm1, test = "Chi")

CWMm2 <- update(CWMm1, ~.-forest.perc.05)
anova(CWMm1,CWMm2, test = "Chi")

CWMm3 <- update(CWMm2, ~. -grass.perc.05)
anova(CWMm2, CWMm3, test = "Chi")

CWMm4 <- update(CWMm2, ~. -forest.perc.125)
anova(CWMm2, CWMm4, test = "Chi")

CWMm5 <- update(CWMm2, ~. -grass.perc.2)
anova(CWMm2, CWMm5, test = "Chi")

summary(CWMm2)#varianza explicada por el modelo 78.73
anova(CWMm2, test = "F")
confint(CWMm2)

#funciones para el c?lculo de effect size
r.se<-function(t.val,df,n){
  # Calcula los valores de r y su error standard en la escala arctanh-1 de Fisher.
  # INPUTS DE LA FUNCION: 
  # t.val: valor de t para la var. explciativa numerica obtenido de la tabla summary del modelo
  # df: grados de libertad residuales del modelo obtenido del summary o la tabla anova del modelo
  # n: número de datos obtenido del # de filas del DF que no sean NA.
  # OUTPUTS DE LA FUNCION: los valores de r y su error standard en la escala transformada
  r<-0.5*log((1+(t.val/sqrt((t.val)^2+df)))/(1-(t.val/sqrt((t.val)^2+df))))
  se<-(1/sqrt(n-3))
  names(r)<-"r transf"
  names(se)<-"SE(r transf)"
  c(r,se)
}

#c?lculo de los cuantiles para calcular los IC
conf.limits.nct <- function(tval.1,df,conf){       
  # tval.1: t valor de la tabla summary 
  # df: grados de libertad del modelo 
  # conf: nivel de confianza deseado. Generalmente 0.95
  # Esta función emplea la función ptnoncent que tambien debe ser cargada.
  Result <- matrix(NA,1,4)
  tval <- abs(tval.1)
  ############################ Estima el cuantil inferior ###########################
  ulim <- 1 - (1-conf)/2
  lc <- c(-tval,tval/2,tval)
  while(ptnoncent(tval,df,lc[1])<ulim)    {
    lc <- c(lc[1]-tval,lc[1],lc[3])  }
  #  Estima el cuantil inferior 
  diff <- 1
  while(diff > .00000001)      {
    if(ptnoncent(tval,df,lc[2])<ulim)
      lc <- c(lc[1],(lc[1]+lc[2])/2,lc[2])
    else lc <- c(lc[2],(lc[2]+lc[3])/2,lc[3])
    diff <- abs(ptnoncent(tval,df,lc[2]) - ulim)
    ucdf <- ptnoncent(tval,df,lc[2])}
  res.1 <- ifelse(tval.1 >= 0,lc[2],-lc[2])
  ############################ Estima el cuantil superior ###########################
  llim <- (1-conf)/2
  uc <- c(tval,1.5*tval,2*tval)
  while(ptnoncent(tval,df,uc[3])>llim)   {
    uc <- c(uc[1],uc[3],uc[3]+tval)
  }
  diff <- 1
  while(diff > .00000001)         {
    if(ptnoncent(tval,df,uc[2])<llim)
      uc <- c(uc[1],(uc[1]+uc[2])/2,uc[2])
    else uc <- c(uc[2],(uc[2]+uc[3])/2,uc[3])
    diff <- abs(ptnoncent(tval,df,uc[2]) - llim)
    lcdf <- ptnoncent(tval,df,uc[2])
  }
  res <- ifelse(tval.1 >= 0,uc[2],-uc[2])
  ############################## Pone los cuantiles y sus niveles confianza en una matriz #####################################
  Result[1,1] <- min(res,res.1)
  Result[1,2] <- lcdf
  Result[1,3] <- max(res,res.1)
  Result[1,4] <- ucdf
  dimnames(Result) <- list("Valores", c("Cuantil Inf", "Prob.Limite.Inf", "Cuantil.Sup", "Prob.Limite.Sup"))
  Result
}

#intervalos de confianza del effect size
IC_r<-function(r,se.r,cuan.inf,cuan.sup){
  # La funcion IC_r calcula los limites inferior y superior del IC de r.
  # INPUTS DE LA FUNCION: 
  # Los valores de r y su error standard se(r) calculados en la función r.se
  # Los cuantiles de la distr t no centrada calculados en la función conf.limits.nct 
  # Luego de calcular el los limites del IC, aplica la transformacion inversa de Fisher
  # para expresar el r y su IC en la escala correcta. 
  # OUTPUTS DE LA FUNCION: los valores de r y los limites de su intervalo de confianza  
  r<-(exp(2*r)-1)/(exp(2*r)+1) 
  l.inf=(exp(2*(r-(cuan.inf*se.r)))-1)/(exp(2*(r-(cuan.inf*se.r)))+1) 
  l.sup=(exp(2*(r+(cuan.sup*se.r)))-1)/(exp(2*(r+(cuan.sup*se.r)))+1) 
  names(r)<-"r"
  names(l.inf)<-"Limite.Inf IC"
  names(l.sup)<-"Limite.Sup IC"
  return(c(r,l.inf,l.sup))
}

ptnoncent <- function(tx, df, nonc = 0, itrmax = 1000, errmax= 1E-6)
{if(min(df) <= 0)
  stop("All df must be > 0")
  lengths <- c(length(tx), length(df), length(nonc))
  if(any(lengths < (ltx <- max(lengths)))) {
    tx <- rep(tx, length.out = ltx)
    df <- rep(df, length.out = ltx)
    nonc <- rep(nonc, length.out = ltx)}
  tnc <- numeric(ltx)
  del <- nonc
  negdel <- (tx < 0)
  del <- ifelse(negdel,  - del, del)
  xx <- (tx * tx)/(tx * tx + df)
  lambda <- del * del
  p <- 0.5 * exp(-0.5 * lambda)
  q <- 0.79788456080286496 * p * del
  ss <- 0.5 - p
  a <- rep(0.5, ltx)
  b <- 0.5 * df
  rxb <- (1 - xx)^b
  albeta <- 0.57236494292469997 + lgamma(b) - lgamma(a + b)
  xodd <- pbeta(xx, a, b)
  godd <- 2 * rxb * exp(a * log(xx) - albeta)
  xeven <- 1 - rxb
  geven <- b * xx * rxb
  tnc <- p * xodd + q * xeven
  itr <- 0
  err <- rep(1, ltx)
  while((itr <- itr + 1) <= itrmax && max(err) > errmax) {
    a <- a + 1
    xodd <- xodd - godd
    xeven <- xeven - geven
    godd <- (godd * xx * (a + b - 1))/a
    geven <- (geven * xx * (a + b - 0.5))/(a +0.5)
    p <- (p * lambda)/(2 * itr)
    q <- (q * lambda)/(2 * itr + 1)
    ss <- ss - p
    tnc <- tnc + p * xodd + q * xeven
    err <- 2 * ss * (xodd - godd)}
  if(itr > itrmax)
    warning("maximum number of iteration reached")
  tnc <- tnc + 1 - pnorm(del)
  ifelse(negdel, 1 - tnc, tnc)}


#verificaci?n del ajuste del modelo con un an?lisis de residuales

par(mfrow=c(1,2))
plot(CWMm2, which=c(1,2))
par(mfrow=c(1,1))

#visualizaci?n de resultados

library(ggplot2)
library(visreg);library(arm)


#tama?o del efecto e IC grass05
r.se(4.85,21,25)
conf.limits.nct(-4.85,21,0.95)
IC_r(0.92,0.21,2.37,7.25)#ES=0.72; IC.i=0.22; IC.s=0.97

p1 <- visreg(CWMm2, xvar="grass.perc.05",rug=1,type="conditional",
       scale="response",xlab="grass.per.05.c", cex=1.7,
       ylab="CWM.wing_loading", gg=T)+geom_jitter() + labs(x="Grass area 0.5 km", y="CWM wing loading") +
  theme_classic() + theme(axis.text = element_text(size = 11, colour = "black"), axis.title = element_text(size = 12), 
                          plot.margin = unit(c(0,0,0,0.2), "cm"))+
  ylim(11,15) + annotate("text", x = -1.4, y = 15, label = expression(paste(italic("r"),"= 0.72 (0.22 - 0.97)")), size = 3.5, hjust = 0, colour = "Black")
  #theme(legend.position = "none", axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, face = "italic", family = "Arial", size = 11.5), text = element_text(size = 12))
#ggsave("CWM_WL_grass05c.tiff", width = 15, height = 15, units = "cm", dpi = 300)


#tama?o del efecto e IC forest125
r.se(4.13,21,25)
conf.limits.nct(4.13,21,0.95)
IC_r(0.80,0.21,1.77,6.41)#ES=0.66; IC.i=0.28; IC.s=0.96

p2 <- visreg(CWMm2, xvar="forest.perc.125",rug=1,type="conditional",
       scale="response",xlab="forest.perc.125", cex=1.7,
       ylab="CWM.wing_loading", gg=T)+geom_jitter() + labs(x="Forest area 1.25 km", y="CWM wing loading") +
  theme_classic() + theme(axis.text.y = element_blank(), axis.text.x = element_text(size = 11, colour = "black"),
                          axis.title.y = element_blank(), axis.title.x = element_text(size = 12), plot.margin = unit(c(0,0,0,1), "cm")) +
  ylim(11,15)+ annotate("text", x = -0.95, y = 15, label = expression(paste(italic("r"), "= 0.66 (0.28 - 0.96)")), size = 3.5, hjust = 0, colour = "Black") +
  scale_x_continuous(breaks = c(-1,0,1))# + 

#ggsave("CWM_WL_forest125c.tiff", width = 15, height = 15, units = "cm", dpi = 300)

#tama?o del efecto e IC grass2
r.se(2.8,21,25)
conf.limits.nct(2.8,21,0.95)
IC_r(0.57,0.21,0.63,4.90)#ES=0.51; IC.i=0.36; IC.s=0.91

p3 <- visreg(CWMm2, xvar="grass.perc.2",rug= 1, type="conditional",
       scale="response",xlab="grass.per.2.c", cex=1.7,
       ylab="CWM.wing_loading", gg=T)+geom_jitter() + labs(x="Grass area 2km", y="CWM wing loading") +
  theme_classic() + theme(axis.text.x = element_text(size = 11, colour = "black"), axis.text.y = element_blank(), 
                          axis.title.y = element_blank(), axis.title.x = element_text(size = 12), plot.margin = unit(c(0,0,0,1), "cm")) + 
  ylim(11,15)+annotate("text", x = -1.3, y = 15, label = expression(paste(italic("r"), "= 0.51 (0.36 - 0.91)")), size = 3.5, hjust = 0, colour = "Black")# + 

#ggsave("CWM_WL_grass2c.tiff", width = 15, height = 15, units = "cm", dpi = 300)


library(cowplot)
par(mar=c(20,10,20,10))
plot_grid(p1, p2, p3, ncol = 3, align = "h")
#ggsave("model_cwm.tiff", width= 15, height = 14,units = "cm", dpi = 300)


#FRic----


DF5 <- na.omit(DF)
gamma=fitdist(DF5$FRic,"gamma")#mejor 
lognormal=fitdist(DF5$FRic,"lnorm")
normal=fitdist(DF5$FRic, "norm")
par(mfrow=c(1,2))
cdfcomp(list(gamma, lognormal,normal),addlegend=T,
        legendtext = c("Gamma","LogNorm","Normal"))
qqcomp(list(gamma, lognormal,normal),addlegend=T,
       legendtext = c("Gamma","LogNorm","Normal"))
par(mfrow=c(1,1),cex.lab=1, cex.axis=1, cex=1)
gofstat(list(gamma,lognormal,normal))


library(lme4)

fricM <- glm(FRic ~ forest.perc.05 + grass.perc.05 +
               forest.perc.125 + grass.perc.2, family = Gamma(link = "log"), data = DF)
summary(fricM)

anova(fricM, test = "Chi")

#an?lisis de residuales 

par(mfrow=c(1,2))
plot(fricM, which=c(1,2))
par(mfrow=c(1,1))

#Feve----


gamma=fitdist(DF5$FEve,"gamma")
lognormal=fitdist(DF5$FEve,"lnorm")
normal=fitdist(DF5$FEve, "norm")#mejor ajuste
par(mfrow=c(1,2))
cdfcomp(list(gamma, lognormal,normal),addlegend=T,
        legendtext = c("Gamma","LogNorm","Normal"))
qqcomp(list(gamma, lognormal,normal),addlegend=T,
       legendtext = c("Gamma","LogNorm","Normal"))
par(mfrow=c(1,1),cex.lab=1, cex.axis=1, cex=1)
gofstat(list(gamma,lognormal,normal))
par(1, 1)

feveM <- glm(FEve~forest.perc.05 + grass.perc.05 +
               forest.perc.125 + grass.perc.2, data = DF)
summary(feveM)
anova(feveM,test = "Chi")

#LRT
feveM1 <- update(feveM, ~.-forest.perc.05)
anova(feveM,feveM1, test = "Chi")

feveM2 <- update(feveM1, ~.-grass.perc.05)
anova(feveM1, feveM2, test = "Chi")

feveM3 <- update(feveM2,~.-forest.perc.125)
anova(feveM2,feveM3, test = "Chi")

feveM4 <- update(feveM3, ~.-grass.perc.2)
anova(feveM4, feveM3, test = "Chi")

summary(feveM3)
anova(feveM3, test = "F")
confint(feveM3)

#an?lsiis de residuales
par(mfrow=c(1,2))
plot(feveM3, which=c(1,2))
par(mfrow=c(1,1), mar=c(1,1,1,1))

#visualizaci?n del modelo
#tama?o del efecto e intervalos de confianza
r.se(2.65,22,25)
conf.limits.nct(2.65,22,0.95)
IC_r(0.53,0.21,0.51,4.73)#ES=0.48; IC.i=0.36; IC.s=0.90

i1 <- visreg(feveM3, xvar="grass.perc.2",type="conditional",
       scale="response",xlab="grass.perc.2",rug=1, cex=1.7,
       ylab="FEve", gg=T)+geom_jitter() + labs(x="Grass area 2 km", y="FEve") +
  theme_classic() + theme(axis.title = element_text(size = 15), axis.text = element_text(colour = "black", size= 15)) + 
  ylim(0.2,0.7) +annotate("text", x = -1, y = 0.7, label = expression(paste(italic("r"), "= 0.48 (0.36 - 0.90)")), size = 4.5, hjust = 0, colour = "Black")# +  
#ggsave("feve_grass2c.tiff", width = 15, height = 15, units = "cm", dpi = 300)



#FDiv----


gamma=fitdist(DF5$FDiv,"gamma")
lognormal=fitdist(DF5$FDiv,"lnorm")
normal=fitdist(DF5$FDiv, "norm")#mejor ajuste
par(mfrow=c(1,2))
cdfcomp(list(gamma, lognormal,normal),addlegend=T,
        legendtext = c("Gamma","LogNorm","Normal"))
qqcomp(list(gamma, lognormal,normal),addlegend=T,
       legendtext = c("Gamma","LogNorm","Normal"))

par(mfrow=c(1,1))

gofstat(list(lognormal,gamma, normal))


mfdiv <- glm(FDiv~forest.perc.05 + grass.perc.05 +
               forest.perc.125 + grass.perc.2, data = DF)
summary(mfdiv)
anova(mfdiv,test = "Chi")

#LRT

mfdiv1 <- update(mfdiv, ~.-forest.perc.05)
anova(mfdiv,mfdiv1, test = "Chi")

mfdiv2 <- update(mfdiv1, ~.-grass.perc.05)
anova(mfdiv1,mfdiv2, test = "Chi")

mfdiv3 <- update(mfdiv2,~.-grass.perc.2)
anova(mfdiv2,mfdiv3, test = "Chi")

mfdiv4 <- update(mfdiv3, ~.-forest.perc.125)
anova(mfdiv4,mfdiv3, test = "Chi")

summary(mfdiv3)
anova(mfdiv3, test = "F")
confint(mfdiv3)

#residuales
par(mfrow=c(1,2), mar=c(5,6,3,3))
plot(mfdiv1, which = c(1,2))
par(mfrow=c(1,1), mar=c(1,1,1,1))


#visualizaci?n de resultados
#tama?o del efecto e intervalos de confianza
r.se(3.3,22,25)
conf.limits.nct(3.3,22,0.95)
IC_r(0.65,0.21,1.08,5.45)#ES=0.57; IC.i=0.33; IC.s=0.93

i2 <- visreg(mfdiv3, xvar="forest.perc.125",type="conditional",
       scale="response",xlab="forest.perc.125",rug=1, cex=1.7,
       ylab="FDiv", gg=T)+geom_jitter() + labs(x="Forest area 1.25 km", y="FDiv") +
  theme_classic() + theme(axis.title = element_text(size = 15, colour = "black"),axis.text = element_text(size = 15, colour= "black")) +
  annotate("text", x = -0.8, y = 1, label = expression(paste(italic("r"), "= 0.57 (0.33 - 0.93)")), size = 4.5, hjust = 0, colour = "Black") +
  scale_x_continuous(breaks = c(-1,0,1))
#ggsave("fdiv_forest125c.tiff", width = 15, height = 15, units = "cm", dpi = 300)

library(cowplot)
plot_grid(i1,i2, ncol=2, align = "h")
#ggsave("models_fdiv-feve.tiff", height = 15, width = 15, units = "cm", dpi= 300)


#number of functional groups----

noGF <- read.csv("data.FR.csv", header = T, sep = ";", dec = ",")
str(noGF)
str(DF)

DF$GF <- noGF$no.FG

library(fitdistrplus)

pois <- fitdist(DF$GF, "pois")
norm <- fitdist(DF$GF, "norm")
lnorm <- fitdist(DF$GF, "lnorm")
nbino <- fitdist(DF$GF, "nbinom")
par(mfrow=c(1,2))
cdfcomp(list(pois,norm,lnorm,nbino), addlegend = T,
        legendtext = c("Poisson", "Normal","LogNorm","nbinom"))
qqcomp(list(pois,norm,lnorm,nbino))
gofstat(list(pois,norm,lnorm,nbino))
par(mfrow=c(1,1))

FGm1 <- glm(GF~forest.perc.05 + grass.perc.05 +
            forest.perc.125 + grass.perc.2, data=DF)
summary(FGm1)
anova(FGm1, test = "Chisq")

#residual analysis
par(mfrow=c(1,2), mar=c(6,5,3,3))
plot(FGm1, which=c(1,2))
par(mfrow=c(1,1))

#likelihood ratio test
FGm2 <- update(FGm1,~.-grass.perc.2)
anova(FGm1,FGm2, test = "Chisq")

FGm3 <- update(FGm2, ~.-forest.perc.125)
anova(FGm2,FGm3, test = "Chisq")

FGm4 <- update(FGm3,~.-grass.perc.05)
anova(FGm3,FGm4, test = "Chisq")

FGm5 <- update(FGm4, ~.-forest.perc.05)
anova(FGm4,FGm5,test = "Chisq")

summary(FGm4)
confint(FGm4)
anova(FGm4, test = "F")

#residual analysis
par(mfrow=c(1,2))
plot(FGm4,which=c(1,2))
par(mfrow=c(1,1))


#size effect

r.se(3.176,24,25)
conf.limits.nct(3.176,25,0.95)
IC_r(0.609,0.213,1.002,5.29)# r=0.57(0.32-0.93)

library(visreg)

visreg(FGm4, xvar = "forest.perc.05", scale = "response", type = "conditional",
       rug=1, gg=T)+geom_jitter()+theme_classic()+labs(x="Forest area 0.5 km", y="Phyllostomid functional groups")+
  theme(axis.title = element_text(size=15,colour = "black"), axis.text = element_text(size = 15, colour = "black"))+
  annotate("text", x=-1.7, y=6.8, label= expression(paste(italic("r"),"= 0.57 (0.32 - 0.93)")), size = 5, hjust=0, colour= "black") +
  scale_x_continuous(breaks = c(-1,0,1.5)) + xlim(-1.85,1)
ggsave("noFG.tiff", width = 10, height = 15, units = "cm", dpi = 300)

#plot residual analyses all models
#par(mfrow=c(4,2), cex.lab=1.8, mar=c(6,5,3,3))
#plot(CWMm2, which = c(1,2))
#plot(feveM3, which=c(1,2))
#plot(mfdiv3, which=c(1,2))
#plot(FGm4, which = c(1,2))
#par(mfrow=c(1,1))
#dev.off()

