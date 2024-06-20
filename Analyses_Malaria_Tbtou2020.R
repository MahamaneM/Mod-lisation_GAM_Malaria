##############Analyse des données sur le paludisme###################
###########Dynamique spatio-temporelles des cas de paludisme#########
#######################Dr MAIGA Mahamane#############################

#Imortation de la base de données----

library(readxl)
BaseACP <- read_excel("~/Projet_PaluTbtu_ACP&Modèle GAM/BDD_ACP&GAM.xlsx")

#Exploration des données ----
summary(BaseACP)
str(BaseACP)

#Analyse descriptives----
library(gtsummary)
library(tidyverse)
theme_gtsummary_language(language = "fr",decimal.mark = ",",big.mark = " ")

BaseACP %>% 
  select(Cas,Annee) %>% 
  tbl_summary(by=Annee,statistic = list(all_continuous() ~ "{sum}")) %>% 
  add_overall(last=T)

BaseACP %>% 
  select(Cas,Inc,Annee) %>% 
  tbl_summary(by=Annee) %>% 
  add_overall(last=T) %>% 
  add_p()

BaseACP %>% 
  select("T°Moy_min" ,"T°Moy_max",Annee) %>% 
  tbl_summary(by=Annee) %>% 
  add_overall(last=T)

#Analyse factorielle: L'ACP pour reduire nos variables dans des dimensions
library(FactoMineR)
library(factoextra)
library(FactoInvestigate)

res<- PCA(BaseACP[,c(6:30)],quali.sup = 2,quanti.sup =1)
fviz_eig(res, addlabels=TRUE)
fviz_pca_var(res, choix = "var", axes = c(1,2),col.var="cos2",gradient.cols=c("#00AFBB","#E7B800","#FC4E07"),repel=TRUE)
fviz_pca_var(res, choix = "var", axes = c(1,3),col.var="cos2",gradient.cols=c("#00AFBB","#E7B800","#FC4E07"),repel=TRUE)
fviz_pca_var(res, choix = "var", axes = c(2,3),col.var="cos2",gradient.cols=c("#00AFBB","#E7B800","#FC4E07"),repel=TRUE)

Investigate(res)
Investigate(res,file = "CA.Rmd", document = "pdf_document")


##Les contributions des variables aux dimensions----
fviz_contrib(res, choice = "var", axes = 1, top = 30)
fviz_contrib(res, choice = "var", axes = 2, top = 15) 
fviz_contrib(res, choice = "var", axes = 3, top = 15)
fviz_contrib(res, choice = "var", axes = 4, top = 15)

##Creation de la base des dimensions----
Dataset <- cbind(BaseACP, res$ind$coord[,1:3])
summary(Dataset)


#Calcul du décalage----
library(mgcv)
Met <- Dataset$Dim.1
Mod0 <- gam(Cas~offset(log(Pop))+s(Met), data= Dataset, family = nb())

summary(Mod0)

plot(Mod0)

Dev.res <- summary(Mod0)$dev.expl

GCV.res <- Mod0$gcv.ubre


for (i in 1:15){
  
  
  Clag1 <- Dataset$Cas[-c(1:i)]
  
  Poplag1 <- Dataset$Pop[-c(1:i)]
  
  lag1 <- Met[-c((105-(i-1)):105)]
  
  Mod1 <- gam(Clag1~offset(log(Poplag1))+s(lag1), family = nb())
  
  summary(Mod1)
  
  plot(Mod1)
  
  Dev.res <- cbind(Dev.res, summary(Mod1)$dev.expl)
  
  GCV.res <- cbind(GCV.res, Mod1$gcv.ubre)
  
  
}

Dev.res

GCV.res


lag <- 5

Met <- Dataset$Dim.1


ClagFin <- Dataset$Cas[-c(1:lag)]

PoplagFin <- Dataset$Pop[-c(1:lag)]

lagFin <- Met[-c((105-(lag-1)):105)]

ModFin <- gam(ClagFin~offset(log(PoplagFin))+s(lagFin), family = nb())

summary(ModFin)

plot(ModFin)


Met <- Dataset$Dim.2


Mod0 <- gam(Cas~offset(log(Pop))+s(Met), data= Dataset, family = nb())

summary(Mod0)

plot(Mod0)

Dev.res <- summary(Mod0)$dev.expl

GCV.res <- Mod0$gcv.ubre


for (i in 1:15){
  
  
  Clag1 <- Dataset$Cas[-c(1:i)]
  
  Poplag1 <- Dataset$Pop[-c(1:i)]
  
  lag1 <- Met[-c((105-(i-1)):105)]
  
  Mod1 <- gam(Clag1~offset(log(Poplag1))+s(lag1), family = nb())
  
  summary(Mod1)
  
  plot(Mod1)
  
  Dev.res<- cbind(Dev.res, summary(Mod1)$dev.expl)
  
  GCV.res <- cbind(GCV.res, Mod1$gcv.ubre)
  
  
}

Dev.res

GCV.res


lag <- 9

Met <- Dataset$Dim.2


ClagFin <- Dataset$Cas[-c(1:lag)]

PoplagFin <- Dataset$Pop[-c(1:lag)]

lagFin <- Met[-c((105-(lag-1)):105)]

ModFin <- gam(ClagFin~offset(log(PoplagFin))+s(lagFin), family = nb())

summary(ModFin)

plot(ModFin)


Met <- Dataset$Dim.3


Mod0 <- gam(Cas~offset(log(Pop))+s(Met), data= Dataset, family = nb())

summary(Mod0)

plot(Mod0)

Dev.res <- summary(Mod0)$dev.expl

GCV.res <- Mod0$gcv.ubre


for (i in 1:15){
  
  
  Clag1 <- Dataset$Cas[-c(1:i)]
  
  Poplag1 <- Dataset$Pop[-c(1:i)]
  
  lag1 <- Met[-c((105-(i-1)):105)]
  
  Mod1 <- gam(Clag1~offset(log(Poplag1))+s(lag1), family = nb())
  
  summary(Mod1)
  
  plot(Mod1)
  
  Dev.res <- cbind(Dev.res, summary(Mod1)$dev.expl)
  
  GCV.res <- cbind(GCV.res, Mod1$gcv.ubre)
  
  
}

Dev.res

GCV.res


lag <- 13

Met <- Dataset$Dim.3


ClagFin <- Dataset$Cas[-c(1:lag)]

PoplagFin <- Dataset$Pop[-c(1:lag)]

lagFin <- Met[-c((105-(lag-1)):105)]

ModFin <- gam(ClagFin~offset(log(PoplagFin))+s(lagFin), family = nb())

summary(ModFin)

plot(ModFin)


lag <- 13


ClagFin <- Dataset$Cas[-c(1:lag)]

PoplagFin <- Dataset$Pop[-c(1:lag)]

AnFin <- Dataset$Annee[-c(1:lag)]

#Met1 <- Dataset$Dim.1[-c((1:(lag-1)),105)]

#Met2 <- Dataset$Dim.2[-c((36-(lag-1)):105)]

#Met3<- Dataset$dim.3[-c((36-(lag-1)):105)]

SEI1 <- Dataset$Dim.1[-c((1:(lag)),105+4)]

SEI2 <- Dataset$Dim.2[-c(1:lag)]

SEI3 <- Dataset$Dim.3[-c(1:lag)]


length(ClagFin) 

length(PoplagFin)


length(SEI1)

length(SEI2)

length(SEI3)

#Modélisation GAM prenant en compte le décalage----
#A GARDER 1mois pour Met 1, 0 pour Met2 (Modele GAM prenant en compte le decalage=lag)

K=10

ModGeneral <- gam(ClagFin~offset(log(PoplagFin))+s(SEI1,k=K,bs="cs")+s(SEI2,k=K, bs="cs")+s(SEI3,k=K, bs="cs"), family ="quasipoisson")



plot(ModGeneral, pages = 1, )

summary(ModGeneral)

layout(1)



plot_gam(ModGeneral, ylab="SEI1")

plot_gam(ModGeneral, pred = "SEI2", title = "Predictor 'SEI2'")

plot_gam(ModGeneral, pred = "SEI1", title = "Predictor 'SEI1'")

# Modele GAM prenant en consideration SEI1 et SEI2 et le decalage----

K=10

ModGeneral2 <- gam(ClagFin~offset(log(PoplagFin))+s(SEI1,k=K,bs="cs")+s(SEI2,k=K, bs="cs"), family ="quasipoisson")

plot(ModGeneral2, pages = 1, )

summary(ModGeneral2)

layout(1)



#Analyse de la Serie temporelle----

##Representation de la serie temporelle----

inc.ts<- ts(BaseACP$Inc,start = 2020,frequency = 52)
plot(inc.ts)

##Detection des points de changements avec le changepointAnalysis----

library(changepoint)

###Decoupage par la moyenne

cpt.mean(inc.ts)
plot(cpt.mean(inc.ts))

###Decoupage par la variance

cpt.var(inc.ts)
plot(cpt.var(inc.ts))

###Decoupage par la moyenne et la variance

cpt.meanvar(inc.ts)
plot(cpt.meanvar(inc.ts))

plot(cpt.meanvar(inc.ts),cpt.width=3)
print(cpt.meanvar(inc.ts))

###Multiples changepoints
mcpt1 <- changepoint::cpt.mean(inc.ts, method = "PELT")
plot(mcpt1,cpt.width=2)

mcpt2 <- changepoint::cpt.var(inc.ts, method = "PELT")
plot(mcpt2,cpt.width=2)


mcpt <- changepoint::cpt.meanvar(inc.ts, penalty="AIC",minseglen=10,method = "PELT")
plot(mcpt,cpt.width=2)
print(mcpt)

mcpt <- changepoint::cpt.meanvar(log(inc.ts), penalty="AIC",minseglen=10,method = "PELT")
plot(mcpt,cpt.width=2,ylab="Log Incidence")
print(mcpt)

####Ajustement des parametres du changepoint analysis-----
mcpt <- changepoint::cpt.meanvar(inc.ts, penalty="AIC",minseglen=15,method = "PELT")
plot(mcpt,cpt.width=2,xlab= "Temps",ylab="Incidence")
print(mcpt)

####Ajustement avec le log
mcpt <- changepoint::cpt.meanvar(log(inc.ts), penalty="AIC",minseglen=15,method = "PELT")
plot(mcpt,cpt.width=2,xlab= "Temps",ylab="Log Incidence")
print(mcpt)

library(ggchangepoint)
ggcptplot(
  inc.ts,
  change_in = "mean_var",
  cp_method = "PELT",
  cptline_alpha = 1,
  cptline_color = "blue",
  cptline_type = "solid",
  cptline_size = 0.5)

ggecpplot(
  inc.ts,
  algorithm = "divisive",
  min_size = 2,
  cptline_alpha = 1,
  cptline_color = "blue",
  cptline_type = "solid",
  cptline_size = 0.5)


###Analyse de la tendance et saisonnalité----

plot(inc.ts,ylab="Incidence hebdomadaire")
inc.decom.add<-decompose(inc.ts,type = "add")
plot(inc.decom.add)

inc.decomp.mult<-decompose(inc.ts,type = "mult")
plot(inc.decomp.mult)


################################FIN################################
