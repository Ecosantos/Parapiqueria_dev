#######################################################
#		Script Parapiqueria - FRAMEWORK
#		 	Em desevolvimento
#		Iniciado em: 05/08/2024
#		Ultima atualização: 06/08/2024
######################################################

library(popbio)
library(ggplot2)
library(tidyverse)
library(openxlsx)
library(ggrepel)
library(ggridges)
library(broom)

rm(list=ls())

setwd("C:/Artigos e resumos publicados submetidos ideias/Notes/9 - ITV/Parapiqueria - Dados e scripts")

#=======================================================
#		ACCESSORY FUNCTION
# Automatically detect C, b, and non-zero elements
#=======================================================
#ven is the target MPM

WoodPar<-function(ven){
mynonzero.ven<- which(ven> 0)	#Determina quem non-zero elementos da matriz de transição (subdiagonal + classes que reproduzem)
myC2ven.part1<-diag(-1,length(mynonzero.ven))
myC2ven.part2<-(numeric(dim(ven)[1]))%*%t(numeric(length(mynonzero.ven)))		#MODIFICADO!
diag(myC2ven.part2[,-c(seq(from=1,to=ncol(myC2ven.part2),by=1))])<-1			#+RECENTE
diag(myC2ven.part2[,-c(seq(from=0,to=(ncol(myC2ven.part2)),by=1))])<-1			#+RECENTE
myC2ven.part2<-as.data.frame(myC2ven.part2)
venR<-replacing(ven)
colnames(myC2ven.part2)<-venR[mynonzero.ven]
myC2ven.part2<-replace(myC2ven.part2, which(colnames(myC2ven.part2)=="F"),numeric(dim(myC2ven.part2)[[1]]))
myC2ven.part2<-as.matrix(myC2ven.part2)
colnames(myC2ven.part2)<-NULL
myC2ven<-rbind(myC2ven.part1,myC2ven.part2)
myb.ven<- apply(myC2ven, 1, max)
out<-list(myC2ven,myb.ven,mynonzero.ven)
names(out)<-c("C","b","nonzero")
return(out)
}

#=======================================================
#		EXAMPLE - Model Toy
#-------------------------------------------------------
# Simulate an hypothetical populational
# 		assess model's accuracy
#========================================================
file.edit("ModelToyv1.R")



#=======================================================
#	DATA LOADING and STANDARDIZATION
#-------------------------------------------------------
# Estimate recruitment in Parapiqueria between 2022-2024
#========================================================
#2022
Mar22<-read.xlsx("Dados brutos/Dados parapiqueria - Completo - Consolidado 05Aug2024.xlsx",sheet="Março2022")
Abr22<-read.xlsx("Dados brutos/Dados parapiqueria - Completo - Consolidado 05Aug2024.xlsx",sheet="Abril2022")
Mai22<-read.xlsx("Dados brutos/Dados parapiqueria - Completo - Consolidado 05Aug2024.xlsx",sheet="Maio2022")

#2023
Mar23<-read.xlsx("Dados brutos/Dados parapiqueria - Completo - Consolidado 05Aug2024.xlsx",sheet="Março2023")
Abr23<-read.xlsx("Dados brutos/Dados parapiqueria - Completo - Consolidado 05Aug2024.xlsx",sheet="Abril2023")
Mai23<-read.xlsx("Dados brutos/Dados parapiqueria - Completo - Consolidado 05Aug2024.xlsx",sheet="Maio2023")

#2024
Mar24<-read.xlsx("Dados brutos/Dados parapiqueria - Completo - Consolidado 05Aug2024.xlsx",sheet="Março2024")
Abr24<-read.xlsx("Dados brutos/Dados parapiqueria - Completo - Consolidado 05Aug2024.xlsx",sheet="Abril2024")
Mai24<-read.xlsx("Dados brutos/Dados parapiqueria - Completo - Consolidado 05Aug2024.xlsx",sheet="Maio2024")

#---------------------------------------------------------
#		Merge Census
#---------------------------------------------------------
##2024
#Mask quadrants as it won't be necessary here
census2024<-rbind(
data.frame(Mar24,Month="3"),
data.frame(Abr24,Month="4"),
data.frame(Mai24,Month="5"))%>%
as_tibble()%>%
group_by(Site,Plot,Month)%>%
summarise(Imaturos=sum(Imaturos),
		Reprodutivos=sum(Reprodutivos))%>%
ungroup()


##2023
census2023<-rbind(
data.frame(Mar23,Month="3"),
data.frame(Abr23,Month="4"),
data.frame(Mai23,Month="5"))%>%
as_tibble()%>%
group_by(Site,Plot,Month)%>%
summarise(Imaturos=sum(Imaturos),
		Reprodutivos=sum(Reprodutivos))%>%
ungroup()

##2022
#Masking quadrants is not necessary as data previous to 2023
# quadrant division does not exist
census2022<-rbind(
data.frame(Mar22,Month="3"),
data.frame(Abr22,Month="4"),
data.frame(Mai22,Month="5"))%>%
select(Site,Plot,Imaturos,Reprodutivos,Month)


#=======================================================
#			RECRUITMENT
#-------------------------------------------------------
# Estimate recruitment in Parapiqueria between 2022-2024
#========================================================

Census_all<-rbind(
cbind(census2024,year="2024"),
cbind(census2023,year="2023"),
cbind(census2022,year="2022"))%>%
pivot_longer(Imaturos:Reprodutivos,names_to="stage")


Census_all_max<-rbind(
cbind(census2024,year="2024"),
cbind(census2023,year="2023"),
cbind(census2022,year="2022"))%>%
group_by(Site,year,Plot)%>%
summarise(MaxIm=max(Imaturos),
	MaxRep=max(Reprodutivos))%>%
mutate(MaxTot=ifelse(MaxIm>MaxRep,MaxIm,MaxRep))%>%
pivot_longer(MaxIm:MaxTot,names_to="stage")



cbind(census2024,year="2024")%>%head()
Census_all_max
TESTE<-Census_all_max%>%filter(Plot=="11")

pairCensus<-Census_all_max %>%
  mutate(year = as.numeric(year)) %>%
  group_by(Plot) %>%
  arrange(year, stage) %>%
  mutate(VAR = paste0(stage, "-", lead(stage, order_by = interaction(stage,year)))) %>%
  mutate(t0y = year,
         t1y = lead(year, order_by = VAR))%>%
  mutate(t0 = value, 
         t1 = lead(value, order_by = VAR)) %>%
filter(t1y>t0y)%>%
arrange(Plot,VAR)

Timelags_base<-Census_all_max %>%
  mutate(year = as.numeric(year)) %>%
  group_by(Plot) %>%
  arrange(year, stage) %>%
  mutate(VAR = paste0(stage, ">", lead(stage, order_by = interaction(year,stage)))) %>%
  mutate(t0y = year,
         t1y = lead(year, order_by = VAR))%>%
  mutate(t0 = value, 
         t1 = lead(value, order_by = VAR)) %>%
filter(t1y>t0y)%>%	#It only returns with timelag. Change interaction to return same year variables
arrange(Plot,VAR)


#Pairing Maximum reprodutctive to Max Total
RepTot<-left_join(
filter(Timelags_base,stage=="MaxRep")%>%select(Site:Plot,t0y,t0),
filter(Timelags_base,stage=="MaxTot")%>%select(Site:Plot,t1y,t1))%>%
mutate(VAR="MaxRep>MaxTot")%>%
select(VAR,Site:t0y,t1y,t0,t1)	#Reorder columns

#Pairing Maximum reprodutctive to Max Total
TotIm<-left_join(
filter(Timelags_base,stage=="MaxTot")%>%select(Site:Plot,t0y,t0),
filter(Timelags_base,stage=="MaxIm")%>%select(Site:Plot,t1y,t1))%>%
mutate(VAR="MaxTot>MaxIm")%>%
select(VAR,Site:t0y,t1y,t0,t1)	#Reorder columns


