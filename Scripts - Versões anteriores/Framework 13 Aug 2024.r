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
#--------------------------------------------------------
#		Data preparation
#--------------------------------------------------------
Census_all<-rbind(
cbind(census2024,year="2024"),
cbind(census2023,year="2023"),
cbind(census2022,year="2022"))%>%
pivot_longer(Imaturos:Reprodutivos,names_to="stage")


#Maximum number of individuals 
Census_all_max<-rbind(
cbind(census2024,year="2024"),
cbind(census2023,year="2023"),
cbind(census2022,year="2022"))%>%
group_by(Site,year,Plot)%>%
summarise(MaxIm=max(Imaturos),
	MaxRep=max(Reprodutivos))%>%
mutate(MaxTot=ifelse(MaxIm>MaxRep,MaxIm,MaxRep))%>%
pivot_longer(MaxIm:MaxTot,names_to="stage")

	
#setting t0 and t1
#Pairing timelags

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

#----------------------------------------------------------------------------
# Max Rep in t0 vs. MaxTot in t1
#----------------------------------------------------------------------------
#Pairing Maximum reprodutctive to Max Total
RepTot<-left_join(
filter(Timelags_base,stage=="MaxRep")%>%select(Site:Plot,t0y,t0),
filter(Timelags_base,stage=="MaxTot")%>%select(Site:Plot,t1y,t1))%>%
mutate(VAR="MaxRep>MaxTot")%>%
select(VAR,Site:t0y,t1y,t0,t1)%>%	#Reorder columns
mutate(Site=as.factor(Site))%>%
mutate(period=as.factor(paste0(t0y,"-",t1y)))

#----------------------------------------------------------------------------
# Max Rep in t0 vs. MaxIm in t1
#----------------------------------------------------------------------------
RepIm<-left_join(
filter(Timelags_base,stage=="MaxRep")%>%select(Site:Plot,t0y,t0),
filter(Timelags_base,stage=="MaxIm")%>%select(Site:Plot,t1y,t1))%>%
mutate(VAR="MaxRep>MaxIm")%>%
select(VAR,Site:t0y,t1y,t0,t1)%>%	#Reorder columns
mutate(Site=as.factor(Site))%>%
mutate(period=as.factor(paste0(t0y,"-",t1y)))

#----------------------------------------------------------------------------
# 				RECRUITMENT ANALYSES
#----------------------------------------------------------------------------
library(lme4)
library(DHARMa)
library(performance)
library(glmmTMB)
library(MuMIn)

options(na.action=na.fail)

#----------------------------------------------------------------------------
## Maximum Reprodutives > Maximum Total
#Neg. Binomial 
nb_RepTot<-glmmTMB::glmmTMB(t1~0+t0*Site+(1|period),
				family=nbinom2(link = "log"),data=RepTot)

nb_RepTot_ls<-dredge(nb_RepTot)%>%get.models(subset=NA)

nb_RepTot_df<-data.frame(
	formula=cbind(lapply(nb_RepTot_ls,formula)),
		Var="Rep>Tot",Dist="Neg.Bin",
			AICcmodavg::aictab(nb_RepTot_ls))
nb_RepTot_df

m2<-update(nb_RepTot,t1~0+t0+Site+(1|period))
m3<-update(nb_RepTot,t1~0+t0+Site*period)

ggeffects::predict_response(m2, terms=c("Site","period"))%>%plot()


# Best model is t1~0+t0+Site+(1|census) Negative binomial
BestModel<-update(nb_RepTot,t1~0+t0+Site+(1|period))

bestS11B<-update(BestModel,~0+t0+(1|period), data=filter(RepTot,Site=="S11B"))
bestS11C<-update(BestModel,~0+t0+(1|period), data=filter(RepTot,Site=="S11C"))

bestS11B%>%parameters::parameters()
ggeffects::predict_response(bestS11B, terms=c("t0[0:208]"))%>%data.frame()%>%head()



#-------------------------------------------------------------
#	REPRODUCTION PER PLOT
#-------------------------------------------------------------
RecruitRate_site<-RepTot%>%
ungroup()%>%
select(-c(year,t0y:t1y))%>%
nest(data=c(Plot,t0,t1))%>%
 mutate(
    fit = map(data, ~ MASS::glm.nb(t1 ~ 0+t0, data = .)),
    tidy_fit = map(fit, ~ tidy(.) %>% mutate(across(c(estimate, p.value), ~ round(., 3))))
  ) %>%
  unnest(tidy_fit)%>%
mutate(VR="Recruitment",
	Exp_estimate=exp(estimate),
	SElow=exp(estimate-std.error),
	SEhigh=exp(estimate+std.error))

RecruitRate_plot<-RecruitRate_site%>%
ggplot(.,aes(x=VR ,y=Exp_estimate))+
geom_pointrange(aes(ymin=SElow,ymax=SEhigh,color=Site,shape=period),alpha=0.8,
	position = position_dodge2(width = 0.9))+
theme_bw(base_size=16)+
scale_color_manual(values=c("#6e86e7","#ff9211"))+
theme(legend.position="bottom")+
facet_grid(.~VR)+
ylab(NULL)+xlab(NULL)

RecruitRate_plot
#=========================================================================
#		 ESTIMATING SURVIVAL
#=========================================================================
#-------------------------------------------------------------
#	TRANSFORMING CENSUS IN MONTH TIMESERIES
#-------------------------------------------------------------

##-----2024
Census2024_ts<-census2024%>%arrange(Site,Plot,Month)%>%
select(Plot,Month,Imaturos,Reprodutivos)%>%
pivot_longer(c(Imaturos,Reprodutivos),names_to="Stage",values_to="Indv")%>%
pivot_wider(names_from=Month,values_from="Indv")%>%
arrange(Plot)%>%
mutate(Plot_tag=paste0("Plot_",Plot))%>%
select(-c(Stage,Plot))%>%
 group_split(Plot_tag, .keep = TRUE) %>%
  `names<-`({.} %>% map(~ .x$Plot_tag[1]) %>% unlist()) %>%
  ## If you want to discard the grouping variable, do the following step as well
  map(~ .x %>% select(-Plot_tag))

Census2024_ts

##-----2023
Census2023_ts<-census2023%>%arrange(Site,Plot,Month)%>%
select(Plot,Month,Imaturos,Reprodutivos)%>%
pivot_longer(c(Imaturos,Reprodutivos),names_to="Stage",values_to="Indv")%>%
pivot_wider(names_from=Month,values_from="Indv")%>%
arrange(Plot)%>%
mutate(Plot_tag=paste0("Plot_",Plot))%>%
select(-c(Stage,Plot))%>%
 group_split(Plot_tag, .keep = TRUE) %>%
  `names<-`({.} %>% map(~ .x$Plot_tag[1]) %>% unlist()) %>%
  ## If you want to discard the grouping variable, do the following step as well
  map(~ .x %>% select(-Plot_tag))

Census2023_ts


##-----2022
Census2022_ts<-census2022%>%arrange(Site,Plot,Month)%>%
select(Plot,Month,Imaturos,Reprodutivos)%>%
pivot_longer(c(Imaturos,Reprodutivos),names_to="Stage",values_to="Indv")%>%
pivot_wider(names_from=Month,values_from="Indv")%>%
arrange(Plot)%>%
mutate(Plot_tag=paste0("Plot_",Plot))%>%
select(-c(Stage,Plot))%>%
 group_split(Plot_tag, .keep = TRUE) %>%
  `names<-`({.} %>% map(~ .x$Plot_tag[1]) %>% unlist()) %>%
  ## If you want to discard the grouping variable, do the following step as well
  map(~ .x %>% select(-Plot_tag))

Census2022_ts

#==========================================================================
# ESTIMATE SURVIVAL COMPONENT OF THE POPULATION MATRIX MODELS
#==========================================================================
# ADJUST PARAMETERS TO OTIMIZATION

# C matrix need adjustments
# because.....

newC<-WoodPar(A2)$C
newC<-as.vector(newC)
newC[c(4,9,15)]<-1
newC<-newC%>%matrix(5)
newC

#Non-zero elements is fine
newnonzero<-WoodPar(A2)$nonzero

#b need adjustments
newb<-WoodPar(A2)$b
newb[c(1,2,3,4,5)]<-1



names(Census2022_ts)<-paste0(names(Census2022_ts),"_2022")
names(Census2023_ts)<-paste0(names(Census2023_ts),"_2023")
names(Census2024_ts)<-paste0(names(Census2024_ts),"_2024")


all_census_ts<-as.list(c(
		Census2022_ts,
		Census2023_ts,
		Census2024_ts))


mpm_list<-NULL
for(i in 1:length(all_census_ts)){
 mpm_list[[i]]<-tryCatch(
		QPmat(all_census_ts[[i]]%>%as.matrix(), 
			newC,
			newb,
			newnonzero),
 error = function(e) { return(NA)})
}

names(mpm_list)<-names(all_census_ts)


mpm_list<-mpm_list[lapply(
		lapply(mpm_list,complete.cases),
			any,FALSE)%>%unlist()]

mpm_list%>%length()
#----------------------------------------------------------------------------
#Uma possibilidade de incluir 1 como recrutamento para todas as matrizes
# Ou da mesma forma um valor fixo de recrutamento, algo como a média
#Somando as matrizes segue uma logica mais semelhante a seguinte
#----------------------------------------------------------------------------
# MatU + MatF
#Separa os MPMs (MatU) por riachos
mpm_list_S11C<-mpm_list[grep("^Plot_([1-9]|10)_.*$", names(mpm_list))]
mpm_list_S11B<-mpm_list[grep("^Plot_(1[1-9]|20)_.*$", names(mpm_list))]

#Fixando em 1
matF<-matrix(c(0,0,1,0),ncol=2)	

#Fixando a reprodução de acordo com os glms
mean_matF_S11B<-matrix(
	c(0,0,
	exp(fixef(bestS11B)$cond[[1]]),
	  0),ncol=2)

mean_matF_S11C<-matrix(
	c(0,0,
	exp(fixef(bestS11C)$cond[[1]]),
	  0),ncol=2)


# Soma MatU+MatF
## Pelo modelo
mpm_list_recruit_S11C<-Map(`+`, mpm_list_S11C, list(mean_matF_S11C))
mpm_list_recruit_S11B<-Map(`+`, mpm_list_S11B, list(mean_matF_S11B))

## Fixado em 1
mpm_list_fix1recruit<-Map(`+`, mpm_list, list(matF))


mpm_list_fix1recruit

#----------------------------------------------------------------------------
#	Gráfico das taxas vitais
#----------------------------------------------------------------------------
Vital_rates_df<-data.frame(do.call(rbind,lapply(mpm_list_fix1recruit,as.vector)),var="vr")%>%rownames_to_column(var = "VAR")%>%
as_tibble()%>%
separate(VAR,sep="_",into = c("x", "Plot", "year"))%>%
select(-x)%>%
pivot_longer(X1:X4,values_to="value",names_to="vital_rate")%>%
mutate(Plot=as.numeric(Plot),
	 year=year)%>%
mutate(Site=ifelse(Plot<11,"S11C","S11B"))%>%
group_by(var,vital_rate,Site,year)%>%
summarise(Mean=mean(value),
		SD=sd(value),
		N=n(),
		SE=SD/sqrt(N),
		CI=1.96*(SD/sqrt(N)),                   
		lower95=quantile(value,.025),          
		higher95=quantile(value,.975))


Vital_rates_plot<-Vital_rates_df%>%
mutate(VR="Survival")%>%
mutate(stages=case_when(vital_rate=="X1"~ "Stasis[Immat]",
				vital_rate=="X2"~ "Growth[Immat.>Adult]",
				vital_rate=="X3"~ "Recruitment",
				vital_rate=="X4"~ "Surv[Adult]"))%>%
mutate(stages=forcats::fct_relevel(stages, c("Statis[Immat]","Growth[Immat.>Adult]", "Surv[Adult]","Recruitment")))%>%
filter(vital_rate!="X3")%>%
ggplot(.,aes(y=Mean,x=stages,group=year))+
geom_pointrange(aes(ymin=Mean-SE,ymax=Mean+SE,color=Site,shape=year),alpha=0.8,
	position = position_dodge2(width = 0.4))+
scale_x_discrete(labels=function(l) parse(text=l))+
scale_color_manual(values=c("#ff9211","#6e86e7"))+
theme_bw(base_size=16)+
theme(legend.position="bottom")+
facet_grid(.~VR)+
xlab(NULL)+ylab(NULL)




#Eu adiciono as legendas nos gráficos 
#pq por algum motivo as cores ficam trocadas 
#se eu não mexer manualmente
# Dai é melhor deixar cada gráfico com a legenda  
#	e ai depois eu retiro durante o cowplot.

cowplot::plot_grid(rel_widths=c(.2,.8),
RecruitRate_plot+theme(legend.position="none"),
Vital_rates_plot+theme(legend.position="right"))

#=========================================================================================
#	BUILDING MATRIX POPULATION MODEL
#=========================================================================================




#----------------------------------------------------------------------------
#	Gráfico das taxas vitais
#----------------------------------------------------------------------------


rbind(
#Matrix de taxas vitais
data.frame(do.call(rbind,lapply(mpm_list_recruit,as.vector)),var="vr")%>%rownames_to_column(var = "VAR"),
# Matrix de sensitivities
#data.frame(do.call(rbind,lapply(mpm_list_sens,as.vector)),var="sensitivity")%>%rownames_to_column(var = "VAR")
#Matrix de elasticities
data.frame(do.call(rbind,lapply(mpm_list_elas,as.vector)),var="elasticity")%>%rownames_to_column(var = "VAR"))%>%
as_tibble()%>%
separate(VAR,sep="_",into = c("x", "Plot", "year"))%>%
select(-x)%>%
pivot_longer(X1:X4,values_to="value",names_to="vital_rate")%>%
mutate(Plot=as.numeric(Plot),
	 year=year)%>%
mutate(Site=ifelse(Plot<11,"S11C","S11B"))%>%
group_by(var,vital_rate,Site,year)%>%
summarise(Mean=mean(value),
		SD=sd(value),
		N=n(),
		CI=1.96*(SD/sqrt(N)),                   # Efetivo para distribuição normal
		lower95=quantile(value,.025),          # Mais efetivo para caracterizar a distribuição                                                   empiríca não normal de uma amostra.
		higher95=quantile(value,.975))%>%
mutate(stages=case_when(vital_rate=="X1"~ "S[I_I]",
				vital_rate=="X2"~ "S[I_A]",
				vital_rate=="X3"~ "F",
				vital_rate=="X4"~ "S[A_A]"))%>%
ggplot(.,aes(y=Mean,x=stages,group=year))+
geom_pointrange(aes(ymin=Mean-SD,ymax=Mean+SD,color=Site,shape=year),alpha=0.8,
	position = position_dodge2(width = 0.9))+
scale_x_discrete(labels=function(l) parse(text=l))+
facet_wrap(.~var,scale="free")



rbind(
data.frame(do.call(rbind,lapply(mpm_list,as.vector)),var="vr"),
data.frame(do.call(rbind,lapply(mpm_list_elas,as.vector)),var="elasticity"),
data.frame(do.call(rbind,lapply(mpm_list_sens,as.vector)),var="sensitivity"))%>%
rownames_to_column(., "x")%>%
as_tibble()%>%
separate(x,sep="_",into = c("x", "Plot", "year"))%>%
pivot_longer(X1:X4,values_to="value",names_to="vital_rate")%>%
select(-x)%>%
mutate(Plot=as.numeric(Plot))%>%
mutate(Site=ifelse(Plot<11,"S11C","S11B"))%>%
group_by(var,vital_rate,Plot,Site)%>%
summarise(Mean=mean(value),
		SD=sd(value),
		N=n(),
		CI=1.96*(SD/sqrt(N)),                   # Efetivo para distribuição normal
		lower95=quantile(value,.025),          # Mais efetivo para caracterizar a distribuição                                                   empiríca não normal de uma amostra.
		higher95=quantile(value,.975))%>%
ggplot(.,aes(y=Mean,x=vital_rate,group=Site))+
geom_pointrange(aes(ymin=Mean-SD,ymax=Mean+SD,color=Site),alpha=0.8,
	position = position_dodge2(width = 0.9))+
facet_grid(var~.,scales="free")






QPmat(
as.matrix(mean(lapply(Census2024_ts,as.matrix))),
	newC,
	newb,
	newnonzero)%>%sensitivity()

Census2024_ts
lapply(
lapply(Census2024_ts,as.matrix),
QPmat,newC,
	newb,
	newnonzero)%>%mean()

install.packages("popdemo")


teste<-QPmat(
lapply(Census2023_ts[c(11:20)],as.matrix),
	newC,
	newb,
	newnonzero)



teste