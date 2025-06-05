#######################################################
#		Script Parapiqueria 
#		 Em desevolvimento
###   Implementa Wood's quadratic program 
###   Ver max indivíduos rep x imaturos
######################################################

library(popbio)
library(ggplot2)
library(tidyverse)
library(openxlsx)
library(ggrepel)
library(ggridges)

rm(list=ls())

setwd("C:/Artigos e resumos publicados submetidos ideias/Notes/9 - ITV/Parapiqueria - Dados e scripts")
datadir<-"C:/Artigos e resumos publicados submetidos ideias/Notes/9 - ITV/Parapiqueria - Dados e scripts"


#--------------------------------------------------------------------------------
#		EXAMPLE 
# Make the periodic matrix structure logical
#--------------------------------------------------------------------------------
# Possible transitions between months
## Seeds -> seeds		# seed_surv = 0.1 just as example
## Juvenile -> Juvenile	# s0=0.3 just as example
## Juvenile -> Adults	# s1=0.7 just as example
## Adults -> Adults	# s2=0.3 just as example
## Recruitment		# f2= No reproduction between months of the same season
#--------------------------------------------------------------------------------

vr <- list(seed_surv=0.1 , s0=0.3,s1=0.7, s2=0.3,f2=0)
pre <- expression(matrix2(c(
s0,  f2*seed_surv,
s1, s2  ),   stages[-1] ))

stages<-c("seed","Juvenile","Adult")

A2 <- eval(pre, vr)
A2 

pop.projection(A2,c(100,100))
stable.stage(A2)
round(stable.stage(A2),2)


replacing<-function(A){
A<-A
A[upper.tri(A)]<-"R"
A[1,]<-"F"
A[lower.tri(A,diag=T)]<-"S"
return(A)
}

replacing (A2)


# Estabelece os parametros para rodar modelo inverso
## baseado na função autoQPmat que criei durante minha especialização
ven<-A2

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

WoodPar(A2)


popsim.ven<-pop.projection(A2,rep(1,ncol(A2)),1000)
serie.ven<-popsim.ven$stage.vector[,1:100]


WoodPar(A2)


QPmat(serie.ven, WoodPar(A2)$C,WoodPar(A2)$b,WoodPar(A2)$nonzero)

#=======================================================================================
#	Load empirical data
#=======================================================================================

#2022
Mar22<-read.xlsx(paste0(datadir,"/Dados brutos/Dados parapiqueria - Consolidado 22Mar2024.xlsx"),sheet="Março2022")
Abr22<-read.xlsx(paste0(datadir,"/Dados brutos/Dados parapiqueria - Consolidado 22Mar2024.xlsx"),sheet="Abril2022")
Mai22<-read.xlsx(paste0(datadir,"/Dados brutos/Dados parapiqueria - Consolidado 22Mar2024.xlsx"),sheet="Maio2022")

#2023
Mar23<-read.xlsx(paste0(datadir,"/Dados brutos/Dados parapiqueria - Consolidado 22Mar2024.xlsx"),sheet="Março2023")
Abr23<-read.xlsx(paste0(datadir,"/Dados brutos/Dados parapiqueria - Consolidado 22Mar2024.xlsx"),sheet="Abril2023")
Mai23<-read.xlsx(paste0(datadir,"/Dados brutos/Dados parapiqueria - Consolidado 22Mar2024.xlsx"),sheet="Maio2023")


Mar23_quad<-Mar23
Abr23_quad<-Abr23
Mai23_quad<-Mai23


#------------------------------------------------------------------------------
#		Merge Census
#------------------------------------------------------------------------------
##2023
#Mask quadrants as it won't be necessary here
census2023<-rbind(
data.frame(Mar23,Month="1"),
data.frame(Abr23,Month="2"),
data.frame(Mai23,Month="3"))%>%
as_tibble()%>%
group_by(Site,Plot,Month)%>%
summarise(Imaturos=sum(Imaturos),
		Reprodutivos=sum(Reprodutivos))%>%
ungroup()

##2022
census2022<-rbind(
data.frame(Mar22,Month="1"),
data.frame(Abr22,Month="2"),
data.frame(Mai22,Month="3"))%>%
select(Site,Plot,Imaturos,Reprodutivos,Month)

census2022%>%as_tibble()



#=====================================================================
#	COMPARE DETECTABILITY, LATE GERMINATION
#=====================================================================
#Maximum individuals identified
##2023
Max_census2023<-census2023%>%
group_by(Plot)%>%
summarise(MaxIm=max(Imaturos),
	MaxRep=max(Reprodutivos))%>%
mutate(Site=c(rep("S11C",10),rep("S11B",10)))%>%
mutate(MaxTot=ifelse(MaxIm>MaxRep,MaxIm,MaxRep))

Max_census2023

##2022
Max_census2022<-census2022%>%
select(Plot,Imaturos,Reprodutivos)%>%
group_by(Plot)%>%
summarise(MaxIm=max(Imaturos),
	MaxRep=max(Reprodutivos))%>%
mutate(Site=c(rep("S11C",10),rep("S11B",10)))%>%
mutate(MaxTot=ifelse(MaxIm>MaxRep,MaxIm,MaxRep))


Max_census2022


#-----------------------------------------------------------------------
# Merge all census
#-----------------------------------------------------------------------
All_census<-full_join(
 Max_census2022,Max_census2023,
	by=c("Plot","Site"),suffix = c("22", "23"))

All_census


All_census%>%GGally::ggpairs()+theme_bw()
All_census


#=====================================================================
#	PLOTTING ABUNDANCE
#=====================================================================
All_census%>%
pivot_longer(!c(Plot,Site),names_to="Var",values_to="Indiv")%>%
separate(Var, into = c("Stage", "Ano"), sep = "(?<=\\D)(?=\\d)", remove = FALSE)%>%
mutate(Ano=ifelse(Ano==22,"2022","2023"))%>%
ggplot(., aes(x = Indiv+1, y = Site,fill=Ano)) + 
geom_density_ridges(alpha = 0.7,
jittered_points = TRUE,
 position = position_points_jitter(width = 0.5, height = 0),
                      point_shape = "|", point_size = 2)+
 		xlim(0,400)+
scale_x_log10(breaks=c(1,10,100,500))+
			facet_grid(Ano~Stage)+
theme_minimal(base_size=16)+
theme(legend.position="top")


#=====================================================================
#	PLOTTING Juveniles to Adults
#=====================================================================

All_census%>%
mutate(Surv22=MaxRep22/MaxIm22,
	 Surv23=MaxRep23/MaxIm23)%>%
select(Plot,Site,Surv22,Surv23)%>%
GGally::ggpairs()




All_census%>%
ggplot(.,aes(x=MaxIm22,MaxRep22,fill=Site))+
geom_point()



#=====================================================================
#	CENSUS AS TIME SERIES
#=====================================================================
##-----2023
Census2023_ts<-census2023%>%arrange(Site,Plot,Month)%>%
select(Plot,Month,Imaturos,Reprodutivos)%>%
pivot_longer(c(Imaturos,Reprodutivos),names_to="Stage",values_to="Indv")%>%
pivot_wider(names_from=Month,values_from="Indv")%>%
select(-Stage)%>%
group_split(Plot,.keep = F)%>%
as.list()

##-----2022
Census2022_ts<-census2022%>%arrange(Site,Plot,Month)%>%
select(Plot,Month,Imaturos,Reprodutivos)%>%
pivot_longer(c(Imaturos,Reprodutivos),names_to="Stage",values_to="Indv")%>%
pivot_wider(names_from=Month,values_from="Indv")%>%
select(-Stage)%>%
group_split(Plot,.keep = F)%>%
as.list()

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

QPmat(
as.matrix(mean(lapply(Census2023_ts[c(11:20)],as.matrix))),
	newC,
	newb,
	newnonzero)


#-------------------------------------------------------------------------
#		2023
#-------------------------------------------------------------------------
census2023
est_surv_mxs23<-lapply(Census2023_ts,as.matrix)%>%
#lapply(.,suavizar)%>%
lapply(.,
QPmat, newC,newb,newnonzero)


Census2023_ts[1:10][3]
census2023%>%filter(Site=="S11C")

RiachoA23<-est_surv_mxs23[c(1:10)]	
RiachoB23<-est_surv_mxs23[c(11:20)]
#---------------------------------------------------------------------
#		2022 
#---------------------------------------------------------------------
# demographic parameters estimated by loop as some cannot be estimated
## ex. Census2022_ts[[19]]  #No juvenils identified
#---------------------------------------------------------------------
est_surv_mxs22<-NULL
for(i in 1:length(Census2022_ts)){
est_surv_mxs22[[i]]<-tryCatch(
		QPmat(Census2022_ts[[i]]%>%as.matrix(), 
			newC,
			newb,
			newnonzero),
 error = function(e) { return(NA) })
print(est_surv_mxs22[[i]])}

est_surv_mxs22


RiachoA22<-est_surv_mxs22[c(1:10)]
RiachoB22<-est_surv_mxs22[c(11:20)]

#---------------------------------------------------------------------
# REMOVE THOSE MPMs THAT WASN'T POSSIBLE TO ESTIMATE
#---------------------------------------------------------------------
RiachoA22<- RiachoA22[	
			lapply(
				lapply(
					RiachoA22,complete.cases),
							any,FALSE)%>%unlist()]
RiachoB22<- RiachoB22[	
			lapply(
				lapply(
					RiachoB22,complete.cases),
							any,FALSE)%>%unlist()]
#---------------------------------------------------------------------


par(mfrow=c(2,2))
RiachoA22%>%mean()%>%round(2)%>%image2()
RiachoB22%>%mean()%>%round(2)%>%image2()

RiachoA23%>%mean()%>%round(2)%>%image2()
RiachoB23%>%mean()%>%round(2)%>%image2()
dev.off()

#==========================================================================
#		 ESTIMATE RECRUITMENT
#==========================================================================
repEst_IMtoREP_site<-glm(MaxIm23~0+MaxRep22*Site,
	data=All_census)

#repEst_IMtoREP_site%>%performance::check_model()
#Not working. Problem with package x windows configuration?


repEst_IMtoREP_NOsite<-glm(MaxIm23~0+MaxRep22,
	data=All_census)

repEst_TOTtoIM_site<-glm(MaxIm23~0+MaxTot22*Site,
	data=All_census)

repEst_TOTtoIM_NOsite<-glm(MaxIm23~0+MaxTot22,
	data=All_census)

repEst_TOTtoREP_site<-glm(MaxRep23~0+MaxTot22*Site,
	data=All_census)

repEst_TOTtoREP_NOsite<-glm(MaxRep23~0+MaxTot22,
	data=All_census)

repEst_TOTtoTOT_NOsite<-glm(MaxTot23~0+MaxTot22,
	data=All_census)

repEst_TOTtoTOT_site<-glm(MaxTot23~0+MaxTot22*Site,
	data=All_census)


repEst_REPtoTOT_NOsite<-glm(MaxTot23~0+MaxRep22,
	data=All_census)

repEst_REPtoTOT_site<-glm(MaxTot23~0+MaxRep22*Site,
	data=All_census)

repEst_REPtoTOT_site%>%summary()

 bbmle::AICctab(base=TRUE,weights = TRUE, 
	repEst_IMtoREP_site,repEst_IMtoREP_NOsite,
	repEst_TOTtoIM_site,repEst_TOTtoIM_NOsite,
	repEst_TOTtoREP_site,repEst_TOTtoREP_NOsite,
	repEst_REPtoTOT_site,repEst_REPtoTOT_NOsite,
	repEst_TOTtoTOT_site,repEst_TOTtoTOT_NOsite)



 performance::compare_performance(rank = T,
	repEst_IMtoREP_site,repEst_IMtoREP_NOsite,
	repEst_TOTtoIM_site,repEst_TOTtoIM_NOsite,
	repEst_TOTtoREP_site,repEst_TOTtoREP_NOsite,
	repEst_REPtoTOT_site,repEst_REPtoTOT_NOsite,
	repEst_TOTtoTOT_site,repEst_TOTtoTOT_NOsite)


 performance::compare_performance(
	repEst_TOTtoREP_site,repEst_TOTtoREP_NOsite,
	repEst_REPtoTOT_site,repEst_REPtoTOT_NOsite)%>%plot()


# Recruitment rate
All_census%>%
mutate(Recruit.rate=(MaxRep23/MaxTot22))%>%
ggplot(.,aes(x=Site,y=Recruit.rate,fill=Site))+
geom_boxplot()+
geom_point()+
labs(x="Sitios",
	y="Max Total 2023 / Max Reprodutivo 2022",
	title="Taxa de recrutamento")+
theme_bw(base_size=16)+
theme(legend.position="bottom")


All_census%>%
mutate(Recruit.rate=(MaxTot22-MaxRep22))%>%
rstatix::t_test(Recruit.rate~Site)


# Model with more aderence to the hypothesis of different recruitment
All_census%>%
ggplot(.,aes(x=MaxTot22,y=MaxRep23,label=Plot))+
geom_point()+
geom_label_repel(aes(fill=Site))+
labs(y="Max TOTAL em 2022",
x="Max Reprodutivos em 2023",
title="Recrutamento")+
theme_bw(base_size=16)


cowplot::plot_grid(ncol=1,
BestModel,plotTotxRep)


#=====================================================================
#	MERGE REPRODUCTION + SURVIVAL TO BUILD MPMs
#=====================================================================

#---------------------------------------------------------------------
#	ASSUMING DIFERENCES IN RECRUITMENT ALONG THE SITES
#---------------------------------------------------------------------


Rec_est_SiteB<-glm(MaxTot23~0+MaxRep22,
	data=filter(All_census,Site=="S11B"))

Rec_est_SiteA<-glm(MaxTot23~0+MaxRep22,
	data=filter(All_census,Site=="S11C"))

Rec_est_SiteA%>%summary()
Rec_est_SiteB%>%summary()

Rec_est_SiteA%>%plot()
Rec_est_SiteB%>%plot()


matF_A<-matrix(c(0,0,Rec_est_SiteA$coeff,0),2)
matF_B<-matrix(c(0,0,Rec_est_SiteB$coeff,0),2)


RiachoA22_mxout<-RiachoA22%>%mean()+matF_A
RiachoB22_mxout<-RiachoB22%>%mean()+matF_B


par(mfrow=c(2,2))
RiachoA22_mxout%>%round(2)%>%image2()
RiachoA22_mxout%>%elasticity()%>%round(2)%>%image2()
RiachoB22_mxout%>%round(2)%>%image2()
RiachoB22_mxout%>%elasticity()%>%round(2)%>%image2()


RiachoA22_mxout%>%lambda()
RiachoB22_mxout%>%lambda()


Rage::plot_life_cycle(elasticity(RiachoA22_mxout),
stages = c("J", "A"))

data.frame(
RiachoA=RiachoA22_mxout%>%elasticity()%>%as.vector(),
RiachoB=RiachoB22_mxout%>%elasticity()%>%as.vector())


Elas<-data.frame(
Var="Elasticidades",
Vr=c("J.J","J.A","FA","A.A"),
RiachoA=RiachoA22_mxout%>%elasticity()%>%as.vector(),
RiachoB=RiachoB22_mxout%>%elasticity()%>%as.vector())


VRs<-data.frame(
Var="Vital rates",
Vr=c("J.J","J.A","FA","A.A"),
RiachoA=RiachoA22_mxout%>%as.vector(),
RiachoB=RiachoB22_mxout%>%as.vector())


rbind(VRs,Elas)%>%
pivot_longer(c(RiachoA,RiachoB),names_to="Site",values_to="Values")%>%
mutate(Vr= factor(Vr, levels=c("J.J","J.A","A.A","FA")))%>% 
ggplot(.,aes(x=Vr, y=Values, fill=Site))+
geom_bar(stat="identity",position = position_dodge(),color="black")+
theme_bw(base_size=16)+
facet_grid(Var~.,scale="free")+
theme(legend.position="top")

#--------------------------------------------------------------
# COMPARE MEAN DEMOGRAPHIC RATE, VARIANCE and SENSITIVITY
#--------------------------------------------------------------
par(mfrow=c(2,3))
est_surv_mxs22

est_surv_mxs22%>%mean()%>%image2(.)
est_surv_mxs22%>%var2()%>%image2()
est_surv_mxs22%>%mean()%>%sensitivity()%>%image2()

est_surv_mxs23%>%mean()%>%image2()
est_surv_mxs23%>%var2()%>%image2()
est_surv_mxs23%>%mean()%>%sensitivity()%>%image2()
#--------------------------------------------------------------


#Matrizes
par(mfrow=c(2,1))
est_surv_mxs22%>%lapply(.,lambda)%>%unlist()%>%hist(.,main="Lambdas em 2022",xlim=c(0.3,2.7))
est_surv_mxs23%>%lapply(.,lambda)%>%unlist()%>%hist(.,main="Lambdas em 2023",xlim=c(0.3,2.7))


#=====================================================================
#	COMPARE DETECTABILITY, LATE GERMINATION
#=====================================================================
#Maximum individuals identified
##2023
Max_census2023<-census2023%>%
group_by(Plot)%>%
summarise(MaxIm=max(Imaturos),
	MaxRep=max(Reprodutivos))

Max_census2023

##2022
Max_census2022<-census2022%>%
select(Plot,Imaturos,Reprodutivos)%>%
group_by(Plot)%>%
summarise(MaxIm=max(Imaturos),
	MaxRep=max(Reprodutivos))%>%
mutate(Site=c(rep("S11C",10),rep("S11B",10)))

Max_census2022

#==============================================================
#PLOT
#==============================================================


repEst<-glm(Max_census2023$MaxIm~
Max_census2022$MaxRep,family="poisson")

repEst[[1]][2]%>%exp()

repEst%>%summary()
repEst%>%plot()

plot(repEst$fitted.values~
Max_census2022$MaxRep,
xlim=c(10,150),ylim=c(10,150))
abline(a=0,b=1)

repEst

plot(Max_census2023$MaxIm
~Max_census2022$MaxRep,xlim=c(0,200),ylim=c(0,200))
abline(a=0,b=1)
points(

cbind(Max_census2023$MaxIm,fitted(repEst))



Max_census2022%>%
ggplot(.,aes(x=MaxIm,y=MaxRep, label = Plot))+
geom_point()+
geom_label_repel(aes(fill=Site))+
geom_abline(intercept = 0, slope = 1)+
labs(x="Número máximo de indivíduos Jovens",
	y="Número máximo de indivíduos Adultos")+
xlim(0,200)+ylim(0,200)


Max_census2023%>%
mutate(Ano="2023")%>%
ggplot(.,aes(x=MaxIm,y=MaxRep, label = Plot))+
geom_point()+
geom_label_repel(aes(fill=Site))+
geom_abline(intercept = 0, slope = 1)+
labs(x="Número máximo de indivíduos Jovens",
	y="Número máximo de indivíduos Adultos")+
xlim(0,200)+ylim(0,200)+
theme_bw(base_size=16)+
facet_grid(.~Ano)











#---------------------------------------------------------------
#	Assumindo um banco de sementes temporário
#---------------------------------------------------------------
P2023all
teste<-P2023all

later_seeds<-max(P2023all[,2])-max(P2023all[,1])
later_seeds_vec<-rep(0,length(teste[,1]))
#	later_seeds_vec[-length(later_seeds_vec)]<-rep(later_seeds/2,2)
	later_seeds_vec[1]<-rep(later_seeds)

teste<-data.frame(teste,dorm=later_seeds_vec)


#Sementes dormentes primeiro
teste_dor<-t(teste)[c(3,1,2),]
teste_dor

testemx<-matrix(c(0,1,1,0,1,1,1,0,0),3)
testemx

teste_dor

testWoodPar<-testemx%>%WoodPar();testWoodPar

Aout2_suave<-QPmat(suavizar(teste_dor), testWoodPar$C,testWoodPar$b,testWoodPar$nonzero)
Aout2_suave

teste[c(3,1,2)]%>%t()
matrix(teste[c(3,1,2),1])
Aout2_suave%*%t(teste)[c(3,1,2),1]
Aout2_suave%*%t(teste)[c(3,1,2),2]


rm(Aout2)
Aout2<-QPmat(teste_dor, testWoodPar$C,testWoodPar$b,testWoodPar$nonzero)
Aout2

suavizar(teste_dor)


teste[c(3,1,2)]%>%t()
matrix(teste[c(3,1,2),1])
Aout2%*%t(teste)[c(3,1,2),1]
Aout2%*%t(teste)[c(3,1,2),2]



Aout3<-QPmat(t(teste)[c(3,1,2),], testWoodPar3$C,testWoodPar3$b,testWoodPar3$nonzero)
Aout3




nonzero <- c(3,5,6)	#s1 e F2 como não zeros
	matrix(c(1:9),3)
## create C matrix
C <- rbind(diag(-1,3));C
## calculate b (transpose is not necessary - either way works)
b <- apply(C, 1, max);b

Aout4<-QPmat(teste_dor, C,b,nonzero)
Aout4
teste[c(3,1,2)]%>%t()
matrix(teste[c(3,1,2),1])
Aout2%*%t(teste)[c(3,1,2),1]
Aout2%*%t(teste)[c(3,1,2),2]




matrix(c(1:9),3)

nonzero <- c(1,2,4,5,6,8,9)	#s1 e F2 como não zeros
## create C matrix
C <- rbind(diag(-1,4))
## calculate b (transpose is not necessary - either way works)
b <- apply(C, 1, max)
Aout2<-QPmat(t(teste), C,b,nonzero)
Aout2

t(teste)


matrix(c(1:9),3)

#======================================================================================
#	FUN��ES CRIADAS POR MIM PARA A AN�LISE
#======================================================================================
#Fun��es
#file.edit("Funcoes.R")
source("Funcoes.R")
#S�o elas:
#Par�metros - Seleciona a s�rie temporal at� o limite dela alcan�ar a estrutura et�ria est�vel
#replacing - Substitui os valores da matriz por simbolo de S (sobreviv�ncia), F (fecundidade) e outros ....
#suavizar - A partir da s�rie temporal ajusto um modelo suavizador (losses ou smooth) para evitar estocasticidade demogr�fica
#cria - Cria diferentes hist�rias de vida conforme descrito na monografia
#getMatA2 - busca as matrizes selecionadas no banco de dados COMADRE
#autoQPmat - gera as matrizes C, b e o vetor de par�metros a serem estimados e aplica a fun��o QPmat do pacote popbio
#autoQPmat.X - mesma coisa que anterior por�m incorpora a estocasticidade

#======================================================================================
#				SIMULANDO HIST�RIAS DE VIDA
#======================================================================================

file.edit("Simulando historias de vida.R")

#======================================================================================
#			DADOS EMP�RICOS DE HIST�RIAS DE VIDA
#======================================================================================

file.edit("Dados empiricos.R")


#======================================================================================
#	An�lise com popula��es humanas a partir dos dados do DemogR
#======================================================================================

file.edit("populacoes humanas conhecidas - DemogR.R")


#======================================================================================
#				SIMULANDO HIST�RIAS DE VIDA
#======================================================================================
#file.edit("Populacao brasileira - Wood quadratic program method.R")
#file.edit("Popula��o brasileira.R")







