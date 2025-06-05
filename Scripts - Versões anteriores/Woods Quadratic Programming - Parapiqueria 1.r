
library(popbio)
library(ggplot2)
library(tidyverse)
library(openxlsx)

rm(list=ls())

setwd("C:/Artigos e resumos publicados submetidos ideias/Notes/9 - ITV/Parapiqueria - Dados e scripts")
datadir<-"C:/Artigos e resumos publicados submetidos ideias/Notes/9 - ITV/Parapiqueria - Dados e scripts"


#--------------------------------------------------------------------------------
#		EXAMPLE 
# Make the matrix structure logical
#--------------------------------------------------------------------------------
# Possible transitions between months
## Seeds -> seeds
## Juvenile -> Juvenile
## Juvenile -> Adults
## Adults -> Adults
#--------------------------------------------------------------------------------
vr <- list(seed_surv=0.1 , s0=0.5,s1=0.5, s2=0.5,f2=0)
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
#	Load data
#=======================================================================================
dir(datadir)

P2022_04<-read.xlsx(paste0(datadir,"/2022/Parapiqueria_demo_04_23.xlsx"),sheet="Planilha1")

P2022_04<-read.xlsx(paste0(datadir,"/2022/Parapiqueria_abril2022.xlsx"),sheet="Planilha1")
P2022_05<-read.xlsx(paste0(datadir,"/2022/Parapiqueria_demo_05_23.xlsx"),sheet="Planilha1")




P2023_03<-read.xlsx(paste0(datadir,"/2023/Parapiqueria_demo_03_23.xlsx"),sheet="Planilha1")
P2023_04<-read.xlsx(paste0(datadir,"/2023/Parapiqueria_demo_04_23.xlsx"),sheet="Planilha1")
P2023_05<-read.xlsx(paste0(datadir,"/2023/Parapiqueria_demo_05_23.xlsx"),sheet="Planilha1")

# Remove ultima linha
P2023_03<-P2023_03[-501,]
P2023_04<-P2023_04[-501,]
P2023_05<-P2023_05[-501,]


#Nenhum juvenil em Maio
P2023_05[,4]<-0

head(P2023_03)

P2023all<-rbind(
apply(filter(P2023_03,Site=="S11C",Plot=="2")[,c(4,5)],2,sum),
apply(filter(P2023_04,Site=="S11C",Plot=="2")[,c(4,5)],2,sum),
apply(filter(P2023_05,Site=="S11C",Plot=="2")[,c(4,5)],2,sum))


t(P2023all)[,-3]

M2023<-QPmat(t(P2023all), WoodPar(A2)$C,WoodPar(A2)$b,WoodPar(A2)$nonzero)

M2023

vr <- list(seed_surv=0.1 , s0=0.5,s1=0.5, s2=0.5,f2=1)
pre <- expression(matrix2(c(
s0,  f2*seed_surv,
s1, 0  ),   stages[-1] ))
A2 <- eval(pre, vr)

A2 

WoodPar(A2)$C

estmx<-QPmat(t(P2023all), WoodPar(A2)$C,WoodPar(A2)$b,WoodPar(A2)$nonzero)

P2023all
estmx%*%P2023all[1,]
estmx%*%P2023all[2,]


#---------------------------------------------------------------
#	Manualmente
#---------------------------------------------------------------
nonzero <- c(2,3,4)	#s1 e F2 como não zeros
	#matrix(c(1:4),2)
## create C matrix
C <- rbind(diag(-1,3))
## calculate b (transpose is not necessary - either way works)
b <- apply(C, 1, max)
Aout<-QPmat(t(P2023all), C,b,nonzero)
Aout

#Excelente ajuste, porém sobrevivência é irreal!
## Provavelmente pq algumas sementes do banco de sementes vão germinando ao longo do ano
P2023all
Aout%*%P2023all[1,]
Aout%*%P2023all[2,]



#Suavisando
suavizar<-function(serie){
Suavao<-NULL
for(i in 1:dim(serie)[1]){
x<-1:dim(serie)[2]
y<-as.vector(prop.table(serie,2)[i,])
Suavao[[i]]<-predict(loess(y~x))}
return(do.call(rbind, Suavao))}


P2023suave<-suavizar(t(P2023all))*100
P2023suave

Aout_suav<-QPmat(P2023suave, C,b,nonzero)
Aout_suav


teste_kill<-P2023all
teste_kill[3,2]<-45

Aout<-QPmat(t(teste_kill), C,b,nonzero)
Aout

Aout%>%lambda()

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







