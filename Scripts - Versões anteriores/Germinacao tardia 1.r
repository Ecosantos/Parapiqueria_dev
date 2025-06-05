################################################
#	Script Germinação Tardia em Parapiqueria
#	by Gabriel Santos 
#	22 Mar 2024
###############################################


library(popbio)
library(ggplot2)
library(tidyverse)
library(openxlsx)


rm(list=ls())
#setwd("C:/Artigos e resumos publicados submetidos ideias/Notes/9 - ITV/Parapiqueria - Dados e scripts")
datadir<-"C:/Artigos e resumos publicados submetidos ideias/Notes/9 - ITV/Parapiqueria - Dados e scripts"


Mar22<-read.xlsx(paste0(datadir,"/Dados brutos/Dados parapiqueria - Consolidado 22Mar2024.xlsx"),sheet="Março2022")
Abr22<-read.xlsx(paste0(datadir,"/Dados brutos/Dados parapiqueria - Consolidado 22Mar2024.xlsx"),sheet="Abril2022")
Mai22<-read.xlsx(paste0(datadir,"/Dados brutos/Dados parapiqueria - Consolidado 22Mar2024.xlsx"),sheet="Maio2022")


Mar23<-read.xlsx(paste0(datadir,"/Dados brutos/Dados parapiqueria - Consolidado 22Mar2024.xlsx"),sheet="Março2023")
Abr23<-read.xlsx(paste0(datadir,"/Dados brutos/Dados parapiqueria - Consolidado 22Mar2024.xlsx"),sheet="Abril2023")
Mai23<-read.xlsx(paste0(datadir,"/Dados brutos/Dados parapiqueria - Consolidado 22Mar2024.xlsx"),sheet="Maio2023")


#Calcula a média por quadrante em cada plot
Media_plot_quad<-rbind(
data.frame(Mar23,Mes="Mes1"),
data.frame(Abr23,Mes="Mes2"),
data.frame(Mai23,Mes="Mes3"))%>%
as_tibble()%>%
select(Site,Plot,Quadrante,Imaturos,Reprodutivos,Mes)%>%
pivot_longer(c(Imaturos,Reprodutivos),names_to="Stages",values_to="Indiv")%>%
pivot_wider(names_from=Mes,values_from=Indiv)%>%
group_by(Site,Plot,Stages)%>%
summarise(
	Mes1_mu = mean(Mes1), Mes1_sd = sd(Mes1),
	Mes2_mu = mean(Mes2), Mes2_sd = sd(Mes2),
	Mes3_mu = mean(Mes3), Mes3_sd = sd(Mes3))


#Jovens em Março x Reprodutivos em Abril
par(mfrow=c(1,2))

plot(
filter(Media_plot_quad,Stages=="Reprodutivos")$Mes2_mu~
filter(Media_plot_quad,Stages=="Imaturos")$Mes1_mu,
xlim=c(0,6),ylim=c(0,6),
ylab="Média de Reprodutivos no mês Abril",
xlab="Média de Jovens em Março",
main="Relação entre jovens e adultos de parapiqueria \n em t vs t+1")
abline(a=0,b=1,col="grey70",lty=2)


# Total de jovens em março + abril x reprodutivos em Maio
Soma_mes1e2<-filter(Media_plot_quad,Stages=="Imaturos")$Mes1_mu+
filter(Media_plot_quad,Stages=="Imaturos")$Mes2_mu


plot(
filter(Media_plot_quad,Stages=="Reprodutivos")$Mes3_mu~Soma_mes1e2,
xlim=c(0,6),ylim=c(0,6),
ylab="Média de Reprodutivos no mês Maio",
xlab="Média de Jovens em Março+Abril",
main="Relação entre jovens e adultos de parapiqueria \n em t vs t+1")
abline(a=0,b=1,col="grey70",lty=2)

plot(
filter(Media_plot_quad,Stages=="Reprodutivos")$Mes3_mu~
filter(Media_plot_quad,Stages=="Imaturos")$Mes2_mu,
xlim=c(0,6),ylim=c(0,6),
ylab="Média de Reprodutivos no mês Abril",
xlab="Média de Jovens em Março",
main="Relação entre jovens e adultos de parapiqueria \n em t vs t+1")
abline(a=0,b=1,col="grey70",lty=2)

Mar23%>%select(

rbind(
data.frame(Mar23,Mes="Mes1"),
data.frame(Abr23,Mes="Mes2"),
data.frame(Mai23,Mes="Mes3"))%>%
as_tibble()%>%
select(Site,Plot,Quadrante,Imaturos,Reprodutivos,Mes)%>%
group_by(Site,Plot,Quadrante,Mes)%>%
mutate(Max_quad_Imat=max(Imaturos),
	Max_quad_Rep=max(Reprodutivos))%>%
group_by(Site,Plot,Mes)%>%
mutate(Max_plot_Imat=max(Imaturos),
	Max_plot_Rep=max(Reprodutivos))%>%
ungroup()%>%ungroup()%>%
pivot_longer(c(Imaturos,Reprodutivos),names_to="Stages",values_to="Indiv")%>%
pivot_wider(names_from=Mes,values_from=Indiv)
group_by(Site,Plot,Stages)%>%
summarise(
	Mes1_mu = mean(Mes1), Mes1_sd = sd(Mes1),
	Mes2_mu = mean(Mes2), Mes2_sd = sd(Mes2),
	Mes3_mu = mean(Mes3), Mes3_sd = sd(Mes3))






rbind(
data.frame(Mar23,Mes="Mes1"),
data.frame(Abr23,Mes="Mes2"),
data.frame(Mai23,Mes="Mes3"))%>%
as_tibble()%>%
select(Site,Plot,Quadrante,Imaturos,Reprodutivos,Mes)%>%
group_by(Site,Plot,Quadrante,Mes)%>%
mutate(Max_quad_Imat=max(Imaturos),
	Max_quad_Rep=max(Reprodutivos))%>%
group_by(Site,Plot,Mes)%>%
mutate(Max_plot_Imat=max(Imaturos),
	Max_plot_Rep=max(Reprodutivos))%>%
ungroup()%>%ungroup()%>%
pivot_longer(c(Imaturos,Reprodutivos),names_to="Stages",values_to="Indiv")%>%
pivot_wider(names_from=Mes,values_from=Indiv)%>%
arrange(Site,Plot,Quadrante)




rbind(
data.frame(Mar23,Mes="Mes1"),
data.frame(Abr23,Mes="Mes2"),
data.frame(Mai23,Mes="Mes3"))%>%
as_tibble()%>%
select(Site,Plot,Quadrante,Imaturos,Reprodutivos,Mes)%>%
group_by(Site,Plot,Quadrante,Mes)%>%
pivot_longer(c(Imaturos,Reprodutivos),names_to="Stages",values_to="Indiv")%>%
pivot_wider(names_from=Mes,values_from=Indiv)%>%
mutate(Max=max(across(Mes1:Mes3),na.rm=TRUE))%>%






lm(filter(TESTE,Stages=="Reprodutivos")$Mes2_mu~
filter(TESTE,Stages=="Imaturos")$Mes1_mu)%>%
residuals()%>%plot(ylab="Diferença média no número de indivíduos")
abline(a=0,b=0,col="red",lty=2)











# LIXO
rbind(
data.frame(Mar23,Mes=1),
data.frame(Abr23,Mes=2),
data.frame(Mai23,Mes=3))%>%
as_tibble()%>%
select(Site,Plot,Mes,Quadrante,Imaturos,Reprodutivos)%>%
group_by(Site,Plot,Quadrante)%>%
mutate(Max_Imat=max(Imaturos))%>%
mutate(Max_Rep=max(Reprodutivos))%>%
mutate(Max_INDIV=ifelse(Max_Rep>=Max_Imat,Max_Rep,Max_Imat))%>%
mutate(Max_What=ifelse(Max_Rep>=Max_Imat,"Reprodutivos","Imaturos"))%>%
ungroup(Site,Plot)%>%select(Max_What)%>%table()


rbind(
data.frame(Mar23,Mes="Mes1"),
data.frame(Abr23,Mes="Mes2"),
data.frame(Mai23,Mes="Mes3"))%>%
as_tibble()%>%
select(Site,Plot,Mes,Quadrante,Imaturos,Reprodutivos)%>%
group_by(Site,Plot,Quadrante)%>%
mutate(Max_Imat=max(Imaturos))%>%
mutate(Max_Rep=max(Reprodutivos))%>%
mutate(Max_INDIV=ifelse(Max_Rep>=Max_Imat,Max_Rep,Max_Imat))%>%
mutate(Max_What=ifelse(Max_Rep>=Max_Imat,"Reprodutivos","Imaturos"))%>%
pivot_longer(c(Imaturos,Reprodutivos),names_to="Stage",values_to="Values")%>%
ggplot(.,aes(x=Mes,y=Values,group=Quadrante))+
geom_line(aes(color=Stage))+
facet_wrap(~Site+Plot,scale="free")



