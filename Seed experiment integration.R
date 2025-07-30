#'############################################
# Script for integration of seed experiments and genetic data
# from Parapiqueria cavalcantei
#
# by Gabriel Santos
#	28 July 2025
#'############################################


library(ggplot2)

#'=============================================
#	Genetic diversity
#Baseado no artigo da Barbara Leal et al. 2025
#AR = Allelic Richness
#HO = Heterozigozidade observada
#HE = Heterozigozidade esperada 
#'=============================================
ARS11C<-1.440; ARS11C.conf=c(1.343,1.534)
ARS11B<-1.390; ARS11B.conf=c(1.257,1.467)



#=============================================
#	Seed experiments
#=============================================
library(openxlsx)
library(tidyverse)


#-----------------------------------------------------------
#	GERMINAÇÃO
# Dados disponíveis em: [[germ_parapiqueria.xlsx]]
# obsidian://open?vault=Notes&file=9%20-# %20ITV%2FArquivos%2FDados%20outros%20projetos%2Fgerm_parapiqueria.xlsx
#setwd("C:\\Artigos e resumos publicados submetidos ideias\\Notes\\9 - ITV\\Arquivos\\Dados outros projetos")
#-----------------------------------------------------------

germination_raw <- read.xlsx("Data/Germination.xlsx", startRow = 5,sheet="Germination")

germination<-germination_raw <- read.xlsx("Data/Germination.xlsx", startRow = 5,sheet="Germination")%>% 
filter(!(Treatment %in% c("Mean","SD","Treatment")))%>%
select(Treatment,viable,"%viab","#germ.","%germ",time)%>%
mutate(across(-Treatment, as.numeric))

colnames(germination)<-c("Site","Viables","Viab.rate","Germinated","Germ.rate","Time")

t.test(germination$Germ.rate~ germination$Site)
t.test(germination$Time~ germination$Site)  
t.test(germination$Viab.rate~ germination$Site)  


ndados<-table(germination$Site)
ndados[1] == ndados[2]

germplot<-germination%>%
pivot_longer(!Site,names_to="VAR",values_to="Values")%>%
filter(!(VAR %in% c("Germinada","Viables","Time")))%>%
group_by(Site,VAR)%>%
summarise(Mean=mean(Values),
		SD=sd(Values),
		lower95=quantile(Values,.025),          # Mais efetivo para caracterizar a distribuição                                                   empiríca não normal de uma amostra.
		higher95=quantile(Values,.975))%>%
mutate(Sig=case_when(VAR == "Viab.rate" ~"NonSig",
			   VAR == "Germ.rate" ~"Sig",
			   VAR == "Time" ~"Sig"))%>%
mutate(Site=ifelse(Site=="Riacho 1","S11C","S11B"))%>%
mutate(VAR=factor(VAR, levels = c("Germ.rate","Time","Viab.rate")))%>%
ggplot(.,aes(x=Site,y=Mean))+
geom_pointrange(aes(ymin=lower95,ymax=higher95,linetype=Sig,color=Site,shape=Sig))+
scale_color_manual(values=c("#ff9211","#6e86e7"))+
scale_shape_manual(values=c(21,19))+
scale_linetype_manual(values=c(2,1))+
facet_grid(VAR~.)+
labs(y="%")+
theme_bw(base_size=14)

germplot


# Time = média de dias de germinação de%
germTimeplot<-germination%>%
pivot_longer(!Site,names_to="VAR",values_to="Values")%>%
filter(VAR =="Time")%>%
group_by(Site,VAR)%>%
summarise(Mean=mean(Values),
		SD=sd(Values),
		lower95=quantile(Values,.025),          # Mais efetivo para caracterizar a distribuição                                                   empiríca não normal de uma amostra.
		higher95=quantile(Values,.975))%>%
mutate(Sig=case_when(VAR == "Viab.rate" ~"NonSig",
			   VAR == "Germ.rate" ~"Sig",
			   VAR == "Time" ~"Sig"))%>%
mutate(Site=ifelse(Site=="Riacho 1","S11C","S11B"))%>%
mutate(VAR=factor(VAR, levels = c("Germ.rate","Time","Viab.rate")))%>%
ggplot(.,aes(x=Site,y=Mean))+
geom_pointrange(aes(ymin=lower95,ymax=higher95,linetype=Sig,color=Site,shape=Sig))+
scale_color_manual(values=c("#ff9211","#6e86e7"))+
scale_shape_manual(values=c(19))+
scale_linetype_manual(values=c(1))+
facet_grid(VAR~.)+
labs(y="Dias")+
theme_bw(base_size=14)

#-----------------------------------------------------------
#	FECUNDIDADE
# Dados disponíveis em: [[Fecundidade_Parapiqueria_2023.xlsx]]
#obsidian://open?vault=Notes&file=9%20-%20ITV%2FArquivos%2FDados%20outros%20projetos%2FFecundidade_Parapiqueria_2023.xlsx
#setwd("C:\\Artigos e resumos publicados submetidos ideias\\Notes\\9 - ITV\\Arquivos\\Dados outros projetos")
#-----------------------------------------------------------
seeds<- read.xlsx(file.choose(), sheet="Fecundidade")


# "ANOVA" poisson
glm(n_sementes~Local, family="poisson",data=seeds)%>%summary()

#-----------------------------------------------------------
#	PESO
# Dados disponíveis em: [[germ_parapiqueria.xlsx]]
# obsidian://open?vault=Notes&file=9%20-# %20ITV%2FArquivos%2FDados%20outros%20projetos%2Fgerm_parapiqueria.xlsx
#-----------------------------------------------------------

peso<- read.xlsx(file.choose(), startRow = 8,sheet="peso")
pesoclean<-peso%>%filter(!(Grupo %in% 
	c("Media","media","sd","SD","Sementes de individuos do riacho 2","Grupo")))%>%
mutate(Site=c(rep("S11C",8),rep("S11B",8)))

colnames(pesoclean)<-c("Grupo","Peso","Site")


t.test(as.numeric(pesoclean$Peso)~pesoclean$Site)



pesoplot<-pesoclean%>%
group_by(Site)%>%
mutate(Peso=as.numeric(Peso)*1000)%>%
summarise(Mean=mean(Peso),
		SD=sd(Peso),
		lower95=quantile(Peso,.025),          # Mais efetivo para caracterizar a distribuição                                                   empiríca não normal de uma amostra.
		higher95=quantile(Peso,.975))%>%
mutate(VAR="Peso \n(1000 sementes)")%>%
ggplot(.,aes(x=Site,y=Mean))+
geom_pointrange(aes(ymin=lower95,ymax=higher95,color=Site),linetype=1,shape=19)+
scale_color_manual(values=c("#ff9211","#6e86e7"))+
labs(y="Peso (mg)",
	x=NULL)+
facet_grid(VAR~.)+
theme_bw(base_size=14)


library(cowplot)

plotlegend<-germplot+theme(legend.position="top", legend.box="vertical",
  legend.spacing.y = unit(-0.1, "cm")  # Ajuste o valor conforme necessário
)



genplot<-data.frame(Site=c("S11C","S11B"),
	     Div=c(ARS11C,ARS11B),
		lower=c(ARS11C.conf[1],ARS11B.conf[1]),
		higher=c(ARS11C.conf[2],ARS11B.conf[2]))%>%
mutate(VAR="Gen.Div")%>%
ggplot(.,aes(x=Site,y=Div))+
geom_pointrange(aes(ymin=lower,ymax=higher,color=Site),linetype=2,shape=21)+
scale_color_manual(values=c("#ff9211","#6e86e7"))+
labs(y="Riq. Alélica",
	x=NULL)+
facet_grid(VAR~.)+
theme_bw(base_size=14)+
theme(strip.background =element_rect(fill="tomato"))+
theme(legend.position="none")


plot_grid(rel_heights = c(0.2,0.25,0.5,0.25,0.25),ncol=1,
cowplot::ggdraw(
get_plot_component(plotlegend,'guide-box-top', return_all = TRUE)),
	pesoplot+theme(
		legend.position="none",
		axis.text.x=element_blank(),
		plot.margin = margin(0, 1, 0, 0)),
	germplot+facet_grid(VAR~.)+	
		xlab(NULL)+theme(
		legend.position="none",
		axis.text.x=element_blank(),
		plot.margin = margin(0, 1, 0, 0)),
	germTimeplot+facet_grid(VAR~.)+
		xlab(NULL)+theme(
		legend.position="none",
		axis.text.x=element_blank(),
		plot.margin = margin(0, 1, 0, 0)),	
	genplot+theme(
		plot.margin = margin(0, 1, 0, 0))+	
	scale_y_continuous(breaks=c(1.3,1.4,1.5)))	

