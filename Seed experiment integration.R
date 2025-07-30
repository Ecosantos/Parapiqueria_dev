#'#####################################################################
# Script for integration of seed experiments and genetic data
# from Parapiqueria cavalcantei
#
# by Gabriel Santos
# 28 July 2025
#'#####################################################################


library(ggplot2)
library(openxlsx)
library(tidyverse)


#'=============================================
#	Genetic diversity
#Baseado no artigo da Barbara Leal et al. 2025
#https://doi.org/10.1093/aob/mcaf030
#AR = Allelic Richness
#HO = Heterozigozidade observada
#HE = Heterozigozidade esperada 
#'=============================================
ARS11C<-1.440; ARS11C.conf=c(1.343,1.534)
ARS11B<-1.390; ARS11B.conf=c(1.257,1.467)



#=============================================
#	Seed experiments
#=============================================
#-----------------------------------------------------------
#	FECUNDIDADE
# Dados disponíveis em: [[Fecundidade_Parapiqueria_2023.xlsx]]
#obsidian://open?vault=Notes&file=9%20-%20ITV%2FArquivos%2FDados%20outros%20projetos%2FFecundidade_Parapiqueria_2023.xlsx
#setwd("C:\\Artigos e resumos publicados submetidos ideias\\Notes\\9 - ITV\\Arquivos\\Dados outros projetos")
#-----------------------------------------------------------
seeds<- read.xlsx("Data/Fecundity.xlsx", sheet="Fecundity")


# "ANOVA" poisson
glm(Seeds~Site, family="poisson",data=seeds)%>%summary()

#-----------------------------------------------------------
#	GERMINATION
#-----------------------------------------------------------
germination_raw <- read.xlsx("Data/Germination.xlsx", startRow = 5,sheet="Germination")

germination<-germination_raw <- read.xlsx("Data/Germination.xlsx", startRow = 5,sheet="Germination")%>% 
filter(!(Treatment %in% c("Mean","SD","Treatment")))%>%
select(Treatment,viable,"%viab","#germ.","%germ",time)%>%
mutate(across(-Treatment, as.numeric))

colnames(germination)<-c("Site","Viables","Viab.rate","Germinated","Germ.rate","Time")

colnames(dadosclean)<-c("Riacho","Viaveis","Viab.rate","Germinada","Germ.rate","Tempo")

t.test(germination$Germinated~ germination$Site)
t.test(germination$Germ.rate~ germination$Site)
t.test(germination$Time~ germination$Site)  
t.test(germination$Viab.rate~ germination$Site)  


# Check if both streams have six replicates
ndados<-table(germination$Site)
ndados[1] == ndados[2]




#-----------------------------------------------------------
#	SEED MASS
#-----------------------------------------------------------

mass<- read.xlsx("Data/Germination.xlsx", startRow = 8,sheet="Mass")
massclean<-mass%>%filter(!(Group %in% 
	c("Mean","SD","Seeds from stream S11B","Group")))%>%
mutate(Site=c(rep("S11C",8),rep("S11B",8)))

colnames(massclean)<-c("Group","Mass","Site")


t.test(as.numeric(massclean$Mass)~massclean$Site)



#===========================================================
#	PLOTS ------
#===========================================================

# Seeds produced

seedsplot<-seeds%>%
 group_by(Site)%>%
 summarise(Mean=mean(Seeds),
	SD=sd(Seeds),
	lower95=quantile(Seeds,.025),          # Mais efetivo para caracterizar a distribuição                                                   empiríca não normal de uma amostra.
	higher95=quantile(Seeds,.975))%>%
mutate(Sig="Sig")%>%
mutate(Facet_name="Fecundity")%>%
ggplot(.,aes(x=Site,y=Mean))+
geom_pointrange(aes(ymin=lower95,ymax=higher95,linetype=Sig,color=Site,shape=Sig))+
scale_color_manual(values=c("#ff9211","#6e86e7"))+
scale_linetype_manual(values=c(1))+
facet_grid(.~Facet_name)+
labs(y="Seeds produced",
	x=NULL)+
theme_bw(base_size=20)

seedsplot


# Viable seeds
viabplot<-germination%>%
pivot_longer(!Site,names_to="VAR",values_to="Values")%>%
filter(VAR =="Viab.rate")%>%
group_by(Site,VAR)%>%
summarise(Mean=mean(Values),
		SD=sd(Values),
		lower95=quantile(Values,.025),          # Mais efetivo para caracterizar a distribuição                                                   empiríca não normal de uma amostra.
		higher95=quantile(Values,.975))%>%
mutate(Sig=case_when(VAR == "Viab.rate" ~"NonSig"))%>%
mutate(Site=ifelse(Site=="Riacho 1","S11C","S11B"))%>%
mutate(VAR=factor(VAR, levels = c("Germ.rate","Time","Viab.rate")))%>%
mutate(Facet_name="Viability")%>%
ggplot(.,aes(x=Site,y=Mean))+
geom_pointrange(aes(ymin=lower95,ymax=higher95,linetype=Sig,color=Site,shape=Sig))+
scale_color_manual(values=c("#ff9211","#6e86e7"))+
scale_shape_manual(values=c(21,19))+
scale_linetype_manual(values=c(2))+
facet_grid(.~Facet_name)+
labs(y="Viable seeds (%)",
	x=NULL)+
theme_bw(base_size=20)

viabplot


# Germinated seeds
germplot<-germination%>%
pivot_longer(!Site,names_to="VAR",values_to="Values")%>%
filter(VAR =="Germ.rate")%>%
group_by(Site,VAR)%>%
summarise(Mean=mean(Values),
		SD=sd(Values),
		lower95=quantile(Values,.025),          # Mais efetivo para caracterizar a distribuição                                                   empiríca não normal de uma amostra.
		higher95=quantile(Values,.975))%>%
mutate(Sig=case_when(VAR == "Germ.rate" ~"NonSig"))%>%
mutate(Site=ifelse(Site=="Riacho 1","S11C","S11B"))%>%
mutate(VAR=factor(VAR, levels = c("Germ.rate","Time","Viab.rate")))%>%
mutate(Facet_name="Germination")%>%
ggplot(.,aes(x=Site,y=Mean))+
geom_pointrange(aes(ymin=lower95,ymax=higher95,linetype=Sig,color=Site,shape=Sig))+
scale_color_manual(values=c("#ff9211","#6e86e7"))+
scale_shape_manual(values=c(21,19))+
scale_linetype_manual(values=c(1))+
facet_grid(.~Facet_name)+
labs(y="Germinated seeds (%)",
	x=NULL)+
theme_bw(base_size=20)

germplot

# Time = Mean days to germinate
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
mutate(Facet_name="Time")%>%
ggplot(.,aes(x=Site,y=Mean))+
geom_pointrange(aes(ymin=lower95,ymax=higher95,linetype=Sig,color=Site,shape=Sig))+
scale_color_manual(values=c("#ff9211","#6e86e7"))+
scale_shape_manual(values=c(19))+
scale_linetype_manual(values=c(1))+
facet_grid(.~Facet_name)+
labs(y="Mean days \nto germinate",
	x=NULL)+
theme_bw(base_size=20)

germTimeplot




# Mass
massplot<-massclean%>%
group_by(Site)%>%
mutate(Mass=as.numeric(Mass)*1000)%>%
summarise(Mean=mean(Mass),
		SD=sd(Mass),
		lower95=quantile(Mass,.025),          # Mais efetivo para caracterizar a distribuição                                                   empiríca não normal de uma amostra.
		higher95=quantile(Mass,.975))%>%
mutate(VAR="Mass \n(1000 sementes)")%>%
mutate(Facet_name="Mass")%>%
ggplot(.,aes(x=Site,y=Mean))+
geom_pointrange(aes(ymin=lower95,ymax=higher95,color=Site),linetype=1,shape=19)+
scale_color_manual(values=c("#ff9211","#6e86e7"))+
labs(y="Mass per \n1000 individuals (mg)",
	x=NULL)+
facet_grid(.~Facet_name)+
theme_bw(base_size=20)

massplot

library(cowplot)

plotlegend<-germplot+theme(legend.position="top", legend.box="vertical",
  legend.spacing.y = unit(-0.1, "cm")  # Ajuste o valor conforme necessário
)

massplot

# Genetic data plot
genplot<-data.frame(Site=c("S11C","S11B"),
	     Div=c(ARS11C,ARS11B),
		lower=c(ARS11C.conf[1],ARS11B.conf[1]),
		higher=c(ARS11C.conf[2],ARS11B.conf[2]))%>%
mutate(VAR="Gen.Div")%>%
mutate(Facet_name="Genetic diversity \n (Leal et al. 2025)")%>%
ggplot(.,aes(x=Site,y=Div))+
geom_pointrange(aes(ymin=lower,ymax=higher,color=Site),linetype=2,shape=21)+
scale_color_manual(values=c("#ff9211","#6e86e7"))+
labs(y="Heterozygosis",
	x=NULL)+
facet_grid(.~Facet_name)+
theme_bw(base_size=20)+
#theme(strip.background =element_rect(fill="tomato"))+
theme(legend.position="none")

genplot

#-----------------------------------------------------------
#      Merge all plots
#-----------------------------------------------------------
MergedPlots<-plot_grid(ncol=3,  align = "v",
  axis = "l",
	seedsplot+theme(
		legend.position="none"),
	massplot+theme(
		legend.position="none"),
	viabplot+theme(
		legend.position="none"),
	germplot+theme(
		legend.position="none"),
	germTimeplot +theme(
		legend.position="none"),
#	genplot+theme(
#		legend.position="none")+s
#cale_y_continuous(breaks=c(1.3,1.4,1.5))
cowplot::ggdraw(get_plot_component(plotlegend,'guide-box-top', return_all = TRUE))
)	

MergedPlots


ggsave(MergedPlots,"svg",)

