#'######################################################
#		Script Parapiqueria - FRAMEWORK
#		 	Em desevolvimento
#		Iniciado em: 05/08/2024
#		Ultima atualização: 06/08/2024
##'#####################################################

library(popbio)
library(ggplot2)
library(tidyverse)
library(openxlsx)
library(ggrepel)
library(ggridges)
library(broom)

(list=ls())



#'=======================================================
#    =======  WORKFLOW
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
## 1.Estimate survival, stasis and growth using a Quadratic Programming inverse method
##  1.1. Step 1. Define the rules/constraints that must be given to the QP - Quadratic programming (C,b,non-zeros)
## 2. Reach a best method to estimate recruitment, as it cannot be directly estimated with QP
## 3. Apply QP and retrieve MPMs
## 4. Complement MPMs with recruitment
## 5. Calculate lambda and plot results
## 6. Population viability (more complete for the sake of relatório final 2024)
## 6.1. Simulations with reducing vital rates'
} ## Goals
#'=======================================================


#'=======================================================
# ----		ACCESSORY FUNCTION
# Automatically detect C, b, and non-zero elements
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{## - ven is the target MPM
## - myC2ven make matrix C
##   - myC2ven must be make in two parts because it is not a simple identity matrix
## - mynonzero.ven determines the non-zero elements
## - b elements NEED EXPLANATION 
## 
######  Additional information 
## Função ainda em desenvolvimento, parece não funcionar para matrizes 2x2 ainda.
## Não sei pq isso ocorre!
###
}  ## Rationale
#'=======================================================


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

#'=======================================================
## EXAMPLE - Model Toy
#'-------------------------------------------------------
{# Simulate an hypothetical population
# 		to assess model's accuracy
## Must run in order to determine C, non-zero elements and b
## which will be used to estimate survival
## 
## Also, contain model validation
} ##Rationale
##'========================================================
source("ModelToyv1.R")

#Alternative
#file.edit("ModelToyv1.R")

#'=======================================================
#	DATA LOADING and STANDARDIZATION
#'-------------------------------------------------------
# Estimate recruitment in Parapiqueria between 2022-2024
#'========================================================
### 2022
Mar22<-read.xlsx("Dados brutos/Dados parapiqueria - Completo - Consolidado 05Aug2024.xlsx",sheet="Março2022")
Abr22<-read.xlsx("Dados brutos/Dados parapiqueria - Completo - Consolidado 05Aug2024.xlsx",sheet="Abril2022")
Mai22<-read.xlsx("Dados brutos/Dados parapiqueria - Completo - Consolidado 05Aug2024.xlsx",sheet="Maio2022")

### 2023
Mar23<-read.xlsx("Dados brutos/Dados parapiqueria - Completo - Consolidado 05Aug2024.xlsx",sheet="Março2023")
Abr23<-read.xlsx("Dados brutos/Dados parapiqueria - Completo - Consolidado 05Aug2024.xlsx",sheet="Abril2023")
Mai23<-read.xlsx("Dados brutos/Dados parapiqueria - Completo - Consolidado 05Aug2024.xlsx",sheet="Maio2023")

### 2024
Mar24<-read.xlsx("Dados brutos/Dados parapiqueria - Completo - Consolidado 05Aug2024.xlsx",sheet="Março2024")
Abr24<-read.xlsx("Dados brutos/Dados parapiqueria - Completo - Consolidado 05Aug2024.xlsx",sheet="Abril2024")
Mai24<-read.xlsx("Dados brutos/Dados parapiqueria - Completo - Consolidado 05Aug2024.xlsx",sheet="Maio2024")

#'---------------------------------------------------------
##          Merge Census
{####### Masking quadrants is necessary, 
####### so quadrants is not included in group_by 
####### Excepting for census 2022 because quadrant was not implemented yet
} # comments about group_by quadrants...
#'---------------------------------------------------------

### 2024
#Mask quadrants as it won't be necessary here
census2024<-rbind(
data.frame(Mar24,Month="3"),
data.frame(Abr24,Month="4"),
data.frame(Mai24,Month="5"))%>%
as_tibble()%>%
group_by(Site,Plot,Month)%>%    #Quadrants not included
summarise(Imaturos=sum(Imaturos),
		   Reprodutivos=sum(Reprodutivos))%>%
ungroup()


### 2023
census2023<-rbind(
data.frame(Mar23,Month="3"),
data.frame(Abr23,Month="4"),
data.frame(Mai23,Month="5"))%>%
as_tibble()%>%
group_by(Site,Plot,Month)%>%
summarise(Imaturos=sum(Imaturos),
		Reprodutivos=sum(Reprodutivos))%>%
ungroup()

### 2022
census2022<-rbind(
data.frame(Mar22,Month="3"),
data.frame(Abr22,Month="4"),
data.frame(Mai22,Month="5"))%>%
select(Site,Plot,Imaturos,Reprodutivos,Month)

#'======================================================
#	  ---- RECRUITMENT ----
#'-------------------------------------------------------
# Estimate recruitment in Parapiqueria between 2022-2024
#'=======================================================

#'--------------------------------------------------------
##		Data preparation
#'--------------------------------------------------------
Census_all<-rbind(
cbind(census2024,year="2024"),
cbind(census2023,year="2023"),
cbind(census2022,year="2022"))%>%
pivot_longer(Imaturos:Reprodutivos,names_to="stage")


#### Maximum number of individuals
Census_all_max<-rbind(
cbind(census2024,year="2024"),
cbind(census2023,year="2023"),
cbind(census2022,year="2022"))%>%
group_by(Site,year,Plot)%>%
summarise(MaxIm=max(Imaturos),
          MaxRep=max(Reprodutivos))%>%
mutate(MaxTot=ifelse(MaxIm>MaxRep,MaxIm,MaxRep))%>%
pivot_longer(MaxIm:MaxTot,names_to="stage")

	
#'---------------------------------------------------------
##		PAIRING TIMELAGS
#' --------------------------------------------------------
###     setting t0 and t1 
#'--------------------------------------------------------

Timelags_base<-Census_all_max %>%
  mutate(year = as.numeric(year)) %>%
  group_by(Plot) %>%
  arrange(year, stage) %>%
  mutate(VAR = paste0(stage, ">", 
                lead(stage, order_by = interaction(year,stage)))) %>%
  mutate(t0y = year,
         t1y = lead(year, order_by = VAR))%>%
  mutate(t0 = value, 
         t1 = lead(value, order_by = VAR)) %>%
filter(t1y>t0y)%>%	#It only returns with timelag. Change interaction to return same year variables
arrange(Plot,VAR)

#### Max Rep in t0 vs. MaxTot in t1
#Pairing Maximum reprodutctive to Max Total

RepTot<-left_join(
filter(Timelags_base,stage=="MaxRep")%>%select(Site:Plot,t0y,t0),
filter(Timelags_base,stage=="MaxTot")%>%select(Site:Plot,t1y,t1))%>%
mutate(VAR="MaxRep>MaxTot")%>%
select(VAR,Site:t0y,t1y,t0,t1)%>%	#Reorder columns
mutate(Site=as.factor(Site))%>%
mutate(period=as.factor(paste0(t0y,"-",t1y)))

##### Max Rep in t0 vs. MaxIm in t1

RepIm<-left_join(
filter(Timelags_base,stage=="MaxRep")%>%select(Site:Plot,t0y,t0),
filter(Timelags_base,stage=="MaxIm")%>%select(Site:Plot,t1y,t1))%>%
mutate(VAR="MaxRep>MaxIm")%>%
select(VAR,Site:t0y,t1y,t0,t1)%>%	#Reorder columns
mutate(Site=as.factor(Site))%>%
mutate(period=as.factor(paste0(t0y,"-",t1y)))

#'----------------------------------------------------------------------------
#   ---- RECRUITMENT ANALYSES ----
#'----------------------------------------------------------------------------
library(lme4)
library(DHARMa)
library(performance)
library(glmmTMB)
library(MuMIn)

options(na.action=na.fail)

#'----------------------------------------------------------------------------
## Maximum Reprodutives > Maximum Immatures
# Neg. Binomial has the best fit
#'-------------------------------------------------------------------------

nb_RepIm<-glmmTMB::glmmTMB(t1~0+t0*Site+(1|period),
				family=nbinom2(link = "log"),data=RepIm)

nb_RepIm_ls<-dredge(nb_RepIm)%>%get.models(subset=NA)

nb_RepIm_df<-data.frame(
	formula=cbind(lapply(nb_RepIm_ls,formula)),
		Var="Rep>Tot",Dist="Neg.Bin",
			AICcmodavg::aictab(nb_RepIm_ls))
nb_RepIm_df

m2<-update(nb_RepIm,t1~0+t0+Site+(1|period))
m3<-update(nb_RepIm,t1~0+t0+Site*period)

ggeffects::predict_response(m2, terms=c("Site","period"))%>%plot()


# Best model is t1~0+t0+Site+(1|census) Negative binomial
BestModel<-update(nb_RepIm,t1~0+t0+Site+(1|period))

ggeffects::predict_response(
  BestModel, 
  terms=c("t0[0:208]", #[0,208] represent the minimum and maximum individuals record in plot per month and pear stage(?)
          "Site",
          "period"))%>%
                    plot()


BestModel%>%parameters::parameters(.,exponentiate =T)
BestModel%>%summary()
car::Anova(BestModel)

bestS11B<-update(BestModel,~0+t0+(1|period), data=filter(RepIm,Site=="S11B"))
bestS11C<-update(BestModel,~0+t0+(1|period), data=filter(RepIm,Site=="S11C"))

bestS11B%>%parameters::parameters()
ggeffects::predict_response(bestS11B, terms=c("t0[0:208]"))%>%data.frame()%>%head()



## Dispersion factor in the negative distribution.
{
## Initially I was using this for simulate variation in fecundity but not sure if it is still necessary
}### Rationale --- MAY BE REMOVED!!!!
Theta_bestS11B<-bestS11B$fit$par[names(bestS11B$fit$par)=="theta"]
Theta_bestS11C<-bestS11C$fit$par[names(bestS11C$fit$par)=="theta"]

#'-------------------------------------------------------------
####	T-TEST 
####	Test if there is difference in recruitment between the streams
#'-------------------------------------------------------------

RepIm%>%mutate(rec_rate=t0/t1)%>%
glm(rec_rate~Site,data=.)

#'-------------------------------------------------------------
#### REPRODUCTION PER PLOT
#'-------------------------------------------------------------
RecruitRate_site<-RepIm%>%
ungroup()%>%
select(-c(year,t0y:t1y))%>%
nest(data=c(Plot,t0,t1))%>%
 mutate(
    fit = map(data, ~ MASS::glm.nb(t1 ~ 0+t0, data = .)),
    tidy_fit = map(fit, ~ parameters::parameters(.,exponentiate = T))
  ) %>%
  unnest(tidy_fit)%>%
mutate(VR="Recruitment")

RecruitRate_plot<-RecruitRate_site%>%
ggplot(.,aes(x=VR ,y=Coefficient,group=period))+
geom_pointrange(aes(ymin=Coefficient-SE,ymax=Coefficient+SE,color=Site,shape=period),alpha=0.8,
	position = position_dodge2(width = 0.7))+
theme_bw(base_size=16)+
scale_color_manual(values=c("#6e86e7","#ff9211"))+
theme(legend.position="bottom")+
facet_grid(.~VR)+
ylab(NULL)+xlab(NULL)

RecruitRate_plot
#'========================================================================
#	---- ESTIMATING SURVIVAL ---- 
#'========================================================================
## TRANSFORMING CENSUS IN MONTH TIMESERIES 

## 2024
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

## 2023
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


## 2022
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



#'=========================================================================
## ---- ESTIMATE SURVIVAL COMPONENTS OF THE POPULATION MATRIX MODELS ----
#           "ModelToyv1.R" must be sourced
#'=========================================================================

#'-------------------------------------------------------------
### ADJUST PARAMETERS TO OTIMIZATION  ----
#'------------------------------------------------------------
### C matrix and b need adjustments
# because the Woodspar function is not fully functional for 2x2 matrices
#'------------------------------------------------------------

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


#'------------------------------------------------------------------------
### REMOVE THOSE CENSUSES WHERE MATRICES CANNOT BE ESTIMATED  
#'------------------------------------------------------------------------
{
  # Two plots at S11B could not have demographic information estimated
  # because individuals are close to zero.
  # Plots are:
    ## Plot_19_2022 - Only adult individuals recorded?
    ## Plot_15_2024 - Only 2 individuals in  March 
  # I'm not sure if I can just remove them. Would be nice 
  # On next section I provide an alternative by including matrix 2x2 with all elements equal to zero
  # There are very few consequences by doing that:
    # 1. Growth rate of year 2022 was close to 1 before this correction 
      ## and become more reliable after this correction
    # 2.  Extinction rate become significant different between streams 
}## Rationale - !!!!!!!!!! DECISIONS TO MAKE !!!!!!!!!!!!
#'-----------------------------------------------------------------------

mpm_list_bkp<-mpm_list<-NULL
for(i in 1:length(all_census_ts)){
mpm_list_bkp[[i]]<-mpm_list[[i]]<-tryCatch(
		QPmat(all_census_ts[[i]]%>%as.matrix(), 
			newC,
			newb,
			newnonzero),
 error = function(e) { return(NA)})
}

names(mpm_list)<-names(mpm_list_bkp)<-names(all_census_ts)

# ##	---- This soluction remove plots where estimates could not be performed. 
# See below 
mpm_list<-mpm_list[lapply(
		lapply(mpm_list,complete.cases),
			any,FALSE)%>%unlist()]

mpm_list%>%length()

mpm_list


#'-------------------------------------------------------------
##	---- Checking NULL and transform to zero 
{## 
## NULL here represents populations that show very small number of individuals 
## Or even got extinct during the sampling (plots 15 and 19) 
## Now I transform them in matrices with all elements being equal to zero
} # Rationale 
#'-------------------------------------------------------------
# Detect problem plots
problems_plots<-mpm_list_bkp[lapply(lapply(mpm_list_bkp,complete.cases),any,FALSE)=="FALSE"]

problems_plots


all_census_ts[names(problems_plots)]

# Create a list with a single MPM with all elements equal to zero
zero_mpm<-list()
zero_mpm[[1]]<-matrix(rep(0,4),2)

mpm_list_bkp[names(mpm_list_bkp) %in% names(problems_plots)]
mpm_list_bkp[names(mpm_list_bkp) %in% names(problems_plots)]<-zero_mpm  #Include zero matrices in the list of estiamted matrices
mpm_list_bkp[names(mpm_list_bkp) %in% names(problems_plots)]

Census2022_ts$Plot_15_2022
Census2023_ts$Plot_15_2023
Census2024_ts$Plot_15_2024

# Now make mpm_list being equal to mpm_list_bkp
mpm_list <- mpm_list_bkp


#'=========================================================================
#   LINKING RECRUITEMENT TO QP ESTIMATION 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{ # There are few possibilities to estimate recruitment.
  # 1. Fixing recruitment based on best model estimated from GLM
  # 2. Simple ratio between t1/t0 
  # NOTE. Fixing recruitment to 1 is no longer an option
    # because there is better options 
}# !!!!!!!!!!!!!! TWO POSSIBILITIES TO DECIDE !!!!!!!!!!!!!!
#'=========================================================================

{
  # It sounds I'm working with two different scales.
  # 
  #sqrt(sqrt(exp(fixef(bestS11B)$cond[[1]])))
  # Transforming recruitment as sqrt-sqrt seems like a option to rescale  
} # URGENT NEED TO DETERMINE

# MatU + MatF
#Split MPMs by stream
mpm_list_S11C<-mpm_list[grep("^Plot_([1-9]|10)_.*$", names(mpm_list))]
mpm_list_S11B<-mpm_list[grep("^Plot_(1[1-9]|20)_.*$", names(mpm_list))]

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


mpm_list_recruit_S11B%>%lapply(.,as.vector)%>%do.call(rbind,.)%>%head()
mpm_list_recruit_S11C%>%lapply(.,as.vector)%>%do.call(rbind,.)%>%head()

mpm_list_recruit_full<-c(mpm_list_recruit_S11C,mpm_list_recruit_S11B)
#'=======================================================================
# 		Vital rate statistics and plot
#'=======================================================================
# OVERAL SUMMARY
data.frame(do.call(rbind,lapply(mpm_list_recruit_full,as.vector)),var="vr")%>%rownames_to_column(var = "VAR")%>%
as_tibble()%>%
separate(VAR,sep="_",into = c("x", "Plot", "year"))%>%
select(-x)%>%
pivot_longer(X1:X4,values_to="value",names_to="vital_rate")%>%
mutate(Plot=as.numeric(Plot),
	 year=year)%>%
mutate(Site=ifelse(Plot<11,"S11C","S11B"))%>%
group_by(var,vital_rate)%>%
summarise(Mean=mean(value),
		SD=sd(value),
		N=n(),
		SE=SD/sqrt(N))


# PER PLOT
Vital_rates<-data.frame(do.call(rbind,lapply(mpm_list_recruit_full,as.vector)),var="vr")%>%rownames_to_column(var = "VAR")%>%
as_tibble()%>%
separate(VAR,sep="_",into = c("x", "Plot", "year"))%>%
select(-x)%>%
pivot_longer(X1:X4,values_to="value",names_to="vital_rate")%>%
mutate(Plot=as.numeric(Plot),
	 year=year)%>%
mutate(Site=ifelse(Plot<11,"S11C","S11B"))


Vital_rates_df<-Vital_rates%>%
group_by(var,vital_rate,Site,year)%>%
summarise(Mean=mean(value),
		SD=sd(value),
		N=n(),
		SE=SD/sqrt(N),
		CI=1.96*(SD/sqrt(N)),                   
		lower95=quantile(value,.025),          
		higher95=quantile(value,.975))

#'-------------------------------------------------------------------------
# TESTING DIFFERENCES AMONG VITAL RATES PER STREAMS ----
#'-------------------------------------------------------------------------

Vital_rates%>%
filter(vital_rate!="X3")%>%
select(-c(year,var))%>%
nest(data=c(Plot,Site,value))%>%
mutate(pairwise= map(data, ~ t.test(value ~ Site, data = .)))%>% 
pull(pairwise)%>%
purrr::set_names(c("Stasis[Immat]", "Growth[Immat.>Adult]", "Surv[Adult]"))%>%
tibble(
  Comparison = names(.),
  t_value = map_dbl(., ~ .x$statistic),
  df = map_dbl(., ~ .x$parameter),
  p_value = map_dbl(., ~ .x$p.value),
  conf_low = map_dbl(., ~ .x$conf.int[1]),
  conf_high = map_dbl(., ~ .x$conf.int[2]),
  mean_S11B = map_dbl(., ~ .x$estimate[1]),
  mean_S11C = map_dbl(., ~ .x$estimate[2])
)


#'-------------------------------------------------------------------------
# TESTING SIGN ACROSS YEARS
#'-------------------------------------------------------------------------

Vital_rates%>%
filter(vital_rate!="X3")%>%
select(-c(Plot,Site,var))%>%
mutate(year=as.numeric(year))%>%
nest(data=c(year,value))%>%
mutate(pairwise= map(data, ~ cor.test(.$value, .$year,method="pearson")))%>%
pull()%>%
purrr::set_names(c("Stasis[Immat]", "Growth[Immat.>Adult]", "Surv[Adult]"))%>%
tibble(
  Vital_rate = names(.),
  Correlation = map_dbl(., ~ .x$estimate[1]),
  conf_low = map_dbl(., ~ .x$conf.int[1]),
  conf_high = map_dbl(., ~ .x$conf.int[2]),
  t = map_dbl(., ~ .x$statistic),
  df = map_dbl(., ~ .x$parameter),
  p_value = map_dbl(., ~ .x$p.value))



#'-------------------------------------------------------------------------
#	    PLOT
#'-------------------------------------------------------------------------

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

Vital_rates_plot


#'----------------------------------------------------------------------------
#	  Observed Growth rate across the years
#'----------------------------------------------------------------------------
lambsSite<-rbind(
do.call(rbind,lapply(mpm_list_recruit_S11C,lambda))%>%
	data.frame(Site="S11C",mod="model")%>%
			rownames_to_column(var = "VAR"),
do.call(rbind,lapply(mpm_list_recruit_S11B,lambda))%>%
	data.frame(Site="S11B",mod="model")%>%
			rownames_to_column(var = "VAR"))

#TESTING DIFFERENCES
t.test(lambsSite$.~lambsSite$Site)




lambsSite_df<-lambsSite%>%rename(., Lambda=.)%>%
as_tibble()%>%
separate(VAR,sep="_",into = c("x", "Plot", "year"))%>%
select(-x)%>%
mutate(Plot=as.numeric(Plot),
	 year=year)%>%
mutate(Site=ifelse(Plot<11,"S11C","S11B"))%>%
group_by(Site,year)%>%
summarise(Mean=mean(Lambda),
		SD=sd(Lambda),
		N=n(),
		SE=SD/sqrt(N),
		CI=1.96*(SD/sqrt(N)),                   
		lower95=quantile(Lambda,.025),          
		higher95=quantile(Lambda,.975))

lambsSite_df


lambsPlot<-lambsSite_df%>%
mutate(VAR="PopGrowthRate")%>%
ggplot(.,aes(y=Mean,x=VAR,group=year))+
geom_hline(yintercept=1,col="red", linetype="longdash")+
geom_pointrange(aes(ymin=lower95,ymax=higher95,color=Site,shape=year),alpha=0.8,
	position = position_dodge2(width = 0.7))+
#  geom_line(aes(color = Site, group = Site), 
#            position = position_dodge2(width = 0.7))+
scale_x_discrete(labels=function(l) parse(text=l))+
scale_color_manual(values=c("#ff9211","#6e86e7"))+
theme_bw(base_size=16)+
theme(legend.position="bottom")+
facet_grid(.~VAR)+
xlab(NULL)+ylab(NULL)

lambsPlot

#'----------------------------------------------------------------------------
#	  Extinction probability - Population viability analyses
#'----------------------------------------------------------------------------
############### WORKING ##############

# Mean/VAR stable stage distribution
lapply(mpm_list_recruit_full,stable.stage)%>%do.call(rbind,.)%>%apply(.,2,mean)
lapply(mpm_list_recruit_full,stable.stage)%>%do.call(rbind,.)%>%apply(.,2,sd)


#------------ 

n_init<-c(50,50)
n_threshold<-sum(n_init)*.10

projS11B_10<-stoch.projection(mpm_list_recruit_S11B, 
n_init,	#Estabelece meio indivíduo em cada estágio,logo, ao somar, teriamos quanto que a população doubrou,triplicou...
#	stable.stage(mean(mpm_list_recruit_S11B))*100,
	tmax=10,nreps=1000)

projS11B_30<-stoch.projection(mpm_list_recruit_S11B, 
n_init,	#Estabelece meio indivíduo em cada estágio,logo, ao somar, teriamos quanto que a população doubrou,triplicou...
#	stable.stage(mean(mpm_list_recruit_S11B))*100,
	tmax=30,nreps=1000)

projS11C_10<-stoch.projection(mpm_list_recruit_S11C, 
n_init,	#Estabelece meio indivíduo em cada estágio,logo, ao somar, teriamos quanto que a população doubrou,triplicou...
#	stable.stage(mean(mpm_list_recruit_S11C))*100,
	tmax=10,nreps=1000)

projS11C_30<-stoch.projection(mpm_list_recruit_S11C, 
n_init,	#Estabelece meio indivíduo em cada estágio,logo, ao somar, teriamos quanto que a população doubrou,triplicou...
#	stable.stage(mean(mpm_list_recruit_S11C))*100,
	tmax=10,nreps=1000)



projALL_10<-stoch.projection(c(mpm_list_recruit_S11B,mpm_list_recruit_S11C),
n_init,	#Estabelece meio indivíduo em cada estágio,logo, ao somar, teriamos quanto que a população doubrou,triplicou...
#	stable.stage(mean(mpm_list_recruit_S11B))*100,
	tmax=10,nreps=1000)

projALL_30<-stoch.projection(c(mpm_list_recruit_S11B,mpm_list_recruit_S11C),
n_init,	#Estabelece meio indivíduo em cada estágio,logo, ao somar, teriamos quanto que a população doubrou,triplicou...
#	stable.stage(mean(mpm_list_recruit_S11B))*100,
	tmax=10,nreps=1000)



#'-------------------------------------------------------------------------
# Create proptable_modif a special prop table ------------------
#'-------------------------------------------------------------------------
## Rationale  --------------
# proptable_modif is an adaptation of default prop.table
# This function allow to include FALSE or TRUE when there is no legitime true or false extinction 
#'-------------------------------------------------------------------------
proptable_modif <- function(data, threshold) {
  # equivalent to prop.table(table(apply(data,1,sum)> n_threshold))*100
  res <- table(factor(apply(data, 1, sum) > threshold, levels = c(FALSE, TRUE)))
  # Equivalent to prop.table(ext10.probS11B)*100
  prop <- prop.table(res) * 100
  return(prop)
}
#'-------------------------------------------------------------------------

apply(projS11C_30,1,sum)%>%range()
apply(projS11B_30,1,sum)%>%range()


# Chi-square of extinction rate between streams
data.frame(rbind(
  proptable_modif(projS11B_10, n_threshold),
  proptable_modif(projS11C_10, n_threshold)),
  row.names = c("S11B", "S11C"))%>%
  print()%>%chisq.test()

data.frame(rbind(
  proptable_modif(projS11B_30, n_threshold),
  proptable_modif(projS11C_30, n_threshold)),
  row.names = c("S11B", "S11C"))%>%
  print()%>%chisq.test()


rbind(
data.frame(projS11B_10,Site="S11B"),
data.frame(projS11C_10,Site="S11C"))%>%
mutate(total=rowSums(select_if(., is.numeric), na.rm = TRUE))%>%
mutate(Mean=mean(total),
	 Median=median(total))%>%
ggplot(.,aes(x=total,group=Site))+
geom_histogram(
	aes(y=..count../sum(..count..),
    		fill=Site),
	position = 'identity',color="black",alpha=.3)+
scale_fill_manual(values=c("#6e86e7","#ff9211"))




#'========================================================================================
#	BUILDING MATRIX POPULATION MODEL
#'========================================================================================















#'========================================================================================
#	BUILDING MATRIX POPULATION MODEL
#'========================================================================================


#'----------------------------------------------------------------------------
#	Gráfico das taxas vitais
#'----------------------------------------------------------------------------


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