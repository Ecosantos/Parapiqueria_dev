#'######################################################
#		Script Parapiqueria - FRAMEWORK
#		 	Em desevolvimento
#		Iniciado em: 05/08/2024
#		Ultima atualização: 06/08/2024
#'#####################################################

library(popbio)
library(ggplot2)
library(tidyverse)
library(openxlsx)
library(ggrepel)
library(ggridges)
library(broom)

rm(list=ls());gc()


set.seed(1)

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
# ----		ACCESSORY FUNCTION - WoodPar
# Automatically detects C, b, and non-zero elements
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
#'========================================================
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
#	  ---- RECRUITMENT
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
#   ---- RECRUITMENT ANALYSES
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
  BestModel,terms=c("t0[0:208]", #[0,208] represent the minimum and maximum individuals record in plot per month and pear stage(?)
          "Site","period"))%>%plot()


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
  select(-c(Site,year,t0y:t1y))%>%
  nest(data=c(Plot,t0,t1))%>%
  mutate(
    fit = map(data, ~ MASS::glm.nb(t1 ~ 0+t0, data = .)),
    tidy_fit = map(fit, ~ parameters::parameters(.,exponentiate = T))
  ) %>%
  unnest(tidy_fit)%>%
  mutate(VR="Recruitment")

RecruitRate_site

{RecruitRate_plot<-RecruitRate_site%>%
  ggplot(.,aes(x=VR ,y=Coefficient,group=period))+
  geom_pointrange(aes(ymin=Coefficient-SE,ymax=Coefficient+SE,shape=period),alpha=0.8,
                  position = position_dodge2(width = 0.7))+
  theme_bw(base_size=16)+
  scale_color_manual(values=c("#6e86e7","#ff9211"))+
  theme(legend.position="bottom")+
  facet_grid(.~VR)+
  ylab(NULL)+xlab(NULL)} #RecruitRate_plot

RecruitRate_plot

#'========================================================================
#	---- ESTIMATING SURVIVAL
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
## ---- ESTIMATE SURVIVAL COMPONENTS OF THE POPULATION MATRIX MODELS
#           "ModelToyv1.R" must be sourced
#'=========================================================================

#'-------------------------------------------------------------
### ADJUST PARAMETERS TO OTIMIZATION
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

# USAR LOGARÍTIMO?
# Ver em [[Implementação dos modelos inversos em Parapiqueria]]
# all_census_ts<-lapply(all_census_ts, function(df) log(df + 1))


#'------------------------------------------------------------------------
### REMOVE THOSE CENSUSES WHERE MATRICES CANNOT BE ESTIMATED  
#'------------------------------------------------------------------------
{
  # Two plots at S11B could not have demographic information estimated
  # because individuals are close to zero.
  # Plots are:
  ## Plot_19_2022 - Only adult individuals recorded?
  ## Plot_15_2024 - Only 2 individuals in  March 
  #I solved this issue by including matrix 2x2 with all elements equal to zero 
  #     instead of remove the plots
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
zero_mpm[[1]]<-matrix(rep(0.01,4),2)

mpm_list_bkp[names(mpm_list_bkp) %in% names(problems_plots)]
mpm_list_bkp[names(mpm_list_bkp) %in% names(problems_plots)]<-zero_mpm  #Include zero matrices in the list of estiamted matrices
mpm_list_bkp[names(mpm_list_bkp) %in% names(problems_plots)]

Census2022_ts$Plot_15_2022
Census2023_ts$Plot_15_2023
Census2024_ts$Plot_15_2024

# Now make mpm_list being equal to mpm_list_bkp
mpm_list <- mpm_list_bkp

# Make mpms for the full season
mpm_list<-lapply(mpm_list, function(x) x^2)


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

mean_matF<-matrix(
  c(0,0,
    exp(fixef(BestModel)$cond[[1]]),
    0),ncol=2)

# Remove valores negativos já que eles não são realistas
mpm_list<-lapply(mpm_list, function(mat) pmax(mat, 0))

mean_vrs_period<-mean(mpm_list)

mean_vrs_period

sd_vrs_period<-sqrt(var2(mpm_list))
var_vrs_period<-var2(mpm_list)

mpm_list_plusF<-mpm_list_plusF_bkp<-Map(`+`, mpm_list, list(mean_matF))


lapply(mpm_list_plusF,lambda)%>%unlist%>%mean()
lapply(mpm_list_plusF,lambda)%>%unlist%>%sd()

stoch_lamb_obs<-stoch.growth.rate(mpm_list_plusF, prob=NULL,maxt=50000,verbose=F)
stoch_lamb_obs
lapply(stoch_lamb_obs,exp)


#'=============================================================
#'  PLOTs
#'=============================================================

Vital_rates<-data.frame(do.call(rbind,lapply(mpm_list_plusF,as.vector)),var="vr")%>%rownames_to_column(var = "VAR")%>%
  as_tibble()%>%
  separate(VAR,sep="_",into = c("x", "Plot", "year"))%>%
  select(-x)%>%
  pivot_longer(X1:X4,values_to="value",names_to="vital_rate")%>%
  mutate(Plot=as.numeric(Plot),
         year=year)%>%
  mutate(Site=ifelse(Plot<11,"S11C","S11B"))


Vital_rates_df<-Vital_rates%>%
  group_by(var,vital_rate,year)%>%
  summarise(Mean=mean(value),
            SD=sd(value),
            N=n(),
            SE=SD/sqrt(N),
            CI=1.96*(SD/sqrt(N)),                   
            lower95=quantile(value,.025),          
            higher95=quantile(value,.975))

#Summarised
Vital_rates%>%
  group_by(var,vital_rate)%>%
  summarise(Mean=mean(value),
            SD=sd(value),
            N=n(),
            SE=SD/sqrt(N),
            CI=1.96*(SD/sqrt(N)),                   
            lower95=quantile(value,.025),          
            higher95=quantile(value,.975))%>%
  mutate(across(c(Mean:higher95), ~ round(.x, 3)))

lapply(mpm_list_plusF,lambda)%>%unlist%>%mean()
lapply(mpm_list_plusF,lambda)%>%unlist%>%sd()
  
Vital_rates_plot<-Vital_rates_df%>%
    mutate(VR="Survival")%>%
    mutate(stages=case_when(vital_rate=="X1"~ "Stasis[Immat]",
                            vital_rate=="X2"~ "Growth[Immat.>Adult]",
                            vital_rate=="X3"~ "Recruitment",
                            vital_rate=="X4"~ "Surv[Adult]"))%>%
    mutate(stages=forcats::fct_relevel(stages, c("Statis[Immat]","Growth[Immat.>Adult]", "Surv[Adult]","Recruitment")))%>%
    filter(vital_rate!="X3")%>%
    ggplot(.,aes(y=Mean,x=stages,group=year))+
    geom_pointrange(aes(ymin=Mean-SE,ymax=Mean+SE,shape=year),alpha=0.8,
                    position = position_dodge2(width = 0.4))+
    scale_x_discrete(labels=function(l) parse(text=l))+
    scale_color_manual(values=c("#ff9211","#6e86e7"))+
    theme_bw(base_size=16)+
    theme(legend.position="bottom")+
    facet_grid(.~VR)+
    xlab(NULL)+ylab(NULL)
  
RecruitRate_plot
Vital_rates_plot

#'=============================================================
#'  Extinction probability based on counting data
#   Based on Morris & Doak 2002
#'=============================================================
logN<-log(RepTot$t1/RepTot$t0)

mean(RepTot$t1)

abund_ext<-countCDFxt(mu=mean(logN), sig2=var(logN), nt=3, Nc=80.6, Ne=10,tmax = 10)
abund_ext

# PLOT CDF extinction rate
abund_ext%>%rownames_to_column(.,var="t")%>%
  mutate(t=as.numeric(t))%>%
  pivot_longer(!t,values_to="Rate",names_to="VAR")%>%
  ggplot(.,aes(x=t,y=Rate,group=VAR))+
  geom_line(aes(color=VAR))


#'=============================================================
#'  Extinction probability based on MPM
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Three steps must be performed first:
# 1. Create the initial number of individuals 
#       based on the mean value of immatures and reproductives
# 2 Simulate vital rates to include in the model
# 3. Create a set of matrices with these new estimated variables
#'=============================================================

#'--------------------------------------------------------------
# Initial number of individuals
#'--------------------------------------------------------------

Mean_indiv<-Census_all_max%>%
  #filter(  Remove 1% dos dados para evitar outliers
  #  value < quantile(value,.995) & 
  #  value > quantile(value,.005))%>%
  group_by(stage)%>%
  summarise(Mean=mean(value),
            Median=median(value),
            sd=sd(value))

Mean_indiv

#'--------------------------------------------------------------
# Simulate vital rates
#'--------------------------------------------------------------

BestModel_coef<-(BestModel%>%summary%>%coef)$cond%>%exp()

BestModel_coef[1,1]
BestModel_coef[1,2]

rec_sim<-
  0.1+    #A small factor has been added to make sure individuals will not recruit zero indivíduals
  rnbinom(1000, 
          mu =   BestModel_coef[1,1],
          size = sigma(BestModel) #Desviation in the model
  )

hist(rec_sim)

s1_sim<-replicate(1000,betaval(mean_vrs_period[1],var_vrs_period[1]))
s2_sim<-replicate(1000,betaval(mean_vrs_period[2],var_vrs_period[2]))
s3_sim<-replicate(1000,betaval(mean_vrs_period[4],var_vrs_period[4]))

{par(mfrow=c(2,2))
  s1_sim%>%hist(main="stasis")
  abline(v=mean_vrs_period[1],col="red")
  
  s2_sim%>%hist(main="growth")
  abline(v=mean_vrs_period[2],col="red")
  
  s3_sim%>%hist(main="survival")
  abline(v=mean_vrs_period[2],col="red")
  
  hist(rec_sim,breaks=30,main="Recrutamento",freq=F)  #Simulated recruitment
  (RepIm$t1/RepIm$t0)%>%hist(breaks=30,freq=F,add=T,col=alpha("orange",0.6))  #Observed recruitment
  abline(v=exp(parameters::parameters(BestModel)[1,2]),col="red") #Esimated recruitment
} #Plot simulated distribution



#'--------------------------------------------------------------
# Create a set of new matrices with simulated vital rates
#'--------------------------------------------------------------

stages<-c("seed","Juvenile","Adult")

# Create a more personalized life cycle for Parapiqueria
# It differs from ModelToyv1.R just because the name of the vital rates and recruitment

lifecycle_para <- expression(matrix2(c(
  s1,  recruit,
  s2, s3  ),   stages[-1] ))

vrstoch<-function(){
  vr <- list( s1=sample(s1_sim)[1],
              s2=sample(s2_sim)[1],
              s3=sample(s3_sim)[1],
              recruit=sample(rec_sim)[1])
  return(vr)
}

eval(lifecycle_para, vrstoch())

#Create the matrices
StochA_sim<-replicate(10000,(eval(lifecycle_para, vrstoch())))

StochA <- lapply(seq(dim(StochA_sim)[3]), function(i) StochA_sim[,,i])

lapply(StochA,lambda)%>%unlist%>%hist(,xlim=c(0,3))

stoch_lamb_sim<-stoch.growth.rate(StochA, prob=NULL,maxt=50000,verbose=F)

lapply(StochA,lambda)%>%unlist%>%mean
lapply(StochA,lambda)%>%unlist%>%range

stoch_lamb_obs<-stoch.growth.rate(mpm_list_plusF, prob=NULL,maxt=50000,verbose=F)
stoch_lamb_obs
lapply(stoch_lamb_obs,exp)


NLamb<-Timelags_base%>%
  filter(VAR=="MaxTot>MaxTot")%>%
  mutate(NLamb=t1/t0)%>%
  select(Site,Plot,value,t0y:NLamb)


range(NLamb$NLamb)
hist(unlist(lapply(StochA,lambda)),freq=F,col="blue",xlim=c(0,8),breaks=30)
hist(NLamb$NLamb,col=alpha("red",.6),add=T,freq=F,breaks=30)

mean(NLamb$NLamb)


lapply(stoch_lamb_sim,exp)

resultado<-stoch.quasi.ext(StochA, 
              n0 = Mean_indiv$Median[1:2], 
              Nx=Mean_indiv[3,3]/10,
              maxruns=10,
              tmax = 10, nreps=100)
resultado

matplot(resultado, xlab="Years", ylab="Probabilidade cumulativa de se extinguir", 
        type='l', lty=1, col="grey20", las=1,
        main="Probabilidade cumulativa ao longo do ano de atravessar o treshold de 10% da população inicial")

#'===============================================================
# Simulando redução nas taxas vitais
#'===============================================================

#'#######     ADAPTANDO A FUNÇÃO PARA CONTER Redução nas taxas vitais
Myviab_func<-function (matrices, n0, Nx, tmax = 50, maxruns = 10, nreps = 5000, 
                       prob = NULL, sumweight = NULL, verbose = TRUE,damage=damage) {
  damage=damage
  if (is.list(matrices)) {
    matrices <- matrix(unlist(matrices), ncol = length(matrices))
  }
  x <- length(n0)
  if (is.null(sumweight)) {
    sumweight <- rep(1, x)
  }
  y <- dim(matrices)[2]
  ext <- matrix(numeric(maxruns * tmax), ncol = maxruns)
  for (h in 1:maxruns) {
    if (verbose) {
      message("Calculating extinction probability for run ", 
              h)
    }
    prob.ext <- numeric(tmax)
    for (i in 1:nreps) {
      n <- n0
      for (t in 1:tmax) {
        col <- sample(1:y, 1, prob = prob)
        A <- matrix(matrices[, col], nrow = x)
        n <- ((A-(A*damage)) %*% n)
        N <- sum(sumweight * round(n))
        if (N < Nx) {
          prob.ext[t] <- prob.ext[t] + 1
          break
        }
      }
    }
    prob.ext <- cumsum(prob.ext/nreps)
    ext[, h] <- prob.ext
  }
  ext
}
#'###			FIM!		#####################################
#'######################################################################

sim_damage<-seq(0,0.5,by=0.05)

sim_damage

temp<-NULL
out<-NULL

tmax=11   #Tempo simulado
for(i in 1:length(sim_damage)){
temp<-Myviab_func(StochA, 
           n0 = Mean_indiv$Median[1:2], 
           Nx=Mean_indiv[3,3]/10,        #Mean_indiv[3,3]/10 representa 10% dos indivíduos médios
           maxruns=10,                   # Número de réplicas em cada simulação?
           tmax = tmax, nreps=1000,
           damage=sim_damage[i])
out[[i]]<-data.frame(MEAN=apply(temp,1,mean),
                     SD=apply(temp,1,sd),
                     Dano=sim_damage[i],
                     t=seq(1,tmax,by=1))
print(paste0("Conduzindo simulação de dano a ",sim_damage[i]*100,"%" ))
}


viab_para<-do.call(rbind,out)%>%
  mutate(PROP=100-(MEAN*100),
         ymin=100-((MEAN-SD)*100),
         ymax=100-((MEAN+SD)*100))%>%
  

viab_para[viab_para>100]<-100
viab_para[viab_para<0]<-0





###############################################
###############################################
###############################################
#
#     RESTO DE CÒDIGOS - LIMPAR!
#
###############################################
###############################################
###############################################











#Simulações para S1
    for(i in 1:length(cenariosSobrevivencia)){
      vr2<-vr
      vr2$s1<-cenariosSobrevivencia[i]
      AA <- eval(post, vr2)
      resultado2[i]<-lambda(AA)
      points(resultado2[i]~cenariosSobrevivencia[i],col=5)
      print(resultado2[i])
    }
    




rep(BestModel_coef[1,1],10)
rnbinom(1000, 
      mu =   rnorm(100,BestModel_coef[1,1],BestModel_coef[1,2]),
      size = sigma(BestModel)      #Standard error 
      )%>%hist()


summary(BestModel)
rnbinom(1000, 
        mu =   rnorm(100,0.007173 ,0.003201),
        size = sigma(BestModel)      #Standard error 
)%>%exp()%>%hist()



rec_sim<-0.1+    #A small factor has been added to make sure individuals will not recruit zero indivíduals
  rnbinom(1000, 
        mu =   BestModel_coef[1,1],
        size = sigma(BestModel) #Desviation in the model
        )


rec_sim
rec_sim

mean(mpm_list)
mean(mpm_list)^2

mean_vrs<-mean(lapply(mpm_list, function(x) x^3))

mean_vrs

sd_vrs<-sqrt(var2(lapply(mpm_list, function(x) x^3)))
var_vrs<-var2(lapply(mpm_list, function(x) x^3))

mean_vrs[1]
sd_vrs[1]


MPMtemp<-out<-poptemp<-NULL




vrstoch<-function(){
  vr <- list( s0=betaval(mean_vrs[1],var_vrs[1]),
              s1=betaval(mean_vrs[2],var_vrs[2]),
              s2=0,
              recruit=sample(rec_sim)[1])
  return(vr)
}

stages<-c("seed","Juvenile","Adult")

lifecycle_para <- expression(matrix2(c(
  s0,  recruit,
  s1, s2  ),   stages[-1] ))

StochA<-replicate(1000,(eval(lifecycle_para, vrstoch())))

StochA
lapply(StochA,lambda)%>%unlist%>%hist(,xlim=c(0,1.5))

stoch.quasi.ext(
  StochA,
  n0,
  Nx,
  tmax = 50,
  maxruns = 10,
  nreps = 5000,
  prob = NULL,
  sumweight = NULL,
  verbose = TRUE
)
