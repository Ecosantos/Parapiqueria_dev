# Script temporário 2
# Inclui pedaços do script Viabilidade populacional de Ipomoea que haviam sido removidos para Parapiqueria. 
# Acho que tudo já foi incluido no script principal e pode ser removido nos próximos commits

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
##'-------------------------------------------------------------------------

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



rbind(
  #Matrix de taxas vitais
  data.frame(do.call(rbind,lapply(mpm_list_recruit_full,as.vector)),var="vr")%>%rownames_to_column(var = "VAR"),
  # Matrix de sensitivities
  #data.frame(do.call(rbind,lapply(mpm_list_sens,as.vector)),var="sensitivity")%>%rownames_to_column(var = "VAR")
  #Matrix de elasticities
#  data.frame(do.call(rbind,lapply(mpm_list_elas,as.vector)),var="elasticity")%>%rownames_to_column(var = "VAR"))%>%
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



  data.frame(do.call(rbind,lapply(mpm_list,as.vector)),var="vr")%>%
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



