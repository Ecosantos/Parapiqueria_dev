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


do.call(rbind,out)
viab_para<-do.call(rbind,out)%>%
  mutate(PROP=100-(MEAN*100),
         ymin=100-((MEAN-SD)*100),
         ymax=100-((MEAN+SD)*100))
  
  viab_para[viab_para>100]<-100
viab_para[viab_para<0]<-0



viab_para%>%
  #  mutate(dano=as.factor(dano))%>%
  ggplot(.,aes(x=t-1,y=PROP,group=Dano))+
  geom_line(aes(color=Dano*100))+
  geom_pointrange(aes(ymin=ymin, ymax=ymax,color=Dano*100))+
  #  viridis::scale_color_viridis(option="magma")+
  scale_color_stepsn(
    #    colours = c("red", "yellow", "green", "yellow", "red"),
    # colours = viridis::viridis(n = 7, option = "inferno",direction=-1) ,
    colours = rev(viridis::viridis(n = 5, option = "magma", direction = -1)))+ 
  theme_classic(base_size=16)+
  scale_x_continuous(breaks=seq(0,10,by=1),expand = c(0, 0))+
  scale_y_continuous(labels = scales::percent_format(scale = 1),
                     expand = c(0, 0),
                     breaks=c(0,10,seq(50,200,by=50)))+
  geom_hline(yintercept=10,color="tomato",linetype="dashed")+
  labs(color="Impacto/Redução (%)",
       y="Projeção populacional",
       x="Anos projetados")+
  theme(legend.position="bottom")



viab_para
# Script temporário 1
# Inclui pedaços do script Viabilidade populacional de Ipomoea que haviam sido removidos para Parapiqueria. 
# Acho que tudo já foi incluido no script principal e pode ser removido nos próximos commits

tabela_extincao <- do.call(rbind, lapply(seq_along(res), function(i) {
  data.frame(
    dano = res[[i]]$dano,
    firstextinct = res[[i]]$firstextinct,
    extinct_many = res[[i]]$extinct_many,
    lambdas=res[[i]]$lambda
  )
}))


res2<-NULL
for(i in 1:length(sim_damage)){
  res[[i]] <- list()
  res[[i]]$projpop<-pop.projection((AA-(AA*im_damage[i])),stable.stage(AA),iterations=11)$pop.sizes
  res[[i]]$extinctpop<- res[[i]]$projpop<.1
  res[[i]]$firstextinct<-which(res[[i]]$extinctpop == TRUE)[1]
  res[[i]]$extinct_many<-length(which(res[[i]]$extinctpop == TRUE))  
  res[[i]]$lambda<-lambda(AA-(AA*dano[i]))
  res[[i]]$dano<-dano[i]
}







Myviab_func(StochA, 
            n0 = Mean_indiv$Median[1:2], 
            Nx=Mean_indiv[3,3]/10,        #Mean_indiv[3,3]/10 representa 10% dos indivíduos médios
            maxruns=10,                   # Número de réplicas em cada simulação?
            tmax = tmax, nreps=1000,
            damage=sim_damage[i])