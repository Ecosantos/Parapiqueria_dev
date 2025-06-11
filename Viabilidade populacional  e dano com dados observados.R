mpm_list_recruit_full


temp<-NULL
out<-NULL

tmax=11   #Years simulated (from t0 to t10)
for(i in 1:length(sim_damage)){
  temp<-Myviab_func(mpm_list_recruit_full, 
                    n0 = Mean_indiv$Median[1:2], 
                    Nx=Mean_indiv[3,3]/10,        #Mean_indiv[3,3]/10 representa 10% dos indivíduos médios
                    maxruns=10,                   # Número de réplicas em cada simulação?
                    tmax = tmax, nreps=100,
                    damage=sim_damage[i])
  out[[i]]<-data.frame(Extinct.rate=apply(temp,1,mean),
                       SD=apply(temp,1,sd),
                       Dano=sim_damage[i],
                       t=seq(1,tmax,by=1))
  print(paste0("Simulating damage at ",sim_damage[i]*100,"%" ))
}


viab_para<-do.call(rbind,out)%>%
  mutate(Persistprop=100-(Extinct.rate*100),
         ymin=100-((Extinct.rate-SD)*100),
         ymax=100-((Extinct.rate+SD)*100))

viab_para[viab_para>100]<-100
viab_para[viab_para<0]<-0


viab_para%>%filter(t==10)


viab_para%>%
  #  mutate(dano=as.factor(dano))%>%
  ggplot(.,aes(x=t-1,y=Persistprop,group=Dano))+
  geom_line(aes(color=Dano*100))+
  geom_pointrange(aes(ymin=ymin, ymax=ymax,color=Dano*100))+
  #  viridis::scale_color_viridis(option="magma")+
  scale_color_stepsn(
    #    colours = c("red", "yellow", "green", "yellow", "red"),
    # colours = viridis::viridis(n = 7, option = "inferno",direction=-1) ,
    colours = rev(viridis::viridis(n = 5, option = "magma", direction = -1)))+ 
  theme_classic(base_size=24)+
  scale_x_continuous(breaks=seq(0,10,by=1),expand = c(0, 0))+
  scale_y_continuous(labels = scales::percent_format(scale = 1),
                     expand = c(0, 0),
                     breaks=c(0,10,seq(50,200,by=50)))+
  geom_hline(yintercept=10,color="tomato",linetype="dashed")+
  labs(color="Impacto/Redução (%)",
       y="Projeção populacional",
       x="Anos projetados")+
  theme(legend.position="right")