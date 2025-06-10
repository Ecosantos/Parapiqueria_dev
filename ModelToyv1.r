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

vr <- list(seed_surv=0.1 , s0=0.3,s1=0.7, s2=0,f2=0)
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

replacing(A2)

popsim.ven<-pop.projection(A2,rep(1,ncol(A2)),1000)
serie.ven<-popsim.ven$stage.vector[,1:100]


WoodPar(A2)

QPmat(serie.ven, WoodPar(A2)$C,WoodPar(A2)$b,WoodPar(A2)$nonzero)

#====================================================================
#		CHECK ACCURACY
#====================================================================

parametros<-function(A,tol=3){
# tol define a tolerância para considerar que chegou-se à assintota
# A é o MPM
serie<-NULL
x<-n<-n0<-rep(100,ncol(A))#Começa com 100 individuos em cada classe etária
i<-1
while (all(x==round(stable.stage(A),tol))!=TRUE){#Para de projetar a população quando encontra a assíntota 
i=i+1
n<-A%*%n
x<-round(prop.table(n),tol)
print(i)
}
return(i)}


#limite = Tolerância/Pop alcança estabilidade	## A partir da função parametros
Intervalos=c(2,8)		#Replica o número de censos necessários em uma série temporal

limite<-parametros(A2,tol=2)


serie.ven[,limite]

serie.ven[,c(2:9)]

QPmat(serie.ven[,c(1:3)], WoodPar(A2)$C,WoodPar(A2)$b,WoodPar(A2)$nonzero)

MPMs<-NULL


for(i in min(Intervalos):max(Intervalos)){
  MPMs[[i]] <- list() 
	for(j in 1:(limite-max(Intervalos))){
		MPMs[[i]][[j]]<-QPmat(serie.ven[,c(j:(j+i))], WoodPar(A2)$C,WoodPar(A2)$b,WoodPar(A2)$nonzero)
	print(paste(i,j))
	}
print(serie.ven[,c(j:(j+i))])
	}

#Remove valores nulos. No caso os MPMs construidos com intervalo = 1 (dois censos), que não existem e não podem ser computados
MPMs<-MPMs[unlist(lapply(MPMs,is.null))==FALSE]


VRs_dif<-MPMs_dif<-list()
for(i in 1:length(MPMs)){
MPMs_dif[[i]]<-Map(`-`, MPMs[[i]], list(A2))
VRs_dif[[i]]<-lapply(MPMs_dif[[i]],as.vector)%>%do.call(rbind,.)%>%data.frame(.,census=paste(i+2))}

library(scales)

VRs_dif[[1]]%>%glimpse()

apply(VRs_dif[[1]][,-5],2,mean)
apply(VRs_dif[[1]][,-5],2,sd)


scientific_10 <- function(x) {
  parse(text=gsub("e", " %*% 10^", scales::scientific_format()(x)))
}

VRs_dif_plot<-VRs_dif%>%do.call(rbind,.)%>%
pivot_longer(!census,names_to="VR",values_to="Values")%>%
group_by(census,VR)%>%
summarise(Mean=mean(Values),
		SD=sd(Values))%>%
mutate(stages=case_when(VR=="X1"~ "Stasis[Immat]",
				VR=="X2"~ "Growth[Immat.>Adult]",
				VR=="X3"~ "Recruitment",
				VR=="X4"~ "Surv[Adult]"))%>%
mutate(stages=forcats::fct_relevel(stages, c("Statis[Immat]","Growth[Immat.>Adult]", "Surv[Adult]","Recruitment")))%>%
filter(VR!="X3")


VRs_dif_plot%>%
ggplot(.,aes(x=census,y=Mean,group=census))+
geom_pointrange(aes(ymin=Mean-SD,ymax=Mean+SD,group=census,color=census),
		position = position_dodge2(width = 0.7))+
ylab("Error")+xlab("# censuses")+
scale_y_continuous(label=scientific_10)+
scale_color_manual(values=c(
'#ff6347', '#6e8ab6', '#5b7bb3', '#496daf', '#375fa9', '#2350a4', '#00429d'))+
geom_hline(yintercept=0,color="red",linetype=2)+
scale_x_discrete(labels=function(l) parse(text=l))+
theme_minimal(base_size=16)+
facet_wrap(.~stages,scales="free",ncol=3)