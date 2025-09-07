#'--------------------------------------------------------------------------------
#		EXAMPLE 
# Make the periodic matrix structure logical
#'--------------------------------------------------------------------------------
# Possible transitions between months
## Seeds -> seeds		# seed_surv = 0.1 just as example
## Juvenile -> Juvenile	# s0=0.3 just as example
## Juvenile -> Adults	# s1=0.7 just as example
## Adults -> Adults	# s2=0.3 just as example
## Recruitment		# f2= No reproduction between months of the same season
#'--------------------------------------------------------------------------------

require (popbio)
require(tidyverse)
#'===========================================================
# Determine parameters and timeseries ----
#'===========================================================

# Create two matrices
vr <- list(seed_surv=0.1 , s0=0.3,s1=0.7, s2=0.3,f2=0)
vr2 <- list(seed_surv=0.1 , s0=0.3,s1=0.7, s2=0,f2=0) #Force S2 adult survival to zero
stages<-c("seed","Juvenile","Adult")

pre <- expression(matrix2(c(
  s0,  f2*seed_surv,
  s1, s2  ),   stages[-1] ))

A2 <- eval(pre, vr)
A3 <- eval(pre, vr2)

A2
A3

# Create timeseries
popsim.venA2<-pop.projection(A2,rep(1,ncol(A2)),1000)
popsim.venA3<-pop.projection(A3,rep(1,ncol(A3)),1000)
serie.venA2<-popsim.venA2$stage.vector[,1:100]
serie.venA3<-popsim.venA3$stage.vector[,1:100]


A3
pop.projection(A3,rep(1,ncol(A2)),2)
pop.projection(A2,rep(1,ncol(A2)),2)

#'====================================================================
#		Deine constraints matrix and QP components ----
#'====================================================================

# Define non-zero elements
nonzeroA2<-which(A2> 0); nonzeroA2
nonzeroA3<-which(A3> 0); nonzeroA3



# Constraints for A2 (a11, a21, a22)
C_A2 <- rbind(
  c(-1,  0,  0),  # -a11 ≤ 0
  c( 0, -1,  0),  # -a21 ≤ 0
  c( 0,  0, -1),  # -a22 ≤ 0
  c( 1,  1,  0),  # a11 + a21 ≤ 1
  c( 0,  0,  1)   # a22 ≤ 1
)

bA2 <- c(0, 0, 0, 1, 1)

# Constraints for A3 (a11 and a21)
C_A3 <- rbind(
  c(-1,  0),  # -a11 ≤ 0
  c( 0, -1),  # -a21 ≤ 0
  c( 1,  1)   # a11 + a21 ≤ 1
)
bA3 <- c(0, 0, 1)


A3
A2

# Usando com QPmat
QPmat(serie.venA2[, 1:3], C = C_A2, b = bA2, nonzero = nonzeroA2)
QPmat(serie.venA3[, 1:3], C = C_A3, b = bA3, nonzero = nonzeroA3)

#'====================================================================
#		Check accuracy ----
#'====================================================================
# Stable_growth function assess when population reach stable growth rate
# It is necessary because Quadratic programming cannot estimate parameters from populations in equilibrium/stable

###		Simulate timeseries ----

stable_growth<-function(A,tol=3){
  # A is the MPM
  # tol defines the tolerance to to reach the asymptotic growth rate
  serie<-NULL
  x<-n<-n0<-rep(100,ncol(A))#stats with 100 individuals each stage
  i<-1
  while (all(x==round(stable.stage(A),tol))!=TRUE){#Project population until it reach the stable growth rate 
    i=i+1
    n<-A%*%n
    x<-round(prop.table(n),tol)
    print(i)
  }
  return(i)}


### Define time to to reach stability -----

limitA2<-stable_growth(A2,tol=2)
limitA3<-stable_growth(A3,tol=2)   #In this case, population reach stability in two census, 
#so, census 1 must be included to make sure populations
#are not stable along the estimation of demographic parameters
limitA3<-15 # Two years is too little to any estimation. 
# Here we need to force the limit to overcome the interval used in our model

# Check population structure when reach stability
serie.venA2[,limitA2]
serie.venA3[,limitA3]


# Define the number of intervals considered in the parameter estimation
Interval=c(2,8)		



#'========================================================================
## Check accuracy under different timeseries length ----
# Estimate demographic parameter with different time series length
# time series length is defined by Interval
#'========================================================================

# Estimation for A2
MPMsA2<-NULL
for(i in min(Interval):max(Interval)){
  MPMsA2[[i]] <- list() 
  for(j in 1:(limitA2-max(Interval))){
    MPMsA2[[i]][[j]]<-QPmat(serie.venA2[,c(j:(j+i))], C_A2,bA2,nonzeroA2)
    print(paste(i,j))
  }
  print(serie.venA2[,c(j:(j+i))])
}
MPMsA2[[3]]

# Estimation for A3
MPMsA3<-NULL
for(i in min(Interval):max(Interval)){
  MPMsA3[[i]] <- list() 
  for(j in 1:(limitA3-max(Interval))){
    MPMsA3[[i]][[j]]<-QPmat(serie.venA3[,c(j:(j+i))], C_A3,bA3,nonzeroA3)
    print(paste(i,j))
  }
  print(serie.venA3[,c(j:(j+i))])
}

MPMsA3


# Remove null values.
# Null values appears for intervals = 1 (two censuses). 
# They are expected to not exist
MPMsA2<-MPMsA2[unlist(lapply(MPMsA2,is.null))==FALSE]
MPMsA3<-MPMsA3[unlist(lapply(MPMsA3,is.null))==FALSE]



#'=========================================================================
## Check differences (accuracy)  ----
# Check differences between estimated and known values
#'=========================================================================
library(scales)


# Accessory function to facilite reading too small values
scientific_10 <- function(x) {
  parse(text=gsub("e", " %*% 10^", scales::scientific_format()(x)))
}



# Defina a função para calcular a diferença relativa
calculate_relative_difference <- function(estimated_matrix, expected_matrix) {
  # (estimado - esperado) / estimado
  # Atenção: Se algum elemento em estimated_matrix for zero,
  # o resultado para aquele elemento será Inf, -Inf ou NaN,
  # o que é matematicamente correto para esta definição de erro relativo.
  (estimated_matrix - expected_matrix) / expected_matrix
}



## A2
VRs_difA2<-MPMs_difA2<-list()
for(i in 1:length(MPMsA2)){
  # Aplica a função personalizada usando Map
  # estimated_matrix virá de MPMsA2[[i]] (cada matriz estimada)
  # expected_matrix virá de A2 (a matriz de referência)
  MPMs_difA2[[i]] <- Map(calculate_relative_difference, MPMsA2[[i]], list(A2))

  # O restante do seu código para organizar em um data frame permanece o mesmo
  VRs_difA2[[i]] <- lapply(MPMs_difA2[[i]], as.vector) %>%
    do.call(rbind,.)%>%data.frame(.,census=paste(i+2))
}


apply(VRs_difA2[[1]][,-5],2,mean)
apply(VRs_difA2[[1]][,-5],2,sd)


## A3
VRs_difA3<-MPMs_difA3<-list()
for(i in 1:length(MPMsA3)){
  # Aplica a função personalizada usando Map
  # estimated_matrix virá de MPMsA2[[i]] (cada matriz estimada)
  # expected_matrix virá de A2 (a matriz de referência)
  MPMs_difA3[[i]] <- Map(calculate_relative_difference, MPMsA3[[i]], list(A3))

  # O restante do seu código para organizar em um data frame permanece o mesmo
  VRs_difA3[[i]] <- lapply(MPMs_difA3[[i]], as.vector) %>%
    do.call(rbind,.)%>%data.frame(.,census=paste(i+2))
}

apply(VRs_difA3[[1]][,-5],2,mean)
apply(VRs_difA3[[1]][,-5],2,sd)

# Accessory function to facilite reading too small values
scientific_10 <- function(x) {
  parse(text=gsub("e", " %*% 10^", scales::scientific_format()(x)))
}



VRs_dif_df<-rbind(
  data.frame(do.call(rbind,VRs_difA2),matrix="A2"),
  data.frame(do.call(rbind,VRs_difA3),matrix="A3"))%>%
  pivot_longer(!c(census,matrix),names_to="VR",values_to="Values")%>%
  group_by(census,VR,matrix)%>%
  summarise(Mean=mean(Values),
            SD=sd(Values))%>%
  mutate(stages=case_when(VR=="X1"~ "Stasis[Immat]",
                          VR=="X2"~ "Growth[Immat.>Adult]",
                          VR=="X3"~ "Recruitment",
                          VR=="X4"~ "Surv[Adult]"))%>%
  mutate(stages=forcats::fct_relevel(stages, c("Statis[Immat]","Growth[Immat.>Adult]", "Surv[Adult]","Recruitment")))%>%
  filter(VR!="X4" | matrix!="A3" )%>%# This represent S2,2 and is zero, so there is no error here
  filter(VR!="X3")  

VRs_dif_df

A3


VRs_dif_df%>%
filter(matrix=="A3")%>%
  mutate(stages = case_when(
    stages == "Stasis[Immat]" ~ "Stasis~(S[\"1,1\"])",
    stages == "Growth[Immat.>Adult]" ~ "Maturation~(M[\"2,1\"])"
  )) %>%
  # opcional: reordenar os níveis se necessário
  mutate(stages = forcats::fct_relevel(
    stages,
    c("Stasis~(S[\"1,1\"])", "Maturation~(M[\"2,1\"])")
  ))%>%
  ggplot(.,aes(x=census,y=Mean,group=census))+
  geom_pointrange(aes(ymin=Mean-SD,ymax=Mean+SD,
                      group=census,color=census,shape=matrix),
                  position = position_dodge2(width = 0.7),size=1.05)+
  ylab("Error")+xlab("Number of censuses")+
    scale_y_continuous(label=scientific_10,
      limits=c(-1*10^-15,1*10^-15))+
  scale_color_manual(values=c(
    # Colour palette option 1'#ff6347', '#6e8ab6', '#5b7bb3', '#496daf', '#375fa9', '#2350a4', '#00429d'
    # Colour palette option 2'#FFD700', '#d9f0d3', '#a6dba0', '#5aae61', '#1b7837', '#006837', '#004529'
    '#000000', '#484848', '#595959', '#6a6a6a', '#7a7a7a', '#8c8c8c', '#9e9e9e'
    ))+
  geom_hline(yintercept=0,color="tomato",linetype=2)+
  scale_x_discrete(labels=function(l) parse(text=l))+
  theme_minimal(base_size=16)+
  facet_wrap(.~stages, scale="free",labeller = label_parsed,ncol=2)


ggsave(
  filename = "Figures/Raw figures/figura 4.pdf",
  width = 24,               
  height = 10,              
  units = "cm",
  dpi = 300                 
)


