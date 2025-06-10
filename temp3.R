library(popbio)
## list nonzero elements
nonzero <- c( 1, 2, 5, 6, 7, 9)
## create C matrix
C <- rbind(diag(-1,6), c(1,1,0,0,0,0), c(0,0,1,1,0,0), c(0,0,0,0,0,1))
C
## calculate b (transpose is not necessary - either way works)
b <- apply(C, 1, max)
b
nematode
QPmat(nematode, C,b,nonzero)

# Matriz A2
vr <- list(seed_surv=0.1 , s0=0.3,s1=0.7, s2=0,f2=0)
pre <- expression(matrix2(c(
  s0,  f2*seed_surv,
  s1, s2  ),   stages[-1] ))


stages<-c("seed","Juvenile","Adult")

A2 <- eval(pre, vr)
A2 


# Matriz A3
vr <- list(seed_surv=0.1 , s0=0.3,s1=0.7, s2=0.5,f2=0)
pre <- expression(matrix2(c(
  s0,  f2*seed_surv,
  s1, s2  ),   stages[-1] ))


stages<-c("seed","Juvenile","Adult")

A3 <- eval(pre, vr)
A3 


# Matriz A3
A3nonzero <- c(1, 2, 4)
A3C <- rbind(
  diag(-1, 4),                  # -x1, -x2, -x3, -x4 ≤ 0
  c(1, 0, 1, 0),                # x1 + x3 ≤ 1 (coluna Juvenile)
  c(0, 1, 0, 1)                 # x2 + x4 ≤ 1 (coluna Adult)
)
A3b <- c(0, 0, 0, 0, 1, 1)



# Matriz A2
C_full <- rbind(
  diag(-1, 4),             # -x1, -x2, -x3, -x4 ≤ 0
  c(1, 0, 1, 0),           # x1 + x3 ≤ 1
  c(0, 1, 0, 1)            # x2 + x4 ≤ 1
)

# 3. Extraia apenas as colunas referentes a `nonzero`
C_reduced <- C_full[, nonzero]  # 6x2

# Passo 2: Extrair colunas correspondentes às variáveis livres
A2nonzero <- c(1, 2)
A2C <- C_full[, A2nonzero]

# Passo 3: b continua igual
A2b <- c(0, 0, 0, 0, 1, 1)

all_census_ts[[1]]
QPmat(all_census_ts[[1]]%>%as.matrix(),
      C=C_reduced,
      b=A2b,
      nonzero=A3nonzero)


QPmat(all_census_ts[[1]]%>%as.matrix(),
      C=C_full,
      b=A3b,
      nonzero=A3nonzero)

all_census_ts[[1]]%>%as.matrix()
serie.ven[,c(1:3)]

A2
popsim.ven<-pop.projection(A2,rep(1,ncol(A2)),1000)


serie.ven[,c(1:3)]
QPmat(serie.ven[,c(1:3)], C_reduced,A2b,A2nonzero)

all_census_ts[[1]]%>%as.matrix()
QPmat(all_census_ts[[1]]%>%as.matrix(), C_reduced,A2b,A2nonzero)



popsim.ven

A2R<-A2 + matrix(c(0,0,1.02,0),ncol=2)
lambda(A2R)

pop.projection(A2 + matrix(c(0,0,1.02,0),ncol=2),
               n=c(5,5), iterations = 30)$stage.vectors%>%
  stage.vector.plot()


A3

