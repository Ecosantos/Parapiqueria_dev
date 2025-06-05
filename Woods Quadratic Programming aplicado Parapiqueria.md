> [!info] **Compilação pessoal de informações**
> - **Temática**: XXXXXXXXXXXXXXXX
> - **Tags:** #ITV #Projeto/Parapiqueria 

- Buscar scripts em C:\Artigos e resumos publicados submetidos ideias\Notes\9 - ITV\Dados-scripts-ITV\Parapiqueria - Dados e scripts

Ver artigo: https://www.researchgate.net/publication/370521978_Using_mechanistic_models_to_assess_temporary_closure_strategies_for_small_scale_fisheries


>[! grid] Framework Demografia Parapiqueria
>![[Demografia de Parapiqueria.excalidraw|Framework Demografia Parapiqueria]]
>![[Problemas inversos aplicados a parapiqueria.excalidraw|TESTE]]



# Recursos para serem trabalhados com mais calma

#### Função para projetar a população até a assíntota
- Retorna tempo estimado para alcançar assíntota
- Retorna população projetada até a assíntota
```R
parametros<-function(A,tol=3){
# tol define a tolerância para considerar que chegou-se à assintota
# A é o MPM
serie<-NULL
x<-n<-n0<-rep(100,ncol(A))					#Começa com 100 individuos em cada classe etária
i<-1
while (all(x==round(stable.stage(A),tol))!=TRUE){	#Para de projetar a população quando encontra a assíntota 
i=i+1
n<-A%*%n
x<-round(prop.table(n),tol)
print(i)
}
serie<-pop.projection(A,n0,19)$stage.vector[,1:19]
resultado<-list(n.stable=x,time.serie=serie,duration=i)
out<-NULL
out[[1]]<-serie
out[[2]]<-resultado
return(out)
}


```