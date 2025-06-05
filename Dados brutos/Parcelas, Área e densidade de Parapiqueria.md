>[! hint| right]
![[Parapiqueria_IMG_2024MAR14_103434.jpg|250x200]]

> [!info] **Compilação pessoal de informações**
> - **Temática**: Delineamento amostral de Parapiqueria; Estimativa de densidade por metro quadrado
> - **Tags:** #Projetos/ITV/Parapiqueria   
> - **Ver também:**
> 	- [[Demografia de Parapiqueria - geral overview]]
> 	- [[Recrutamento em Parapiqueria - Notas]]
> 	- [[Taxa de crescimento populacional Parapiqueria]]
> 	- 

# PARCELAS: 
- **Plots 1-10** = S11C = Riacho 1 
- **Plots 11-20** = S11B = Riacho 2

# Informações de amostragem de Parapiqueria


- Parcelas de 25x25 cm
	- Totalizando 0,0625m² [^2] [^1]
		- Densidades geralmente superiores a 1000 indivíduos/m²

>[! Column|bg-green]- Exemplo de densidade por m² - 
>>[! hint] Informações e Metadados
>> No exemplo a seguir estão representadas as abundâncias calculadas em cada plot no ano de 2023 para o córrego do corpo de canga S11C. 
>> - MaxIm = Número máximo de Imaturos
>> - MaxRep= Número máximo de Reprodutivos
>>- Site = Córrego amostrado
>> - MaxTot = Máximo númeri de individuos [max(Imaturos ou Reprodutivos)]
>> - Dens_m2 = Densidade de individuos por parcela 
>> 	- Calculado como MaxTot $\times$ 16
>
>>[! hint] Exemplo considerando apenas as parcelas do córrego S11C amostradas no ano de 2023
>>
>> | Plot | MaxIm | MaxRep | Site | MaxTot | Dens_m2 |
>> | ---- | ----- | ------ | ---- | ------ | ------- |
>> | 1    | 46    | 47     | S11C | 47     | 752     |
>> | 2    | 36    | 71     | S11C | 71     | 1136    |
>> | 3    | 147   | 38     | S11C | 147    | 2352    |
>> | 4    | 103   | 72     | S11C | 103    | 1648    |
>> | 5    | 83    | 72     | S11C | 83     | 1328    |
>> | 6    | 80    | 57     | S11C | 80     | 1280    |
>> | 7    | 139   | 64     | S11C | 139    | 2224    |
>> | 8    | 118   | 73     | S11C | 118    | 1888    |
>> | 9    | 137   | 83     | S11C | 137    | 2192    |
>> | 10   | 231   | 137    | S11C | 231    | 3696    |
> 

## Coordenadas
- Coordenadas em kml.
	- Esse é o formato necessário para abrir com o pacote sf. Ver em: [[Shapefiles no R]]
![[Parapiqueria S11B - Parcelas demografia - 26maio2024.kml]]
![[Parapiqueria S11C- Parcelas demografia - 26maio2024.kml]]

==POR ALGUM MOTIVO ESTÁ FALTANDO AS COORDENADAS DOS PLOTS 07 e 10.== ^813e0e
- [ ] Atualizar as coordenadas das parcelas de Parapiqueria

- [ ] **No plot 8 e no 9, uma das quinas do durepox soltou e saiu totalmente do lugar. Não conseguimos descobrir onde era. Fizemos uma contagem mas deve estar errada!  eu acho que teremos q desencanar desses dois plots esse ano. Ano que vem a gente fixa a quina novamente com durepox e start over! (Mensagem da Talita em Abril/2025)**
# Padrões de abundância
- S11C mais abundante que S11B
	- Principalmente por conta de indivíduos imaturos
		- Número de indivíduos varia muito de ano pra ano. 
			- Curiosamente a diferença ao longo dos anos parece ser maior para reprodutivos

## Abundância difere entre os riachos e ao longo do ano? *Provavelmente não*
>[! info|right wsmed]
![[Pasted image 20240806213957.png|500]]
- Inicialmente havia uma discussão de que a abundância entre os riachos diferia.
- De fato parece haver uma certa diferenciação onde S11C $>$ S11B
	- Essa diferença é principalmente devido aos indivíduos imaturos ($\beta_{S11C} = 0.51 \pm 0.23$; p-valor = 0.027)[^3]
	- Sendo *bem menor e não significativa* em indivíduos reprodutivos ($\beta_{S11C} = 0.06 \pm 0.19$; p-valor = 0.74)[^3]
- Também parece haver uma certa redução no número de indivíduos ao longo do ano [^4]
	- Porém apenas para indivíduos reprodutivos  $Rep_{\beta_{year}} = -0.31 \pm 0.11$ (p-valor= 0.005).[^5]
- Quando incluímos `year x site` no glm a única relação que permanece é entre indivíduos reprodutivos e ano, sem diferença entre as parcelas. 
- **OBS:***Todos os glms aqui são com binomial negativa utilizando o pacote MASS*
	- A performance do glm.nb é bem superior que poisson. Ver figuras abaixo. Ainda que também não seja excelente
- Código disponível em [[#Diferenças entre anos e riachos com glms]]
- Uma possível explicação para os indivíduos reprodutivos terem apresentado uma relação negativa com o ano é que em 2024 talvez ainda tivesse muitos indivíduos jovens[^6]. 
	- No futuro eu poderia tentar adicionar algumas análises considerando a proporção $\frac{\text{Individuos reprodutivos}}{\text{Indivíduos total}}$ . Essa é uma pergunta interessante que falaria sobre a fenologia dessa espécie.

>[! grid]
>![[Pasted image 20240806221944.png|glm.nb(abundância ~ year*Site)]]
>![[Pasted image 20240806221903.png|glm.nb(abundância ~ year)]]
>![[Pasted image 20240806221757.png|glm.nb(abundância ~ Site)]]
>![[Pasted image 20240806221240.png|check_model() de glm utilizando distribuição poisson]]
>![[Pasted image 20240806221533.png|check_model() utilizando glm com distribuição binomial negativa]]

## Abundância ao longo do tempo entre riachos

Código disponível em: [[#Plots de abundância ao longo do tempo entre riachos]]
>[! column] Abundância de Parapiqueria calvancantei ao longo do tempo x riachos
>>As figuras A e B estão representando o total de indivíduos em cada mês, para cada parcela, em cada riacho, ao longo de cada ano. A diferença entre A e B é que em B são apresentados todos os censos de maneira continuada enquanto em A realizei a quebra dos censos ao longo dos anos. Por fim, C e D mostram a abundância de indivíduos por riacho, a ideia era demonstrar que o riacho S11C tem maior abundância, algo que é sugerido pelo gráfico D para imaturos (laranja bem acima para imaturos) mas é bem difícil de ver. *Por outro lado, no gráfico C acabou evidenciando uma possível diminuição de indivíduos nas parcelas ao longo dos anos amostrados*. Essa diferença no entanto não é robusta e já foi discutida acima. 
>
>>![[Pasted image 20240806151156.png| Plot de séries temporais focado em meses amostrados]]


## Diversidade genética
- [[Diversidade genética]] estimada pela Bárbara
	- Ver artigo em: [[Barbara Leal et al - Genetica de plantas criticas.docx]]
	- Utilizou [[Single Nucleotide Polymorphism|SNPs]] obtidos a partir do método de representatividade reduzida[^9] 
- Resultados
	- Há três clusters genético para _P. cavalcantei_ representando cada riacho amostrado. [^7]
		- Essa diferenciação por exemplo não existe em Carajasia e Ipomoea
	- Ainda que haja três clusters, os valores de [[Diversidade genética|FST]] (?) indicam uma baixa variação entre as populações, sugerindo que seriam a mesma população geneticamente falando.
	- Parapiqueria apresenta uma diversidade genética intermediária comparada a outras espécies
		- Ipomoea cavalcantei ($H_o$ = 0.150)>> Parapiqueria cavalcantei ($H_o$ = 0.092) >>> Carajasia ($H_o$ = 0.004)
	- Comparação entre riachos. Interpretação pessoal
		- Diversidade genética não é significativamente diferente entre riachos.
			- Como é possível concluir comparando os valores médios e os intervalos de confiança de $AR$, $He$ e $Ho$
			- >[! info]- Conclusões que eu havia feito assumindo que os valores eram diferentes (não havia prestado atenção no intervalo de confiança)
			 > Riacho S11C tem a maior diversidade genética ($AR$ e $Ho$). 
			  >- No entanto, a heterozigosidade esperada ($H_E$) é menor em comparação com os outros riachos, sugerindo que, embora a diversidade observada seja mais alta, a diversidade genética total prevista não é tão alta. Isso pode indicar um desequilíbrio nas frequências alélicas ou uma possível dominância de alguns alelos.


Tabela 1. Diversidade genética de Parapiqueria cavalcantei _based on the set of SNPs obtained through reduced represent methods_. N = número de indivíduos amostrados, N mean locus = mean number of individuals per locus, AR = allelic richness, A% = percentage of total alleles observed across loci, HO = observed heterozygosity, HE = expected heterozygosity, FIS = inbreeding coefficient, NE = effective population size. Numbers within parentheses indicate confidence intervals.

|       **Species/Locality**       | **N**  | **N mean locus** |         **AR**          |  **A%**   |         **HO**          |         **HE**          |         **FIS**         | **FST** |      **NE**      |
| :------------------------------: | :----: | :--------------: | :---------------------: | :-------: | :---------------------: | :---------------------: | :---------------------: | :-----: | :--------------: |
| Parapiqueria cavalcantei (TOTAL) |   38   |      31.18       |            -            |     -     |   0.092 (0.083-0.108)   |   0.161 (0.148-0.188)   |   0.428 (0.357-0.506)   | -0.014  | 18.7 (13.5-27.4) |
|        Stream R1 (=S11C)         |   20   |       16.6       |   1.440 (1.343-1.534)   |   89.29   |   0.092 (0.081-0.111)   |   0.153 (0.132-0.182)   |   0.327 (0.378-0.472)   |    -    |        -         |
|        Stream R2 (=S11B)         |   7    |       5.55       |   1.390 (1.257-1.467)   |   75.00   |   0.086 (0.071-0.104)   |   0.155 (0.134-0.184)   |   0.403 (0.442-0.615)   |    -    |        -         |
|          ~~Stream R3~~           | ~~11~~ |     ~~9.03~~     | ~~1.440 (1.305-1.524)~~ | ~~82.14~~ | ~~0.099 (0.083-0.119)~~ | ~~0.158 (0.141-0.185)~~ | ~~0.339 (0.337-0.492)~~ |  ~~-~~  |      ~~-~~       |

## Visualização extra

|            Visualização extra             |                                                                                                                                     Gráfico/Contexto                                                                                                                                     |
| :---------------------------------------: | :--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------: |
| ![[Pasted image 20240807103356.png\|200]] | Density plot de abundâncias ao longo do ano em cada riacho. Inicialmente foi discutido uma possível alternância na abundância de Parapiqueria. Algo como: no ano 2022 era maior no riacho S11B e no ano de 2023 maior no riacho 211C. Não sei se isso é realmente visivel nesse gráfico. |

# Minimum reproducible codes


## Diferenças entre anos e riachos com glms

```{R comparando_riachos_vs_anos}
library(broom)
library(performance)

Census_all<-rbind(
cbind(census2024,year="2024"),
cbind(census2023,year="2023"),
cbind(census2022,year="2022"))%>%
pivot_longer(Imaturos:Reprodutivos,names_to="stage")


Census_all_max<-rbind(
cbind(census2024,year="2024"),
cbind(census2023,year="2023"),
cbind(census2022,year="2022"))%>%
group_by(Site,year,Plot)%>%
summarise(MaxIm=max(Imaturos),
	MaxRep=max(Reprodutivos))%>%
mutate(MaxTot=ifelse(MaxIm>MaxRep,MaxIm,MaxRep))%>%
pivot_longer(MaxIm:MaxTot,names_to="stage")

#-------------------------------------------------------------------------------------------------------
#    Dados
#-------------------------------------------------------------------------------------------------------
# A tibble: 180 × 5
# Groups:   Site, year [6]
   Site  year   Plot stage  value
   <chr> <chr> <dbl> <chr>  <dbl>
 1 S11B  2022     11 MaxIm     23
 2 S11B  2022     11 MaxRep   108
 3 S11B  2022     11 MaxTot   108
 4 S11B  2022     12 MaxIm     20
 5 S11B  2022     12 MaxRep    96
 6 S11B  2022     12 MaxTot    96
 7 S11B  2022     13 MaxIm     55
 8 S11B  2022     13 MaxRep   107
 9 S11B  2022     13 MaxTot   107
10 S11B  2022     14 MaxIm     35
#-------------------------------------------------------------------------------------------------------


Census_all_max%>%
ggplot(.,aes(x=year, y=value,group=interaction(Site,stage)))+
geom_point(aes(color=Site))+
geom_smooth(aes(fill=Site,color=Site),method="lm")+
scale_color_manual(values=c("#6e86e7","#ff9211"))+
scale_fill_manual(values=c("#6e86e7","#ff9211"))+
#scale_y_log10()+
facet_grid(.~stage)+
theme_bw()+
labs(title="Abundancia de Parapiqueria riachos x anos",
x="Anos",y="Abundância")

Census_all_max%>%
mutate(year=as.numeric(year))%>%
nest(data = -stage)%>%
 mutate(
    fit = map(data, ~ MASS::glm.nb(value ~ year*Site, data = .)),
    tidy_fit = map(fit, ~ tidy(.) %>% mutate(across(c(estimate, p.value), ~ round(., 3))))
  ) %>%
  unnest(tidy_fit)%>%
group_by(stage)%>%
select(-c(fit,data))%>%
group_split()


library(performance)
glm(value~as.numeric(year)*Site,family = poisson(),
data=filter(Census_all_max,stage=="MaxRep"))%>%check_model()

glm(value~as.numeric(year)*Site,
data=filter(Census_all_max,stage=="MaxRep"))%>%check_model()


MASS::glm.nb(value~as.numeric(year)*Site,
data=filter(Census_all_max,stage=="MaxRep"))%>%check_model()
```

## Plots de abundância ao longo do tempo entre riachos
```{r Plots_de_série_temporal_por_plot} 
#----------------------------------------------------------------------
# Dados necessários para rodar os gráficos a seguir
#----------------------------------------------------------------------

Census_all<-rbind(
cbind(census2024,year="2024"),
cbind(census2023,year="2023"),
cbind(census2022,year="2022"))%>%
pivot_longer(Imaturos:Reprodutivos,names_to="stage")


# A tibble: 360 × 6
   Site   Plot Month year  stage        value
   <chr> <dbl> <chr> <chr> <chr>        <dbl>
 1 S11B     11 3     2024  Imaturos        72
 2 S11B     11 3     2024  Reprodutivos     0
 3 S11B     11 4     2024  Imaturos        23
 4 S11B     11 4     2024  Reprodutivos    56
 5 S11B     11 5     2024  Imaturos         0
 6 S11B     11 5     2024  Reprodutivos    57
 7 S11B     12 3     2024  Imaturos        78
 8 S11B     12 3     2024  Reprodutivos     0
 9 S11B     12 4     2024  Imaturos        44
10 S11B     12 4     2024  Reprodutivos    78

#-------------------------------------------------------
#	Colour palettes
#OrBl: https://www.vis4.net/palettes/#/20|d|ff9211,ffa445,ffb66c,ffc890,ffdab5,ffecd4,fff0f9|e2e3fa,c4c7f5,a4acf0,8292ea,597ae4,0062de|0|0
#-------------------------------------------------------
OrBlPallette<-c(
'#ff9211', '#ff9d30', '#ffa84d', '#ffb264', '#ffbd7a', 
'#ffc890', '#ffd3a6', '#ffdebb', '#ffe9ce', '#ffeee3', 
'#d3d5f8', '#c4c7f5', '#b4baf3', '#a4acf0', '#939fed', 
'#8292ea', '#6e86e7', '#597ae4', '#2d6ee1', '#0062de')
#--------------------------------------------------------


Census_all%>%
ggplot(.,aes(x=Month,y=value,group=interaction(Plot,stage)))+
geom_line(aes(color=Plot))+
facet_grid(stage~year,scales="free")+
scale_color_gradient(breaks=seq(0,20,1))

ggbyPlot_raw<-Census_all%>%
ggplot(.,aes(x=Month,y=value,group=interaction(Plot,stage)))+
geom_line(aes(color=as.factor(Plot)),size=1.3)+
facet_grid(stage~year,scales="free")+
#scale_color_gradient(breaks=seq(0,20,1))+
scale_color_manual(values=OrBlPallette)+
#scale_color_distiller(breaks=seq(0,20,1),palette = 'Spectral')+
guides(color=guide_legend(ncol=2))+
theme_bw(base_size=12)+
labs(color="S11C,S11B",
title=expression(paste("Dinâmica populacional de ", italic("Parapiqueria cavalcantei"))),
subtitle=expression(paste("Número de indivíduos contabilizados de ", italic("P. cavalcantei"))))

ggbyPlot<-Census_all%>%
group_by(Month,Plot,stage)%>%
summarise(mean=mean(value),
		sd=sd(value))%>%
ggplot(.,aes(x=Month,y=mean,group=interaction(Plot,stage)))+
geom_line(aes(color=as.factor(Plot)),size=0.1)+
geom_pointrange(aes(x=Month,ymax=mean+sd,ymin=mean-sd,color=as.factor(Plot)))+
facet_grid(stage~.,scales="free")+
#scale_color_gradient(breaks=seq(0,20,1))+
scale_color_manual(values=OrBlPallette)+
#scale_color_distiller(breaks=seq(0,20,1),palette = 'Spectral')+
guides(color=guide_legend(ncol=2))+
theme_bw(base_size=12)+
labs(
color="S11C,S11B",
y="Indivíduos",x="Mês amostrado (Março a Maio)")



ggbyYear<-Census_all%>%
group_by(Month,year,stage,Site)%>%
summarise(mean=mean(value),
		sd=sd(value),
		se=sd/sqrt(n()))%>%
ggplot(.,aes(x=Month,y=mean,group=interaction(year,Site)))+
geom_line(aes(color=as.factor(year),linetype=Site),size=0.1)+
geom_pointrange(aes(x=Month,ymax=mean+sd,ymin=mean-sd,color=as.factor(year)),
 position=position_dodge(width=.1)
)+
facet_grid(stage~.,scales="free")+
#viridis::scale_color_viridis(discrete = TRUE,option="viridis")+
theme_bw(base_size=12)+
labs(
color="Ano",
linetype="Riachos",
y="Indivíduos",x="Mês amostrado (Março a Maio)")

#minimal year
min_year<-min(as.numeric(Census_all$year))

ggfullYear<-Census_all%>%
  distinct(Site, Plot, year, stage) %>%
  tidyr::crossing(Month = as.character(1:12)) %>%
  left_join(Census_all, by = c("Site", "Plot", "year", "stage", "Month")) %>%
  arrange(Site, Plot, year, stage, as.integer(Month))%>%
mutate(rel_month= as.numeric(Month) +(as.numeric(year) - min_year)*12)%>%
ggplot(.,aes(x=rel_month,y=value,group=interaction(Plot,stage)))+
geom_line(aes(color=Plot))+
facet_grid(stage~.,scales="free")+
guides(color=guide_legend(ncol=2, nbreaks=19))+
scale_color_gradient(breaks=seq(0,20,1))+
labs(
color="S11C,S11B",
y="Indivíduos",x="Mês amostrado (Março a Maio)")


cowplot::plot_grid(
ggbyPlot_raw,ggfullYear,
ggbyYear,ggbyPlot,labels="AUTO")



```


## Visualização extra
##### Density plot
```{r density_plot}

library(ggridges)

# formato dos dados
> Census_all_max
# A tibble: 180 × 5
# Groups:   Site, year [6]
   Site  year   Plot stage  value
   <chr> <chr> <dbl> <chr>  <dbl>
 1 S11B  2022     11 MaxIm     23
 2 S11B  2022     11 MaxRep   108
 3 S11B  2022     11 MaxTot   108
 4 S11B  2022     12 MaxIm     20
 5 S11B  2022     12 MaxRep    96
 6 S11B  2022     12 MaxTot    96
 7 S11B  2022     13 MaxIm     55
 8 S11B  2022     13 MaxRep   107
 9 S11B  2022     13 MaxTot   107
10 S11B  2022     14 MaxIm     35



Census_all_max%>%
ggplot(., aes(x = value+1, y = Site,fill=Site)) + 
geom_density_ridges(alpha = 0.7,
jittered_points = TRUE,
 position = position_points_jitter(width = 0.5, height = 0),
                      point_shape =21, point_size = 2)+
 		xlim(0,400)+
scale_x_log10(breaks=c(1,10,100,500))+
			facet_grid(year~stage)+
theme_bw(base_size=16)+
theme(legend.position="bottom")+
labs(title=expression(paste("Abundância de ", italic("Parapiqueria cavalcantei"))),
x="Total de indivíduos registrados",
y="Proporção de frequência \n(density plot)")
```


# Notas
[^1]: Ferramentas para calcular área
[^2]: Para dar 1m² (=1000cm²) é necessário multiplicar por 16
[^3]: Ver glm(Abundância~Site)
[^4]: Ver glm(Abundância ~ year)
[^5]: Esse é o resultado do glm com distribuição binomial negativa. Quando um glm com poisson é utilizado todos os valores dão significativos
[^6]: uma espécie de delay reprodutivo
[^7]: **Segundos os autores:** "The low fine-scale genetic structure in _C. cangae_ and _P. cavalcantei_ may also be associated with a combination of life-history traits, particularly seed dispersal and establishment, often assumed to be the main determinants of spatial patterns (Hamrick & Trapnell 2011). Although further studies are needed, the persistence of three co-occurring genetic clusters within each sampled area of _P. cavalcantei_ may be explained by long-distance dispersal followed by selfing reproduction, which has likely prevented the gene flow among individuals locally". Retirado de [[Barbara Leal et al - Genetica de plantas criticas.docx]]
[^9]:  *"...based on the set of SNPs obtained through reduced represent methods."* Segundo os autores