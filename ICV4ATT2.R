####TÍTULO####
'A Influência dos Scouts na Performance das Equipes em um Torneio de Futebol'

####RESUMO####
'Resumo do Projeto'

####ABSTRACT####
'Resumo em Inglês do Artigo'

####INTRODUÇÃO####
'Contextualização do Trabalho'

####MATERIAIS E MÉTODOS####

####Coleta dos Dados####

setwd("C:/Users/KÁTIA/Desktop/EVOLUTION 2020/IC/REGRESSÃO/APLICADA AO FUTEBOL")
dados=read.table("ICV4.txt",header=T)
attach(dados)
head(dados)
str(dados)
d=data.frame(B,CC,D,DF,DRC,F,FC,GM,GS,MC,MI,P,PB,PC,PL,PP,PR,PTS,SSG,T,BICD,DFXGS,PPFC,PXGM,PXPR,TXFC,TXGM)
d=data.frame(dados[,c(-1,-35)])
str(d)
'Descrever os processos que serão executados (Análise Exploratória, Modelagem, ...) e onde serão (R, Excel, Python)'

####RESULTADOS####


####Análise Exploratória dos Dados####

####outliers####

{
outliers=data.frame(TIME)
for(i in 1:length(d)){
  
  #boxplot(d[i],main=ls(d[i]));
  #print(summary(d[i]));
  print(ls(d[i]))
  print(boxplot(d[i])$out)
  if(length(boxplot(d[i])$out) != 0){
    outliers=cbind.data.frame(outliers,d[i])
  }

}

#Outliers Identificados

ls(outliers)
outliers=data.frame(outliers[,-1])
for(i in 1:length(outliers)){
  boxplot(outliers[i],main=ls(outliers[i]));
  print(ls(outliers[i]))
  print(boxplot(outliers[i])$out);
}

#Boxplots usados: SSG&GM,PL,TXGM&B
boxplot(PL,main='Passes Longos')
par(mfrow=c(1,2))
boxplot(GM,main='Gols Marcados'); boxplot(SSG,main='Jogos Sem Sofrer Gols');
par(mfrow=c(1,2))
boxplot(TXGM,main='Média de Toques na Bola \npor Gol Marcado'); boxplot(B,main='Bloqueios')


}


library(ggplot2)
library(tidyverse)
install.packages("hrbrthemes")
install.packages("viridis")

library(hrbrthemes)
library(viridis)

outliers=data.frame(TIME)
for(i in 1:length(d)){
  
  #boxplot(d[i],main=ls(d[i]));
  #print(summary(d[i]));
  #print(ls(d[i])) #retirei
  #print(boxplot(d[i])$out) #retirei
  if(length(boxplot(d[i])$out) != 0){
    outliers=cbind.data.frame(outliers,d[i])
  }
  
}

#Automatizando o Processo
ls(outliers)
outliers=data.frame(outliers[,-1])
for(i in 1:length(outliers)){
  
  boxplot(outliers[i],main=names(outliers)[i]);
  print(names(outliers)[i])
  print(boxplot(outliers[i])$out)
  
  cat("\n");
  
  #Automatizar código para identificar outliers inferiores, pois usando >=min(...) só selecionar superiores
  #Maneira que eu encontrei para fazer a diferenciação de outliers superiores e inferiores
  if(length(boxplot(outliers[i])$out)==dim(subset(data.frame(TIME,outliers[,i]),outliers[i]>=min(boxplot(outliers[i])$out)))[1]){
    print(subset(data.frame(TIME,outliers[i]),outliers[i]>=min(boxplot(outliers[i])$out)));
  } else {
    print(subset(data.frame(TIME,outliers[i]),outliers[i]<=max(boxplot(outliers[i])$out)));
  }
  
  cat("\n\n");
  
  
}


bxplt=data.frame( 
  x1=c(rep("Passes Longos",20),rep("Passes Regressores",20)),
  y1=c(PL,PR)
)

bxplt %>%
  ggplot( aes(x=x1, y=y1, fill=x1)) +
  geom_boxplot(outlier.alpha = 0,outlier.colour="white", outlier.shape=16,outlier.size=0) +
  scale_fill_viridis(discrete = TRUE, alpha=0.5) +
  geom_jitter(shape=16,position = position_jitter(0.5),color="black", size=1, alpha=0.8) +
  theme_ipsum() +
  theme(legend.position="none",plot.title = element_text(hjust = 0.5,size=12)) +
  labs(title = "Boxplot de Passes") + #x = "TIMES", y="Quantidade de Jogos"
  xlab("") +
  ylab("Quantidade") #centralizar essa poha!
  #ggtitle("Boxplot de Jogos sem sofrer Gols (SSG)") +
  

bxplt=data.frame( 
  x1=rep("TIMES",20),
  y1=TXGM
)

bxplt %>%
  ggplot( aes(x=x1, y=y1, fill=x1)) +
  geom_boxplot(outlier.alpha = 0,outlier.colour="white", outlier.shape=16,outlier.size=0) +
  scale_fill_viridis(discrete = TRUE, alpha=0.5) +
  geom_jitter(shape=16,position = position_jitter(0.5),color="black", size=1.5, alpha=0.8) +
  theme_ipsum() +
  theme(legend.position="none",plot.title = element_text(hjust = 0.5,size=12)) +
  labs(title = "Boxplot de Toques na Bola por Gol Marcado (TXGM)") + #x = "TIMES", y="Quantidade de Jogos"
  xlab("") +
  ylab("Toques") #centralizar essa poha!
#ggtitle("Boxplot de Jogos sem sofrer Gols (SSG)") +


'Interpretar as razões desses outliers e decidir se vão continuar ou não no meu projeto'

####correlação####

library(corrplot)
corrplot(cor(subset(d,select=-c(TIME))), tl.col = "black", tl.srt = 35,cl.length=9,tl.cex=0.75,diag = FALSE,order="hclust",method="square",type="lower",title ='')
'Fazer comentário breve e já complementar os Gráficos de Dispersão e Histogramas
para a decisão que busca selecionar as possíveis melhores variáveis'
corrplot(cor(d), tl.col = "black", tl.srt = 35,cl.length=9,tl.cex=0.75,diag = FALSE,order="hclust",method="square",type="lower",title ='')

#classificação das correlações das variáveis explicativas com a preditora

for_cor=data.frame('Forte Correlação:')
for_cor_pos=data.frame('Forte Correlação Positiva:')
for_cor_neg=data.frame('Forte Correlação Negativa:')
fra_cor=data.frame('Fraca Correlação:')
fra_cor_pos=data.frame('Fraca Correlação Positiva:')
fra_cor_neg=data.frame('Fraca Correlação Negativa:')
mod_cor=data.frame('Moderada Correlação:')
i=2
for(i in 1:length(d)){
  if(abs(cor(PTS,d[i]))>0.7){
    #forte correlação
    for_cor=cbind.data.frame(for_cor,ls(d[i]));
    if(cor(PTS,d[i])>=0){
      #forte correlação positiva
      for_cor_pos=cbind.data.frame(for_cor_pos,ls(d[i]));
    } else { for_cor_neg=cbind.data.frame(for_cor_neg,ls(d[i])); }
  } else {
    if(abs(cor(PTS,d[i]))<=0.4){
      #fraca correlação
      fra_cor=cbind.data.frame(fra_cor,ls(d[i]));
      if(cor(PTS,d[i])>=0){
        #fraca correlação positiva
        fra_cor_pos=cbind.data.frame(fra_cor_pos,ls(d[i]));
      } else { fra_cor_neg=cbind.data.frame(fra_cor_neg,ls(d[i])); }
    } else {
      #moderada correlação
      mod_cor=cbind.data.frame(mod_cor,ls(d[i]));
    }
  }

}

fra_cor
mod_cor
for_cor
length(for_cor)

####gráficos de dispersão####

'Como obter as correlações mais fracas e mais fortes?
Utilizar o Excel para realizar a correlação e passar uma formatação condicional
Encontrar uma função ou implementar no R'

#EXCEL!

#curso-r.com/material/ggplot/

#Correlações fracas entre a variável resposta e a preditora #HORRÍVEL

par(mfrow=c(1,2))
plot(CC,PTS,main='Pontos x Cruzamentos Certos')
plot(PP,PTS,main='Pontos x Perca de Posse')

library(ggplot2)
library(hrbrthemes)

ggplot(dados, aes(x=PP, y=PTS)) +
  geom_point(shape=20,color="black",cex=2.75) +
  geom_smooth(method=lm , color="red", fill=16, se=TRUE,lwd=0.75) +
  xlab("Perca de Posse") +
  ylab("Pontos") +
  theme_minimal() +
  ggtitle("Pontos vs Perca de Posse") +
  theme(
    plot.title=element_text(hjust=0.5 , vjust=0.5)
    )

ggplot(dados, aes(x=CC, y=PTS)) +
  geom_point(shape=20,color="black",cex=2.75) +
  geom_smooth(method=lm , color="red", fill=16, se=TRUE,lwd=0.75) +
  xlab("Cruzamentos Certos") +
  ylab("Pontos") +
  theme_minimal() +
  ggtitle("Pontos vs Cruzamentos Certos") +
  theme(
    plot.title=element_text(hjust=0.5 , vjust=0.5)
  )

#Correlações fortes entre a variável resposta e a preditora #ESSENCIAL

par(mfrow=c(1,2))
plot(GM,PTS,main='Pontos x Gols Marcados')
plot(GS,PTS,main='Pontos x Gols Sofridos')
plot(FC,PTS)

library(ggplot2)
library(hrbrthemes)

ggplot(dados, aes(x=GM, y=PTS)) +
  geom_point(shape=20,color="black",cex=2.75) +
  geom_smooth(method=lm , color="red", fill=16, se=TRUE,lwd=0.75) +
  xlab("Gols Marcados") +
  ylab("Pontos") +
  theme_minimal() +
  ggtitle("Pontos vs Gols Marcados") +
  theme(
    plot.title=element_text(hjust=0.5 , vjust=0.5)
  ) +
  xlim(15,100)

ggplot(dados, aes(x=GS, y=PTS)) +
  geom_point(shape=20,color="black",cex=2.75) +
  geom_smooth(method=lm , color="red", fill=16, se=TRUE,lwd=0.75) +
  xlab("Gols Sofridos") +
  ylab("Pontos") +
  theme_minimal() +
  ggtitle("Pontos vs Gols Sofridos") +
  theme(
    plot.title=element_text(hjust=0.5 , vjust=0.5)
  )

#Correlações Fortes entre preditoras #PÉSSIMO!
par(mfrow=c(1,2))
plot(PC,P,main='Passes x Passes Certos')
plot(FC,F,main='Finalizações x Finalizações Certas')
plot(AP,MC)

ggplot(dados, aes(x=P, y=PC)) +
  geom_point(shape=20,color="black",cex=2.75) +
  geom_smooth(method=lm , color="red", fill=16, se=TRUE,lwd=0.75) +
  xlab("Passes") +
  ylab("Passes Certos") +
  theme_minimal() +
  ggtitle("Passes vs Passes Certos") +
  theme(
    plot.title=element_text(hjust=0.5 , vjust=0.5)
  )

ggplot(dados, aes(x=F, y=FC)) +
  geom_point(shape=20,color="black",cex=2.75) +
  geom_smooth(method=lm , color="red", fill=16, se=TRUE,lwd=0.75) +
  xlab("Finalizações") +
  ylab("Finalizações Certas") +
  theme_minimal() +
  ggtitle("Finalizações vs Finalizações Certas") +
  theme(
    plot.title=element_text(hjust=0.5 , vjust=0.5)
  )

#Correlações Fracas entre preditoras #BOM!
par(mfrow=c(1,2))
plot(CC,D,main='Desarmes x Cruzamentos Certos')
plot(TXGM,MC,main='Toques por Gol Marcado x Cortes')
plot(TXGM,MC)
plot(PR,PXGM,main='Passes por Gol \nMarcado x Passes Regressores')
plot(FC,PXPR)

ggplot(dados, aes(x=CC, y=D)) +
  geom_point(shape=20,color="black",cex=2.75) +
  geom_smooth(method=lm , color="red", fill=16, se=TRUE,lwd=0.75) +
  xlab("Cruzamentos Certos") +
  ylab("Desarmes") +
  theme_minimal() +
  ggtitle("Cruzamentos Certos vs Desarmes") +
  theme(
    plot.title=element_text(hjust=0.5 , vjust=0.5)
  )

ggplot(dados, aes(x=TXGM, y=MC)) +
  geom_point(shape=20,color="black",cex=2.75) +
  geom_smooth(method=lm , color="red", fill=16, se=TRUE,lwd=0.75) +
  xlab("Toques por Gol Marcado") +
  ylab("Cortes") +
  theme_minimal() +
  ggtitle("Toques por Gol Marcado vs Cortes") +
  theme(
    plot.title=element_text(hjust=0.5 , vjust=0.5)
  )

'Interpretar os gráficos em conjunto com a correlação em termos quantitativos
e futebolísticos e identificando variáveis essenciais, descartáveis e horríveis'

####histogramas####

library(moments)

h=1
ass_pos=data.frame(dados[1]) #assimetria positiva
ass_mod_pos=data.frame(dados[1]) #assimetria moderada positiva
ass_neg=data.frame(dados[1]) #assimetria negativa
ass_mod_neg=data.frame(dados[1]) #assimetria moderada negativa
sim=data.frame(dados[1]) #simetria
lepto=data.frame(dados[1]) #leptocurtica
meso=data.frame(dados[1]) #mesocurtica
plati=data.frame(dados[1]) #platicurtica
for(h in 1:length(d)){
  #x=data.matrix(d[h]); #convertendo data frame em matriz numérica
  #hist(x,main = ls(d[h]));
  #print(ls(d[h]))
  #print("Assim:"); print(skewness(d[,h]))
  #print("Curtose:"); print(kurtosis(d[,h]))
  
  #Análise de simetria
  if(skewness(d[,h])>=0.15){
    if(skewness(d[,h])>1){
      ass_pos=cbind.data.frame(ass_pos,d[h]);
    } else {ass_mod_pos=cbind.data.frame(ass_mod_pos,d[h]);}
  } else if(skewness(d[,h])<=-0.15){
    if(skewness(d[,h])<(-1)){
      ass_neg=cbind.data.frame(ass_neg,d[h]);
    } else { ass_mod_neg=cbind.data.frame(ass_mod_neg,d[h]); }
  } else { sim=cbind.data.frame(sim,d[h]); }
  
  #Análise de curtose 
  if(kurtosis(d[,h])>=3.15){
    lepto=cbind.data.frame(lepto,d[h]);
  } else if(kurtosis(d[,h])<=2.85){
    plati=cbind.data.frame(plati,d[h]);
  } else { meso=cbind.data.frame(meso,d[h]); }
  
}

'Quando não há simetria pode surgir a necessidade de transformações posteriores.
É realmente necessário simetria correta para se obter um bom modelo ???'
#...

#Classificação da simetra das variáveis conforme o histograma
ls(ass_neg)
ls(ass_mod_neg)
ls(ass_pos)
ls(ass_mod_pos)
ls(sim)
ls(meso)
ls(lepto)
ls(plati)
'O que fazer a respeito dessa classificação ??'
#...
'As variáveis DRC,MI,PP e PXPR apresentaram simetria , já AP,BICD,CHP,D,DF,GS,MC e PL 
apresentaram assimetria moderada negativa, já B,CC,F,FC,GCA,GM,L,P,PB,PC,PPFC,PR,PRC, 
PTS,T e TXFC apresentaram assimetria moderada positiva e as restantes DFXGS,FXGM,PXGM,
SSG e TXGM apresentaram forte assimetria positiva. Não houve nenhuma forte assimetria negativa'
'Posso resumir mostrando apenas as variáveis que apresentaram forte assimetria ?'

'Em fortes assimetrias positivas, a média é influenciada pelos valores menores.'
ls(ass_pos)
hist(SSG,main='Jogos sem Sofrer Gols')
hist(TXGM,main='Toques por Gol Marcado')
shapiro.test(SSG)$p.value
'A quantidade de clubes com alto é SSG é muito pequena,
portanto a concentração de observações está na parte inferior.
A mesma lógica pode ser empregada para TXGM, pois são poucos
os clubes que possuem alta eficiência e objetividade no aspceto
ofensivo em busca de gols. Logo, há uma quantidade maior de clubes
que necessitam de mais toques na bola para balançar as redes.'


###EXTRA
'Analisando normalidade e coeficiente de assimetria dos casos que apresentaram forte assimetria positiva'
ass_pos=ass_pos[,-1]; str(ass_pos)
j=0
for(j in 1:length(ass_pos)){
  pvlr=shapiro.test(ass_pos[,j])$p.value
  if(pvlr<=0.05){
    print(ls(ass_pos[j]))
    print('Não há normalidade:')
    print(pvlr)
    print('Coeficiente de Assimetria:')
    print(skewness(ass_pos[,j]))
  } else {
    print(ls(ass_pos[j]))
    print('Há normalidade:')
    print(pvlr)
    print('Coeficiente de Assimetria:')
    print(skewness(ass_pos[,j]))
  }
}


###Extra Extra Extra
'Analisando normalidade e coeficiente de assimetria dos casos que apresentaram moderada assimetria positiva'
ass_mod_pos=ass_mod_pos[,-1]; str(ass_mod_pos)
j=0
for(j in 1:length(ass_mod_pos)){
  pvlr=shapiro.test(ass_mod_pos[,j])$p.value
  if(pvlr<=0.05){
    print(ls(ass_mod_pos[j]))
    print('Não há normalidade:')
    print(pvlr)
    print('Coeficiente de Assimetria:')
    print(skewness(ass_mod_pos[,j]))
  } else {
    print(ls(ass_mod_pos[j]))
    print('Há normalidade:')
    print(pvlr)
    print('Coeficiente de Assimetria:')
    print(skewness(ass_mod_pos[,j]))
  }
}


'Não houve forte assimetria negativa em nenhuma das variáveis. Concluindo
que para esse conjunto de dados são raros valores menores que deslocam a média'


'Em assimetrias moderadas negativas, a média continuam sendo influenciada pelos valores maiores.'
ls(ass_mod_neg)
hist(D,main='Desarmes')
hist(BICD,main='Volume de Ações Defensivas')
shapiro.test(D)
'Algumas equipes estão condicionadas a ter que desarmar mais que outras por conta de
estilos de jogo defensivos e assim a distribuição é influenciada por essas observações.
Analogicamente, o volume de ações defensivas segue o mesmo comportamento.'


'Em simetrias podemos inferir com certeza que a média daquela característica é 
correspondente a de todas equipes em geral, pois os dados possuem distribuição normal.
Além disso, não é necessário realizar nenhum tipo de transformação para
aumentar a performance dentro do modelo.'
ls(sim)
hist(DRC,main = 'Dribles Certos')
hist(MI,main = 'Interceptações')
shapiro.test(DRC)$p.value
skewness(DRC)
shapiro.test(MI)$p.value
skewness(MI)
'Percebemos que as distribuições de Dribles Certos e de Interpretações
são normais. Fato esperado no caso das Interceptações, pois é um fundamento
que todos os times praticam constantemente para retomar a posse ou quebrar uma jogada.'


'Concluir que para a possível melhora da performance do modelo,
poderão ser requisitadas transformações nas variáveis que possuirem
forte assimetria. É necessário ou possibilitaria mais resultados significativos
tentar transformar as variávies que possuem assimetria moderada?'


#Curtose
'Não irei interpretar a curtose'


####seleção das variáveis####


'Realizar a seleção das variáveis mais importantes de acordo com os relacionamentos das preditoras entre si e com a resposta.
Considerando também os gráficos analisados.'
sel=data.frame(PTS,GM, FC, GS, PR, DF, SSG, DRC, TXGM, DFXGS, TXFC, BICD, PPFC)


####Modelagem####


setwd("C:/Users/KÁTIA/Desktop/EVOLUTION 2020/IC/REGRESSÃO/APLICADA AO FUTEBOL")
dados=read.table("ICV4.txt",header=T)
attach(dados)
sel=data.frame(PTS,GM, FC, GS, PR, DF, SSG, DRC, TXGM, DFXGS, TXFC, BICD, PPFC)

validacao=read.table("PL1718.txt",header=T); attach(validacao); head(validacao);
v=data.frame(validacao[,-1])

teste=read.table("PL1920R25.txt",header=T); attach(teste); head(teste);
t=data.frame(teste[,-1])


#modelo inicial completo####
m=lm(PTS~GM+FC+GS+PR+DF+SSG+DRC+TXGM+DFXGS+TXFC+BICD+PPFC,data=sel)
m
summary(m)

#verificando multicolineridade
require(faraway)
vif(m)


#Seleção de Modelos


#todas as regressões possíveis / método exaustivo####


require(leaps)
str(sel)
y=sel[,1]; x=sel[,-1];
models=regsubsets(x,y,data=sel,nbest=2); models;
ls(models)
s=summary(models); s;

#Coeficiente de determinação ajustado e Cp de Mallows dos modelos,respectivamente:
s$adjr2;
s$cp;

i=1; n=length(s)*2;
for(i in 1:n){
  if(max(s$adjr2)==s$adjr2[i]){
    print("Melhor adjr2: ")
    print(coef(models,i));
  } else {};
  
  if(min(s$cp)==s$cp[i]){
    print("Menor Cp de Mallows: ");
    print(coef(models,i));
  } else{};
}

#Selecionando os dois melhores modelos com base nos critérios adotados:

#Cp de Mallows
m1=lm(PTS~GM+GS+SSG+TXGM+BICD,data=sel)

#Coeficiente de Determinação ajustado
mcf=lm(PTS~GM+GS+TXGM+BICD+DRC+DFXGS+TXFC+PPFC)


#Selecionando os modelos pelos critérios de Forward e Backward:


#forward####


ajuste0=lm(PTS~1,data=sel)
add1(ajuste0,test='F',scope~GM+FC+GS+PR+DF+SSG+DRC+TXGM+DFXGS+TXFC+BICD+PPFC)
#Como a variável GM é a mais significativa, pois tem o menor p valor
#e consequentemente a maior estatística F, adicionamos ao modelo.

ajuste1=lm(PTS~GM,data=sel)
add1(ajuste1,test='F',scope~GM+FC+GS+PR+DF+SSG+DRC+TXGM+DFXGS+TXFC+BICD+PPFC)
#Logo a variável GS adiciona mais contribuição ao modelo, assim adicionamos.

ajuste2=lm(PTS~GM+GS,data=sel)
add1(ajuste2,test='F',scope~GM+FC+GS+PR+DF+SSG+DRC+TXGM+DFXGS+TXFC+BICD+PPFC)
#Como nenhuma outra variável adiciona contribuição significativa ao modelo,
#encerramos o processo sem adicionar novas covariáveis.
#Assim o modelo selecionado pelo critério de forward é:
m2=lm(PTS~GM+GS,data=sel)


#backward####

ajuste=(m)
drop1(ajuste,test='F')
#Como a variável PR possui o maior valor p e consequentemente a menor estatística F,
#logo ela adiciona pouca contribuição significativa ao modelo, assim a descartamos.

ajuste22=update(ajuste,~.-PR)
drop1(ajuste22,test='F')
#Removemos DF

ajuste33=update(ajuste22,~.-DF)
drop1(ajuste33,test='F')
#Removemos FC

ajuste44=update(ajuste33,~.-FC)
drop1(ajuste44,test='F')
#Removemos SSG

ajuste55=update(ajuste44,~.-SSG)
drop1(ajuste55,test='F')
#Removemos TXFC

ajuste66=update(ajuste55,~.-TXFC)
drop1(ajuste66,test='F')
#Removemos DRC

ajuste77=update(ajuste66,~.-DRC)
drop1(ajuste77,test='F')
#Removemos PPFC

ajuste88=update(ajuste77,~.-PPFC)
drop1(ajuste88,test='F')
#Removemos DFXGS

ajuste99=update(ajuste88,~.-DFXGS)
drop1(ajuste99,test='F')
#Como todas as variáveis remanescentes incrementam efeito significativo ao modelo,
#logo encerramos o processo sem adicionar novas covariáveis
m3=lm(PTS~GM+GS+TXGM+BICD,data=sel)


#AVALIAÇÃO DOS MODELOS####


#analisando os parâmetros dos modelos####


m1
#Retirando variável SSG por conta da interpretação
m1=m3
m2
#ok!
m3
#ok!


#avaliando a multicolinearidade####

require(faraway)
vif(m2)
#ok
vif(m3)
#GM -> retirar TXGM ou BICD
m3=lm(PTS~GM+GS+BICD,data=sel);
vif(mt);


#testando os pressupostos de normalidade, homogeneidade e independência####
require(lmtest)

shapiro.test(residuals(m2)); bptest(m2); dwtest(m2,alternative = "two.sided")
#ok!
shapiro.test(residuals(m3)); bptest(m3); dwtest(m3,alternative = "two.sided")
#ok!

#Testar a performance fora da amostra####

MSEAM=sqrt((sum(predict(m3,sel)-dados$PTS)^2)/20); MSEAM;
MSE=sqrt((sum((predict(m3,v)-v$PTS)^2))/20); MSE;

MSEAM=sqrt((sum(predict(m2,sel)-dados$PTS)^2)/20); MSEAM;
MSE=sqrt((sum((predict(m2,v)-v$PTS)^2))/20); MSE;

#Testar a performance fora da amostra buscando melhorar o desempenho PL17/18####

m2=lm(PTS~GM+GS,data=sel)
m3=lm(PTS~GM+GS+BICD,data=sel)
m4=lm(PTS~GM+GS+TXGM,data=sel)
m5=lm(PTS~GM+GS+DFXGS,data=sel)
m6=lm(PTS~GM+GS+PPFC,data=sel)
m7=lm(PTS~GM+GS+DRC,data=sel)
m8=lm(PTS~GM+GS+TXFC,data=sel)
m9=lm(PTS~GM+GS+SSG,data=sel)
m10=lm(PTS~GM+GS+FC,data=sel)
m11=lm(PTS~GM+GS+DF,data=sel)
m12=lm(PTS~GM+GS+DFXGS+TXGM,data=sel)
m13=lm(PTS~GM+GS+DFXGS+BICD,data=sel)
m14=lm(PTS~GM+GS+DFXGS+PPFC,data=sel)
m15=lm(PTS~GM+GS+DFXGS+TXFC,data=sel)
m16=lm(PTS~GM+GS+DFXGS+SSG,data=sel)
m17=lm(PTS~GM+GS+DFXGS+FC,data=sel)
m18=lm(PTS~GM+GS+DFXGS+TXGM+PPFC,data=sel)
m19=lm(PTS~GM+GS+DF+TXGM,data=sel)
m20=lm(PTS~GM+GS+DF+BICD,data=sel)
m21=lm(PTS~GM+GS+DF+PPFC,data=sel)
m22=lm(PTS~GM+GS+DF+TXFC,data=sel)
m23=lm(PTS~GM+GS+DF+SSG,data=sel)
m24=lm(PTS~GM+GS+DF+FC,data=sel)
m25=lm(PTS~GM+GS+DF+TXGM+PPFC,data=sel)

EOUT=data.frame('Erro');

MSE=sqrt((sum((predict(m2,v)-v$PTS)^2))/20); MSE; EOUT=cbind.data.frame(EOUT,MSE);
MSE=sqrt((sum((predict(m3,v)-v$PTS)^2))/20); MSE; EOUT=cbind.data.frame(EOUT,MSE);
MSE=sqrt((sum((predict(m4,v)-v$PTS)^2))/20); MSE; EOUT=cbind.data.frame(EOUT,MSE);
MSE=sqrt((sum((predict(m5,v)-v$PTS)^2))/20); MSE; EOUT=cbind.data.frame(EOUT,MSE);
MSE=sqrt((sum((predict(m6,v)-v$PTS)^2))/20); MSE; EOUT=cbind.data.frame(EOUT,MSE);
MSE=sqrt((sum((predict(m7,v)-v$PTS)^2))/20); MSE; EOUT=cbind.data.frame(EOUT,MSE);
MSE=sqrt((sum((predict(m8,v)-v$PTS)^2))/20); MSE; EOUT=cbind.data.frame(EOUT,MSE);
MSE=sqrt((sum((predict(m9,v)-v$PTS)^2))/20); MSE; EOUT=cbind.data.frame(EOUT,MSE);
MSE=sqrt((sum((predict(m10,v)-v$PTS)^2))/20); MSE; EOUT=cbind.data.frame(EOUT,MSE);
MSE=sqrt((sum((predict(m11,v)-v$PTS)^2))/20); MSE; EOUT=cbind.data.frame(EOUT,MSE);
MSE=sqrt((sum((predict(m12,v)-v$PTS)^2))/20); MSE; EOUT=cbind.data.frame(EOUT,MSE);
MSE=sqrt((sum((predict(m13,v)-v$PTS)^2))/20); MSE; EOUT=cbind.data.frame(EOUT,MSE);
MSE=sqrt((sum((predict(m14,v)-v$PTS)^2))/20); MSE; EOUT=cbind.data.frame(EOUT,MSE);
MSE=sqrt((sum((predict(m15,v)-v$PTS)^2))/20); MSE; EOUT=cbind.data.frame(EOUT,MSE);
MSE=sqrt((sum((predict(m16,v)-v$PTS)^2))/20); MSE; EOUT=cbind.data.frame(EOUT,MSE);
MSE=sqrt((sum((predict(m17,v)-v$PTS)^2))/20); MSE; EOUT=cbind.data.frame(EOUT,MSE);
MSE=sqrt((sum((predict(m18,v)-v$PTS)^2))/20); MSE; EOUT=cbind.data.frame(EOUT,MSE);
MSE=sqrt((sum((predict(m19,v)-v$PTS)^2))/20); MSE; EOUT=cbind.data.frame(EOUT,MSE);
MSE=sqrt((sum((predict(m20,v)-v$PTS)^2))/20); MSE; EOUT=cbind.data.frame(EOUT,MSE);
MSE=sqrt((sum((predict(m21,v)-v$PTS)^2))/20); MSE; EOUT=cbind.data.frame(EOUT,MSE);
MSE=sqrt((sum((predict(m22,v)-v$PTS)^2))/20); MSE; EOUT=cbind.data.frame(EOUT,MSE);
MSE=sqrt((sum((predict(m23,v)-v$PTS)^2))/20); MSE; EOUT=cbind.data.frame(EOUT,MSE);
MSE=sqrt((sum((predict(m24,v)-v$PTS)^2))/20); MSE; EOUT=cbind.data.frame(EOUT,MSE);
MSE=sqrt((sum((predict(m25,v)-v$PTS)^2))/20); MSE; EOUT=cbind.data.frame(EOUT,MSE);

EOUT

#Testar a performance na edição em andamento PL1920####

EOUT=data.frame('Erro');
MSE=sqrt((sum(((predict(m2,t)*(25/38))-t$PTS)^2))/20); MSE; EOUT=cbind.data.frame(EOUT,MSE);
MSE=sqrt((sum(((predict(m3,t)*(25/38))-t$PTS)^2))/20); MSE; EOUT=cbind.data.frame(EOUT,MSE);
MSE=sqrt((sum(((predict(m4,t)*(25/38))-t$PTS)^2))/20); MSE; EOUT=cbind.data.frame(EOUT,MSE);
MSE=sqrt((sum(((predict(m5,t)*(25/38))-t$PTS)^2))/20); MSE; EOUT=cbind.data.frame(EOUT,MSE);
MSE=sqrt((sum(((predict(m6,t)*(25/38))-t$PTS)^2))/20); MSE; EOUT=cbind.data.frame(EOUT,MSE);
MSE=sqrt((sum(((predict(m7,t)*(25/38))-t$PTS)^2))/20); MSE; EOUT=cbind.data.frame(EOUT,MSE);
MSE=sqrt((sum(((predict(m8,t)*(25/38))-t$PTS)^2))/20); MSE; EOUT=cbind.data.frame(EOUT,MSE);
MSE=sqrt((sum(((predict(m9,t)*(25/38))-t$PTS)^2))/20); MSE; EOUT=cbind.data.frame(EOUT,MSE);
MSE=sqrt((sum(((predict(m10,t)*(25/38))-t$PTS)^2))/20); MSE; EOUT=cbind.data.frame(EOUT,MSE);
MSE=sqrt((sum(((predict(m11,t)*(25/38))-t$PTS)^2))/20); MSE; EOUT=cbind.data.frame(EOUT,MSE);
MSE=sqrt((sum(((predict(m12,t)*(25/38))-t$PTS)^2))/20); MSE; EOUT=cbind.data.frame(EOUT,MSE);
MSE=sqrt((sum(((predict(m13,t)*(25/38))-t$PTS)^2))/20); MSE; EOUT=cbind.data.frame(EOUT,MSE);
MSE=sqrt((sum(((predict(m14,t)*(25/38))-t$PTS)^2))/20); MSE; EOUT=cbind.data.frame(EOUT,MSE);
MSE=sqrt((sum(((predict(m15,t)*(25/38))-t$PTS)^2))/20); MSE; EOUT=cbind.data.frame(EOUT,MSE);
MSE=sqrt((sum(((predict(m16,t)*(25/38))-t$PTS)^2))/20); MSE; EOUT=cbind.data.frame(EOUT,MSE);
MSE=sqrt((sum(((predict(m17,t)*(25/38))-t$PTS)^2))/20); MSE; EOUT=cbind.data.frame(EOUT,MSE);
MSE=sqrt((sum(((predict(m18,t)*(25/38))-t$PTS)^2))/20); MSE; EOUT=cbind.data.frame(EOUT,MSE);
MSE=sqrt((sum(((predict(m19,t)*(25/38))-t$PTS)^2))/20); MSE; EOUT=cbind.data.frame(EOUT,MSE);
MSE=sqrt((sum(((predict(m20,t)*(25/38))-t$PTS)^2))/20); MSE; EOUT=cbind.data.frame(EOUT,MSE);
MSE=sqrt((sum(((predict(m21,t)*(25/38))-t$PTS)^2))/20); MSE; EOUT=cbind.data.frame(EOUT,MSE);
MSE=sqrt((sum(((predict(m22,t)*(25/38))-t$PTS)^2))/20); MSE; EOUT=cbind.data.frame(EOUT,MSE);
MSE=sqrt((sum(((predict(m23,t)*(25/38))-t$PTS)^2))/20); MSE; EOUT=cbind.data.frame(EOUT,MSE);
MSE=sqrt((sum(((predict(m24,t)*(25/38))-t$PTS)^2))/20); MSE; EOUT=cbind.data.frame(EOUT,MSE);
MSE=sqrt((sum(((predict(m25,t)*(25/38))-t$PTS)^2))/20); MSE; EOUT=cbind.data.frame(EOUT,MSE);
EOUT;


#O modelo ideal escolhido foi m13, pois possui uma menor performance e satisfaz os pressupostos####
#Verificar se não há como normalizar aquele outro excelente modelo (m15)
m13=lm(PTS~GM+GS+DFXGS+BICD,data=sel)
m13
summary(m13)
require(faraway)
vif(m13)
require(lmtest)
shapiro.test(residuals(m13)); bptest(m13); dwtest(m13,alternative = "two.sided")

#conclusões treino
qqnorm(residuals(m13)) #distribuição dos resíduos do modelo
plot(predict(m13),residuals(m13),main='Predict x Residuals') #previsão x resíduos
plot(predict(m13),d$PTS,main='Predict x Pontos') #predito x pts
MSE=sqrt((sum((predict(m13)-d$PTS)^2))/20); MSE; 
PE=abs(1-(predict(m13)/d$PTS)); MPE=mean(PE); MPE;

####predições de treino####

pred=data.frame(dados[1],predict(m13),d$PTS); pred;


#Predição da média e mediana dos valores
predict(m13,data.frame(GM=c(mean(d$GM),median(d$GM)),GS=c(mean(d$GS),median(d$GS)),
                       DFXGS=c(mean(d$DFXGS),median(d$DFXGS)),BICD=c(mean(d$BICD),median(d$BICD))
                       ))

#Intervalo de confiança para a previsão de pts dado as variáveis explicativas ao nível de 5%
predict(m13,interval = "confidence",level = 0.95,se.fit = T)

#???
predict(m13,interval = "prediction",level = 0.95,se.fit = T)

#Intervalo de confiança para os parâmetros do modelo ao nível de 5%
confint(m13,level = 0.95)




#Conclusões teste####
plot(predict(m13,t)*(25/38),predict(m13,t)*(25/38)-t$PTS,
     main='Previsão x Resíduos') #previsão x resíduos 
plot(predict(m13,t)*(25/38),t$PTS,
     main='Previsão da Pontuação x Pontuação Real') #predito x pts
MSE=sqrt((sum(((predict(m13,t)*(25/38))-t$PTS)^2))/20); MSE; 
PE=abs(1-((predict(m13,t)*(25/38))/t$PTS)); MPE=mean(PE); MPE;
#Houve um aumento de aproximadamente 6% no erro do treino em comparação com o teste
#Ocorreu um aumento de cerca de 1.78 no Mean Squared Error em comparação com o teste

pred=data.frame(predict(m13,t)*(25/38),t$PTS); pred;
pred=data.frame(predict(m13,t),t$PTS); pred;

predict(m13,data.frame(GM=c(mean(t$GM),median(t$GM)),GS=c(mean(t$GS),median(t$GS)),
                       DFXGS=c(mean(t$DFXGS),median(t$DFXGS)),BICD=c(mean(t$BICD),median(t$BICD))
                     ))



#predict de validação (extra)####
pred=data.frame(validacao[1],predict(m13,v),v$PTS); pred;


#Análise de Acurrácia para Modelo2####
pred=data.frame(dados[1],predict(m2),d$PTS); pred;
pred=data.frame(validacao[1],predict(m13,v),v$PTS); pred;
pred=data.frame(predict(m2,t)*(25/38),t$PTS); pred;

#SSG x Transformação de SSG
library(moments)
skewness(SSG)
plot(SSG,PTS)
cor(SSG,PTS)
hist(SSG)
Z1SSG=sqrt(SSG)
skewness(Z1SSG)
plot(Z1SSG,PTS)
cor(Z1SSG,PTS)
hist(Z1SSG,breaks=3)

mssg=lm(PTS~SSG,data=d)
summary(mssg)
sqrt(sum(predict(mssg,d)^2))

mzssg=lm(PTS~Z1SSG,data=d)
sqrt(sum(predict(mzssg,d)^2))


setwd("C:/Users/KÁTIA/Desktop/EVOLUTION 2020/IC/REGRESSÃO/APLICADA AO FUTEBOL")
ext=read.table("BR2018.txt",header=T)
attach(ext)
e=data.frame(ext[,-1]); e;
pred=data.frame(ext[1],predict(m13,e),e$PTS); pred;
