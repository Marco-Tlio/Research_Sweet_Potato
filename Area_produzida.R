  abrv_estados<-c("RO","AC","AM","RR","PA")
abrv_estados <-c(abrv_estados,"AP","TO","MA","PI","CE")
abrv_estados <-c(abrv_estados,"RN","PB","PE","AL","SE")
abrv_estados <-c(abrv_estados,"BA","MG","ES","RJ","SP")
abrv_estados <-c(abrv_estados,"PR","SC","RS","MS","MT")
abrv_estados <-c(abrv_estados,"GO","DF")

library(stringr)

###########Área plantanda
tabela_brasil_Areaplantando <- read.csv("dados/tabela_brasil_Areaplantando.csv", 
                                        skip = 4)
tabela<- tabela_brasil_Areaplantando[-c(26,nrow(tabela_brasil_Areaplantando)),]
tabela[tabela == "-"] <- NA
tabela$X[7:33]<-abrv_estados
aux1 <- stringr::str_sub(tabela$X[-c(1:33)],start = -3,end=-2)
tabela$Estados <- c(rep(0,times = length(1:33)),aux1)
tabela <- tabela[c(1,44,2:(ncol(tabela)-1))]
tabela <- tabela[-227,]
tabelat <- na.omit(tabela)
tabelat[3:ncol(tabelat)] <- lapply(tabelat[3:ncol(tabelat)], FUN = function(y){as.numeric(y)})

#Quantos tem
qo = nrow(tabela) - 33
qa = nrow(tabelat) - 25
qo
qa
qa/qo

#Estados
uniest = unique(tabelat$Estados)[-1]
uniest
length(uniest)

#Testes em 2000
presi = tabelat[111,c(1,2,43,44)]
total = tabelat[1,43]
batat = tabelat[1,44]
##Equações Diferentes
(presi[1,4]/presi[1,3])/(batat/total)

#Serie temporal(Criação)
listaArea = list()
t = 3
b = 4
for (i in 1:21) {
  aux = tabelat[-c(1:25),c(1,2,t,b)]
  auxtt1 = tabelat[1,t]
  auxtb1 = tabelat[1,b]
  auxql = c()
  for(j in 1:nrow(aux)){
    auxeq1 = aux[j,4]
    auxeq2 = aux[j,3]
    ql = (auxeq1/auxeq2)/(auxtb1/auxtt1)
    auxql = c(auxql,ql)
  }
  aux$QL <- auxql
  aux$Teste <- ifelse(aux$QL >=2, "O", "X")
  listaArea[[i]] = aux
  t = t+2
  b = b+2
}
names(listaArea) <- 0:20

#Pegar os 10 primeiros de 2000 e c
npp = "Presidente Prudente"
p50 = sort(listaArea[[21]]$Batata.doce.20,index.return = T,decreasing = T)$ix[1:10]
npp = listaArea[[21]]$X[str_detect(listaArea[[21]]$X, npp)]
pp0 = which(listaArea[[21]]$X %in% npp) 
numeropp0 = sort(listaArea[[21]]$QL,index.return = T,decreasing = T)$ix[pp0]
np5 = listaArea[[21]]$X[p50]
qlp50 = listaArea[[21]]$QL[p50]
qlpp0 = listaArea[[21]]$QL[pp0]
np5
qlp50




#Tabela dos primeiros 5 e a criacao da serie temporal de prudente para A e a tabela dos dois
qlpp = c(qlpp0)
numero_de_pp = c(numeropp0)
datafr = data.frame()
datafr <- rbind(datafr,qlp50)
names(datafr) <- np5
round(datafr,3)
for( i in 1:20){
  listaux = listaArea[[i]]
  p5a = which(listaux$X %in% np5)
  ppa = which(listaux$X %in% npp) 
  qlp5a = listaux$QL[p5a]
  datafr <- rbind(datafr,qlp5a)
  qlppa <- listaux$QL[ppa]
  qlpp = c(qlpp,qlppa)
  numero_de_pp = c(numero_de_pp,sort(listaux$QL,index.return = T,decreasing = T)$ix[ppa])
}
rownames(datafr)<- lapply(0:20, as.character)
round(datafr,3)
round(datafr,3)
datafr2 <- datafr
serie_temp = ts(datafr2,start = 2000,end = 2020)
Aqlpp=qlpp


#Plots
stats::ts.plot(serie_temp,gpars= list(col= c(rainbow(5),"black")))
legend("topleft", legend = names(datafr2), col = c(rainbow(5),"black"), lty = 1,text.width = 2,cex=0.60)
title(ylab = "QL para Área",main = "Série temporal da Área")

plotpp<- serie_temp[,6]
stats::ts.plot(plotpp,gpars= list(col= "black"))
legend("topleft", legend = names(datafr2)[ncol(datafr2)], col = "black", lty = 1,text.width = 2,cex=0.60)
title(ylab = "QL para Área",main = "Série temporal da Área para PP")

Série_Temporal_Área <- serie_temp
stats::plot.ts(Série_Temporal_Área)


##Variaveis utilizadas




tabela_brasil_Areaplantando <- read.csv("dados/tabela_brasil_Areaplantando.csv", 
                                        skip = 4)
tabela2<- tabela_brasil_Areaplantando[-c(26,nrow(tabela_brasil_Areaplantando)),]
tabela2[tabela2 == "-"] <- 0
tabela2 <- tabela2[c(1,34:592),]
tabela2[2:ncol(tabela2)] <- lapply(tabela2[2:ncol(tabela2)], FUN = function(y){as.numeric(y)})
tabela2 <- tabela2[-560,]
na2 <- which(is.na(tabela2),arr.ind = T)
tabela2 <- tabela2[-na2,]
tabela2
tabela3<- tabelat[-c(2:25),]
##Ordenada
s1 = sort(tabela3$Batata.doce.20,index.return = T,decreasing = T)$ix
s2 = sort(tabela2$Batata.doce.20,index.return = T,decreasing = T)$ix
tabelao1 <- tabela3[s1,]
tabelao2 <- tabela2[s2,]
###areas
Gini1 = c()
Gini2 = c()
Gini3 = c()
t = 3
b = 4

for (i in 1:21) {
  aux1 = tabela3[-1,c(1,2,t,b)]
  aux2 = tabelao1[-1,c(1,2,t,b)]
  aux3 = aux1
  aux3 = aux3[order(aux1[,4],decreasing = T),]
  auxtt1 = aux1[,3]/sum(aux1[,3])
  auxtb1 = aux1[,4]/sum(aux1[,4])
  auxtt2 = aux2[,3]/sum(aux2[,3])
  auxtb2 = aux2[,4]/sum(aux2[,4])
  auxtt3 = aux3[,3]/sum(aux3[,3])
  auxtb3 = aux3[,4]/sum(aux3[,4])
  x1 = cumsum(auxtt1)
  y1 = cumsum(auxtb1)
  soma1 = x1[1]*y1[1]
  x2 = cumsum(auxtt2)
  y2 = cumsum(auxtb2)
  soma2 = x2[1]*y2[1]
  x3 = cumsum(auxtt3)
  y3 = cumsum(auxtb3)
  soma3 = x3[1]*y3[1]
  
  for(j in 2:(nrow(aux2)-1)){
    soma1 = soma1+ ((y1[j+1]-y1[j])*(x1[j+1]+x1[j+1]))
    soma2 = soma2+ ((y2[j+1]-y2[j])*(x2[j+1]+x2[j+1]))
    soma3 = soma3+ ((y3[j+1]-y3[j])*(x3[j+1]+x3[j+1]))
  }
  Gini1 = c(Gini1,1-soma1)
  Gini2 = c(Gini2,1-soma2)
  Gini3 = c(Gini3,1-soma3)
  
  
  t = t+2
  b = b+2
}

#Usar gini 3 Organizado por ano
serie_tempg = ts(Gini3,start = 2000,end = 2020)
Gini3
serie_tempg
stats::ts.plot(serie_tempg,gpars= list(col= "black",xlab="Ano", ylab="Gini Locacional",main = "Série temporal do Gini Locacional"),ylim=c(0,1))
ggplot(teste_tabela, aes(x = factor(teste), y = Gini3,fill="blue")) +
  geom_bar(stat = "identity")+
  labs(y= "Gini Locacional", x = "Anos",title = "Gini Locacional em série temporal")+
  geom_text(aes(label=round(Gini3,2)),vjust = -0.2) + 
  scale_fill_manual(values=c("blue"))+
  guides(fill = FALSE)  




#Two-Way-Joining
library(tidyverse)

dt2 <- datafr %>%
  rownames_to_column() %>%
  gather(colname, value, -rowname)
head(dt2)


dt2 = dt2 %>%
  mutate(classe1 = factor(ifelse(dt2[,3]<2,0,1)),QL = cut(dt2[,3],c(0,2,20,40,100,600)))

dt2[,1]<- rep(2000:2020,times=10)

ggplot(dt2, aes(x = rowname, y = colname, fill = QL)) +
  geom_tile(aes(fill = QL),colour = "black") +
  labs(y="Ql das Microregiões",x="Anos")+
  scale_fill_manual(values=c("Blue","yellow","dark orange","#800000","#400000")) +
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous(breaks=2000:2020)


#####WIlision
#Serie temporal(Criação)
listaAreaWs = list()
t = 3
b = 4
for (i in 1:21) {
  aux = tabelat[-c(1:25),c(1,2,t,b)]
  auxtt1 = tabelat[1,t]
  auxtb1 = tabelat[1,b]
  s = sum((aux[,4]-auxtb1)**2)
  auxWs = c()
  for(j in 1:nrow(aux)){
    auxeq1 = aux[j,4]
    auxeq2 = aux[j,3]
    
    Ws = sqrt(s*(auxeq2/auxtt1))/auxtb1
    auxWs = c(auxWs,Ws)
  }
  aux$Ws <- auxWs

  listaAreaWs[[i]] = aux
  t = t+2
  b = b+2
}
names(listaAreaWs) <- 0:20
