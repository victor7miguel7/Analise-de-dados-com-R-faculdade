x <- gl(100,2,labels=c("M","F"))
drinks <- factor(c("beer","beer","wine","water"))
summary(drinks)
drinks

mean(drinks == "beer")

drinks <- factor(c("beer","beer","wine","water"))
drinks
levels(drinks)[1] <- "water"

lista_pessoas <- list(nomes=c("João", "Paula", "Maria", "Ingrid", "José", "Marcos"), pesos=c(80, 65, 70, 58, 78, 70), alturas=c(1.70, 1.66, 1.65, 1.60, 1.76, 1.70))
str(lista_pessoas)

pesos=c(80, 65, 70, 58, 78, 70) 
alturas=c(1.70, 1.66, 1.65, 1.60, 1.76, 1.70)
IMC=pesos/alturas^2

lista_pessoas$IMC <- lista_pessoas$pesos / lista_pessoas$alturas^2

lista_pessoas[[1]][1]

lista_pessoas[[4]][3]

data("airquality")

lista<-lapply(airquality, function(x){mean(x)})
str(lista)
teste1 <- lista[4]
teste2 <- lista$Temp

x <- colMeans(chuvas)
mean(x)

mediaslinhas <- rowMeans(chuvas)
max(mediaslinhas)
min(x)

sum( 4.707,  4.923,  5.100,  4.924,  5.186,  5.469,  4.742,  5.325,  4.234,  5.951)/10

aux<-chuvas["mun_81" , ]; aux<-sum(aux[1:10])#52.2
aux<-chuvas[81 , ]; aux<-sum(aux[1:10])#52.2
aux<-chuvas[81 , ]; aux<-sum(aux[seq(1,10,1)])
aux<-chuvas["mun_81" ]; aux<-sum(aux[1:10])
aux<-chuvas["mun_81" , ]; aux<-sum(aux[c(1:10)])

somaMunicipio <- rowSums(chuvas)
max(somaMunicipio)

somaDia <- colSums(chuvas)
min(somaDia)

sum(470.7,  492.3,  510.0,  492.4,  518.6,  546.9,  474.2,  532.5,  423.4,  595.1)