getwd()

## Questão 01
# Use os dados abaixo para gerar os gráfico. Para o segundo gráfico (barras), 
# use a escala logarítmica e as seguintes cores: "#E6E6E6", "#666666". 
# Além disso, os gráficos podem ser organizados com a função layout().

### Gráfico 01
MRT_1F <-c(517.1468515630205, 85.13094142168089, 30.333207896694553, 12.694776264558937, 3.3041601673945418, 1.1823111717498882, 1.1892293502386786)
MRT_3F <-c(156.68929936163462, 11.540837783562276, 0.4512835621696538, 0.4509797929766453, 0.4502068233039181, 0.4496185276300172, 0.4543157082191288)
MRT_5F <-c(83.90319666471157, 0.3068151086494968, 0.30522314133037304, 0.3072588968084928, 0.30655265997285697, 0.3055812715727718, 0.3053297166713006)
MRT_10F <-c(29.55430642951759, 0.19832832665772515, 0.1971923924717474, 0.19796648905716516, 0.19615594370806338, 0.2034569237883263, 0.19617420889447737)
MRT_15F <-c(11.317736530583566, 0.167364215666193, 0.16172168266811013, 0.16701085329580515, 0.1598052657153692, 0.1645934043532696, 0.16216563797118075)
MRT_sem_F <-c(11.93430909937736, 0.6095414637034009, 0.6060645101029295, 0.612167181646899, 0.6146761002685637, 0.6096747087200697, 0.6125810476877268)
clock <- c(0.1, 0.5, 1, 1.5, 2, 2.5, 3)

plot(clock,
     MRT_1F,
     type = "o",
     pch = 4,
     col = "black",
     xlab = "Time between Things request (seconds)",
     ylab = "Response Time (sec.)")
lines(clock, MRT_3F, type="o", pch=11, col="yellow")
lines(clock, MRT_5F, type="o", pch=1, col="red")
lines(clock, MRT_10F, type="o", pch=2, col="blue")
lines(clock, MRT_15F, type="o", pch=5, col="pink")
lines(clock, MRT_sem_F, type="o", pch=4, col="green")
legend("topright",
       pch = c(4,11,1,2,5,4),
       col = c("black","yellow","red","blue","pink","green"),
       legend = c("1 Fog","3 Fogs","5 Fogs","10 Fogs","15 Fogs","w/o Fogs"),
       lty = 1)

### Gráficos 02 a 06
g2 <- rbind(MRT_sem_F, MRT_1F)
g3 <- rbind(MRT_sem_F, MRT_3F)
g4 <- rbind(MRT_sem_F, MRT_5F)
g5 <- rbind(MRT_sem_F, MRT_10F)
g6 <- rbind(MRT_sem_F, MRT_15F)

#par(mfrow = c(3,2))
layout(matrix(c(1,2,3,4,5,6), 3, 2, byrow = TRUE))

barplot(g2,
        beside = T,
        xlab = "Time between Things request",
        ylab = "Response Time (s)",
        names.arg = clock,
        log = "y",
        col = c("#E6E6E6", "#666666"))
legend("topright", col=c("#E6E6E6", "#666666"),
       legend=c("w/o Fog","1 Fog"), pch=c(15,15))

barplot(g3,
        beside = T,
        xlab = "Time between Things request",
        ylab = "Response Time (s)",
        names.arg = clock,
        log = "y",
        col = c("#E6E6E6", "#666666"))
legend("topright", col=c("#E6E6E6", "#666666"),
       legend=c("w/o Fog","3 Fogs"), pch=c(15,15))

barplot(g4,
        beside = T,
        xlab = "Time between Things request",
        ylab = "Response Time (s)",
        names.arg = clock,
        log = "y",
        col = c("#E6E6E6", "#666666"))
legend("topright", col=c("#E6E6E6", "#666666"), 
       legend=c("w/o Fog","5 Fogs") ,pch=c(15,15))

barplot(g5,
        beside = T,
        xlab = "Time between Things request",
        ylab = "Response Time (s)",
        names.arg = clock,
        log = "y",
        col = c("#E6E6E6", "#666666"))
legend("topright", col=c("#E6E6E6","#666666"),
       legend=c("w/o Fog","10 Fogs"), pch=c(15,15))

barplot(g6,
        beside = T,
        xlab = "Time between Things request",
        ylab = "Response Time (s)",
        names.arg = clock,
        log = "y",
        col = c("#E6E6E6", "#666666"))
legend("topright", col=c("#E6E6E6", "#666666"), 
       legend=c("w/o Fog","15 Fogs"), pch=c(15,15))


## Questão 02
# Para a tabela abaixo que classifica a qualidade de refeição de acordo com 
# categorias de preços crie um gráfico de barras empilhadas
# par(mfrow = c(1,1))
quality <- c("Good","Very Good","Excelent")
mealPrice <- c("$10-19","$20-29","$30-39","$40-49")
l1 <- c(53.8,33.9,2.6,0)
l2 <- c(43.6,54.2,60.5,21.4)
l3 <- c(2.6,11.9,36.8,78.6)
dados <- rbind(l1,l2,l3)

barplot(dados,
        beside = T,
        names.arg = mealPrice,
        col = rainbow(3),
        ylab = "Percentuais",
        ylim = c(0,100),
        main = "Meal Price" )
legend("topleft",legend=quality, col=rainbow(3), pch=15, title="quality Rating")

## Questão 03
# Faça o histograma das temperaturas do mês de maio do dataset airquality. 
# No entanto, primeiro converta as temperaturas para graus Celsius através da 
# expressão °C = (°F − 32) / 1.8. Após isso, gere o histograma.
# Também  adicione título, nomes de eixos, cor e curva de densidade

library (dplyr)

dados <- airquality %>% filter(Month == 5)
dados$Temp <- (dados$Temp - 32) /1.8

hist(dados$Temp,
     col = rainbow(10),
     density = 80,
     probability = T,
     main = "Temperaturas - Maio",
     ylab = "Densidade",
     xlab = "Temperaturas")
lines(density(dados$Temp), col="black")

## Questão 04
# Crie um gráfico de pizza com a porcentagem da tabela total de vendas por país.
# Use o dataset abaixo. Inclua a porcentagem de cada fatia, as cores das fatias
# e o nome do gráfico. 
# Adicionalmente, use o comando legend() para incluir a legenda do gráfico.

sales <- read.table("https://training-course-material.com/images/8/8f/Sales.txt",header=TRUE)
qtd <- nrow(sales)
pct <- round(sales$SALES / sum(sales$SALES) * 100,2)
lbls <- paste(pct, "%", sep="")

pie(x = sales$SALES,
    labels = lbls,
    main = "Vendas por País",
    col = rainbow(qtd))
legend("bottomleft", col=rainbow(qtd), pch=15, legend=sales$COUNTRY, title="País")

## Questão 05
# Utilize os dados de contagens de insetos em unidades experimentais agrícolas 
# tratados com inseticidas diferentes disponível no R em InsectSprays e construa
# um boxplot sem a presença dos outliers para os 6 tipos de inseticidas. 
# Coloque título e nomes de eixos adequados. 
# Além disso, as caixas devem ser preenchidas com a cor “yellow”
dados <- InsectSprays
boxplot(dados$count ~ dados$spray,
        xlab = "Tipos de Inseticidas",
        ylab = "Quantidade",
        col = "yellow",
        outline = F,
        main = "Quantidade de Insetos por Tipo de Inseticida")

## Questão 06
# Gere os gráficos abaixo a partir dos seguintes dados:
# monitoringCloudData_0.1.csv, monitoringCloudData_0.5.csv, 
# monitoringCloudData_1.csv e monitoringCloudData_NONE.csv. 
# Esse dados consistem de informações de monitoramento dos recursos de uma 
# máquina virtual. Basicamente, é necessário gerar gráficos de linha do tempo 
# de coleta dos recursos (currentTime) em relação a memória usada (usedMemory).
# Porém, é necessário ajustar a coluna “currentTime” para que o tempo fique 
# contínuo e a coluna “usedMemory” para que todas as informações fiquem 
# megabytes. Por fim, use a função layout() para organizar os gráficos
install.packages("anytime")
library(anytime)
library(tidyr)
library(dplyr)

f1 <- read.csv2(file="monitoringCloudData_NONE.csv", sep=",")
f2 <- read.csv2(file="monitoringCloudData_0.1.csv", sep=",")
f3 <- read.csv2(file="monitoringCloudData_0.5.csv", sep=",")
f4 <- read.csv2(file="monitoringCloudData_1.csv", sep=",")

# Convertendo tudo de usedMemory para MB
f1 <- f1 %>%  separate(col="usedMemory", into=c("usedMemory","umUnit"), sep=-2)
f2 <- f2 %>%  separate(col="usedMemory", into=c("usedMemory","umUnit"), sep=-2)
f3 <- f3 %>%  separate(col="usedMemory", into=c("usedMemory","umUnit"), sep=-2)
f4 <- f4 %>%  separate(col="usedMemory", into=c("usedMemory","umUnit"), sep=-2)

f1$usedMemory <- as.double(f1$usedMemory) 
f2$usedMemory <- as.double(f2$usedMemory) 
f3$usedMemory <- as.double(f3$usedMemory) 
f4$usedMemory <- as.double(f4$usedMemory) 

f1$usedMemory[f1$umUnit=="GB"] <- f1$usedMemory[f1$umUnit=="GB"]*1024
f2$usedMemory[f2$umUnit=="GB"] <- f2$usedMemory[f2$umUnit=="GB"]*1024
f3$usedMemory[f3$umUnit=="GB"] <- f3$usedMemory[f3$umUnit=="GB"]*1024
f4$usedMemory[f4$umUnit=="GB"] <- f4$usedMemory[f4$umUnit=="GB"]*1024

# Convertendo as datas de currentTime para tempo continuo
f1$ct<-anytime(f1$currentTime)
f2$ct<-anytime(f2$currentTime)
f3$ct<-anytime(f3$currentTime)
f4$ct<-anytime(f4$currentTime)

f1$ct2 <- difftime(f1$ct, min(f1$ct), units = "hours")
f2$ct2 <- difftime(f2$ct, min(f2$ct), units = "hours")
f3$ct2 <- difftime(f3$ct, min(f3$ct), units = "hours")
f4$ct2 <- difftime(f4$ct, min(f4$ct), units = "hours")

layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
plot(f1$ct2,f1$usedMemory ,type="l",main="Memory Analysis (None Workload)",xlab="Time (hour)",ylab="Used Memory (MB)",font.main=2)
plot(f2$ct2,f2$usedMemory ,type="l",main="Memory Analysis (Workload of 0.1)",xlab="Time (hour)",ylab="Used Memory (MB)",font.main=2)
plot(f3$ct2,f3$usedMemory ,type="l",main="Memory Analysis (Workload of 0.5)",xlab="Time (hour)",ylab="Used Memory (MB)",font.main=2)
plot(f4$ct2,f4$usedMemory ,type="l",main="Memory Analysis (Workload of 1.0)",xlab="Time (hour)",ylab="Used Memory (MB)",font.main=2)

#Questao 7
#(10 pontos) Filtrando os conteúdos com apenas UM país de origem (coluna "country"), 
#utilize o Plotly para obter um gráfico de pizza dos 10 países com mais conteúdo na plataforma.

install.packages("plotly")
library(dplyr)
library(plotly)

dados <- read.csv("netflix_titles.csv")

dados_filtrados <- dados %>%
  filter(!is.na(country) & country != "") %>%
  filter(!grepl(",", country))

conteudos_por_pais <- dados_filtrados %>%
  count(country) %>%
  arrange(desc(n)) %>%
  slice(1:10)

plot_ly(conteudos_por_pais, labels = ~country, values = ~n, type = 'pie') %>%
  layout(title = 'Top 10 países com mais conteúdo na Netflix',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

#Questao 8
#Considerando os 10 países descritos na questão anterior, utilize o Plotly para 
#obter uma tabela com as colunas "País" e "Total de conteúdos". Para tal tabela, 
#é necessário que o cabeçalho esteja com o fundo da célula cinza e a letra em 
#branco, e que todos os textos das células estejam @centralizados.


df <- data.frame(conteudos_por_pais)
colnames(df) <- c("País", "Total de conteúdos")
pais <- as.list(df$País)
total_conteudos <- as.list(df$`Total de conteúdos`)

plot_ly(
  type = "table",
  header = list(
    values = names(df),
    fill = list(color = "gray"),
    font = list(color = "white"),
    align = "center"
  ),
  cells = list(
    values = list(pais, total_conteudos),
    align = "center"
  )
)

#Questao 9
#Utilizando o Plotly, obtenha um gráfico de linha com pontos (como na imagem abaixo) 
#contendo a quantidade de conteúdo por década do Netflix, onde o eixo X representa as 
#décadas e o eixo Y a quantidade de conteúdo disponível na plataforma
#(coluna "release_year"). Use uma linha azul para representar as séries e uma linha 
#amarela representando os filmes. (Obs: considerar como década o período que compreende 
#os anos de XXX0 à XXX9, por exemplo, 2000 à 2009)

dados$decada <- as.numeric(substr(dados$release_year, 1, 3)) * 10

dados_por_decada <- dados %>%
  filter(!is.na(release_year)) %>%
  group_by(decada, type) %>%
  summarise(contagem = n()) %>%
  ungroup()

plot_ly(dados_por_decada, x = ~decada, y = ~contagem, color = ~type,
        type = 'scatter', mode = 'lines+markers', colors = c('yellow', 'blue')) %>%
  layout(#title = 'Quantidade de Conteúdo por Década na Netflix',
    xaxis = list(title = 'Década'),
    yaxis = list(title = 'Qnd. Conteúdo'),
    legend = list(title = 'Tipo de Conteúdo', 
                  x = 1, y = 0.95))

#Questao 10
#Utilizando o Plotly, obtenha um gráfico que representa a quantidade de filmes 
#lançados de um determinado gênero entre os anos de 2000 e 2010. Este gráfico 
#deve ser de barras lado-a-lado (como na imagem abaixo), com cada barra 
#representando a quantidade de filmes dos gêneros 
#"Dramas", "Action & Adventure" e "Comedies" lançados naquele determinado ano. 
#(Obs: Para filmes com múltiplos gêneros, considerar apenas o primeiro descrito 
#na coluna "listed_in").

filmes20002010 <- dados %>%
  filter(type == "Movie", release_year > 2000, release_year < 2010)

filmes20002010$primeiro_genero <- sub(",.*", "", filmes20002010$listed_in)

filmes20002010$primeiro_genero <- factor(filmes20002010$primeiro_genero,
                                         levels = c("Dramas", "Action & Adventure", "Comedies"))


filmes20002010$primeiro_genero <- recode(filmes20002010$primeiro_genero, "Dramas" = "Drama", "Action & Adventure" = "Ação e Aventura", "Comedies" = "Comédia")

dados_agrupados <- filmes20002010 %>%
  group_by(release_year, primeiro_genero) %>%
  summarise(contagem = n()) %>%
  filter(primeiro_genero %in% c("Drama", "Ação e Aventura", "Comédia")) %>%
  ungroup()

plot_ly(dados_agrupados, x = ~release_year, y = ~contagem, color = ~primeiro_genero,
        type = 'bar', width = 0.5) %>%
  layout(title = 'Quantidade de Filmes por Gênero entre 2000 e 2010',
         xaxis = list(title = 'Ano de Lançamento'),
         yaxis = list(title = 'Qnt. de Lançamentos'),
         barmode = 'group',
         legend = list(title = 'Gênero'))
