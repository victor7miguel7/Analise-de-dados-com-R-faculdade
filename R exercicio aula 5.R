View(airquality)
min(airquality$Ozone)
x <- airquality[complete.cases(airquality),]
min(x$Ozone[x$Month==5])

#Extraia o subconjunto do data frame em que os valores de Ozônio estão acima de 
#25 e os valores da temperatura (Temp) estão abaixo de 90. Qual é a média do 
#Solar.R nesse subconjunto?

#forma1
subconjunto <- x[x$Ozone > 25 & x$Temp < 90, ]
mean(subconjunto$Solar.R)
#forma2
teste <- subset(x, subset= Ozone > 25 & Temp < 90)
mean(teste$Solar.R)

nrow(x)


genomas <- as.data.frame(read.csv("https://www.dropbox.com/s/vgh6qk395ck86fp/genomes.csv?dl=1"))
View(genomas)

max(genomas$Chromosomes)
#cromossomosMaxQuarenta <- genomas$Chromosomes > 40
cromosAcima40 <- subset(genomas, Chromosomes > 40)
#print(cromossomosMaxQuarenta)

max(genomas$Plasmids)
#Selecione os organismos que contém plasmídeos e também possui mais de um cromossomo.
temPlasmTemCromos <- subset(genomas, subset = Plasmids > 0 & Chromosomes > 1 )

nrow(genomas)

#############################################################################

cancer_stats <- as.data.frame(read.csv("https://www.dropbox.com/s/g97bsxeuu0tajkj/cancer_stats.csv?dl=1"))

#cria subset so com mais casos famininos do que masculinos
MaisFem <- subset(cancer_stats, Male.Cases < Female.Cases)

#SELECIONA APENAS AS COLUNAS MASCULINAS
SoHomem <-cancer_stats[, c("Site", "Male.Cases", "Male.Deaths")]
#max(SoHomem$Male.Deaths/SoHomem$Male.Cases, na.rm = TRUE)
#CRIA UMA NOVA COLUNA COM A TAXA DE MORTALIDADE
SoHomem$Taxa.Mortalidade <- SoHomem$Male.Deaths/SoHomem$Male.Cases
min(SoHomem$Taxa.Mortalidade, na.rm = TRUE)#SELECIONA A MENOR TAXA

#SELECIONA APENAS AS COLUNAS FEMININAS
SoMulher <-cancer_stats[, c("Site", "Female.Cases", "Female.Deaths")]
#CRIA UMA NOVA COLUNA COM A TAXA DE MORTALIDADE
SoMulher$Taxa.Mortalidade <- SoMulher$Female.Deaths/SoMulher$Female.Cases
max(SoMulher$Taxa.Mortalidade, na.rm = TRUE)#SELECIONA MAIOR TAXA

#linhaPiorTaxa <- subset(SoMulher, Taxa.Mortalidade == 0.8181141)

##########################################################################

mouse.info <- data.frame(
  mouse.color = c( "purple", "red", "yellow","brown"),
  mouse.weight = c(23, 21, 18, 26),
  stringsAsFactors = FALSE
)

str(mouse.info)

print(mouse.info[4,1])