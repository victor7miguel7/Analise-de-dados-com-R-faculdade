setwd("C:/Users/victo/Documents")
dados <- read.csv("catsM.csv")

media_bwt <- mean(dados$Bwt)
print(media_bwt)

getwd()
############################################################################
caracois <- read.csv("Snail_feeding.csv",header=T, strip.white= T, na.strings= "")

media_caracois <- mean(caracois$Depth)
print(media_caracois)

# Padronizando os valores para "male" e "female"
caracois$Sex <- ifelse(caracois$Sex %in% c("male", "Male", "males"), "male", 
                       ifelse(caracois$Sex %in% c("female", "female s"), "female", NA))

unique(caracois$Sex)


#Criando uma subset apenas com os caracois femeas e pequenos, para dps ver
#a maior distância percorrida por eles

dados_filtrados <- subset(caracois, Sex == "female" & Size == "small")

maior_distancia <- max(dados_filtrados$Distance)
############################################################################

# Carregando os dados usando read.csv()
Sparrows <- read.csv("Sparrows.csv", header = TRUE)

# Exibindo os dados
View(Sparrows)

# Carregando os dados usando read.table() com delimitador de vírgula
Sparrows2 <- read.table("Sparrows.csv", header = TRUE, sep = ",")

# Exibindo os dados
View(Sparrows2)

unique(Sparrows2$Species)

sparrows_filtrados <- subset(Sparrows2, Species == "SSTS")


maior_cabeça <- max(sparrows_filtrados$Head)
MENOR_CABEÇA <- min(sparrows_filtrados$Head)
print(maior_cabeça)


# Identificando linhas duplicadas (incluindo a primeira ocorrência)
duplicados_incluindo_primeira <- Sparrows2[duplicated(Sparrows2) | duplicated(Sparrows2, fromLast = TRUE), ]

# Visualizando as linhas duplicadas, incluindo a primeira ocorrência
print(duplicados_incluindo_primeira)

unique(Sparrows2$Sex)

# Padronizando os valores para "Male" e "Female"
Sparrows2$Sex <- ifelse(Sparrows2$Sex %in% c("Male", "Males"), "Male", 
                        ifelse(Sparrows2$Sex %in% c("Female", "Femal", "Femal e"), "Female", NA))


female <- Sparrows[Sparrows$Sex == "Female",]
male <- Sparrows[Sparrows$Sex == "Male",]

print(mean(female$Tarsus)) #21.12007
print(mean(male$Tarsus)) #21.62668



print(which(is.na(Sparrows$Wing)))



# Encontrar as posições dos NA
na_positions <- which(is.na(Sparrows2$Wing))

# Valores para substituir os NA
substitutos <- c(59, 56.5, 57)

# Substituir os NA com os valores específicos
Sparrows2$Wing[na_positions] <- substitutos

print(mean(Sparrows2$Wing))

Sparrows_Ordenado <- Sparrows2[order(Sparrows2$Wing, Sparrows2$Head), ]


