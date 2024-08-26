library(stringr)

texto <- "João tem 30 anos e Maria 25. Eles têm números de telefone diferentes: João - (123) 456-7890 e Maria - 098-765-4321. Além disso, eles têm endereços de e-mail diferentes: joao@example.com e maria@email.com. Ambos nasceram em 1990. João trabalha na empresa XYZ, enquanto Maria é freelancer. Pedro tem 28 anos e possui o número (111) 222-3333 e o e-mail pedro@example.com. Ana, com 29 anos, tem o telefone 999-888-7777 e o e-mail ana@example.com"

pattern = "\\b\\d+ anos\\b"

result <- str_extract_all(texto, pattern)[[1]]
print(result)

#################################################################

pattern = "\\(\\d{3}\\) \\d{3}-\\d{4}|\\d{3}-\\d{3}-\\d{4}"

result <- str_extract_all(texto, pattern)[[1]]
print(result)

###############################################################

pattern = "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}"

result <- str_extract_all(texto, pattern)[[1]]
print(result)

################################################################

install.packages("ggplot2")
library(ggplot2)
library(dplyr)

View(diamonds)
help(diamonds)

diamonds %>% 
  filter(cut=="Very Good" & carat>0.7) %>%
  summarise(média = mean(price))


#########################################################

novodf <- diamonds %>% filter(carat>0.5) %>% arrange((price))

########################################################

cut_premium <- diamonds %>% filter(cut == "Premium")
proporcao <- 13791/53940
print(proporcao)

########################################################

install.packages("AER")
library(AER)

data("Fertility")
help(Fertility)
str(Fertility)

Fertility%>%select(age,work)%>%slice(35:50)

#####################################################

morekids_work_df <- Fertility%>% filter(morekids == "yes" & work > 30)

proporcao2 <- 26278 / 254654
print(proporcao2)

#####################################################
# Filtre o subconjunto de mulheres entre 22 e 24 anos e determine à proporção 
# que tiveram menino como primogênito (gender1). Note que 22 e 24 deve ser incluído.

m_22_24 <- Fertility %>% filter(age >21 & age < 25)
prim_masc <- m_22_24 %>% filter(gender1 == "male")
proporcao3 <- 7567/ 15024
print(proporcao3)

###########################################################
library(tidyr)
catfish <- read.csv("Catfish.csv")
str(catfish)
treatment <- read.csv("Treatment.csv")
str(treatment)

catfish <- catfish %>% 
  gather(key="Month",value="Weight",March, April) %>%
  unite(col="Species", Genus, Species, sep=".")

Catfish_Treatment <- merge(x = catfish, 
                           y = treatment, 
                           by = "Tank",
                           all = T)
str(Catfish_Treatment)
View(Catfish_Treatment)

#####################################################################
#Qual o peso médio dos bagres do sexo masculino considerando o mês de Abril e o tratamento 2?
unique(Catfish_Treatment$Tank)

Catfish_Treatment %>% filter(Sex == "Male", Month == "April", Food == "Treatment2") %>% summarise(media = mean(Weight))

#################################################################
#Qual tratamento e qual tanque possuem mais bagres com peso maior ou igual a 80?
unique(Catfish_Treatment$Tank)

catfish_clean <- Catfish_Treatment %>% filter(Weight >= 80) 
# Calcula a soma dos pesos por tanque
soma_pesos_por_tanque <- catfish_clean %>%
  group_by(Tank) %>%
  summarise(soma_peso = sum(Weight))

# Encontra o tanque com a maior soma de pesos
tanque_maior_soma <- soma_pesos_por_tanque %>%
  filter(soma_peso == max(soma_peso))

# Exibe o resultado
print(tanque_maior_soma)

#Descobrindo maior tratamento com bagres +80kg
soma_pesos_por_tratamento <- catfish_clean %>%
  group_by(Food) %>%
  summarise(soma_peso2 = sum(Weight))

#############################################################

agora <- read.csv("agora.csv")
View(agora)
'''
# Substituir vários padrões
agora$Aprovação <- str_replace_all(agora$Aprovação, 
                                   pattern = c( "Não 1" = "0"))

unique(agora$Aprovação)

# Transformando em numerico
agora$Aprovação <- as.numeric(agora$Aprovação)

Taxa_aprovacao <- agora %>%
  group_by(Região) %>%
  summarise(soma_apv = sum(Aprovação))'''

#usando a coluna certa
Taxa_aprovacao2 <- agora %>%
  group_by(Região) %>%
  summarise(soma_apv = sum(Capital.Aprov.))

str(agora)



################################################
so_nordeste <- agora %>% filter(Região == "NE")

Cap_total_recursos <- so_nordeste %>%
  group_by(Instituição) %>%
  summarise(soma_apv = sum(Total.Aprovado))  

Cap_total_recursos <- Cap_total_recursos %>% 
  arrange(desc(soma_apv))  # Ordena o data frame pelo valor de soma_apv em ordem decrescente

############################################

unique(so_nordeste$Recomendação.CA)

# Substituir vários padrões
so_nordeste$Recomendação.CA <- str_replace_all(so_nordeste$Recomendação.CA, 
                                   pattern = c( "Recomendada" = "1"))
# Como "Não Recomendada" vai virar "Não 1", agr será transformado em 0

so_nordeste$Recomendação.CA <- str_replace_all(so_nordeste$Recomendação.CA, 
                                               pattern = c( "Não 1" = "0"))

# Transformando em numerico
so_nordeste$Recomendação.CA <- as.numeric(so_nordeste$Recomendação.CA)


Taxa_reprovacao <- so_nordeste %>% filter(Aprovação == 0) %>%
  group_by(Instituição) %>%
  summarise(soma_reco = sum(Recomendação.CA)) %>% 
  arrange(desc(soma_reco))

