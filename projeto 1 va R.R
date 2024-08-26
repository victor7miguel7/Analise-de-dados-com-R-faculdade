install.packages("stringr")

# Carregar o pacote stringr
library(stringr)

# Concatenando strings
nome1 <- "Borussia"
nome2 <- "Dortmund"

nomes_juntos <- str_c(nome1, nome2, sep = " ")
print(nomes_juntos)

str_c(c("Real", "River"), c("Madrid","Plate"), sep = "_") # usando vetor

str_c(c("Seleção", "Brasileira", "de", "Futebol"), collapse = " ") # Juntando varias strings


# Ordenando strings 

Nomes <- c("Victor", "Gustavo", "Thiago")

str_sort(Nomes) # Coloca em ordem alfabética

str_sort(Nomes, decreasing = TRUE) # oposto da ordem alfabética


# Interpolando strings

primeiro_nome <- c("Cristiano", "Lionel", "Neymar")
sobrenome <- c("Ronaldo", "Messi","Jr")
str_glue("Meu nome é {primeiro_nome}. {primeiro_nome} {sobrenome}.")#Ex1

idade_minima <- 18
resto_idade <- c(21,19,14)

str_glue("Meu nome é {primeiro_nome} {sobrenome}, e tenho {idade_minima + resto_idade} anos.")


# Detectando presença
data <- read.csv("IMDB Dataset.csv", sep = ",")

str_detect(data$review[1:100], pattern = "good" )

indices <- str_which(data$review[1:100], pattern = "good")#mostra os indices onde é true
print((indices))

data$review[indices] # Traz o conteudo da coluna review com indices true


# Criando subsets

subset <- str_subset(data$review[1:20], pattern = "good" )
print(subset)

# Reescrevendo string com a função str_replace
str_replace(data$review[5], pattern = "good", replacement = "BOM" )


#Visualizando com str

str_view(subset, pattern = "good")

# Obtendo parte fixa das strings

s <- c("01-Feminino", "02-Masculino", "03-Indefinido")
str_sub(s, start = 4) 


# Utilizando REGEX
jogadores <- c(
  'Gareth Bale', 'Diego Souza', 'Rafael Leão',
  'Luka Modric', '...'
)

sobrenomes <- str_extract(jogadores, '[:alpha:]+$')
print(sobrenomes)

# Duplicando strings

string_multiplicada <- str_dup(nome1, 3)
print(string_multiplicada)

# Criando um nome através do str_c e do str_dup
silaba_duplicada <- str_dup("ta",3)
resto_palavra <- "ravô"

str_c(silaba_duplicada, resto_palavra)

#Quantas vzs se repete um objeto dentro da string
str_count("paralelepipedo", pattern = "e")

# Substituir um padrão usando str_replace
texto <- "A cor do carro é vermelha"
result <- str_replace(texto, "vermelha", "azul")
print(result)  

# Dividir uma string usando str_split
sport <- "Sport-Club-do-Recife"
result <- str_split(sport, "-")
print(result)  

# Descobrir localização de uma substring
texto2 <- "Onde está localizada a palavra Manchester ?"
result <- str_locate(texto2, pattern = "Manchester")
print(result) 

# Extrair uma substring usando str_sub
texto3 <- "Elimine o menor clube entre os 2: Sport | Santa Cruz"
result <- str_sub(texto3, start = 43, end = 55)
print(result)


# Remover espaços em branco no começo e fim do texto usando str_trim
text <- "   Atlético de Madrid   "
result <- str_trim(text)
print(result)  

# Verificar se uma string contém um padrão usando str_detect
text <- "Você viu o filme?"
result <- str_detect(text, "filme")
print(result)  # TRUE


# Transformar para maiúsculas usando str_to_upper
text <- "tudo em minúsculas"
result <- str_to_upper(text)
print(result)  # "TUDO EM MINÚSCULAS"

# Transformar para minúsculas usando str_to_lower
text <- "TUDO EM MAIÚSCULAS"
result <- str_to_lower(text)
print(result)  # "tudo em maiúsculas"

# Contando tamanho

str_length("Paralelepipedo")
