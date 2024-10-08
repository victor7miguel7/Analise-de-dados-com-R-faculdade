# Carregar as bibliotecas necessárias
library(ggplot2)
library(reshape2)

# Carregar o dataset VADeaths
data("VADeaths")
View(VADeaths)
# Converter o dataset para formato longo para ggplot
VADeaths_long <- melt(VADeaths)

# Criar o gráfico de barras empilhadas
ggplot(VADeaths_long, aes(x = Var2, y = value, fill = Var1)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Mortalidade em VA por Grupos de Idade e Localização",
       x = "Grupos por localizaçao", 
       y = "Taxa de Mortalidade",
       fill = "Faixa de Idades") +
  scale_fill_brewer(palette = "Set1") +  # Paleta de cores
  theme_minimal()

##################################################

# Dados dos estágios da doença
estagios <- c("moderado", "leve", "leve", "severo", "leve", "moderado", "moderado", 
              "moderado", "leve", "leve", "severo", "leve", "moderado", "moderado", 
              "leve", "severo", "moderado", "moderado", "moderado", "leve")

# Contagem das ocorrências de cada estágio
contagem <- table(estagios)

# Cálculo das porcentagens
porcentagem <- round(100 * contagem / sum(contagem), 1)

# Cores para as fatias
cores <- c("lightblue", "lightgreen", "salmon")

# Criação do gráfico de pizza
pie(contagem, 
    labels = paste(names(contagem), "-", porcentagem, "%"),  # Nomes com porcentagem
    col = cores, 
    main = "Distribuição dos Estágios da Doença")

# Adicionar legenda ao gráfico
legend("topright", legend = names(contagem), fill = cores, title = "Estágios")

########################################3

# Corrigir o caminho do arquivo
dados <- read.csv("C:/Users/victo/Downloads/flu.csv", header = TRUE, sep = ",")

# Visualizar as primeiras linhas do dataset
head(dados)
# Verificar o tipo de dados da coluna
str(dados)

# Plotar o histograma com curva de densidade
hist(dados$age, freq = FALSE, col = "lightblue", main = "Histograma das Idades das Mortes",
     xlab = "Idade", ylab = "Densidade")

# Adicionar a curva de densidade
lines(density(dados$age), col = "red", lwd = 2)

#### pt 2
# Definir o tamanho da amostra
n <- 35

# Criar 200 amostras da população e calcular suas médias
set.seed(123)  # Definir uma seed para reprodutibilidade
medias_amostras <- replicate(200, mean(sample(dados$age, size = n, replace = TRUE)))

# Visualizar as primeiras 6 médias
head(medias_amostras)



# Plotar o histograma das médias das amostras
hist(medias_amostras, freq = FALSE, col = "lightgreen", main = "Histograma das Médias Amostrais (n = 35)",
     xlab = "Média das Amostras", ylab = "Densidade")

# Adicionar a curva de densidade
lines(density(medias_amostras), col = "blue", lwd = 2)


############################################################

# Ver o diretório de trabalho atual
getwd()

load("bdims.RData")

# Carregar o dataset
load("bdims.RData")

# Filtrar as mulheres
mulheres <- subset(bdims, sex == 0)

# Calcular o intervalo de confiança de 98,5%
intervalo_conf <- t.test(mulheres$hgt, conf.level = 0.985)

# Visualizar o intervalo de confiança
intervalo_conf$conf.int

########################

sd <- 0.5
N <- 300
erro <- 0.1
nc <- (1-0.99)/2
n <- (qnorm(nc,lower.tail = F)^2*sd^2*N)/((erro^2*(N-1))+(qnorm(nc,lower.tail= F)^2*sd^2))
n
##################################

# Definir os parâmetros
media <- 400       # Média do consumo (gramas)
desvio_padrao <- 45 # Desvio padrão (gramas)
limite <- 500      # Limite que queremos analisar

# Calcular o z-score
z <- (limite - media) / desvio_padrao

# Calcular a proporção acumulada abaixo de 500 gramas
proporcao_acumulada <- pnorm(z)

# Calcular a proporção acima de 500 gramas
proporcao_acima <- 1 - proporcao_acumulada

# Converter para porcentagem
proporcao_porcentagem <- proporcao_acima * 100

# Exibir o resultado com duas casas decimais e o símbolo de porcentagem
sprintf("%.2f%%", proporcao_porcentagem)




