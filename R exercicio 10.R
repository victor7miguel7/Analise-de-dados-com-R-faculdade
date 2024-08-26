install.packages("dplyr")
library(dplyr)

df=data.frame(Theoph)

df$Dose

#Qual o comando seleciona apenas a coluna Dose de df ? 
#Ao submeter a resposta remova todos os espaços em branco.
select(df, Dose)
df%>%select

#Qual o comando apresenta os dados para as doses maiores que 5 mg/kg ? 
#Ao submeter a resposta remova todos os espaços em branco.

filter(df, Dose > 5)
df%>%filter(Dose>5)

#Qual o comando seleciona as linhas de 10-20 ?Dica: use o "slice". 
#Ao submeter a resposta remova todos os espaços em branco.

df%>%slice(10:20)

#Qual comando apresenta os dados para as doses maiores que 5 e cujo o tempo
#desde a administração do medicamento (Time) é maior que a média do mesmo? 
#Use apenas um único comando.  Ao submeter a resposta remova todos os espaços em branco.

df%>%filter(Dose>5&Time>mean(Time))

# Qual comando organizar df por peso (decrescente) ?
df%>%arrange(desc(Wt))

# Qual comando organizar df por peso (crescente) e tempo (decrescente) ?

df%>%arrange(Wt,desc(Time))

# Qual comando cria uma nova coluna chamada "tendencia" que é igual à 
# Time-mean(Time)?

df%>%mutate(tendencia=Time-mean(Time))

# Qual comando apresenta a maior concentração de teofilina ? Não use nenhum 
# nome para a coluna resultante

df%>%summarise(max(conc))

# Para os exercícios abaixo, usaremos dois conjuntos de dados relacionados aos
# tempos de atraso de vôos do Bureau of Transportation Statistics dos EUA 
# (X673598238_T_ONTIME_REPORTING e L_UNIQUE_CARRIERS.csv_). 
# Para carregar os dados é importante que vocês usem os seguintes argumentos: 
# quote="\"", sep = "," . Note que a extensão ".csv_" está correta. 
# O data frame do arquivo (X673598238_T_ONTIME_REPORTING)  possui apenas 
# informações da companhia aérea por código. No entanto, queremos saber os
# nomes das companhias aéreas. Assim, faça o merge dos datasets 
# "X673598238_T_ONTIME_REPORTING" e "L_UNIQUE_CARRIERS.csv_" através das
# colunas "OP_UNIQUE_CARRIER" e "Code". Em seguida, responda as questões abaixo.

X673598238_T_ONTIME_REPORTING <- read.csv("673598238_T_ONTIME_REPORTING.csv", quote="\"", sep=",")
L_UNIQUE_CARRIERS <- read.csv("L_UNIQUE_CARRIERS.csv_", quote="\"", sep=",")

merged_df <- merge(X673598238_T_ONTIME_REPORTING, L_UNIQUE_CARRIERS, by.x="OP_UNIQUE_CARRIER", by.y="Code")

getwd()


# Qual companhia teve o maior atraso ?
merged_df %>% summarise(max_delay = max(DEP_DELAY_NEW, na.rm = TRUE)) #1667
merged_df%>%arrange(desc(DEP_DELAY_NEW)) #American Airlines Inc.

# Qual companhia atrasa mais na média ?
media_atraso <- merged_df %>%
  group_by(Description) %>%
  summarise(media = mean(DEP_DELAY_NEW, na.rm = TRUE)) %>%
  arrange(desc(media))

#Qual companhia teve a maior proporção dos atrasos? 
#As linhas que não possuem informações dos atrasos precisam ser removidas.
cleaned_df <- merged_df %>% filter(!is.na(DEP_DELAY_NEW))

proportion_delayed <- cleaned_df %>%
  group_by(Description) %>%
  summarise(
    total_flights = n(),
    delayed_flights = sum(DEP_DELAY_NEW > 0, na.rm = TRUE),
    proportion_delayed = delayed_flights / total_flights
  ) %>%
  arrange(desc(proportion_delayed))

# Você está encarregado de analisar um conjunto de dados que contém casos de 
# tuberculose (TB) relatados entre 1995 e 2013, ordenados por país, idade e sexo.
# O recurso mais exclusivo desses dados é o seu sistema de codificação. 
# As colunas de três a vinte e três codificam quatro partes separadas de 
# informações em seus nomes de coluna: 
# (i) As três primeiras letras de cada coluna indicam se a coluna contém casos 
# novos ou antigos de TB. 
# (ii) As próximas duas letras descrevem os tipos de casos que estão sendo contados. 
# (iii) A sexta letra descreve o sexo dos pacientes com tuberculose. 
# Os números restantes descrevem a faixa etária dos pacientes com TB. 
# Carregue o conjunto de dados de http://stat405.had.co.nz/data/tb.csv como um
# novo dataframe chamado TB. 
# Observe que o conjunto de dados TB é desordenado de várias maneiras. 
# Primeiramente, remova a coluna "new_sp" que contém o total de casos. 
#Em seguida, mova os valores das colunas 3 até 22 para duas colunas chamadas 
#de “Código” e "N_casos". Dica use o gather (). 
#Após isso, divida os conteúdos da variável “Código” em cada sublinhado(”_”). 
#Dica use o separate () e chame as colunas resultantes de "caso", "tipo” e "sexofaixa”. 
#Por fim, divida a variável que contém o sexo e a faixa etária ("sexofaixa”) 
#de modo a criar uma coluna de "sexo" e uma coluna de "faixa". 
#De posse do data frame resultante, responda as questões abaixo.

install.packages("tidyr")
library(tidyr)

# Carregar o conjunto de dados
TB <- read.csv("http://stat405.had.co.nz/data/tb.csv")
TB <- select(TB, -new_sp)

TB <- TB %>%
  gather(key = "Código", value = "N_casos", 3:22, na.rm = TRUE)

TB <- TB %>%
  separate(Código, into = c("caso", "tipo", "sexofaixa"), sep = "_")

TB <- TB %>%
  separate(sexofaixa, into = c("sexo", "faixa"), sep = 1)

#Qual foi a quantidade de casos para a Tailândia (TH) de pessoas do sexo Masculino?
TB %>%
  filter(iso2 == "TH", sexo == "m") %>%
  summarise(total_casos = sum(N_casos)) #223309

#Qual a proporção de casos para os estados unidos (US) ?
#Não considerar valores NAs da coluna "N_casos".

us <- TB%>% filter(iso2 == "US")

sum(us$N_casos)/sum(TB$N_casos) #0.003801693

#Qual a quantidade de casos para a faixa etária 2534 do sexo feminino?

TB %>% filter(faixa == "2534" & sexo  == "f" ) %>% summarise(total_casos = sum(N_casos))

#  Qual foi a quantidade de casos para a década de 2000 ? 
#  A década de 2000, também referida como anos 2000, compreende o período de tempo 
#  entre 1 de janeiro de 2000 e 31 de dezembro de 2009.

TB %>% filter(year >= 2000) %>% summarise(total_casos = sum(N_casos))



