summary(vetor01)#mostra um resumo matematico do vetor
321 / 15000
sd(vetor01, na.rm = TRUE)#parametro na.rm remove os valores nulos
mean(vetor01, na.rm = TRUE)

table(vetor01 > 7 & vetor01 < 8) #metodo table realiza contagem do vetor
table(vetor01 > 9 | vetor01 < 1)

vetor01 <- vetor01[!is.na(vetor01)]#ok
vetor01 <- vetor01[is.na(vetor01)]#false
vetor01 <- !is.na(vetor01)#false
vetor01[!is.na(vetor01)] -> vetor01#ok
vetor01 <- vetor01[-which(is.na(vetor01))]#ok
vetor01 <- vetor01[which(is.na(vetor01))]#false

load("C:/Users/victo/Downloads/vetor (1).RData")
summary(vetor01)
vetor01