y <- 5

mult <- function(x){
  return(x*y)
}

mult(10)
##############################################################################
prisoes <- function(estados, tiposPrisoes) {
  # Verifica se os estados fornecidos são válidos
  if (!all(estados %in% rownames(USArrests))) {
    return("Estado Inválido")
  }
  
  # Verifica se os tipos de prisões fornecidos são válidos
  if (!all(tiposPrisoes %in% colnames(USArrests))) {
    return("Tipo de Prisão Inválido")
  }
  
  # Calcula a soma das prisões para os estados e tipos de prisões fornecidos
  total_prisoes <- sum(USArrests[estados, tiposPrisoes])
  
  return(total_prisoes)
}

prisoes (estados="Oregon", tiposPrisoes=c("Rape","Murder"))

prisoes (estados=c("Nevada", "Washington"), tiposPrisoes=c("UrbanPop", "Assault"))

prisoes (estados="Pernambuco", tiposPrisoes=c("Rape", "Mürder"))


prisoes(estados="Tennessee", tiposPrisoes=c("Rape","Murder"))

prisoes(estados=c("California ","Miami", "Arizona"), tiposPrisoes=("Assault"))

prisoes(estados=c("Pennsylvania","Mississippi", "Nebraska"), tiposPrisoes=c("Rape","UrbanPop","Assault"))

prisoes(estados=c("Vermont","Wisconsin", "Texas"), tiposPrisoes=c("Rape","Assalto"))




#####################################################################################

minhasNotas_1 <- function(Exe_1=0, VA_1=0, Exe_2=0, Proj=0, VA_2=0, VA_3=0, Opt=1, threshold=7) {
  
  # Substituir NAs por 0
  Exe_1[is.na(Exe_1)] <- 0
  Exe_2[is.na(Exe_2)] <- 0
  
  # Verificar se as notas estão dentro do intervalo correto (0 a 10)
  if (any(c(Exe_1, VA_1, Exe_2, Proj, VA_2, VA_3) < 0) || any(c(Exe_1, VA_1, Exe_2, Proj, VA_2, VA_3) > 10)) {
    stop("Erro: As notas devem estar no intervalo de 0 a 10.")
  }
  
  # Verificar se Opt está dentro do intervalo correto (1 a 4)
  if (Opt < 1 || Opt > 4) {
    stop("Erro: A opção 'Opt' deve ser um número entre 1 e 4.")
  }
  
  # Se Opt for 1, calcular a média da 1 VA
  if (Opt == 1) {
    if (VA_1 == 0) {
      return("Aluno não possui nota para a 1 VA")
    } else {
      media = ((mean(Exe_1) * 5) + (VA_1 * 5)) / 10
    }
  }
  
  # Se Opt for 2, calcular a média da 2 VA
  if (Opt == 2) {
    if (VA_2 == 0) {
      return("Aluno não possui nota para a 2 VA")
    } else {
      media = ((mean(Exe_2) * 2) + (Proj * 5) + (VA_2 * 3)) / 10
    }
  }
  
  # Se Opt for 3, calcular a média da 3 VA
  if (Opt == 3) {
    if (VA_3 == 0) {
      return("Aluno não possui nota para a 3 VA")
    } else {
      media = VA_3
    }
  }
  
  # Se Opt for 4, calcular a média final e verificar se o aluno foi aprovado ou está na final
  if (Opt == 4) {
    # Verificar se o aluno possui notas suficientes para calcular a média final
    if (any(c(VA_1, VA_2, VA_3) == 0) || Proj == 0) {
      return("Aluno não possui notas suficientes para calcular a média final.")
    } else {
      medias = c(((mean(Exe_1) * 5) + (VA_1 * 5)) / 10, ((mean(Exe_2) * 2) + (Proj * 5) + (VA_2 * 3)) / 10, VA_3)
      medias = sort(medias, decreasing = TRUE)
      media = (medias[1] + medias[2]) / 2
    }
  }
  
  # Verificar se a média está acima ou abaixo do threshold e retornar a mensagem apropriada
  if (media >= threshold) {
    mensagem = "Acima da Média"
  } else {
    mensagem = "Abaixo da Média"
  }
  
  # Se Opt for 4, incluir mensagem de aprovação ou final
  if (Opt == 4) {
    if (media >= threshold) {
      resultado = paste(media, "-- Aprovado!!")
    } else {
      resultado = paste(media, "-- Na Final!!")
    }
  } else {
    resultado = paste("Média da", ifelse(Opt == 1, "1VA:", ifelse(Opt == 2, "2VA:", "3VA:")), media, "--", mensagem)
  }
  
  return(resultado)
}

# Exemplo de uso:
minhasNotas_1(Exe_1=c(10,9,7,5,NA,NA), VA_1=3, Exe_2=c(7,8), Proj=4, VA_2=7,VA_3=9, Opt=1, threshold=7)


minhasNotas_1(Exe_1=c(10,9,7,5,10,4,NA,6,7,8,10), VA_1=8, Exe_2=c(8,5,7,8), Proj=c(7,5), VA_2=7, VA_3=9, Opt=1, threshold=8)

minhasNotas_1(Exe_1=c(10,9,7,5,NA,4,5,6,7,8,NA), VA_1=2, Exe_2=c(10,5.7,7.8,NA), Proj=4, VA_2=7, VA_3=9, Opt=2, threshold=5)

minhasNotas_1(Exe_1=c(10,9,7,5,10,4,5,6,7,8,10), VA_1=8, Exe_2=c(8,5,7,8), Proj=c(4,5), VA_2=7, VA_3=9, Opt=2, threshold=8)

minhasNotas_1(Exe_1=c(NA,NA,7.5,1,10,2,5,6,7,8,10), VA_1=4, Exe_2=c(7,11,8.8,5,7,8), Proj=4, VA_2=7, VA_3=9, Opt=4, threshold=7)

minhasNotas_1(Exe_1=c(10,9,7,5,10,4,5,6,7,8,10), VA_1=8, Exe_2=c(6,8,9,8,5,7,8), Proj=7, VA_2=7, VA_3=9, Opt=4, threshold=8)


minhasNotas_1(Exe_1=c(10,9,7,5,10,4,5,6,7,8,10), VA_1=8, Exe_2=c(8,5,7,8), Proj=4, VA_2=7, VA_3=9, Opt=2, threshold=8)