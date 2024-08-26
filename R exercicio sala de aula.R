piratesurvey <- data.frame(
  name = c("Astrid", "Lea", "Sarina", "Remon", "Letizia", "Babice", "Jonas", "Wendy", "Niveditha", "Gioia"),
  sex = c("F", "F", "F", "M", "F", "F", "M", "F", "F", "F"),
  age = c(30, 25, 25, 29, 22, 22, 35, 19, 32, 21),
  superhero = c("Batman", "Superman", "Batman", "Spiderman", "Batman",
                "Antman", "Batman", "Superman", "Maggott", "Superman"),
  tattoos = c(11, 15, 12, 5, 65, 3, 9, 13, 900, 0),
  stringsAsFactors = FALSE
)
View(piratesurvey)
summary(piratesurvey$age)#mediana 25.0

dfFem <- subset(x=piratesurvey,subset = sex == "F")
View(dfFem) 
mean(dfFem$age)#media idade fem 24.5

dfMas <- subset(x=piratesurvey,subset = sex == "M")
View(dfMas) 
mean(dfMas$age)#media idade mas 32

max(dfMas$tattoos)#maximo tatto = 9

