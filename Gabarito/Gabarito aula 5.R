#GABARITO AULA 5

all(receita$Data == despesa$Data)

#1 o R trabalha com o operador boleano "&"

#a) 
sum(despesa$Data == receita$Data) == nrow(despesa)

# 2. Repare que no momento em que estavamos ajeitando as variaveis de fiscal nao utilizamos exatamente o mesmo comando trocando apenas o argumento de pattern e replacement.

# a) Como voce poderia escrever um ciclo for nesse trecho para que ele tomasse pattern = a partir de um vetor padrao = c("[.]", ",") e replacement = de um vetor substitui = c("", ".")?

padrao <- c("[.]" , ",")
substitui <- c("" , ".")

despesaN <- fiscal$Despesa

for(subs in 1:2) {
  despesaN <- gsub(padrao[subs] , substitui[subs] , despesaN)
}

despesaN

for (coluna in 2:3) {
  fiscalN <- fiscal[ , coluna]  
  for (subs in 1:2) {
    fiscalN <- gsub(padrao[subs] , substitui[subs], fiscalN)
  }
  fiscal[ , coluna] <- fiscalN
  
}

View(fiscal)

#c) A funcao "as.numeric" converte charcters em numeros (doubles). A funcao is.numeric retorna se o elemento é lido como numero ou nao (true ou false). 

as.numeric(fiscal[,1])
#E possivel converter datas em numeros.
palavras <- c("aula" , "erre")
as.numeric(palavras)
#nao e possivel transformar palavras em numeros, viram "NAs"

#Vamos voltar agora a coluna de datas de fiscal para o formato "datas"
as.Date(fiscal[,1])

#3
#Primeiro vamos ver se os dados estao em numero. Como nao estao, vamos transforma-los em numero
str(ipub[2:8])
ipub[,2:8] <- apply(X = ipub[,2:8],
                    MARGIN = 2,
                    FUN = as.numeric)

#agora, vamos dividir os dados de investimento pelos valores do PIB
ipubP <- ipub[,2:7]/ipub[,8]
View(ipubP)

#Por fim, adicionar a coluna de anos na nossa nova base
ipubP <- cbind(ipub[,1] , ipubP)
colnames(ipubP) <- c("Ano", "GC" , "GE", "GM", "GG" , "EPU" , "SP")

#Finalmente, vamos criar o grafico
plot(x = ipubP$Ano,
     y = ipubP$GC,
     type = "l",
     xlab = "Ano",
     ylab = "Investimento (%PIB)",
     col = "blue", 
     main = "Investimento Publico",
     ylim = c(0,.05))

lines(x = ipubP$Ano,
      y = ipubP$GE,
      col = "red")

lines(x = ipubP$Ano,
      y = ipubP$GM,
      col = "green")

lines(x = ipubP$Ano,
      y = ipubP$GG,
      col = "black")

lines(x = ipubP$Ano,
      y = ipubP$EPU,
      col = "yellow")

lines(x = ipubP$Ano,
      y = ipubP$SP,
      col = "gray")

legend("bottomright",
       legend = c("GC", "GE" , "GM" , "GG" , "EPU" , "SP"),
       col = c("blue", "red" , "green" , "black" , "yellow") , 
       pch = 16, 
       cex = .6) 

#4

#Vamor primeiro incluir o PIB, que já está em valores trimestrais 

View(pib)

base.eer <- cbind(base.eer, pib$Valor)

#Agora, para a divida, precisamos criar os valores trimestrais e depois adicionar a base de dados

View(divida)
nrow(divida)

#Vamos criar uma coluna de trimestres
divida$Tri <- rep(1:80, each = 3)

#Primeiro, por "seguranca", pra nao ter que a cada erro arrumar as variaveis, vamos criar uma nova variavel que e a divida.tri, apenas determinando que ela sera um vetor
divida.tri <- c()

#Depois, vamos fazer um novo ciclo em que ele monte essa variavel como a media do valor da divida a cada trimestre
for (tri in 1:80) {
  
  media <- mean(divida$valor[divida$Tri == tri])
  divida.tri <- c(divida.tri , media)
  
}

#Vamos ver como ficou essa variavel...
divida.tri

#Isso mesmo, um vetor de 80 observacoes, uma pra cada trimestre. Os primeiros sao "NA" porque as observacoes estao faltantes. Mas vamos conferir a estrutura para ver como esta...
str(divida.tri)

#Otimo, a estrutura esta com numeros. Agora, vamos juntar esse vetor novo a nossa base. Para isso, vamos usar o comando "cbind", avisando a forma que queremos juntar. Se usar apenas o comando c() , o R vai entender que estamos formando uma lista.

base.eer <- cbind(base.eer , divida.tri)
View(base.eer)