#1

#a) dica: O objeto resultado não precisa existir no "global environment" , pode apenas existir dentro da função. Pergunta: como deixar que o "return" seja apenas no final, criando um objeto interno à função?


variacao.1.a <- function(dados = c(), na.rm = T) {
  
  resultado <- if(na.rm) {
    
    length(unique(dados[!is.na(dados)]))!= 1
    
  } else length(unique(dados)) != 1
  
  return(resultado)
  
}

#b) existem dois jeitos, com condicional e sem condicional

#primeiro jeito com condicional: usar função "any", que diz se existe pelo menos um NA

variacao.1.b.cond <- function(dados = c()) {
  
  if(any(is.na(dados))) {
    
    return(length(unique(dados[!is.na(dados)]))!= 1)
    
  } else return(length(unique(dados)) != 1)
  
}

# usar função sum, pra saber se existe pelo menos 1 NA

variacao.1.b.b <- function(dados = c()) {
  
  if(sum(is.na(dados))>=1) {
    
    return(length(unique(dados[!is.na(dados)]))!= 1)
    
  } else return(length(unique(dados)) != 1)
  
}

#sem condicional: faz direto

variacao.1.b.subset <- function(dados = c()) length(unique(dados[!is.na(dados)]))!= 1

#c 

variação.1.d <- function(dados) length(unique(dados[!is.na(dados)]))!= 1
variação.1.d(c(1,1,1))
# a função funciona da mesma forma para os dois. A diferença é em avisar a fórmula qual tipo de informação espera-se ter em cada argumento.


#d

variacao.1.e <- function(dados = c()) {
  
  if(sum(is.na(dados))>=1) {
    
    (length(unique(dados[!is.na(dados)]))!= 1)
    
  } else (length(unique(dados)) != 1)
  
}
variacao.1.e(c(1,NA,1))
variacao.1.e(c(1,NA,3))                                                                                      

# função for da Aula 2: 
for(numero in 1:10) numero + 5
##funcionou

#2
#a)
#usar função atributo para nomear vetor da função n.obs. Vamos fazer uma função que retorne o resultado, e que esse seja o número de não faltantes.


teste.2 <- c(1:10)

n.obs.2 <- function(vetor) {
  resultado <- sum(!is.na(vetor))
  attr(resultado, which = "names") <- "numero de nao faltantes"
  return(resultado)
}

#Vamos ver como a função que criamos, n.obs, responde ao "teste.2". Veja que ela nos diz o número de não faltantes.
n.obs.2(teste.2)

#Quando pedimos apenas o "names", ela nos diz apenas "numero de não faltantes"
names(n.obs.2(teste.2))

str(n.obs.2(teste.2))
#Vamos ver qual é a estrutura da função.. através = usar a função structure, não ter a função implícita      

#b) 

datas.1 <- function(periodo = character(), referencia = c()) {
  
  periodo <- as.Date(paste(substr(periodo, start = 1, stop = 4),
                           substr(periodo, start = 5, stop = 6),
                           "15",
                           sep = "/"))
  
  
  
  periodo <- periodo[!is.na(referencia)]
  
  return(c(min(periodo), max(periodo)))
  
}

#a função roda. Agora vamos ver se a leitura é correta

teste.3 <- datas.1(periodo = pim$`Mês (Código)`, referencia = pim$Valor)

teste.3

#sim, ele interpreta a barra igual ao hífen

#c)
#1 - datas no formato brasileiro
#2 fazer R entender datas - usar argumento "format" para falar como as datas tão escritas. Atenção na parte "format", precisa ter o "%" para dizer qual parte está se referindo, mas usar também o separador "/"

datas.BR <- function(periodo = character(), referencia = c()) {
  
  periodo <- as.Date(paste("15" , 
                           substr(periodo, start = 5, stop = 6),
                           substr(periodo, start = 1, stop = 4), 
                           sep = "/"), format = "%d/%m/%Y")
  
  
  periodo <- periodo[!is.na(referencia)]
  
  return(c(min(periodo), max(periodo)))
  
}

periodo <- c(200507 , 200508 , 200509)

dia <- c(20020201)
d <- as.Date(dia, tryFormats =  c("%Y%m%d"))

#d 
#colapse - colapsa todas as partes da função em um unico caractere. É como a função "concatenar" do excel, que junta a todos os elementos um elemento e todos em um só (separados pelo elemento escolhido). Nunca usar, ela tem risco de estragar seus dados juntando todos em 1! 

paste(seq.Date(as.Date("2003-01-01"), as.Date("2016-04-17"), by = 1), collapse = "golpe")

#3
info.pim <- data.frame()

for (uf in unique(pim$`Unidade da Federação`)) {
  
  for (setor in unique(pim$`Seções e atividades industriais (CNAE 2.0)`)) {
    
    pim.UF.Setor <- pim[pim$`Unidade da Federação` == uf &
                          pim$`Seções e atividades industriais (CNAE 2.0)` == setor,]
    
    Obs <- n.obs(pim.UF.Setor$Valor)
    
    inicio.fim <- datas(periodo = pim.UF.Setor$`Mês (Código)`,
                        referencia = pim.UF.Setor$Valor)
    
    info.pim <- rbind(info.pim,
                      data.frame(UF = uf, Setor = setor, 
                                 N.obs = Obs, 
                                 Inicio = inicio.fim[1], 
                                 Fim = inicio.fim[2]))
    
  }
}
pim.UF.Setor[!is.na(pim.UF.Setor$Valor), ]

info.pim[info.pim$N.obs == 227, ]
subset(info.pim , N.obs == 227 , UF = "Amazonas")

#4

plot(unique(pim$`Mês (Código)`)) , 
     pim$Valor[pim$`Unidade da Federação` == "Amazonas" & pim$`Seções e atividades industriais (CNAE 2.0) (Código)`== 129314] , 
     type = "l" , 
     main = "Amazonas" , 
     xlab = "meses" , 
     ylab = "valor" )

plot(AM.dec)

plot(AM.ts)

#Aqui está tendo um problema com o gráfico, ele parece ficar com algumas rupturas.... Vamos conferir se tem o mesmo valor...
sum(AM.ts)
sum(pim$Valor[pim$`Unidade da Federação` == "Amazonas" & pim$`Seções e atividades industriais (CNAE 2.0) (Código)`== 129314])

#5 
# E se quisessemos que uma base da dados inteira fosse da classe "ts"? 
#a) Primeiro, vamos criar uma base de dados em que cada estado seja uma coluna. Para isso, vamos lembrar como está nossa base de dados...

View(pim)
#Vamos facilitar tirando colunas "inuteis"
pim.N <- pim[,c(3,5,9,13)]
View(pim.N)

#Vamos agora escolher só o segmento "industria geral", que é o codigo 129314
pim.ts <- data.frame()

for ( setor in pim.N$`Seções e atividades industriais (CNAE 2.0) (Código)`) {
  pim.ts <- pim.N if pim.N$`Seções e atividades industriais (CNAE 2.0) (Código)` == 129314

  
}


