#1

#a) dica: O objeto resultado n√£o precisa existir no "global environment" , pode apenas existir dentro da fun√ß√£o. Pergunta: como deixar que o "return" seja apenas no final, criando um objeto interno √† fun√ß√£o?


variacao.1.a <- function(dados = c(), na.rm = T) {
  
  resultado <- if(na.rm) {
    
    length(unique(dados[!is.na(dados)]))!= 1
    
  } else length(unique(dados)) != 1
  
  return(resultado)
  
}

#Vamos testar se funciona

teste.2 <- c(1:10)
variacao.1.a(teste.2)

#b) existem dois jeitos, com condicional e sem condicional

#primeiro jeito com condicional: usar fun√ß√£o "any", que diz se existe pelo menos um NA

variacao.1.b.cond <- function(dados = c()) {
  
  if(any(is.na(dados))) {
    
    return(length(unique(dados[!is.na(dados)]))!= 1)
    
  } else return(length(unique(dados)) != 1)
  
}

#testando de novo...
variacao.1.b.cond(teste.2)

# usar fun√ß√£o sum, pra saber se existe pelo menos 1 NA

variacao.1.b.b <- function(dados = c()) {
  
  if(sum(is.na(dados))>=1) {
    
    return(length(unique(dados[!is.na(dados)]))!= 1)
    
  } else return(length(unique(dados)) != 1)
  
}

variacao.1.b.b(teste.2)

#sem condicional: faz direto

variacao.1.b.subset <- function(dados = c()) length(unique(dados[!is.na(dados)]))!= 1

variacao.1.b.subset(teste.2)

#c 

variacao.1.c <- function(dados) length(unique(dados[!is.na(dados)]))!= 1
variacao.1.c(c(1,1,1))
variacao.1.c(teste.2)

# a fun√ß√£o funciona da mesma forma para os dois. A diferen√ßa √© em avisar a f√≥rmula qual tipo de informa√ß√£o espera-se ter em cada argumento.


#d

variacao.1.d <- function(dados = c()) {
  
  if(sum(is.na(dados))>=1) {
    
    (length(unique(dados[!is.na(dados)]))!= 1)
    
  } else (length(unique(dados)) != 1)
  
}
variacao.1.d(c(1,NA,1))
variacao.1.d(c(1,NA,3))                                                                                      
#deu certo! 

#2
#a)
#usar fun√ß√£o atributo para nomear vetor da fun√ß√£o n.obs. Vamos fazer uma fun√ß√£o que retorne o resultado, e que esse seja o n√∫mero de n√£o faltantes.
 #FunÁ„o n.obs original
n.obs <- function(vetor) return(c(sum(!is.na(vetor))))

n.obs.2 <- function(vetor) {
  resultado <- sum(!is.na(vetor))
  attr(resultado, which = "names") <- "numero de nao faltantes"
  return(resultado)
}

#Vamos ver como a fun√ß√£o que criamos, n.obs, responde ao "teste.2". Veja que ela nos diz o n√∫mero de n√£o faltantes.
n.obs.2(teste.2)
teste.2

#Quando pedimos apenas o "names", ela nos diz apenas "numero de n√£o faltantes"
names(n.obs.2(teste.2))

str(n.obs.2(teste.2))
#Vamos ver qual √© a estrutura da fun√ß√£o.. atrav√©s = usar a fun√ß√£o structure, n√£o ter a fun√ß√£o impl√≠cita      

#b) 

datas.1 <- function(periodo = character(), referencia = c()) {
  
  periodo <- as.Date(paste(substr(periodo, start = 1, stop = 4),
                           substr(periodo, start = 5, stop = 6),
                           "15",
                           sep = "/"))
  
  periodo <- periodo[!is.na(referencia)]
  
  return(c(min(periodo), max(periodo)))
  
}

#a fun√ß√£o roda. Agora vamos ver se a leitura √© correta

teste.3 <- datas.1(periodo = pim$`MÍs (CÛdigo)`, referencia = pim$Valor)

teste.3

#sim, ele interpreta a barra igual ao h√≠fen

#c)
#1 - datas no formato brasileiro
#2 fazer R entender datas - usar argumento "format" para falar como as datas t√£o escritas. Aten√ß√£o na parte "format", precisa ter o "%" para dizer qual parte est√° se referindo, mas usar tamb√©m o separador "/"

datas.BR <- function(periodo = character(), referencia = c()) {
  
  periodo <- as.Date(paste("15" , 
                           substr(periodo, start = 5, stop = 6),
                           substr(periodo, start = 1, stop = 4), 
                           sep = "/"), format = "%d/%m/%Y")
  
  
  periodo <- periodo[!is.na(referencia)]
  
  return(c(min(periodo), max(periodo)))
  
}

datas.BR(pim$`MÍs (CÛdigo)` , pim$Valor)


#d 
#colapse - colapsa todas as partes da fun√ß√£o em um unico caractere. √â como a fun√ß√£o "concatenar" do excel, que junta a todos os elementos um elemento e todos em um s√≥ (separados pelo elemento escolhido). Nunca usar, ela tem risco de estragar seus dados juntando todos em 1! 

paste(seq.Date(as.Date("2016-04-17"), as.Date("2021-03-08"), by = 1), collapse = "mentira")

#3
info.pim <- data.frame()

for (uf in unique(pim$`Unidade da FederaÁ„o`)) {
  
  for (setor in unique(pim$`SeÁıes e atividades industriais (CNAE 2.0)`)) {
    
    pim.UF.Setor <- pim[pim$`Unidade da FederaÁ„o` == uf &
                          pim$`SeÁıes e atividades industriais (CNAE 2.0)` == setor,]
    
    Obs <- n.obs(pim.UF.Setor$Valor)
    
    inicio.fim <- datas(periodo = pim.UF.Setor$`MÍs (CÛdigo)`,
                        referencia = pim.UF.Setor$Valor)
    
    info.pim <- rbind(info.pim,
                      data.frame(UF = uf, Setor = setor, 
                                 N.obs = Obs, 
                                 Inicio = inicio.fim[1], 
                                 Fim = inicio.fim[2]))
    
  }
}

View(info.pim)
View(pim.UF.Setor)

pim.UF.Setor[!is.na(pim.UF.Setor$Valor), ]

info.pim[info.pim$N.obs == 227, ]
subset(info.pim , N.obs == 227 , UF = "Amazonas")

#4


#Para montar o gr·fico, conferir se a PIM est· apenas com os dados de inudstria geral ou se precisa limpar, como fizemos em aula


#Tipo 1: seq_along. #Pq colocar seq_along? Para ele ler como valores crescentes (datas)
plot(seq_along(unique(pim$`MÍs (CÛdigo)`)), 
     pim$Valor[pim$`Unidade da FederaÁ„o` == "Amazonas"] ,
     type = "l", 
     main = "Amazonas", 
     xlab = "meses" ,
     ylab = "valor")

#Tipo 2: criar um vetor meses para o eixo x

meses <- as.Date(unique(pim$`MÍs (CÛdigo)`))

datas.4 <- function(periodo = character()) {
  
  periodo <- as.Date(paste(substr(periodo, start = 1, stop = 4),
                           substr(periodo, start = 5, stop = 6),
                           "15",
                           sep = "/"))
  return(periodo)
}

meses <- datas.4(meses)

plot(meses, 
     pim$Valor[pim$`Unidade da FederaÁ„o` == "Amazonas"] ,
     type = "l", 
     main = "Amazonas", 
     xlab = "meses" ,
     ylab = "valor")


#5 
# E se quisessemos que uma base da dados inteira fosse da classe "ts"? 
#a) Primeiro, vamos criar uma base de dados em que cada estado seja uma coluna. Para isso, vamos lembrar como est√° nossa base de dados...

View(pim)
#Vamos facilitar tirando colunas "inuteis"
pim.N <- pim[,c(1,3,7)]
View(pim.N)

#Vamos agora criar uma matriz com seus valores
pim.ts <- matrix(pim.N$Valor , ncol = length(unique(pim.N$`Unidade da FederaÁ„o (CÛdigo)`)) , nrow = 227)

#Por fim, vamos nomear as colunas com os estados
unique(pim$`Unidade da FederaÁ„o`)
colnames(pim.ts) <- unique(pim$`Unidade da FederaÁ„o`)


#b) Vamos usar a funÁ„o ts para transformar os vetores em series de tempo

pim.ts <- ts(pim.ts , start = c(2002,01) , frequency = 12)

#Vamos ver se deu certo
str(pim.ts)
class(pim.ts)

pim.ts

#c) 

#Por fim, vamos tirar a coluna que sÛ tem valores NA

pim.ts <- pim.ts[ , -3]

#E decompor as series

pim.dec <- decompose(pim.ts)

pim.dec
                                               
                                               
