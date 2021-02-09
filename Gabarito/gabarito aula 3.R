## Aula 3 - versão 2

pim <- sidrar::get_sidra(3653, # x = série a ser obtida
                         variable = 3135, # Índice de base fixa sem ajuste sazonal (média de 2012 = 100)
                         period = "200201-202011", # Início e fim da amostra "AAAAMM-AAAAMM"
                         geo = "State", # Escolher UF
                         geo.filter = list("State" = c(13, # AM
                                                       23, # CE
                                                       51, # MT
                                                       31, # MG
                                                       26, # PE
                                                       43, # RS
                                                       35)))  # SP 

## informações sidra

sidrar::info_sidra(3653, wb = T)

##ver informações

View(pim)

##definindo função: ver em quais colunas existe variação

variacao <- function(dados = c(), na.rm = T) {
  
  if(na.rm) {
    
    return(length(unique(dados[!is.na(dados)]))!= 1)
    
  } else return(length(unique(dados)) != 1)
  
}

##testando a função

variacao(pim$`Nível Territorial`)
variacao(pim$Valor)

## outra forma de fazer o subsetting é por [] , usar primeiro numero da linha, depois da coluna. ex:

variacao(pim[,1]) ##linha: todas; coluna: 1 = nível territorial

##ver quais são os valores que são únicos de todas as colunas

col.variacao <- apply(X = pim, MARGIN = 2, FUN = variacao)
col.variacao

##comparar que vetor tem mesmos nomes (é como se fosse uma função com vários "&"s. só será verdadeira se todos forem verdadeiros)

all(colnames(pim) == names(col.variacao))

##limpar a pim só com as colunas que são verdadeiras da col.variacao

pim <- pim[,col.variacao]

##vamos gerar uma tabela resumo dizendo quantas observações não faltantes tem cada vetor. E também pra qual período começa, pra qual período termina

n.obs <- function(vetor) return(c(sum(!is.na(vetor))))

## a soma das informações retiradas do vetor diz a soma dos valores verdadeiros (que não são NA). Ou seja, o n.obs é o número de informações não faltantes

## a função head traz as 6 primeiras informações de cada coluna

head(pim[,c("Mês (Código)", "Mês")])

##criar referência de datas. Usando a coluna mês codigo, usar a função substr, que separa as "palavras" no meio, separando ano do mês. Usando a função paste, colar essas informações com um dia a escolha, com um separador "-". Usando a função as.date, o R interpreta como informação de data. Depois que periodo é definido como referencias com informações não faltantes, retornar minimo e máximo
#periodo = 202002

datas <- function(periodo = character(), referencia = c()) {
  
  periodo <- as.Date(paste(substr(periodo, start = 1, stop = 4),
                           substr(periodo, start = 5, stop = 6),
                           "15",
                           sep = "-"))
  

  
  periodo <- periodo[!is.na(referencia)]
  
  return(c(min(periodo), max(periodo)))
  
}

##realizando um teste para ver quando começam e encerram as observações (não identifica se tem descontinuidade na série):

teste <- datas(periodo = pim$`Mês (Código)`, referencia = pim$Valor)

teste

##Vamos criar uma tabela resumo com todas as informações: para cada setor e cada estado, quando as informações começam e quando terminam. Avaliação com condição lógica sobre as linhas, retorna pim recortado para UF e setor; observações pegam informações não faltantes.Datas pegam o início e o fim com base nas informações do valor. Por fim, info.pim com unir colunas com ela mesma, mais uma linha de resumo.


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

View(info.pim)

## Agora que já temos uma planilha apenas com informações únicas nas colunas, vamos limpar para deixar apenas as linhas que contém informações com números

info.pim <- info.pim[info.pim$N.obs != 0, ]

## qual a presença em todos os estados. Para um setor, depois de excluir aqueles que tem 0 informações, o número de estados é igaul ao número de estados total? azer o mesmo para data (minimo da data pro setor, todas as informações de minimo são iguais?) fazer o mesmo para máximo.
## A presença será a validade das três condições juntas
## A cada rodada do for, os nomes de presença vão ser: nome de presença, menos o último, junto com setor

presenca <- c()

for (setor in unique(info.pim$Setor)) {
  
  condicao1 <- length(unique(info.pim$UF[info.pim$Setor == setor])) == length(unique(info.pim$UF))
  
  condicao2 <- all(min(info.pim$inicio[info.pim$Setor == setor]) == info.pim$inicio[info.pim$Setor == setor])
  
  condicao3 <- all(max(info.pim$fim[info.pim$Setor == setor]) == info.pim$fim[info.pim$Setor == setor])
  
  presenca <- c(presenca, condicao1 & condicao2 & condicao3)
  
  names(presenca) <- c(names(presenca)[-length(presenca)], setor)
  
}

presenca

sum(presenca)

## agora, só sobraram três setores. dentre esses, vamos selecionar indústria geral

pim <- pim[pim$`Seções e atividades industriais (CNAE 2.0)` == "1 Indústria geral",]

## Agora, vamos criar um objeto do tipo série de tempo, para podermos trabalhar com algumas funções já definidas

AM.ts <- ts(pim$Valor[pim$`Unidade da Federação` == "Amazonas"], 
            start = c(2002,01), 
            end = c(2020,11),
            frequency = 12)

## e vamos printar o objeto para ver como ele aparece

print(AM.ts)

## e então, conhecer com qual método o R lê a função

sloop::s3_dispatch(print(AM.ts))

## para ver o gráfico da função, podemos usar a função plot e então ver o gráfico no painel direito. Atenção: se o gráfico não aparecer, é possível que o tamanho do visor esteja muito pequeno. Para resolver, é só aumentá-lo e apertar novo ctr+enter

plot(AM.ts)

## Podemos agora decompor a série entre tendência, variações sazonais e choque aleatórios,criando um novo objeto com a função de decomposição

AM.dec <- decompose(AM.ts)

## então, podemos analisar gráficamente a decomposição

plot(AM.dec)

## Para visualizarmos a série na forma de boxplot podemos criar ciclos de acordo com os períodos, no caso, meses

boxplot(AM.ts ~ cycle(AM.ts), 
        range = 1)

## Vamos ver a distribuição dos quartis de marco, criando o objeto com uma sequencia que começa na observação três (uma vez que a primeira vez que aparece é na terceira posição da serie) e anda de 12 em 12 períodos

marco <- AM.ts[seq(from = 3, to = length(AM.ts), by = 12)]

##e podemos ver os quartis com a função:

quantile(marco)

## Para entender como é visto no bloxplot

acima <- marco > 102.75 & marco < 102.75 + 20.2

max(marco[acima]) # Valor máximo da linha tracejada em março

abaixo <- marco < 82.55 & marco > 82.55 - 20.2

min(marco[abaixo]) # Valor mínimo da linha tracejada em março


## Exercícios --------

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
#usar função atributo para nomear vetor da função n.obs. Preciso nomear o retorno? Ou preciso nomear a função? Atributo "which" é um caractere. Mas retorno é atributo?
#não consegui entender a parte da função str. É o metodo de nomear? não rodou a função do dispatch

teste.2 <- c(1:10)

n.obs.2 <- function(vetor) {
  resultado <- sum(!is.na(vetor))
  attr(resultado, which = "names") <- "numero de nao faltantes"
  return(resultado)
}


names(n.obs.2(teste.2))

str(n.obs.2(teste.2))
#através = usar a função structure, não ter a função implícita      

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
#2 fazer R entender datas - usar argumento "format" para falar como as datas tão escritas

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
#colapse - colapsa todas as partes da função em um unico caractere. É como a função "concatenar" do excel, que junta a todos os elementos um elemento e todos em um só (separados pelo elemento escolhido). Nunca usar! 

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

