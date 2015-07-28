

## 1) CHAMADA DA FUNÇÃO PARA A PRIMEIRA PÁGINA
## 2) CRIAÇÃO DOS DADOS DE SAÍDA (MONTAGEM DAS COLUNAS E CSV)
## 3) CÁLCULO DO TOTAL DE PÁGINAS
## 4) LOOP PARA PAGINAR EM TODAS AS FOLHAS
## 5) ARMAZENAMENTO TEMPORÁRIO DOS DADOS
## 6) MONTAGEM DE ARQUIVO FINAL CONSOLIDADO


library(jsonlite)

## aplicar WORKING DIRECTORY correto
wd <- getwd()
setwd(wd)

wait <- function(x = 10){
  p1 <- proc.time()
  Sys.sleep(x)
  proc.time() - p1 # o esperado é que o tempo de cpu seja mínimo
}

attempts <- 0
maxTry <- 2
p <- c()


ReadFromCartola <- function(pagenumber) {
  path <- paste("http://cartolafc.globo.com/mercado/filtrar.json?page=",pagenumber,"&order_by=preco&status_id=7",sep="") 
  print(paste("Processando:",path))
  i <- 1
  while(i <= maxTry) {
    tryCatch({
      jsonData <- fromJSON(path, flatten=TRUE)
      p <<- setdiff(p,c(pagenumber))
      
      return(jsonData)
    }, error = function(x) {
      print(paste("Erro na leitura da API. Tentativa nro:",i, ". Tentando novamente em 5 segundos.",sep=""))
      wait(1)
      i <<- i + 1
      assign("last.error", NULL, envir = baseenv())
      assign("last.warning", NULL, envir = baseenv())
      if (i == maxTry ) {
        return(NULL)
      }
    })
  }
}

##features
cols <- c("status","status_id","media","pontos","apelido","preco","partida_data","jogos","variacao","id","clube.nome","posicao.nome")
startPage <- 1

rawData<-ReadFromCartola(startPage)

if(is.null(rawData)){
  stop('Erro já na primeira página. Não é possível prosseguir.')
}

roundNumber <- rawData$rodada_id
actualPage <- rawData$page$atual
totalPages <- rawData$page$total

SaveFile <- function(roundNumber,pagenumber) {
  
  outputfile <- paste(getwd(),"/Brasileirao2015Rodada_",roundNumber,"_Page_",pagenumber, ".csv",sep="")
  df<-rawData$atleta[,cols]
  
  ##buscando e gravando os scouts
  df <- cbind(scouts = sapply(rawData$atleta$scout,
                              function(x) {
                                z <- ""
                                if (class(x)!= "data.frame") {return ("")}
                                if (nrow(x) == 0) {return ("")}
                                for (i in 1:nrow(x)) {    
                                  z <- paste(z,x[i,2],"=",x[i,1],";",sep="")
                                }
                                return(substr(z, 1, nchar(z)-1))  
                              }),df)
  
  ##adicionando a rodada
  df <-cbind(rodada = roundNumber, df)  
  write.csv(df, file = outputfile)
}

SaveFile(roundNumber,startPage)

startPage <<- startPage + 1
p <- c(startPage:totalPages)

while (length(p)>0) {
  for (i in p) {
    rawData <- ReadFromCartola(i)
    if(!is.null(rawData)) {
      SaveFile(roundNumber,i)    
    }
  }
  
  print(paste("Pendente de leitura: ",length(p),". Buscando novamente em 60 segundos. Tentativa:",attempts ,sep=""))
  wait(10)   ##nova tentativa para páginas que deram erro
  attempts <- attempts + 1  
}


##geração do CSV final (consolidado)
##--------------------------------------------------------

startPage <- 1
outputfile <- paste(getwd(),"/Brasileirao2015.csv",sep="")
inputfiles <- c()

for (pn in startPage:totalPages) {
  inputfiles <- append(inputfiles,paste(getwd(),"/Brasileirao2015Rodada_",roundNumber,"_Page_",pn, ".csv",sep=""))
}

merged_df <- do.call("rbind", lapply(inputfiles, read.csv, header = TRUE))
write.table(merged_df, outputfile, row.names=F, na="NA", append = T, quote= FALSE, sep = ",", col.names = F)

##--------------------------------------------------------

rm(list=ls()) ##limpeza de cache
