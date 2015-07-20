## 1) Calls the function for the first page
## 2) Creates the data output object
## 3) recovers the total pages to be called
## 4) creates a loop to get information from all pages
## 5) Saves on a temp file  
## 6) consolidates into a single file

cat("\014") 

library(jsonlite)

## Change it to the correct path of your working directory
wd <- getwd()
setwd(wd)

wait <- function(x = 10){
  p1 <- proc.time()
  Sys.sleep(x)
  proc.time() - p1 # The cpu usage should be negligible
}

attempts <- 0
maxTry <- 2
p <- c()


ReadFromCartola <- function(pagenumber) {
  path <- paste("http://cartolafc.globo.com/mercado/filtrar.json?page=",pagenumber,"&order_by=preco&status_id=7",sep="") 
  print(paste("Reading:",path))
  i <- 1
  while(i <= maxTry) {
    tryCatch({
      jsonData <- fromJSON(path, flatten=TRUE)
      p <<- setdiff(p,c(pagenumber))
      
      return(jsonData)
    }, error = function(x) {
      print(paste("An error reading the web site just occurred. Attempt number:",i, ". Trying again in 5 seconds.",sep=""))
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
  stop('Error on the first page. Unable to proceed')
}

roundNumber <- rawData$rodada_id
actualPage <- rawData$page$atual
totalPages <- rawData$page$total

SaveFile <- function(roundNumber,pagenumber) {
  
  outputfile <- paste(getwd(),"/Brasileirao2015Rodada_",roundNumber,"_Page_",pagenumber, ".csv",sep="")
  df<-rawData$atleta[,cols]
  
  ##adding scouts  
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
  
  ##adding the roundnumber
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
  
  print(paste("Still pending to read: ",length(p),". Reading again in 60 seconds.Attempt:",attempts ,sep=""))
  wait(10)   ##another chance for the pages with error
  attempts <- attempts + 1  
}


##consolidates the CSV into a single definitive file
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

rm(list=ls()) ##releases memory
