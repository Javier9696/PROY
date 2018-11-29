# -- Borrar todos los elementos del environment
rm(list=ls())
mdir <- getwd()
# -- Establecer el sistema de medicion de la computadora

Sys.setlocale(category = "LC_ALL", locale = "")

# -- Huso horario
Sys.setenv(tz="America/Monterrey", TZ="America/Monterrey")
options(tz="America/Monterrey", TZ="America/Monterrey")

# -- Cargar y/o instalar en automatico paquetes a utilizar -- #

pkg <- c("base","downloader","dplyr","fBasics","forecast","grid",
         "gridExtra","httr","jsonlite","lmtest","lubridate","moments",
         "matrixStats", "PerformanceAnalytics","plyr","quantmod",
         "reshape2","RCurl","RMySQL", "stats","scales","tseries","stats",
         "TTR","TSA","XML","xts","zoo","TSA")

inst <- pkg %in% installed.packages()
if(length(pkg[!inst]) > 0) install.packages(pkg[!inst])
instpackages <- lapply(pkg, library, character.only=TRUE)

# -- Cargar archivos desde GitHub -- #

RawGitHub <- "https://raw.githubusercontent.com/IFFranciscoME/"
ROandaAPI <- paste(RawGitHub,"ROandaAPI/master/ROandaAPI.R",sep="")
downloader::source_url(ROandaAPI,prompt=FALSE,quiet=TRUE)

# -- Parametros para usar API-OANDA

# Tipo de cuenta practice/live
OA_At <- "practice"
# ID de cuenta
OA_Ai <- 1742531
# Token para llamadas a API
OA_Ak <- "ada4a61b0d5bc0e5939365e01450b614-4121f84f01ad78942c46fc3ac777baa6" 
# Hora a la que se considera "Fin del dia"
OA_Da <- 17
# Uso horario
OA_Ta <- "America/Mexico_City"
# Instrumento
OA_In <- "USD_MXN"
# Granularidad o periodicidad de los precios H4 = Cada 4 horas
OA_Pr <- "H4"
# Multiplicador de precios para convertir a PIPS
MultPip_MT1 <- 10000

Precios_Oanda <- HisPrices(AccountType = OA_At, Granularity = OA_Pr,
                           DayAlign = OA_Da, TimeAlign = OA_Ta, Token = OA_Ak,
                           Instrument = OA_In, 
                           Start = NULL, End = NULL, Count = 500)
suppressMessages(library(plotly)) # Graficas interactivas
suppressMessages(library(quantmod)) # Graficas interactivas
Datos <- list()
Datos <- Precios_Oanda
suppressMessages(library(TTR)) # ANALISIS TEC
Datos$TimeStamp <- as.character(Datos$TimeStamp)


Datos$TimeStamp <- as.Date(Datos$TimeStamp)
Historico <- data.frame("Date"= Datos[,1],
                        "Precioopen"= Datos[,2],
                        "Preciohigh"= Datos[,3],
                        "Preciolow"= Datos[,4],
                        "Precioclose"= Datos[,5])
SM <- SMA(Historico[,2],n=10, wilder = FALSE, ratio = NULL)
Z <- SMA(Historico[,2],n=100, wilder = FALSE, ratio = NULL)
x_Ema <-EMA(Historico[,2],n=10, wilder = FALSE, ratio = NULL)
y <- BBands(Historico[,2], n=10, EMA, sd=2)
macd <-MACD(Historico[,2], nFast = 10, nSlow = 28, nSig = 9, percent = TRUE) # Macd

Historico <- data.frame("Date"= Datos[,1],
                        "Precioopen"= Datos[,2],
                        "Preciohigh"= Datos[,3],
                        "Preciolow"= Datos[,4],
                        "Close"= Datos[,5],
                        "SMA_10" = SM,
                        "SMA_100" = Z,
                        "EMA_10 " = x_Ema,
                        "BBd"= y[,1],
                        "BBu"=y[,3],
                        "Mov_10" = 0,
                        "Mov_100" = 0,
                        "Mov_E" = 0)
                        
#---------------------------------------SMA 10, SMA 100, EMA 10                        
                        
                        
X <- xts(Historico[,2:5],order.by= Historico[,1], frequency = NULL, unique = TRUE)
is.xts(X)

s <- xts(Historico[303:500,2:5],order.by= Historico[303:500,1], frequency = NULL, unique = TRUE)
is.xts(X)


chartSeries(X[,1:4],type = c("candlesticks"), subset = NULL, name = "Candlestick", time.scale= NULL,log.scale = FALSE,
           TA = 'addVo()',
          TAsep=';',
         line.type = "l",
        bar.type = "ohlc",
       theme = chartTheme("white"),
      layout = NA,
     major.ticks='auto', minor.ticks=TRUE,
    yrange=NULL,
   plot=TRUE,
  color.vol = TRUE, multi.col = FALSE)
candleChart(X[,1:4],subset = NULL, type = "candlesticks", show.grid = TRUE, theme = 'white')
#add_BBands(n=20,maType="SMA",sd=2)

chartSeries(X,
            type = c("line"), 
            subset = NULL,
            show.grid = TRUE, 
            name = "Precios",
            time.scale = NULL,
            log.scale = FALSE,
            TA = 'addVo()',
            TAsep=';',
            line.type = "l",
            bar.type = "ohlc",
            theme = chartTheme("white"),
            layout = NA,
            major.ticks='auto', minor.ticks=TRUE,
            yrange=NULL,
            plot=TRUE)
addSMA(n = 10, on = 1, with.col = Cl, overlay = TRUE, col = "brown")
addSMA(n = 100, on = 1, with.col = Cl, overlay = TRUE, col = "orange")
addEMA(n = 10, wilder = FALSE, ratio=NULL, on = 1,
       with.col = Cl, overlay = TRUE, col = "blue")
addBBands(n = 20, sd = 2, ma = "SMA", draw = 'bands', on = -1)


#Analisis de los estadisticos
for ( i in 10:length(Historico$Date)){
  
  if (Historico$Close[i] > Historico$SMA_10[i]){
    Historico$Mov_10[i]<-1 #compra    
    
  }#fin primer if 
  else if (Historico$Close[i] < Historico$SMA_10[i]){
    Historico$Mov_10[i] <-2   
    
  } else
  { Historico$Mov_10[i]<-0
  
  }
  #mantiene
  if (Historico$Close[i] > Historico$EMA_10[i]){
    Historico$Mov_E[i]<-1 #compra    
    
  }#fin primer if 
  else if (Historico$Close[i] < Historico$EMA_10[i]){
    Historico$Mov_E[i] <-2   
    
  } else
  { Historico$Mov_E[i]<-0
  
  }
  
}
#evaluacion MA 100
for ( i in 100:length(Historico$Date)){
  
  if (Historico$Close[i] > Historico$SMA_100[i]){
    Historico$Mov_100[i]<-1 #compra    
    
  }#fin primer if 
  else if (Historico$Close[i] < Historico$SMA_100[i]){
    Historico$Mov_100[i] <-2    
  } else
  { Historico$Mov_100[i]<-0} #mantiene
  
}

#MA 10
hist(Historico$Mov_10,
     main="Frecuencia de Movimientos contra SMA de 10",
     xlab="Patrones:  0.- Mantiene, 1.-Compra, 2.-Venta",
     col="Green",
     freq=FALSE
)

#MA 100
hist(Historico$Mov_100,
     main="Frecuencia de Movimientos contra SMA de 100",
     xlab="Patrones:  0.- Mantiene, 1.-Compra, 2.-Venta",
     col="Blue",
     freq=FALSE
)
#EMA
hist(Historico$Mov_E,
     main="Frecuencia de Movimientos contra EMA de 100",
     xlab="Patrones:  0.- Mantiene, 1.-Compra, 2.-Venta",
     col="Green",
     freq=FALSE
)

#----------------------------------------------------Patrones CandleSticks
aux <-data.frame("Date" = Datos[,1],
                 "Precio_Apertura" = Datos$Open,
                 "Precio_Cierre" = Datos$Close,
                 "Bull_Bear" = NA , 
                 "Amplitud" = 0,
                 "Pattern" = NA,
                 "Number" = 0)
#numbers
#bearish enfulfing 1
#bullish enfulfing 2
# bearish Harami 3
# bullish Harami 4


aux$Bull_Bear[1] <- "Bull"
aux$Amplitud[1] <- abs(aux$Precio_Cierre[1] - aux$Precio_Apertura[1])


for (i in 2:length(Datos$Open)){
  #condicion 1
  if(aux$Precio_Apertura[i] > aux$Precio_Cierre[i]){
    aux$Bull_Bear[i] <- "Bear"
    aux$Amplitud[i] <- abs(aux$Precio_Cierre[i] - aux$Precio_Apertura[i])
    
  } else {
    aux$Bull_Bear[i] <- "Bull"
    aux$Amplitud[i] <- abs(aux$Precio_Cierre[i] - aux$Precio_Apertura[i])
    
  } #fin if
  #condicion 2
  if( aux$Amplitud[i-1] < aux$Amplitud[i]){
    if (aux$Bull_Bear[i-1] == "Bull" & aux$Bull_Bear[i] == "Bear") {
      aux$Pattern[i] <- "Bearish Engulfing" 
      aux$Number[i]<-1
      
    }
    else if (aux$Bull_Bear[i-1] == "Bear" & aux$Bull_Bear[i] == "Bull"){
      aux$Pattern[i] <- "Bullish Engulfing"
      aux$Number[i] <- 2
    }
    if (aux$Bull_Bear[i-1] == "Bull" & aux$Bull_Bear[i] == "Bull") {
      aux$Pattern[i] <- "Bullish"
      aux$Number[i] <- 2
      
    }
    else if (aux$Bull_Bear[i-1] == "Bear" & aux$Bull_Bear[i] == "Bear"){
      aux$Pattern[i] <- "Bearish"
      aux$Number[i]<-1
    }
  }
  
  else if(aux$Amplitud[i-1] > aux$Amplitud[i]){
    if (aux$Bull_Bear[i-1] == "Bull" & aux$Bull_Bear[i] == "Bear") {
      aux$Pattern[i] <- "Bearish Harami"
      aux$Number[i]<-3
      
    }
    else if(aux$Bull_Bear[i-1] == "Bear" & aux$Bull_Bear[i] == "Bull") {
      #else if(aux$Bull_Bear[i-1] == "Bear" & aux$Bull_Bear[i] == "Bull")  {
      aux$Pattern[i] <- "Bullish Harami" 
      aux$Number[i] <- 4
    }
    if (aux$Bull_Bear[i-1] == "Bull" & aux$Bull_Bear[i] == "Bull") {
      aux$Pattern[i] <- "Bullish"
      aux$Number[i] <- 4
      
    }
    else if(aux$Bull_Bear[i-1] == "Bear" & aux$Bull_Bear[i] == "Bear") {
      
      aux$Pattern[i] <- "Bearish"
      aux$Number[i]<-3
    }
    
  }
  
}  # fin for
bearish_egulfing <- length(which(aux$Number == 1))
bullish_egulfing <- length(which(aux$Number == 2))
bearish_harami <- length(which(aux$Number == 3))
bullish_harami <- length(which(aux$Number == 4))
hist(aux$Number,
     main="Frecuencia de Patrones en CandleStick",
     xlab="Patrones: 1.-Bearish Egulfing, 2.-Bullish Egulfing, 3.-Bearish Harami, 4.-Bullish Harami",
     col="darkmagenta",
     freq=TRUE
)

###---- Bollinger Bands
compra <- 0
venta <- 0
mantiene <- 0
for (i in 10:length(Historico$Date)){

  if(Historico$Close[i]< Historico$BBd[i]){
    compra<- compra +1
  }
  else if (Historico$Close[i]> Historico$BBu[i]){
    venta <- venta +1
  }
  else{
    mantiene <- mantiene + 1
  }
    
  
} # fin for

#---------------------MACD




#graficaMACD
chartSeries(X,
            type = c("line"), 
            subset = NULL,
            show.grid = TRUE, 
            name = "Precios",
            time.scale = NULL,
            log.scale = FALSE,
            TA = 'addVo()',
            TAsep=';',
            line.type = "l",
            bar.type = "ohlc",
            theme = chartTheme("white"),
            layout = NA,
            major.ticks='auto', minor.ticks=TRUE,
            yrange=NULL,
            plot=TRUE)
addMACD(fast = 10, slow = 28, signal = 9, type = "SMA")

#----------------------------------BOX Jenkins
ven_500 <- Historico$Close #todos  los valores 
ven_150 <- Historico$Close[1:150] #primeros 150 valores
ven_300 <- Historico$Close[151:300]
ven_301 <- Historico$Close[301:500]


Box <- c()
Box[1]<- Box.test(ven_150, lag = 1, type = "Box-Pierce")$p.value
Box[2]<- Box.test(ven_300, lag = 1, type = "Box-Pierce")$p.value
Box[3]<- Box.test(ven_301, lag = 1, type = "Box-Pierce")$p.value
Box[4]<- Box.test(ven_500, lag = 1, type = "Box-Pierce")$p.value


#-----------------------------------Normalidad
norm <- c()
norm[1] <- ks.test(ven_150, "pnorm", 1, 2)$p.value
norm[2] <- ks.test(ven_300, "pnorm", 1, 2)$p.value
norm[3] <- ks.test(ven_301, "pnorm", 1, 2)$p.value
norm[4] <- ks.test(ven_500, "pnorm", 1, 2)$p.value
