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
         "reshape2","RCurl","RMySQL", "stats","scales","tseries",
         "TTR","TSA","XML","xts","zoo")

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
                           Start = NULL, End = NULL, Count = 900)
suppressMessages(library(plotly)) # Graficas interactivas
Datos <- list()
Datos <- Precios_Oanda
suppressMessages(library(TTR)) # ANALISIS TEC
Datos$TimeStamp <- as.character(Datos$TimeStamp)


Datos$TimeStamp <- as.Date(Datos$TimeStamp)

#Patrones CandleSticks
aux <-data.frame("Date" = Datos[,1],
                 "Precio_Apertura" = Datos$Open,
                 "Precio_Cierre" = Datos$Close,
                 "Bull_Bear" = NA , 
                 "Amplitud" = 0,
                 "Pattern" = NA)
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
      
    }
    else if (aux$Bull_Bear[i-1] == "Bear" & aux$Bull_Bear[i] == "Bull"){
      aux$Pattern[i] <- "Bullish Engulfing" 
    }
    if (aux$Bull_Bear[i-1] == "Bull" & aux$Bull_Bear[i] == "Bull") {
      aux$Pattern[i] <- "Bullish"
      
    }
    else if (aux$Bull_Bear[i-1] == "Bear" & aux$Bull_Bear[i] == "Bear"){
      aux$Pattern[i] <- "Bearish"
    }
  }
  
  else if(aux$Amplitud[i-1] > aux$Amplitud[i]){
    if (aux$Bull_Bear[i-1] == "Bull" & aux$Bull_Bear[i] == "Bear") {
      aux$Pattern[i] <- "Bearish Harami" 
      
    }
    else if(aux$Bull_Bear[i-1] == "Bear" & aux$Bull_Bear[i] == "Bull") {
      #else if(aux$Bull_Bear[i-1] == "Bear" & aux$Bull_Bear[i] == "Bull")  {
      aux$Pattern[i] <- "Bullish Harami" 
    }
    if (aux$Bull_Bear[i-1] == "Bull" & aux$Bull_Bear[i] == "Bull") {
      aux$Pattern[i] <- "Bullish"
      
    }
    else if(aux$Bull_Bear[i-1] == "Bear" & aux$Bull_Bear[i] == "Bear") {
      
      aux$Pattern[i] <- "Bearish"
    }
    
  }
  
}  # fin for




#Grafica EMA Y BBBands

Historico <- data.frame("Date"= Datos[,1],
                        "Precio"= Datos[,5])
X <- EMA(Historico[,2],n=10, wilder = FALSE, ratio = NULL)
y <- BBands(Historico[,2], n=10, EMA, sd=2)



Historico <- data.frame("Date"= Datos[,1],
                        "Precio"= Datos[,5],
                        "EMA"= X,
                        "BBd"= y[,1],
                        "BBu"=y[,3])


plot_ly(Historico) %>%
  
  add_trace(x = Historico[,1], y = ~round(Historico[,2]), type = 'scatter', mode = 'lines', name = 'Precio',
            
            line = list(color = 'red'), hoverinfo = "text", text = ~paste('Precio',round(Historico[,2]))) %>%
  
  add_trace(x = Historico[,1], y = ~round(Historico[,3]), type = 'scatter', mode = 'lines', name = 'EMA',
            
            line = list(color = 'blue'), hoverinfo = "text", text = ~paste('EMA',round(Historico[,3])))  %>%
  add_trace(x = Historico[,1], y = ~round(Historico[,4]), type = 'scatter', mode = 'lines', name = 'BB',
            
            line = list(color = 'orange'), hoverinfo = "text", text = ~paste('BBd',round(Historico[,4])))  %>%
  add_trace(x = Historico[,1], y = ~round(Historico[,5]), type = 'scatter', mode = 'lines', name = 'BB',
            
            line = list(color = 'black'), hoverinfo = "text", text = ~paste('BBu',round(Historico[,5])))  %>%
  
  layout(title = "3 Portafolios distintos objetivos",
         
         xaxis = list(title = "Fechas", showgrid = T),
         
         yaxis = list(title = "Precios"), 
         
         legend = list(orientation = 'h', y = -0.25, x = -0.25))
