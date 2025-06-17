# OBTAIN DATA

#set working directory with the .csv files 
setwd("C:/Users/Mattia/Desktop/NONPARAMETRIC STATISTICS/projectNPS/DatasetCSV")

#creates the list of all the xml files in the directory
listcsv <- dir(pattern = "*.csv")

dataoff <- NULL
databid <- NULL
vdate <- NULL

it = 0


for (i in 1:731){   # number of elements: CHOOSE YOURS
  
  setwd("C:/Users/Mattia/Desktop/NONPARAMETRIC STATISTICS/projectNPS/DatasetCSV")
  #trasform daily data from xml
  df <- read.csv(listcsv[i]) 
  
  #Choose hour of day
  df_1 <- df[df$Ora == 7,]
  df_1 <- df_1[df_1$ZonaMercato == "CALA;CNOR;CSUD;NORD;SARD;SICI;SUD;AUST;COAC;CORS;FRAN;GREC;SLOV;SVIZ;MALT;COUP;MONT;",]
  
  if(is_empty_df <- nrow(df_1) == 0 || ncol(df_1) == 0){
    
  } else {
    
    #Change data types
    df_1$Quantita <- as.numeric(df_1$Quantita)
    df_1$Prezzo <- as.numeric(df_1$Prezzo)
    
    #Build the cumulative observations
    #Sorted offer array
    df_1_off <- df_1[df_1$Tipo == "OFF",]
    df_1_off <- df_1_off[order(df_1_off$Prezzo),]
    
    #sorted bid array
    df_1_bid <- df_1[df_1$Tipo == "BID",]
    df_1_bid <- df_1_bid[order(df_1_bid$Prezzo, decreasing=TRUE),]
    
    #Saving x and y
    off_quant_cum <- cumsum(df_1_off$Quantita)
    bid_quant_cum <- cumsum(df_1_bid$Quantita)
    
    off_prices <- df_1_off$Prezzo
    bid_prices <- df_1_bid$Prezzo
    
    # Creating the step functions in order to visualize them 
    step_off <- stepfun(off_quant_cum, c(off_prices, tail(off_prices, 1)))
    step_bid <- stepfun(bid_quant_cum, c(bid_prices, tail(bid_prices, 1)))
    
    threshold <- c(0,50000)
    
    n.sample <- 1000
    x.off <- seq(threshold[1], threshold[2], length.out = n.sample)
    y.off <- step_off(x.off)
    x.bid <- seq(threshold[1], threshold[2], length.out = n.sample)
    y.bid <- step_bid(x.bid)
    
    date_str <- substr(listcsv[i], 1, 8)
    
    date_obj <- as.Date(date_str, format = "%Y%m%d")
    
    # create the Matrix of daily PrezzoZonale vectors of the period
    if(it == 0){
      vdate <- date_obj
      dataoff <- y.off
      databid <- y.bid
    } else {
      vdate <- c(vdate, date_obj)
      dataoff <-rbind(dataoff, y.off)
      databid <-rbind(databid, y.bid)
    }
    
    it <- it + 1 
    
  }
  
}

# # Costruisci il data frame
# df_offer <- data.frame(Date = vdate, dataoff)
# colnames(df_offer) <- c("Date", paste0("V", 1:1000))
# 
# # Specifica la directory e il nome del file
# output_dir <- "C:/Users/Mattia/Desktop/NONPARAMETRIC STATISTICS/projectNPS/ShortDataset"
# file_name <- "OFF.csv"
# percorso_file <- file.path(output_dir, file_name)
# 
# # Salva il file
# write.table(df_offer, file = percorso_file, sep = "\t", row.names = FALSE, col.names = TRUE)
# 
# # Controlla se è stato salvato correttamente
# if (file.exists(percorso_file)) {
#   print("SAVED SUCESSFULLY")
# } else {
#   print("ERROR")
# }
# 
# 
# 
# df_bid <- data.frame(Date = vdate, databid)
# colnames(df_bid) <- c("Date", paste0("V", 1:1000))
# 
# output_dir <- "C:/Users/Mattia/Desktop/NONPARAMETRIC STATISTICS/projectNPS/ShortDataset"
# file_name <- "BID.csv"
# percorso_file <- file.path(output_dir, file_name)
# 
# # Save the file
# write.table(df_bid, file = percorso_file, sep = "\t", row.names = FALSE, col.names = TRUE)
# 
# # Controlla che sia stato creato
# if (file.exists(percorso_file)) {
#   print("SAVED SUCCESFULLY")
# } else {
#   print("ERROR")
# }






# Costruisci il data frame
df_offer <- data.frame(t(dataoff))
colnames(df_offer) <- c(vdate)

# Specifica la directory e il nome del file
output_dir <- "C:/Users/Mattia/Desktop/NONPARAMETRIC STATISTICS/projectNPS/ShortDataset"
file_name <- "OFF.csv"
percorso_file <- file.path(output_dir, file_name)

# Salva il file
write.table(df_offer, file = percorso_file, sep = "\t", row.names = FALSE, col.names = TRUE)

# Controlla se è stato salvato correttamente
if (file.exists(percorso_file)) {
  print("SAVED SUCESSFULLY")
} else {
  print("ERROR")
}



df_bid <- data.frame(t(databid))
colnames(df_bid) <- c(vdate)

output_dir <- "C:/Users/Mattia/Desktop/NONPARAMETRIC STATISTICS/projectNPS/ShortDataset"
file_name <- "BID.csv"
percorso_file <- file.path(output_dir, file_name)

# Save the file
write.table(df_bid, file = percorso_file, sep = "\t", row.names = FALSE, col.names = TRUE)

# Controlla che sia stato creato
if (file.exists(percorso_file)) {
  print("SAVED SUCCESFULLY")
} else {
  print("ERROR")
}