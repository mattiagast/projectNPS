# KDE: where we have more bid in the curve

#DEMAND - OFFER CURVES


#Processing the data
# set the working directory with the dataset
setwd("C:/Users/Mattia/Desktop/NONPARAMETRIC STATISTICS/projectNPS/DatasetCSV")

listcsv <- dir(pattern = "*.csv")
quantoff <- NULL
quantbid <- NULL

for (i in 1:731){
  #trasform daily data from xml
  df <- read.csv(listcsv[i])
  
  #Choose hour of day
  df_1 <- df[df$Ora == 7,]
  
  df_1 <- df_1[df_1$ZonaMercato == "CALA;CNOR;CSUD;NORD;SARD;SICI;SUD;AUST;COAC;CORS;FRAN;GREC;SLOV;SVIZ;MALT;COUP;MONT;",]
  
  
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
  
  #Plot supply/demand curves
  plot(cumsum(df_1_off$Quantita), df_1_off$Prezzo, xlim=c(0,50000), ylim=c(0,500))
  points(cumsum(df_1_bid$Quantita), df_1_bid$Prezzo)
  
  
  quantoff <- c(quantoff, cumsum(df_1_off$Quantita))
  quantbid <- c(quantbid, cumsum(df_1_bid$Quantita))
  
}



# Hist
hist(quantoff, xlim = c(0, max(quantoff)), breaks = 20, 
     freq = F, col = "lightgray", border = "white", 
     main = "Histogram with Kernel Density", xlab = "quantoff")

# KDE
dens <- density(quantoff, from = 0, bw = 5000)
                # , kernel = "gaussian", bw = 5)

# Plot
lines(dens, col = "blue", lwd = 2)
rug(quantoff, col = "darkgray")  




# Hist
hist(quantbid, xlim = c(0, max(quantbid)), breaks = 20, 
     freq = F, col = "lightgray", border = "white", 
     main = "Histogram with Kernel Density", xlab = "quantoff")

# KDE
dens <- density(quantbid, from = 0, bw = 2500)


# Plot
lines(dens, col = "blue", lwd = 2)


# ggplot
library(ggplot2)

# Crea un data frame
df <- data.frame(quantoff = quantoff)

# Plot elegante e aggiornato
ggplot(df, aes(x = quantoff)) +
  # Istogramma trasparente
  geom_histogram(aes(y = after_stat(density)), 
                 bins = 20, fill = "magenta", color = "white", alpha = 0.3) +
  # KDE con banda personalizzata
  geom_density(color = "red", linewidth = 1.2, bw = 5000) +
  # Rug plot
  geom_rug(color = "darkred", alpha = 0.5) +
  # Titoli e tema
  labs(title = "Density quantity OFF",
       x = "Quantity [MWh]", y = "Density") +
  theme_light() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12)
  )



# ggplot
library(ggplot2)

# Crea un data frame
df <- data.frame(quantbid = quantbid)

# Plot elegante e aggiornato
ggplot(df, aes(x = quantbid)) +
  # Istogramma trasparente
  geom_histogram(aes(y = after_stat(density)), 
                 bins = 20, fill = "cyan", color = "white", alpha = 0.3) +
  # KDE con banda personalizzata
  geom_density(color = "blue", linewidth = 1.2, bw = 2500) +
  # Rug plot
  geom_rug(color = "darkblue", alpha = 0.5) +
  # Titoli e tema
  labs(title = "Density quantity BID",
       x = "Quantity [MWh]", y = "Density") +
  theme_light() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12)
  )


