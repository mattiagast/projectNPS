# OUTLIER ANALYSIS

setwd("C:/Users/Mattia/Desktop/NONPARAMETRIC STATISTICS/projectNPS/ShortDataset")
data_off <- read.csv("OFF.csv", header = T, sep = "\t")
data_bid <- read.csv("BID.csv", header = T, sep = "\t")
colnames(data_off) <- seq(1,470)
colnames(data_bid) <- seq(1,470)

threshold <- c(0,50000)
n.sample <- 1000
x.off <- seq(threshold[1], threshold[2], length.out = n.sample)
x.bid <- seq(threshold[1], threshold[2], length.out = n.sample)

data_off <- as.matrix(data_off)
data_bid <- as.matrix(data_bid)

# SMOOTHING ---------------------------------------------------------------------

library(fda)

noff <- n.sample
offRng <- range(x.off)
nbid <- n.sample
bidRng <- range(x.bid)

# spline order
m <- 1

# number of bases
nbasis_off <- 200
nbasis_bid <- 200

# create the basis with B-Splin
basis_off <- create.bspline.basis(offRng, nbasis = nbasis_off, norder = m)
basis_bid <- create.bspline.basis(bidRng, nbasis = nbasis_bid, norder = m)

data_off_obj <- smooth.basis(y = data_off, argvals = x.off, fdParobj = basis_off)
data_bid_obj <- smooth.basis(y = data_bid, argvals = x.bid, fdParobj = basis_bid)

data_off.fd <- data_off_obj$fd
data_bid.fd <- data_bid_obj$fd

plot(data_off.fd, col = 'blue', lwd = 1)
plot(data_bid.fd, col = 'blue', lwd = 1)


# ggplot

library(ggplot2)
library(tidyr)
library(dplyr)

# Estrai i valori delle funzioni smussate
eval_points <- seq(threshold[1], threshold[2], length.out = 10000)  # punti su cui valutare

# Valuta le funzioni smussate su questi punti
smoothed_off <- eval.fd(eval_points, data_off.fd)  # matrix 1000 x N
smoothed_bid <- eval.fd(eval_points, data_bid.fd)

# Trasformali in dataframe "long"
df_off <- as.data.frame(smoothed_off)
df_off$x <- eval_points
df_off_long <- pivot_longer(df_off, cols = -x, names_to = "id", values_to = "value")

df_bid <- as.data.frame(smoothed_bid)
df_bid$x <- eval_points
df_bid_long <- pivot_longer(df_bid, cols = -x, names_to = "id", values_to = "value")


# Plot OFF
ggplot(df_off_long, aes(x = x, y = value, group = id)) +
  geom_line(alpha = 0.3, color = "steelblue") +
  labs(title = "Smoothed Functional Data - OFF",
       x = "Quantity[MWh]", y = "Price [Euro/MWh]") +
  theme_light() +
  theme(plot.title = element_text(face = "bold"))

# Plot BID
ggplot(df_bid_long, aes(x = x, y = value, group = id)) +
  geom_line(alpha = 0.3, color = "darkgreen") +
  labs(title = "Smoothed Functional Data - BID",
       x = "Quantity [MWh]", y = "Price [Euro/MWh]") +
  theme_light() +
  theme(plot.title = element_text(face = "bold"))


# secondo try
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

# Aggiungi colonna con indice numerico
df_off_long <- df_off_long %>%
  mutate(id_num = as.numeric(gsub("V", "", id)))  # estrae il numero da "V1", "V2", ecc.
df_off_long$id_num <- as.numeric(gsub("V", "", df_off_long$id))


library(RColorBrewer)

ggplot(df_off_long, aes(x = x, y = value, group = id, color = id_num)) +
  geom_line(alpha = 0.8, linewidth = 0.6) +
  scale_color_distiller(palette = "Reds") +   # palette preimpostata "Reds" tutta su toni di rosso
  labs(title = "Smoothed Functional Data - OFF",
       x = "Quantity [MWh]", y = "Price [EUR/MWh]",
       color = "Curve Index") +
  theme_light() +
  theme(plot.title = element_text(face = "bold")) +
  theme(legend.position = "none")



# secondo try
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

# Aggiungi colonna con indice numerico
df_bid_long <- df_bid_long %>%
  mutate(id_num = as.numeric(gsub("V", "", id)))  # estrae il numero da "V1", "V2", ecc.
df_bid_long$id_num <- as.numeric(gsub("V", "", df_bid_long$id))


library(RColorBrewer)

ggplot(df_bid_long, aes(x = x, y = value, group = id, color = id_num)) +
  geom_line(alpha = 0.8, linewidth = 0.6) +
  scale_color_distiller(palette = "Blues") +   # palette preimpostata "Reds" tutta su toni di rosso
  labs(title = "Smoothed Functional Data - BID",
       x = "Quantity [MWh]", y = "Price [EUR/MWh]",
       color = "Curve Index") +
  theme_light() +
  theme(plot.title = element_text(face = "bold")) +
  theme(legend.position = "none")


# OUTLIERS ########
library(roahd)

# Estrai i valori delle funzioni smussate
eval_points <- seq(17000, 37000, length.out = 20)  # punti su cui valutare

# Valuta le funzioni smussate su questi punti
smoothed_off <- eval.fd(eval_points, data_off.fd)  # matrix 1000 x N
smoothed_bid <- eval.fd(eval_points, data_bid.fd)

f_off <- fData(eval_points, t(smoothed_off))
f_bid <- fData(eval_points, t(smoothed_bid))


fbplot(f_off, Fvalue = 2.0, main = "fBoxPlot OFF")
fbplot(f_bid, Fvalue = 2.0, main = "fBoxPlot BID")

outliergram(f_off, Fvalue = 2.0)
outliergram(f_bid, Fvalue = 2.0)


