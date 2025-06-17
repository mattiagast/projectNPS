# FUNCTIONAL INFERENCE

setwd("C:/Users/Mattia/Desktop/NONPARAMETRIC STATISTICS/projectNPS/ShortDataset")
data_off7 <- read.csv("OFF.csv", header = T, sep = "\t")
data_bid7 <- read.csv("BID.csv", header = T, sep = "\t")
vdate7 <- colnames(data_off7)

data_off20 <- read.csv("OFF20.csv", header = T, sep = "\t")
data_bid20 <- read.csv("BID20.csv", header = T, sep = "\t")
vdate20 <- colnames(data_off20)

vdates <- intersect(vdate7, vdate20)


data_off7 <- data_off7[, colnames(data_off7) %in% vdates]
data_off20 <- data_off20[, colnames(data_off20) %in% vdates]

data_bid7 <- data_bid7[, colnames(data_bid7) %in% vdates]
data_bid20 <- data_bid20[, colnames(data_bid20) %in% vdates]


data_off7 <- t(as.matrix(data_off7))[, 1:1000]
data_bid7 <- t(as.matrix(data_bid7))[, 1:1000]

data_off20 <- t(as.matrix(data_off20))[, 1:1000]
data_bid20 <- t(as.matrix(data_bid20))[, 1:1000]


threshold <- c(0,50000)
n.sample <- 1000
x.off <- seq(threshold[1], threshold[2], length.out = n.sample)
x.bid <- seq(threshold[1], threshold[2], length.out = n.sample)

library(fdatest)


# Performing the ITP
ITP.off <- ITP2bspline(data_off7, 
                       data_off20,
                       order = 1, nknots = 120,
                       B=1000,
                       paired = T)


# Plotting the results of the ITP
# plot(ITP.off,main='data',xrange=c(1,45000),xlab='Day')


# Performing the ITP
ITP.bid <- ITP2bspline(data_bid7, 
                       data_bid20,
                       order = 1, nknots = 120,
                       B=1000,
                       paired = T)


# Plotting the results of the ITP
# plot(ITP.bid,main='data',xrange=c(1,50000),xlab='Day')


data_off <- rbind(data_off7, data_off20)
data_bid <- rbind(data_bid7, data_bid20)


library(fda)

noff <- 1000
offRng <- c(0, 50000)
nbid <- 1000
bidRng <- c(0, 50000)

# spline order
m <- 1

# number of bases
nbasis_off <- 200
nbasis_bid <- 200

# create the basis with B-Splin
basis_off <- create.bspline.basis(offRng, nbasis = nbasis_off, norder = m)
basis_bid <- create.bspline.basis(bidRng, nbasis = nbasis_bid, norder = m)

data_off_obj <- smooth.basis(y = t(data_off), argvals = x.off[1:1000], fdParobj = basis_off)
data_bid_obj <- smooth.basis(y = t(data_bid), argvals = x.bid[1:1000], fdParobj = basis_bid)

data_off.fd <- data_off_obj$fd
data_bid.fd <- data_bid_obj$fd

plot(data_off.fd, col = 'blue', lwd = 1)
plot(data_bid.fd, col = 'blue', lwd = 1)


# ggplot

library(ggplot2)
library(tidyr)
library(dplyr)

# Estrai i valori delle funzioni smussate
eval_points <- seq(threshold[1], threshold[2], length.out = 1500)  # punti su cui valutare

# Valuta le funzioni smussate su questi punti
smoothed_off <- eval.fd(eval_points, data_off.fd)  # matrix 1000 x N
smoothed_bid <- eval.fd(eval_points, data_bid.fd)

groups1 <- c(rep(0, 333), rep(1, 333))

# Crea una label per il gruppo: 0 -> WEEK, 1 -> WEEKEND
group_labels <- factor(groups1, levels = c(0, 1), labels = c("H07", "H20"))


n_funzioni <- ncol(smoothed_off)


# OFF
df_off_long <- data.frame(
  x = rep(eval_points, times = n_funzioni),
  value = as.vector(smoothed_off),
  id = rep(1:n_funzioni, each = length(eval_points)),
  group = rep(group_labels, each = length(eval_points))
)


# Supponiamo che il tuo asse x vada da threshold[1] a threshold[2]
x_breaks <- seq(threshold[1], threshold[2], length.out = 120)  # 25 intervalli = 26 punti

# Costruisci un data frame per le bande
pval_df <- data.frame(
  xmin = x_breaks[-length(x_breaks)],
  xmax = x_breaks[-1],
  pval = ITP.off$corrected.pval
)

# Aggiungi la colonna 'shade' per decidere il colore
pval_df$shade <- cut(
  pval_df$pval,
  breaks = c(-Inf, 0.01, 0.1, Inf),
  labels = c("darkgray", "lightgray", NA),
  right = FALSE
)

# Rimuovi righe senza ombreggiatura
pval_df <- pval_df[!is.na(pval_df$shade), ]


ggplot() +
  # Sfondo: bande pval
  geom_rect(data = pval_df,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = shade),
            alpha = 0.3, inherit.aes = FALSE) +
  
  # Curva: dati funzionali
  geom_line(data = df_off_long,
            aes(x = x, y = value, group = id, color = group),
            alpha = 0.3) +
  
  scale_fill_identity() +
  scale_color_manual(values = c("steelblue", "firebrick")) +
  labs(title = "IW 2 Paired populations TEST - OFF",
       x = "Quantity [MWh]", y = "Price [Euro/MWh]",
       color = "Hour Day") +
  theme_light() +
  theme(plot.title = element_text(face = "bold"))





n_funzioni <- ncol(smoothed_bid)


# BID
df_bid_long <- data.frame(
  x = rep(eval_points, times = n_funzioni),
  value = as.vector(smoothed_bid),
  id = rep(1:n_funzioni, each = length(eval_points)),
  group = rep(group_labels, each = length(eval_points))
)


# Supponiamo che il tuo asse x vada da threshold[1] a threshold[2]
x_breaks <- seq(threshold[1], threshold[2], length.out = 120)  # 25 intervalli = 26 punti

# Costruisci un data frame per le bande
pval_df <- data.frame(
  xmin = x_breaks[-length(x_breaks)],
  xmax = x_breaks[-1],
  pval = ITP.bid$corrected.pval
)

# Aggiungi la colonna 'shade' per decidere il colore
pval_df$shade <- cut(
  pval_df$pval,
  breaks = c(-Inf, 0.01, 0.1, Inf),
  labels = c("darkgray", "lightgray", NA),
  right = FALSE
)

# Rimuovi righe senza ombreggiatura
pval_df <- pval_df[!is.na(pval_df$shade), ]


ggplot() +
  # Sfondo: bande pval
  geom_rect(data = pval_df,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = shade),
            alpha = 0.3, inherit.aes = FALSE) +
  
  # Curva: dati funzionali
  geom_line(data = df_bid_long,
            aes(x = x, y = value, group = id, color = group),
            alpha = 0.3) +
  
  scale_fill_identity() +
  scale_color_manual(values = c("steelblue", "firebrick")) +
  labs(title = "IW 2 Paired populations TEST - BID",
       x = "Quantity [MWh]", y = "Price [Euro/MWh]",
       color = "Hour Day") +
  theme_light() +
  theme(plot.title = element_text(face = "bold"))

