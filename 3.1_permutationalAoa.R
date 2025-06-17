# FUNCTIONAL INFERENCE

setwd("C:/Users/Mattia/Desktop/NONPARAMETRIC STATISTICS/projectNPS/ShortDataset")
data_off <- read.csv("OFF.csv", header = T, sep = "\t")
data_bid <- read.csv("BID.csv", header = T, sep = "\t")
vdate <- colnames(data_off)

data_off <- t(as.matrix(data_off))[, 1:750]
data_bid <- t(as.matrix(data_bid))[, 1:750]


threshold <- c(0,38000)
n.sample <- 750
x.off <- seq(threshold[1], threshold[2], length.out = n.sample)
x.bid <- seq(threshold[1], threshold[2], length.out = n.sample)

# Obtain grouping IN week and END week
vdate <- as.Date(sub("X", "", vdate), format = "%Y.%m.%d")
weekdays_abbr <- toupper(format(vdate, "%a")) 
groups1 <- ifelse(weekdays_abbr %in% c("SAB", "DOM"), 1, 0)


library(fdatest)

# Performing the ITP
ITP.off <- ITPaovbspline(data_off ~ groups1,
                            B=1000,nknots=25,order=1,
                            method="responses")

# All graphics on the same device
plot(ITP.off,
     main='Grouping by WEEK vs WEEKEND', plot.adjpval = TRUE,xlab='Quantity[MWh]',xrange=c(0 ,x.off[750])
)


# Performing the ITP
ITP.bid <- ITPaovbspline(data_bid ~ groups1,
                         B=1000,nknots=25,order=1,
                         method="responses")

# All graphics on the same device
plot(ITP.bid,
     main='Grouping by WEEK vs WEEKEND', plot.adjpval = TRUE,xlab='Quantity[MWh]',xrange=c(0 ,x.off[750])
)



library(fda)

noff <- 750
offRng <- c(0, x.off[noff])
nbid <- 750
bidRng <- c(0, x.off[noff])

# spline order
m <- 1

# number of bases
nbasis_off <- 120
nbasis_bid <- 120

# create the basis with B-Splin
basis_off <- create.bspline.basis(offRng, nbasis = nbasis_off, norder = m)
basis_bid <- create.bspline.basis(bidRng, nbasis = nbasis_bid, norder = m)

data_off_obj <- smooth.basis(y = t(data_off), argvals = x.off[1:750], fdParobj = basis_off)
data_bid_obj <- smooth.basis(y = t(data_bid), argvals = x.bid[1:750], fdParobj = basis_bid)

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

# Crea una label per il gruppo: 0 -> WEEK, 1 -> WEEKEND
group_labels <- factor(groups1, levels = c(0, 1), labels = c("WEEK", "WEEKEND"))

# ----- OFF -----
n_funzioni <- ncol(smoothed_off)
eval_points <- seq(threshold[1], threshold[2], length.out = 1500)

# OFF
df_off_long <- data.frame(
  x = rep(eval_points, times = n_funzioni),
  value = as.vector(smoothed_off),
  id = rep(1:n_funzioni, each = length(eval_points)),
  group = rep(group_labels, each = length(eval_points))
)

ggplot(df_off_long, aes(x = x, y = value, group = id, color = group)) +
  geom_line(alpha = 0.3) +
  labs(title = "Smoothed Functional Data - OFF",
       x = "Quantity [MWh]", y = "Price [Euro/MWh]",
       color = "Day Type") +
  theme_light() +
  theme(plot.title = element_text(face = "bold")) +
  scale_color_manual(values = c("steelblue", "firebrick"))

ITP.off$corrected.pval.F




# Supponiamo che il tuo asse x vada da threshold[1] a threshold[2]
x_breaks <- seq(threshold[1], threshold[2], length.out = 25)  # 25 intervalli = 26 punti

# Costruisci un data frame per le bande
pval_df <- data.frame(
  xmin = x_breaks[-length(x_breaks)],
  xmax = x_breaks[-1],
  pval = ITP.off$corrected.pval.F
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
  labs(title = "IW ANOVA TEST - OFF",
       x = "Quantity [MWh]", y = "Price [Euro/MWh]",
       color = "Day Type") +
  theme_light() +
  theme(plot.title = element_text(face = "bold"))




# ----- BID -----
n_funzioni <- ncol(smoothed_bid)
eval_points <- seq(threshold[1], threshold[2], length.out = 1500)

# BID
df_bid_long <- data.frame(
  x = rep(eval_points, times = n_funzioni),
  value = as.vector(smoothed_bid),
  id = rep(1:n_funzioni, each = length(eval_points)),
  group = rep(group_labels, each = length(eval_points))
)

ggplot(df_bid_long, aes(x = x, y = value, group = id, color = group)) +
  geom_line(alpha = 0.3) +
  labs(title = "Smoothed Functional Data - BID",
       x = "Quantity [MWh]", y = "Price [Euro/MWh]",
       color = "Day Type") +
  theme_light() +
  theme(plot.title = element_text(face = "bold")) +
  scale_color_manual(values = c("steelblue", "firebrick"))



# Supponiamo che il tuo asse x vada da threshold[1] a threshold[2]
x_breaks <- seq(threshold[1], threshold[2], length.out = 25)  # 25 intervalli = 26 punti

# Costruisci un data frame per le bande
pval_df <- data.frame(
  xmin = x_breaks[-length(x_breaks)],
  xmax = x_breaks[-1],
  pval = ITP.bid$corrected.pval.F
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
  labs(title = "IW ANOVA TEST - BID",
       x = "Quantity [MWh]", y = "Price [Euro/MWh]",
       color = "Day Type") +
  theme_light() +
  theme(plot.title = element_text(face = "bold"))
