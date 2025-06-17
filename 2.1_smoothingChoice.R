# SMOOTHING: Choose among
# - B-Spline basis
# - Local Averaging
# - Binning
# Based on MSE


#set working directory with the .CSV files 
setwd("C:/Users/Mattia/Desktop/NONPARAMETRIC STATISTICS/projectNPS//DatasetCSV")

#creates the list of all the xml files in the directory
listcsv <- dir(pattern = "*.csv")
i <- 200

# Open file
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

#Saving x and y
off_quant_cum <- cumsum(df_1_off$Quantita)
bid_quant_cum <- cumsum(df_1_bid$Quantita)

off_prices <- df_1_off$Prezzo
bid_prices <- df_1_bid$Prezzo

# Creating the step functions in order to visualize them 
step_off <- stepfun(off_quant_cum, c(off_prices, tail(off_prices, 1)))
step_bid <- stepfun(bid_quant_cum, c(bid_prices, tail(bid_prices, 1)))
plot(step_off, do.points = FALSE, col = "red", main = "Step Function", xlab = "Quantità cumulativa", ylab = "Prezzo",
     ylim = c(-500, 5000))
lines(step_bid, do.points = FALSE, col = "blue")

threshold <- c(0,50000)

n.sample <- 1000
x.off <- seq(threshold[1], threshold[2], length.out = n.sample)
y.off <- step_off(x.off)
x.bid <- seq(threshold[1], threshold[2], length.out = n.sample)
y.bid <- step_bid(x.bid)

# B-SPLINE #####

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

# with the sampling we use as x equispaced observations, so we can use
# so we can use equispaced breaks

Xps_off <- smooth.basis(argvals = x.off, y = y.off, fdParobj = basis_off)
Xps_bid <- smooth.basis(argvals = x.bid, y = y.bid, fdParobj = basis_bid)

# plots
plot(step_off, col = 'salmon', xlab="Quantity", ylab="Prices")
lines(step_bid, col = 'lightblue')
lines(Xps_off$fd , col="darkred",lwd = 1.5)
lines(Xps_bid$fd , col="darkblue",lwd = 1.5)

plot(step_off, col = 'salmon', xlab="Quantity", ylab="Prices", xlim = c(20000,50000), ylim = c(0,500))
lines(step_bid, col = 'lightblue')
lines(Xps_off$fd , col="darkred",lwd = 1.5)
lines(Xps_bid$fd, col="darkblue",lwd = 1.5)

## GCV ---------------------------------------------------------------

# generalized cross-validation for OFF
nbasis <- 30:400
gcv <- numeric(length(nbasis))
for (i in 1:length(nbasis)){
  basis <- create.bspline.basis( offRng, nbasis[i], m)
  gcv[i] <- smooth.basis(x.off, y.off, basis)$gcv
}
par(mfrow=c(1,1))
plot(nbasis,gcv)
nbasis[which.min(gcv)]
abline(v = nbasis[which.min(gcv)], col = 2)

# generalized cross-validation for BID
nbasis <- 30:400
gcv <- numeric(length(nbasis))
for (i in 1:length(nbasis)){
  basis <- create.bspline.basis( bidRng, nbasis[i], m)
  gcv[i] <- smooth.basis(x.bid, y.bid, basis)$gcv
}
par(mfrow=c(1,1))
plot(nbasis,gcv)
nbasis[which.min(gcv)]
abline(v = nbasis[which.min(gcv)], col = 2)

# n basis opt
# OFF: 315
# BID: 388

# but looking the elbow more o less 200


# LOCAL AVERAGES #########

library(np)

dataxoff = seq(x.off[1], tail(x.off, 1), length.out = 100)
datayoff = step_off(dataxoff)

dataoff <- data.frame(x = dataxoff, y = datayoff)

m_loc <- npreg(y ~ x,
               data = dataoff,
               ckertype = "uniform",
               bws = 500)  

x_grid <- data.frame(x = seq(min(x.off), max(x.off), length.out = 10000))

preds <- predict(m_loc, newdata = x_grid, se = T)

# Plot
plot(step_off, col = 'salmon', xlab="Quantity", ylab="Prices", xlim = c(0,50000), ylim = c(-500,500))
lines(x_grid$x, preds$fit, lwd = 2, col = "darkred")



dataxbid = seq(x.bid[1], tail(x.bid, 1), length.out = 100)
dataybid = step_bid(dataxbid)

databid <- data.frame(x = dataxbid, y = dataybid)

m_loc <- npreg(y ~ x,
               data = databid,
               ckertype = "uniform",
               bws = 500)  

x_grid <- data.frame(x = seq(min(x.bid), max(x.bid), length.out = 10000))

preds <- predict(m_loc, newdata = x_grid, se = T)

# Plot
plot(step_bid, col = 'lightblue', xlab="Quantity", ylab="Prices", xlim = c(0,50000), ylim = c(-500,4000))
lines(x_grid$x, preds$fit, lwd = 2, col = "darkblue")



# BINNING #####

uneven_breaks <- c(seq(0,15000,by=500),seq(16000,35000,by=200),seq(35200, 50000, by=500))

m_cut <- lm(y.off ~ cut(x.off,breaks=uneven_breaks))

grid <- seq(range(x.off)[1],range(x.off)[2], length.out = 1000)
preds <- predict(m_cut,list(x.off=grid),se=F)
# Plot
plot(step_off, col = 'salmon', xlab="Quantity", ylab="Prices", xlim = c(0,50000), ylim = c(-500,500))
lines(grid, preds, lwd = 2, col = "darkred")



uneven_breaks <- c(seq(0,10000,by=2000),seq(10200,37000,by=200),seq(38000, 50000, by=2000))

m_cut <- lm(y.bid ~ cut(x.bid,breaks=uneven_breaks))

grid <- seq(range(x.bid)[1],range(x.bid)[2], length.out = 1000)
preds <- predict(m_cut,list(x.bid=grid),se=F)
# Plot
plot(step_bid, col = 'lightblue', xlab="Quantity", ylab="Prices", xlim = c(0,50000), ylim = c(-500,4000))
lines(grid, preds, lwd = 2, col = "darkblue")



