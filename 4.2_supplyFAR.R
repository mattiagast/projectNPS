setwd("C:/Users/Mattia/Desktop/NONPARAMETRIC STATISTICS/projectNPS/PredictionData")
data_off <- read.csv("OFF.csv", header = T, sep = "\t")

data_off <- as.matrix(data_off)

threshold <- c(0,50000)
n.sample <- 1000
x.off <- seq(threshold[1], threshold[2], length.out = n.sample)

library(fda)

noff <- n.sample
offRng <- range(x.off)

# spline order
m <- 1

# number of bases
nbasis_off <- 200

# create the basis with B-Spline
basis_off <- create.bspline.basis(offRng, nbasis = nbasis_off, norder = m)

# smmoth the data
data_off_obj <- smooth.basis(y = data_off, argvals = x.off, fdParobj = basis_off)

# Plot the functional data
data_off.fd <- data_off_obj$fd
palette <- colorRampPalette(c("yellow", "red"))(dim(data_off)[2])
plot.fd(data_off.fd, col = palette, lwd = 1, xlab = "Quantity [MWh]", ylab = "Price [Euro]")

# FPCA of OFF -----------------------------------------------------------------

data_off.fd$basis$params <- data_off.fd$basis$params[-length(data_off.fd$basis$params)]
pca_off <- pca.fd(data_off.fd, nharm = 119, centerfns=TRUE)

# scree plot
plot(pca_off$values, xlab='j',ylab='Eigenvalues')
plot(cumsum(pca_off$values)/sum(pca_off$values), xlab='j', ylab='CPV',ylim=c(0.8,1))

# plot of the FPCs as perturbation of the mean
media_off <- mean.fd(data_off.fd)

# Covariance
library(fields)
linspace <- seq(threshold[1], threshold[2], length.out = 500)
eval <- eval.fd(linspace, data_off.fd)
image.plot(linspace, linspace, (cov(t(eval))))

# FAR(1) for  OFF --------------------------------------------------------------

npc <- 30

# Step 1: Extraction of the scores, eigenvalues, basis and loadings
scores.off <- pca_off$scores[, 1:npc]
lambdas <- pca_off$values[1:npc]
harmonics <- pca_off$harmonics
load <- harmonics$coefs[, 1:npc]

# Data splitting
scores.cal <- scores.off[16:30, ]     # Calibration
scores.pred <- scores.off[61, ]        # Prediction
scores.off <- scores.off[31:60, ]     # Training

# Step 2: dimensions of the matrices
n <- nrow(scores.off)
p <- ncol(scores.off)

# Matrices of delayed observed (X) and current data (Y)
X <- scores.off[1:(n-1), ]
Y <- scores.off[2:n, ]


# Step 3: Estimate of the Hilbert-Schmidt (B)
Psi_hat <- 1/(n-1) * t(X)%*%Y
for(i in 1:p){
  for(j in 1:p){
    Psi_hat[i,j] <- Psi_hat[i,j]/lambdas[j]
  }
}

# Prediction Model
model <- function(scores) {
  Psi_vec <- rep(0, p)
  X_hat <- rep(0, nbasis_off)
  
  for (k in 1:p) {
    for (l in 1:p) {
      Psi_vec[k] <- Psi_hat[k, l] * scores[l] + Psi_vec[k]
    }
    X_hat <- X_hat + Psi_vec[k] * load[, k]
  }
  
  X_hat_fd <- fd(coef = X_hat, basisobj = basis_off)
  X_pred <- media_off + X_hat_fd
  
  return(X_pred)
}

X_pred <- model(scores.pred)
plot(X_pred)


res.vec <- numeric(15)
for (ind in 17:31){
  X_cal <- model(scores.cal[(ind-16), ])
  true_cal <- smooth.basis(y = data_off[, ind], argvals = x.off, fdParobj = basis_off)
  
  eval_true <- eval.fd(linspace ,true_cal$fd)
  eval_pred <- eval.fd(linspace ,X_cal)
  
  res.norm <- abs(eval_true - eval_pred) / pmax(sqrt(diag(cov(t(eval)))), 1)
  res.vec[ind-16] <- max(res.norm) 
  
}

alpha <- 0.1
eps <- quantile(res.vec, probs = 1 - alpha) 




real_curve <- smooth.basis(y = data_off[, 61], argvals = x.off, fdParobj = basis_off)
real_eval <- eval.fd(x.off ,real_curve$fd)

plot(x.off, real_eval, type = 'l')

X_pred_eval <- eval.fd(linspace ,X_pred)
upr <- X_pred_eval + eps * sqrt(diag(cov(t(eval))))
lwr <- X_pred_eval - eps * sqrt(diag(cov(t(eval))))


# Plot della funzione predetta
plot(linspace, X_pred_eval, type = "l", col = "blue", lwd = 2,
     ylab = "Prediction", xlab = "t", ylim = range(c(upr, lwr, X_pred_eval)))

# Aggiunta dei prediction bounds
lines(linspace, upr, col = "red", lty = 2, lwd = 2)
lines(linspace, lwr, col = "red", lty = 2, lwd = 2)


# ggplot
library(ggplot2)

df_pred <- data.frame(
  t = linspace,
  pred = X_pred_eval,
  upr = upr,
  lwr = lwr
)
names(df_pred) <- c("t", "pred", "upr", "lwr")

# Limiti
a <- -550
b <- 400

# Clipping dei prediction bounds
df_pred$upr <- pmin(df_pred$upr, b)
df_pred$lwr <- pmax(df_pred$lwr, a)

# DataFrame della curva reale (già esistente)
df_real <- data.frame(
  t = x.off,
  real = real_eval
)

# Aggiungi colonna label al df_pred e df_real
df_pred$label <- "Predicted"
df_real$label <- "Real"

# Combina i due dataframe in uno solo per le curve
df_lines <- rbind(
  data.frame(t = df_pred$t, value = df_pred$pred, label = df_pred$label),
  data.frame(t = df_real$t, value = df_real$real, label = df_real$label)
)

# Plot finale con linea tratteggiata per la curva reale
ggplot() +
  geom_ribbon(data = df_pred, aes(x = t, ymin = lwr, ymax = upr),
              fill = "magenta", alpha = 0.2) +
  geom_line(data = df_lines, aes(x = t, y = value, color = label, linetype = label), size = 0.5) +
  scale_color_manual(values = c("Predicted" = "darkred", "Real" = "black")) +
  scale_linetype_manual(values = c("Predicted" = "solid", "Real" = "dashed")) +
  theme_light() +
  labs(
    x = "Quantity [MWh]",
    y = "Price [Euro/MWh]",
    color = "Legend",
    linetype = "Legend",
    title = "Prediction 27.02.2024"
  ) +
  theme(
    plot.title = element_text(hjust = 0, face = "bold")
  ) +
  ylim(a, b)
