# GRAPHICAL EXPLORATION OF THE DATASET

# open the data
setwd("C:/Users/Mattia/Desktop/NONPARAMETRIC STATISTICS/projectNPS/ShortDataset")
df <- read.table("PrezzoZonale.txt")

df_23 <- df[1:365, 1:24]
df_24 <- df[366:731, 1:24]

# Heatplots - Final price -------------- 

mtr23 <- as.matrix(df_23)
mtr24 <- as.matrix(df_24)


library(ggplot2)
library(reshape2)
library(viridis)

# Transoform the matrix
df_23_long <- melt(mtr23)
colnames(df_23_long) <- c("Day", "Hour", "Price")
df_23_long$Hour <- as.numeric(df_23_long$Hour)

# Add date column
date_seq <- seq(as.Date("2023-01-01"), by = "day", length.out = 365)
df_23_long$Date <- date_seq[df_23_long$Day]

# Plot
ggplot(df_23_long, aes(x = Date, y = Hour, fill = Price)) +
  geom_tile() +
  scale_fill_gradientn(colours = viridis(200), na.value = "white") +
  scale_y_continuous(breaks = seq(1, 24, by = 3)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(title = "2023 Final Price", x = "Date", y = "Hour") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



# Transoform the matrix
df_24_long <- melt(mtr24)
colnames(df_24_long) <- c("Day", "Hour", "Price")
df_24_long$Hour <- as.numeric(df_24_long$Hour)

# Add date column
date_seq <- seq(as.Date("2024-01-01"), by = "day", length.out = 366)
df_24_long$Date <- date_seq[(df_24_long$Day-365)]

# Plot
ggplot(df_24_long, aes(x = Date, y = Hour, fill = Price)) +
  geom_tile() +
  scale_fill_gradientn(colours = viridis(200), na.value = "white") +
  scale_y_continuous(breaks = seq(1, 24, by = 3)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(title = "2024 Final Price", x = "Date", y = "Hour") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# Boxplot - Hours --------------

library(reshape2)
library(ggplot2)
library(viridis)

# Melt 
df_long <- melt(df[, 1:24])
colnames(df_long) <- c("Hour", "Price")

# Hours as level
df_long$Hour <- as.numeric(gsub("V", "", df_long$Hour))
df_long$Hour <- factor(df_long$Hour, levels = 1:24)

# Boxplot
ggplot(df_long, aes(x = Hour, y = Price)) +
  geom_boxplot(fill = viridis(100)[65], color = viridis(100)[20], outlier.size = 0.7) +
  scale_x_discrete(breaks = seq(1, 24, 3)) +  # ogni 3 ore
  labs(title = "Final Price grouped by Hour", x = "Hour", y = "Price [Euro/MWh]") +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.title = element_text(size = 14, face = "bold")
  )

# Boxplot - Days + ANOVA  --------------

df <- df[,1:24]

# Groping by days
daysOfTheWeek <- c("MON", "TUE", "WED", "THU", "FRI", "SAT", "SUN")
days <- c("SUN")
for(i in length(days):105){
  days <- c(days, daysOfTheWeek)
}
days <- days[1:731]
df <- cbind(df, days)
df$days <- factor(df$days, levels = daysOfTheWeek)


# plot
par(mfrow = c(1,1))
boxplot(df$V7 ~ df$days, main = "2023-2024", 
        xlab = "Hours", ylab = "Price [EUR/MWh]", col = 'lightblue')
means <- tapply(df$V7, df$days, mean, na.rm = TRUE)
for (i in 1:7) {
  lines(c(i - 0.4, i + 0.4), c(means[i], means[i]), col = "blue", lwd = 2)
}

# Plot with ggplot
library(ggplot2)
library(dplyr)

mean_data <- df %>%
  group_by(days) %>%
  summarise(mean_price = mean(V7, na.rm = TRUE))

ggplot(df, aes(x = days, y = V7)) +
  geom_boxplot(fill = viridis(100)[65], color = viridis(100)[20], outlier.size = 0.7) +
  geom_segment(data = mean_data,
               aes(x = as.numeric(days) - 0.4, xend = as.numeric(days) + 0.4,
                   y = mean_price, yend = mean_price),
               color = "springgreen", linewidth = 1.2) +
  labs(title = "Final Price grouped by Day", x = "Days", y = "Price [EUR/MWh]") +
  theme_light() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# verify the assumptions ofANOVA:
# 1) normality (univariate) in each group (7 tests)
Ps <- c(shapiro.test(df$V7[days==daysOfTheWeek[1]])$p,
        shapiro.test(df$V7[days==daysOfTheWeek[2]])$p,
        shapiro.test(df$V7[days==daysOfTheWeek[3]])$p,
        shapiro.test(df$V7[days==daysOfTheWeek[4]])$p,
        shapiro.test(df$V7[days==daysOfTheWeek[5]])$p,
        shapiro.test(df$V7[days==daysOfTheWeek[6]])$p,
        shapiro.test(df$V7[days==daysOfTheWeek[7]])$p) 
Ps

# Not fulfilled -> NONPARAMETRIC ANOVA
df_anova1 <- df[!is.na(df$V7), ]
fit <- aov(df_anova1$V7 ~ df_anova1$days)
summary(fit)

# Test statistics in the pooled sample
T0 <- summary(fit)[[1]][1,4]  
T0

B = 10000
T_stat <- numeric(B) 
n <- dim(df_anova1)[1]

for(perm in 1:B){
  # Permutation:
  permutation <- sample(1:n)
  price_perm <- df_anova1$V7[permutation]
  fit_perm <- aov(price_perm ~ df_anova1$days)
  
  # Test statistic:
  T_stat[perm] <- summary(fit_perm)[[1]][1,4]
}

hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)

p_val <- sum(T_stat>=T0)/B
p_val

# ggplot

library(ggplot2)
library(viridis)

# plot with ggplot
df_hist <- data.frame(T_stat = T_stat)

# Plot
ggplot(df_hist, aes(x = T_stat)) +
  geom_histogram(aes(y = ..density.., fill = ..x..), bins = 30, color = "white") +
  scale_fill_viridis_c() + geom_vline(xintercept = T0, color = "red", linewidth = 1) +
  labs(title = "Permutation Distribution of Test Stat",
       x = "Test Statistic", y = "Density") +
  theme_light() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12)
  ) + annotate("text", x = T0, y = 0, label = "T0", vjust = 4.5, color = "red", size = 4
  ) + theme(legend.position = "none")





# Boxplot - Week + ANOVA -----------------------
df$days <- as.character(df$days)
week.days <- NULL
for(i in 1:731){
  if(df$days[i] == "SAT" || df$days[i] == "SUN"){
    week.days[i] = "END"
  } else {
    week.days[i] = "WEEK"
  }
}

df <- cbind(df, week.days)
weekend <- c("WEEK", "END")
df$week.days <- factor(df$week.days, levels = weekend)

# plot
boxplot(df$V7 ~ df$week.days, main = "2023-2024", 
        xlab = "Hours", ylab = "Price [EUR/MWh]", col = 'lightblue')
means <- tapply(df$V7, df$week.days, mean, na.rm = TRUE)
for (i in 1:2) {
  lines(c(i - 0.5, i + 0.5), c(means[i], means[i]), col = "blue", lwd = 2)
}

# Plot with ggplot
library(ggplot2)
library(dplyr)

means <- df %>%
  group_by(week.days) %>%
  summarise(mean_price = mean(V7, na.rm = TRUE))

ggplot(df, aes(x = week.days, y = V7)) +
  geom_boxplot(fill = viridis(100)[65], color = viridis(100)[20], outlier.size = 0.7) +
  geom_segment(data = means, aes(x = as.numeric(week.days) - 0.4,
                                 xend = as.numeric(week.days) + 0.4,
                                 y = mean_price, yend = mean_price),
               color = "springgreen", linewidth = 1.2) +
  labs(title = "Final Price grouped by Week vs Weekend", x = "Days", y = "Price [EUR/MWh]") +
  theme_light() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 12)
  )


# verify the assumptions:
# i) normality
Ps <- c(shapiro.test(df$V7[week.days==weekend[1]])$p,
        shapiro.test(df$V7[week.days==weekend[2]])$p)
Ps

# Not fulfilled -> NONPARAMETRIC ANOVA
df_anova2 <- df[!is.na(df$V7), ]
fit <- aov(df_anova2$V7 ~ df_anova2$week.days)
summary(fit)

# Test statistics in the pooled sample
T0 <- summary(fit)[[1]][1,4]  
T0

B = 10000
T_stat <- numeric(B) 
n <- dim(df_anova2)[1]

for(perm in 1:B){
  # Permutation:
  permutation <- sample(1:n)
  price_perm <- df_anova2$V7[permutation]
  fit_perm <- aov(price_perm ~ df_anova2$week.days)
  
  # Test statistic:
  T_stat[perm] <- summary(fit_perm)[[1]][1,4]
}

hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)

p_val <- sum(T_stat>=T0)/B
p_val


# ggplot
df_hist <- data.frame(T_stat = T_stat)

# Plot
ggplot(df_hist, aes(x = T_stat)) +
  geom_histogram(aes(y = ..density.., fill = ..x..), bins = 30, color = "white") +
  scale_fill_viridis_c() + geom_vline(xintercept = T0, color = "red", linewidth = 1) +
  labs(title = "Permutation Distribution of Test Stat",
       x = "Test Statistic", y = "Density") +
  theme_light() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12)
  ) + annotate("text", x = T0, y = 0, label = "T0", vjust = 4.5, color = "red", size = 4
  ) + theme(legend.position = "none")




