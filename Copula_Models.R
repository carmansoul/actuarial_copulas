library(copula)
library(VineCopula)
library(dplyr)
library(ggplot2)

data <- read.csv("SwedishMotorInsurance.csv")

X <- data$Claims  
Y <- data$Payment

U <- pobs(X)
V <- pobs(Y)

copula_fit <- BiCopSelect(U, V, familyset = NA)
copula_fit

summary(copula_fit)

#SCATTERPLOT OF WHAT IS LABELED DATA SET 2 IN PAPER: SWEDISH DATA SET
ggplot(data.frame(U, V), aes(x = U, y = V)) +
  geom_point(alpha = 0.5, color = "red") +  # Adjust transparency and color
  labs(title = "Scatter Plot of Claims vs Payment",
       x = "U (Claims)",
       y = "V (Payment)") +
  theme_minimal()

X2 <- data$Insured
Y2 <- data$Payment

U2 <- pobs(X2)
V2 <- pobs(Y2)

copula_fit2 <- BiCopSelect(U2, V2, familyset = NA)
copula_fit2

summary(copula_fit2)

#SCATTERPLOT OF WHAT IS LABELED DATA SET 1 IN PAPER: SWEDISH DATA SET
ggplot(data.frame(U2, V2), aes(x = U2, y = V2)) +
  geom_point(alpha = 0.5) +  # Adjust transparency for better visualization
  labs(title = "Scatter Plot of Pseudo-Observations",
       x = "U2 (Insured)",
       y = "V2 (Payment)") +
  theme_minimal()

#CONTOUR PLOT FOR BOTH SWEDISH DATA SET COPULAS
# Plot Claims & Payment copula
par(mfrow = c(1, 2))  # Set up for two plots side by side
contour(BiCop(family = copula_fit$family, 
              par = copula_fit$par, 
              par2 = copula_fit$par2), 
        main = "Contour Plot: Claims & Payment", col = "blue")

# Plot Insured & Payment copula
contour(BiCop(family = copula_fit2$family, 
              par = copula_fit2$par, 
              par2 = copula_fit2$par2), 
        main = "Contour Plot: Insured & Payment", col = "red")

# Reset plotting layout
par(mfrow = c(1, 1))

data2 <- read.csv("TermLife.csv")

X3 <- data2$INCOME
Y3 <- data2$FACE

U3<- pobs(X3)
V3<- pobs(Y3)

copula_fit3 <- BiCopSelect(U3, V3, familyset = NA)
copula_fit3

summary(copula_fit3)

#SCATTERPLOT OF WHAT IS LABELED BEST FITTED COPULA IN TERM LIFE INSURANCE
ggplot(data.frame(U3, V3), aes(x = U3, y = V3)) +
  geom_point(alpha = 0.5, color = "green") +  # Adjust transparency and color
  labs(title = "Scatter Plot of Pseudo-Observations (INCOME vs FACE)",
       x = "U3 (INCOME)",
       y = "V3 (FACE)") +
  theme_minimal()

#CONTOUR PLOT OF BEST FITTED COPULA IN TERM LIFE INSURANCE
# Contour Plot: INCOME & FACE
contour(BiCop(family = copula_fit3$family, 
              par = copula_fit3$par, 
              par2 = copula_fit3$par2), 
        main = "Contour Plot: INCOME & FACE", col = "green")

X4 <- data2$INCOME
Y4 <- data2$BORROWCVLIFEPOL

U4<- pobs(X4)
V4<- pobs(Y4)

copula_fit4 <- BiCopSelect(U4, V4, familyset = NA)
copula_fit4

summary(copula_fit4)

#Scatterplot for INCOME VS BORROWCVLIFEPOL
ggplot(data.frame(U4, V4), aes(x = U4, y = V4)) +
  geom_point(alpha = 0.5, color = "green") +  # Adjust transparency and color
  labs(title = "Scatter Plot of Pseudo-Observations (INCOME vs BORROWCVLIFEPOL)",
       x = "U3 (INCOME)",
       y = "V3 (BORROWCVLIFEPOL)") +
  theme_minimal()

#Contour Plots for both TermLife Copulas
par(mfrow = c(1, 2))  
# Contour Plot: INCOME & BORROWCVLIFEPOL
contour(BiCop(family = copula_fit4$family, 
              par = copula_fit4$par, 
              par2 = copula_fit4$par2), 
        main = "Contour Plot: INCOME & BORROWCVLIFEPOL", col = "green")

# Contour Plot: INCOME & FACE
contour(BiCop(family = copula_fit3$family, 
              par = copula_fit3$par, 
              par2 = copula_fit3$par2), 
        main = "Contour Plot: INCOME & FACE", col = "green")

#CONTOUR PLOT FOR ALL FOUR CONTOUR PLOTS IN PAPER SO FAR
# Set up for three plots side by side
par(mfrow = c(1, 4))  

# Contour Plot: Claims & Payment
contour(BiCop(family = copula_fit$family, 
              par = copula_fit$par, 
              par2 = copula_fit$par2), 
        main = "Contour Plot: Claims & Payment", col = "blue")

# Contour Plot: Insured & Payment
contour(BiCop(family = copula_fit2$family, 
              par = copula_fit2$par, 
              par2 = copula_fit2$par2), 
        main = "Contour Plot: Insured & Payment", col = "red")

# Contour Plot: INCOME & FACE
contour(BiCop(family = copula_fit3$family, 
              par = copula_fit3$par, 
              par2 = copula_fit3$par2), 
        main = "Contour Plot: INCOME & FACE", col = "green")

# Contour Plot: INCOME & BORROWCVLIFEPOL
contour(BiCop(family = copula_fit4$family, 
              par = copula_fit4$par, 
              par2 = copula_fit4$par2), 
        main = "Contour Plot: INCOME & BORROWCVLIFEPOL", col = "green")


data4 <- read.csv("Medicare.csv")
head(data4)

X6 <- data4$COV_CHG
Y6 <- data4$TOT_D

U6 <- pobs(X6)
V6 <- pobs(Y6)

copula_fit6 <- BiCopSelect(U6, V6, familyset = NA)
copula_fit6

summary(copula_fit6)

#SCATTERPLOT FOR MEDICARE COPULA
ggplot(data.frame(U6, V6), aes(x = U6, y = V6)) +
  geom_point(alpha = 0.5, color = "green") +  # Adjust transparency and color
  labs(title = "Scatter Plot of Pseudo-Observations (CONV_CHG vs TOT_D)",
       x = "U6 (COV_CHG)",
       y = "V6 (TOT_D)") +
  theme_minimal()

#CONTOUR PLOT FOR MEDICARE COPULA
par(mfrow = c(1, 1))
contour(BiCop(family = copula_fit6$family, 
              par = copula_fit6$par, 
              par2 = copula_fit6$par2), 
        main = "Contour Plot: COV_CHG & TOT_D", col = "black")


#Extra Copula that WAS NOT USED
data3 <- read.csv("HealthExpend.csv")
head(data3)

X5 <- data3$EXPENDOP
Y5 <- data3$COUNTOP

U5 <- pobs(X5)
V5 <- pobs(Y5)

copula_fit5 <- BiCopSelect(U5, V5, familyset = NA)
copula_fit5

summary(copula_fit5)

#SCATTERPLOT FOR Health Expenditure COPULA
ggplot(data.frame(U5, V5), aes(x = U5, y = V5)) +
  geom_point(alpha = 0.5, color = "green") +  # Adjust transparency and color
  labs(title = "Scatter Plot of Pseudo-Observations (EXPENDOP vs COUNTOP)",
       x = "U5 (EXPENDOP)",
       y = "V5 (COUNTOP)") +
  theme_minimal()