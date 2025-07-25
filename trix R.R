# R Script: Fertility Rate vs Urban Population and Income
# Author: Nandana P V
# Description: Analyzing the relationship between urbanization, income, and fertility using multiple linear regression
# Libraries used: tseries (for Jarque-Bera test)
# Data source: Excel sheets (imported as CSV using file.choose())

# Load required library
library(tseries)

# Read and prepare the data
d1 <- read.csv(file.choose(), header = TRUE)  # Choose and load CSV version of Excel sheet
View(d1)

# Duplicate dataset for further use
d2 <- d1
colnames(d2) <- c("St", "Q", "L")  # Rename columns: St = Fertility, Q = Urban Population, L = Income
View(d2)

# Summary statistics for Urban Population (Q)
summary(d2$Q)
var(d2$Q)
sd(d2$Q)
cor(d2$Q, d2$St)
cov(d2$Q, d2$St)
mean(d2$Q, trim = 0.10)

# Scatter plot: Urban Population vs Fertility Rate
plot(d2$Q, d2$St, main = "Scatter Diagram 1",
     xlab = "Urban Population", ylab = "Fertility Rate")
abline(lm(d2$St ~ d2$Q))

# Simple Linear Regression: Fertility ~ Urban Population
Reg1 <- lm(d2$St ~ d2$Q)
View(Reg1)
summary(Reg1)
plot(Reg1)

# Residual analysis for Reg1
RS1 <- residuals(Reg1)
RS1sq <- RS1^2
View(RS1sq)

# Add residual squares to data
d3 <- cbind(d2, RS1sq)
View(d3)

# ANOVA and normality test for Reg1
anova(Reg1)
jarque.bera.test(RS1)

# Summary statistics for Income (L)
summary(d2$L)
var(d2$L)
sd(d2$L)
cor(d2$L, d2$St)
cov(d2$L, d2$St)
mean(d2$L, trim = 0.10)

# Scatter plot: Income vs Fertility Rate
plot(d2$L, d2$St, main = "Scatter Diagram",
     xlab = "Income", ylab = "Fertility Rate")
abline(lm(d2$St ~ d2$L))

# Simple Linear Regression: Fertility ~ Income
Reg3 <- lm(d2$St ~ d2$L)
View(Reg3)
summary(Reg3)
plot(Reg3)

# Residual analysis for Reg3
RS3 <- residuals(Reg3)
RS3sq <- RS3^4
View(RS3sq)

# Add residual squares to data
d4 <- cbind(d3, RS3sq)
View(d4)

# ANOVA and normality test for Reg3
anova(Reg3)
jarque.bera.test(RS3)

# Multiple Linear Regression: Fertility ~ Urban Population + Income
Reg2 <- lm(d2$St ~ d2$Q + d2$L)
summary(Reg2)
anova(Reg2)
jarque.bera.test(residuals(Reg2))
plot(Reg2)

# 3D Visualization (Scatter Plot)
# Note: For better 3D visualization, consider using `scatterplot3d` or `plotly` packages
plot(d2$Q, d2$L, main = "Multiple Linear Regression",
     xlab = "Urban Population", ylab = "Income")
