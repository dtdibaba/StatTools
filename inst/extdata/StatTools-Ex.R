tab <- regby(datain = data, byVar = "cat", frmlYX = formula(y ~ x + z), fam = binomial, Model = "glm", Pred = c("Intercept", "X", "Z"), colname = c("Strata", "Variable", "OR(95%CIs)", "P-value"), Intercept = TRUE)



# Logistic regression analysis with a continuous and <br> categorical predictors, and intercept not requested

set.seed(123896)
x = rnorm(50)
z = sample(letters[1:4], 50, T)
z = sample(letters[1:4], 50, T)
cat <- sample(c("B", "W"), 50, T)
y = rbinom(50, 1, 0.5)
data2 <- data.frame(x = x, z = z, cat = cat, y = y)
# If Factor==TRUE include the level labels of the predictor as separate names.
regby(datain = data2, byVar = "cat", frmlYX = formula(y ~ x + z), fam = binomial, Model = "glm", Pred = c("Intercept", "X", "Zb", "Zc", "Zd"), colname = c("Strata", "Variable", "OR(95%CIs)", "P-value"), Factor = TRUE, Intercept = FALSE, EXP = TRUE)

# Multiple Linear regression analysis with a continuous and <br> categorical predictors, and intercept included

set.seed(123896)
x = rnorm(50)
z = sample(letters[1:4], 50, T)
z = sample(letters[1:4], 50, T)
cat <- sample(c("B", "W"), 50, T)
y = rnorm(50, 12, 3)
data3 <- data.frame(x = x, z = z, cat = cat, y = y)
regby(datain = data3, byVar = "cat", frmlYX = formula(y ~ x + z), fam = guassian, Model = "lm", Pred = c("Intercept", "X", "Zb", "Zc", "Zd"), colname = c("Strata", "Variable", "OR(95%CIs)", "P-value"), Factor = TRUE, Intercept = TRUE, EXP = FALSE)

# Cox proportional hazard regression analysis with a continuous predictor

set.seed(1243567)
t <- rnorm(100, 15, 3)
y <- rbinom(100, 1, 0.5)
cat <- sample(c("M", "F"), 100, T)
x <- rnorm(100, 5, 2)
z <- rpois(100, 1)
z <- factor(z)
data3 <- data.frame(t = t, x = x, cat = cat, y = y, z)

regby(datain = data3, byVar = "cat", frmlYX = formula(Surv(t, y) ~ x), Model = "coxph", Pred = c("X"), colname = c("Strata", "Variable", "HR (95%CIs)", "P-value"), Factor = TRUE, Intercept = FALSE)

# Proportional Odds Ordered Logistic Regression

library(MASS)
library(nnet)
library(AER)
set.seed(1243567)
x <- rnorm(50)
z <- sample(c(letters[1:5]), 50, T)
cat <- sample(c("B", "W"), 50, T)
y <- rbinom(50, 1, 0.5)
data4 <- data.frame(x = x, z = z, cat = cat, y = y)

regby(datain = data4, byVar = "cat", frmlYX = formula(z ~ x), Model = "polr", colname = c("Strata", "Variable", "Beta (95%CIs)", "P-value", "Cum_Prob", "OR"), Factor = TRUE, Intercept = FALSE, col.names = TRUE)


# Multinomial Logistic Regression
regby(datain = data4, byVar = "cat", frmlYX = (z ~ x), Model = "multinom", colname = c("Strata", "Variable", "OR (95%CIs)", "P-value"), Factor = TRUE, Intercept = FALSE)

# Linear Mixed Effect Models
lmem <- regby(datain = data4, byVar = "cat", frmlYX = (x ~ y + (1 | z)), Model = "lmer", col.names = FALSE)
lmem

regby(
    datain = mtcars, byVar = "cyl", frmlYX = formula(mpg ~ factor(gear) + factor(am)), fam = guassian,
    Model = "lm", Pred = NULL,
    colname = c("Strata", "Variable", "Beta (95%CIs)", "P-value"),
    Factor = TRUE, Intercept = TRUE, EXP = FALSE
)

regby(
    datain = mtcars, byVar = 'cyl', frmlYX = formula(disp ~ factor(gear) + factor(am)), fam = guassian,
    Model = "lm", Pred = NULL,
    colname = c("Strata", "Variable", "Beta (95%CIs)", "P-value"),
    Factor = TRUE, Intercept = TRUE, EXP = FALSE
)
