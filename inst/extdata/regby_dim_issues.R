# When predictors are not similarly distributed by the stratifying variable, dim issues may arrise.
REG <- by(mtcars, mtcars[, "cyl"], function(x) lm(mpg ~ factor(gear) + factor(am) + disp, data = x))
sum1 <- lapply(REG, summary)

ES <- simplify2array(sum1)

ES <- ES["coefficients", ]
es <- purrr::map(ES, data.frame)
dt <- simplify2array(es)

estimate <- dt[1, ]
estimate <- sprintf("%.2f", unlist(estimate))
estimate <- noquote(estimate)

pval <- dt[4, ]
pval <- sprintf("%.4f", unlist(pval))
# ci<-lapply(REG, confint.lm)
# ci<-as.matrix(ci, ncol=length(ES))
# ci<-as.vector(simplify2array(ci))
SE <- as.vector(unlist(dt[2, ]))
Lower <- as.vector(unlist(dt[1, ])) - 1.96 * SE
Lower <- sprintf("%.2f", Lower)
Upper <- as.vector(unlist(dt[1, ])) + 1.96 * SE
Upper <- sprintf("%.2f", Upper)

pred <- lapply(ES, row.names)
Pred <- matrix(unlist(pred), ncol = 1)
Strata <- rep(c(names(REG)), lapply(pred, length))
beta <- noquote(paste0(estimate, " (", Lower, ", ", Upper, ")"))

Result <- data.frame(Strata, Pred, "Beta (95%CIs)" = beta, "P-value" = pval)
Result
library(StatTools)
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

regby(
    datain = mtcars, byVar = 'cyl', frmlYX = formula(disp ~ factor(gear) + factor(am) + vs), fam = guassian,
    Model = "lm", Pred = NULL,
    colname = c("Strata", "Variable", "Beta (95%CIs)", "P-value"),
    Factor = TRUE, Intercept = TRUE, EXP = FALSE
)
