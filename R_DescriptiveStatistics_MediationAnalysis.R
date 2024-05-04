datafile <- 'C:/DataXLS.xlsx'

library(xlsx)
dat <- as.data.frame(read.xlsx(datafile, 1))
dat
head(dat)
names(dat)
dat$age <- as.numeric(dat$age)
dat$X <- as.numeric(dat$X)
dat$M1 <- as.numeric(dat$M1)
dat$M2 <- as.numeric(dat$M2)
dat$Y <- as.numeric(dat$Y)
round(cor(dat),3)
nrow(dat)	# 466

## descriptive statistics
library(Deducer)
descriptive.table(vars = d(age, X, M1, M2, Y), data = dat, func.names = c("Mean", "St. Deviation", "Min", "Max", "Valid N", "Skew", "Kurtosis"))

## aquared semi-partial correlations

# full model
lmfull <- summary(lm(Y ~ X + M1 + M2, dat))
R2full <- lmfull$r.squared
R2full

# with M1 and M2
lmM1M2 <- summary(lm(Y ~ M1 + M2, dat))
R2M1M2 <- lmM1M2$r.squared
R2M1M2

# with X and M1
lmXM1 <- summary(lm(Y ~ X + M1, dat))
R2XM1 <- lmXM1$r.squared
R2XM1

# with X and M2
lmXM2 <- summary(lm(Y ~ X + M2, dat))
R2XM2 <- lmXM2$r.squared
R2XM2

# b1
r2_spb1 <- R2full - R2XM2
r2_spb1

# b2
r2_spb2 <- R2full - R2XM1
r2_spb2

# c'
r2_spc <- R2full - R2M1M2
r2_spc

# alternative method
lmr <- lm(M1 ~ X + M2, dat)
r_spb1 <- cor(dat$Y, residuals(lmr))
r_spb1
r_spb1^2

lmr <- lm(M2 ~ X + M1, dat)
r_spb2 <- cor(dat$Y, residuals(lmr))
r_spb2
r_spb2^2

lmr <- lm(X ~ M1 + M2, dat)
summary(lmr)
r_spc <- cor(dat$Y, residuals(lmr))
r_spc
r_spc^2

## Cohen's f^2
f2_b1 <- r2_spb1/(1 - R2full)
f2_b1

f2_b2 <- r2_spb2/(1 - R2full)
f2_b2

f2_c <- r2_spc/(1 - R2full)
f2_c

### Mediation Analyses
library(lavaan)
mod2 <- '
	M1 ~ a1*X
	M2 ~ a2*X
	Y ~ b1*M1 + b2*M2 + c*X

	M1 ~~ cov*M2
		
	a1b1 := a1*b1
	a2b2 := a2*b2
	IEtot := a1*b1 + a2*b2
	total := c + a1*b1 + a2*b2
	a1b1_a2b2 := a1*b1 - a2*b2
	a1b1_c := a1*b1 - c
	a2b2_c := a2*b2 - c
	IEtot_c := IEtot - c
'

fit <- sem(mod2, data = dat, meanstructure = TRUE)
summary(fit, standardized = TRUE, rsquare = TRUE)
parameterEstimates(fit)
parameterEstimates(fit)[c("label", "est")]

# partial standardized point estimates
coef(fit, type = "user")["a1"]/sd(dat$M1, na.rm = TRUE)
coef(fit, type = "user")["a2"]/sd(dat$M2, na.rm = TRUE)
coef(fit, type = "user")["c"]/sd(dat$Y, na.rm = TRUE)
coef(fit, type = "user")["a1b1"]/sd(dat$Y, na.rm = TRUE)
coef(fit, type = "user")["a2b2"]/sd(dat$Y, na.rm = TRUE)
coef(fit, type = "user")["IEtot"]/sd(dat$Y, na.rm = TRUE)
coef(fit, type = "user")["total"]/sd(dat$Y, na.rm = TRUE)

# bootstrapping
fitb <- sem(mod2, data = dm, meanstructure = TRUE, se = "bootstrap", bootstrap = 5000)
bfit <- parameterEstimates(fitb, boot.ci.type = "perc", level = 0.95, ci = TRUE)
bfit

