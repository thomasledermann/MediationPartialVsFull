####################################################################################################################################################
### P o w e r   S i m u l a t i o n   f o r   a   M e d i a t i o n   M o d e l   w i t h   2   M e d i a t o r s   u s i n g   R a w   D a t a  ###
### Author: Thomas Ledermann                                                                                                                     ###
### Created: October 29, 2022                                                                                                                    ###
####################################################################################################################################################

sampleSize <- nrow(dm)	# current sample size. dm = raw data
alphaLevel <- .05	# significance level
nsim <- 10000		# number of iterations
setSeed <- 123

# Install and load packages
if(!require("lavaan")) install.packages("lavaan")
library(lavaan)

power <- function(raw.data, alpha = 0.05, reps = 1000, sample.size) {
	id <- 1:nrow(raw.data)
	results  <- sapply(1:reps, function(x) {
		index <- sample(id, size = sample.size, replace = TRUE) 
		b.data <- raw.data[index, ]
		mod <-'
			Y ~ c*X + b1*M1 + b2*M2
			M1 ~ a1*X
			M2 ~ a2*X
			M1 ~~ covM*M2

			a1b1 := a1*b1
			a2b2 := a2*b2
			IEtot := a1*b1 + a2*b2
			total := c + a1*b1 + a2*b2
			ab1_ab2 := a1*b1 - a2*b2
			ab1_c := a1*b1 - c
			ab2_c := a2*b2 - c
			IEtot_c := IEtot - c
	'
	fit <- sem(mod, b.data, missing = "ML")
	ests <- parameterEstimates(fit)
	pa1 <- ests[ests$label == 'a1', 'pvalue']
	pa2 <- ests[ests$label == 'a2', 'pvalue']
	pb1 <- ests[ests$label == 'b1', 'pvalue']
	pb2 <- ests[ests$label == 'b2', 'pvalue']
	pc <- ests[ests$label == 'c', 'pvalue']

	pab1 <- ests[ests$label == 'a1b1', 'pvalue']
	pab2 <- ests[ests$label == 'a2b2', 'pvalue']
	pIEtot <- ests[ests$label == 'IEtot', 'pvalue']
	ptot <- ests[ests$label == 'total', 'pvalue']

	pab1ab2 <- ests[ests$label == 'ab1_ab2', 'pvalue']
	pab1c <- ests[ests$label == 'ab1_c', 'pvalue']
	pab2c <- ests[ests$label == 'ab2_c', 'pvalue']
	pIEtotc <- ests[ests$label == 'IEtot_c', 'pvalue']

	resMatrix <- cbind(pa1, pa2, pb1, pb2, pc, pab1, pab2,
				 pIEtot, ptot, pab1ab2, pab1c, pab2c, pIEtotc)
	resMatrix 
    })
	powerEst <- rowSums(results < alpha)/reps
	names(powerEst) <- c("a1", "a2", "b1", "b2", "c", "a1b1", "a2b2", "IEtot", "total", "ab1_ab2", "ab1_c", "ab2_c", "IEtot_c")
	powerEst
}
power(raw.data = dm, alpha = alphaLevel, reps = nsim, sample.size = sampleSize)

sampleSize <- 380

# Estimate the model
mod <-'	Y1 ~ ca1*X1 + cp21*X2 + ba1*M1 + bp21*M2
	Y2 ~ ca2*X2 + cp12*X1 + ba2*M2 + bp12*M1
	M1 ~ aa1*X1 + ap21*X2
	M2 ~ aa2*X2 + ap12*X1

	# IE
	ie.a1 := aa1*ba1
	ie.a2 := aa2*ba2
	ie.a1p12 := aa1*bp12
	ie.a2p21 := aa2*bp21
	ie.p12a2 := ap12*ba2
	ie.p21a1 := ap21*ba1
	ie.p12p21 := ap12*bp21
	ie.p21p12 := ap21*bp12

	# Total IE
	tie11 := aa1*ba1 + ap12*bp21
	tie22 := aa2*ba2 + ap21*bp12
	tie12 := aa1*bp12 + ap12*ba2
	tie21 := aa2*bp21 + ap21*ba1

	# Total
	t11 := aa1*ba1 + ap12*bp21 + ca1
	t22 := aa2*ba2 + ap21*bp12 + ca2
	t12 := aa1*bp12 + ap12*ba2 + cp12
	t21 := aa2*bp21 + ap21*ba1 + cp21
'
fit <- sem(mod, dm, missing = "ML")
summary(fit)
