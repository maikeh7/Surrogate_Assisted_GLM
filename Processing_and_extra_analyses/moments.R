library(hetGP)
library(laGP)


## test problem variance
fn <- function(x) { 1/3 * (exp(sin(2 * pi * x))) }
## function to generate data with mean f12d and sd fn
# f1d2 = 2 * (exp(-30 * (x - 0.25)^2) + sin(pi * x^2)) - 2
fr <- function(x) { f1d2(x) + rnorm(length(x), sd = fn(x)) }
truth = function(x){f1d2(x)}
## generate data
n <- 100
X0 <- seq(0,1,length=n)
reps <- 15
X <- matrix(rep(X0, reps), ncol=1)
Z <- fr(X)   ## training
ZZ <- fr(X)  ## out-of-sample testing 
ZZm <- sapply(find_reps(X, Z)$Zlist, mean)
q <- qnorm(0.975) ## for PI/CI calculation

Ztrue = truth(X)
ZZtrue = sapply(find_reps(X, Ztrue)$Zlist, mean) #use with 'full ci'
ZZtrue

## full hetGP fit
fit <- mleHetGP(X, Z)
p <- predict(fit, matrix(X0, ncol=1))
se <- sqrt(p$sd2)
CIf <- cbind(l=p$mean - q*se, u=p$mean + q*se)
mse <- sqrt((p$sd2 + p$nugs)/reps) 
CI <- cbind(l=p$mean - q*mse, u=p$mean + q*mse)
pse <- sqrt(p$sd2 + p$nugs)
PI <- cbind(l=p$mean - q*pse, u=p$mean + q*pse)

## coverage calculations
CIfcov <- mean(ZZm > CIf[,1] & ZZm < CIf[,2])
PIcov <- mean(ZZ > PI[,1] & ZZ < PI[,2])
CIcov <- mean(ZZm > CI[,1] & ZZm < CI[,2])

 
##
## moments-based alternatives
##

## first full GP via laGP

## first moments fit
## "fitting the mean"
f1 <- newGP(fit$X0, fit$Z0, d=0.1, g=0.1, dK=TRUE)
mle1 <- jmleGP(f1)
p1 <- predGP(f1, fit$X0, lite=TRUE, nonug=TRUE)
?jmleGP
## second moments fit
v <- sapply(find_reps(X, Z)$Zlist, var)
f2 <- newGP(fit$X0, log(v), d=0.1, g=0.1, dK=TRUE)
mle2 <- jmleGP(f2)
p2 <- predGP(f2, fit$X0, lite=TRUE)

## interval calculations
PIm <- cbind(l=p1$mean - q*sqrt(exp(p2$mean)), u=p1$mean + q*sqrt(exp(p2$mean)))
CIm <- cbind(l=p1$mean - q*sqrt(exp(p2$mean)/reps), u=p1$mean + q*sqrt(exp(p2$mean)/reps))

## deduce full CI from covariance distance implied by lengthscale(s)
D <- distance(fit$X0)
K <- exp(-D/mle1$d)
dof <- min(rowSums(K))*reps
fse <- sqrt(exp(p2$mean)/dof + p1$s2)
CImf <- cbind(l=p1$mean - q*fse, u=p1$mean + q*fse)

## coverage calculations
PImcov <- mean(ZZ > PIm[,1] & ZZ < PIm[,2])
CImcov <- mean(ZZm > CIm[,1] & ZZm < CIm[,2])
CImfcov <- mean(ZZm > CImf[,1] & ZZm < CImf[,2])

## clean up laGP stuff
deleteGPs()

##
## now with Vecchia 
##

## read "library"
source("https://urldefense.com/v3/__https://raw.githubusercontent.com/katzfuss-group/scaledVecchia/master/vecchia_scaled.R__;!!JYXjzlvb!hA_hfrWvcFZnoAFwWRQ7iiHF-iarWCIkwDgyuE-NRZ7FvDHYinuPq-biGpygkdUcGad7CP-oKBqHLOHEDMgGMA$ ")

## noticed that sometimes the vecchia fits are jagged, esp with small m
## could just be that this is because the data are very noisy

## first moments fit (put dummy unif 2nd col)
f1v <- fit_scaled(fit$Z0, cbind(fit$X0, runif(length(fit$X0))), trend="zero", ms=75, nug=NULL)
p1v <- predictions_scaled(f1v, cbind(fit$X0, runif(length(fit$X0))), predvar=TRUE, joint=FALSE)

## second moments fit (put dummy unif 2nd col)
f2v <- fit_scaled(log(v), cbind(fit$X0, runif(length(fit$X0))), trend="zero", ms=75, nug=NULL)
p2v <- predictions_scaled(f2v, cbind(fit$X0, runif(length(fit$X0))), predvar=TRUE, joint=FALSE)

## interval calculations
mvse <- sqrt(exp(p2v$means)/reps)
PImv <- cbind(l=p1v$means - q*sqrt(exp(p2v$means)), u=p1v$means + q*sqrt(exp(p2v$means)))
CImv <- cbind(l=p1v$means - q*mvse, u=p1v$means + q*mvse)

## get lengthscale for full CI calc
d <- f1v$covparms[2]^2 ## lengthscales start in second position
K <- exp(-D/d)
dofv <- min(rowSums(K))*reps
fsev <- sqrt(exp(p2$mean)/dofv + p1v$vars/dofv)
CImfv <- cbind(l=p1$mean - q*fsev, u=p1$mean + q*fsev)

## coverage calculations
PImvcov <- mean(ZZ > PImv[,1] & ZZ < PImv[,2])
CImvcov <- mean(ZZm > CImv[,1] & ZZm < CImv[,2])
CImfvcov <- mean(ZZm > CImfv[,1] & ZZm < CImfv[,2])
#########################################################
# include mean uncertainty / 95th quantile
# use sqrt to transform variance
# This is the method used in the paper
#########################################################
q2=qnorm(.95)
p3v <- predictions_scaled(f1v, cbind(X, runif(length(X))), predvar=TRUE, joint=FALSE)
mypreds = p3v$means
dforig = data.frame(Z = Z, MyMean = mypreds, X = X)
dforig$sq_devs_smooth = (dforig$Z - dforig$MyMean)^2
df3 = dforig %>% group_by(X) %>% summarize(sum_devs = sum(sq_devs_smooth))
df3$Var = df3$sum_devs / reps
df3$rootVar = sqrt(df3$Var)
myVar <-  df3$rootVar
## second moments fit (put dummy unif 2nd col)
f3v <- fit_scaled(myVar, cbind(fit$X0, runif(length(fit$X0))), trend="zero", ms=75, nug=NULL)
varpreds <- predictions_scaled(f3v, cbind(fit$X0, runif(length(fit$X0))), predvar=TRUE, joint=FALSE)

# use delta method to get correct variance
# d/dx(g(\mu))^2 * var(x) // g(x) = x^2 
Delta_variance = (2*varpreds$means)^2 * varpreds$vars
upper95_var = (varpreds$means)^2 + q2*sqrt(Delta_variance)

# coverage calculations
PImv95 <- cbind(l=p1v$means - q*sqrt(upper95_var), u=p1v$means + q*sqrt(upper95_var))
CImv95 <- cbind(l=p1v$means - q*sqrt(upper95_var/reps), u=p1v$means + q*sqrt(upper95_var/reps))
PImvcov95 <- mean(ZZ > PImv95[,1] & ZZ < PImv95[,2])
CImvcov95 <- mean(ZZm > CImv95[,1] & ZZm < CImv95[,2])

## get lengthscale for full CI calc
d <- f1v$covparms[2]^2 ## lengthscales start in second position
K <- exp(-D/d)
dofv <- min(rowSums(K))*reps

fsev <- sqrt(exp(p2$mean + q2*sqrt(p2$s2)) / dofv + p1v$vars/dofv)

CImfv95 <- cbind(l=p1$mean - q*fsev, u=p1$mean + q*fsev)
CImfvcov95 <- mean(ZZm > CImfv95[,1] & ZZm < CImfv95[,2])

## plot everything
## data points

pdf("hetGP.pdf")
plot(X, Z, cex=0.5, pch=20, col="gray", main="Full hetGP fit", ylab = "Y", cex.main=1.5, cex.axis = 1.4)

## full hetGP
lines(X0, p$mean, lwd=2, col=2)
lines(X0, CIf[,1], col=2, lwd=2, lty=4)
lines(X0, CIf[,2], col=2, lwd=2, lty=4)
lines(X0, CI[,1], col=2, lwd=2, lty=2)
lines(X0, CI[,2], col=2, lwd=2, lty=2)
lines(X0, PI[,1], col=2, lwd=2, lty=3)
lines(X0, PI[,2], col=2, lwd=2, lty=3)
legend("topright", c("hetGP"), lty=1, lwd=2, col=2 , cex=1.4)
legend("topleft", c("PI", "CI/rep", "full CI"), lty=c(3,2,4), lwd=2, cex=1.4)
dev.off()

## laGP fits to moments
pdf("laGP.pdf")
plot(X, Z, cex=0.5, pch=20, col="gray", main="Moments-based fit (full GP)", ylab = "Y", cex.main=1.5, cex.axis = 1.4)
lines(X0, p1$mean, col=3, lwd=2)
lines(X0, CImf[,1], col=3, lwd=2, lty=4)
lines(X0, CImf[,2], col=3, lwd=2, lty=4)
lines(X0, PIm[,1], col=3, lwd=2, lty=3)
lines(X0, PIm[,2], col=3, lwd=2, lty=3)
lines(X0, CIm[,1], col=3, lwd=2, lty=2)
lines(X0, CIm[,2], col=3, lwd=2, lty=2)
legend("topright", c("full moments-based fit"), lty=1, lwd=2, col=3, cex=1.4)
legend("topleft", c("PI", "CI/rep", "full CI"), lty=c(3,2,4), lwd=2, cex=1.4)
dev.off()

## vecchia fits to moments
pdf("Vecchia.pdf")
plot(X, Z, cex=0.5, pch=20, col="gray", main="Moments-based fits (Vecchia)", ylab = "Y", cex.main=1.5, cex.axis=1.4)
lines(X0, p1v$means, col=4, lwd=2)
lines(X0, CImfv[,1], col=4, lwd=2, lty=4)
lines(X0, CImfv[,2], col=4, lwd=2, lty=4)
lines(X0, PImv[,1], col=4, lwd=2, lty=3)
lines(X0, PImv[,2], col=4, lwd=2, lty=3)
lines(X0, CImv[,1], col=4, lwd=2, lty=2)
lines(fit$X0, CImv[,2], col=4, lwd=2, lty=2)

## vecchia fits to moments
# *with mean uncertainty and using 95th quantile of variance*
lines(X0, p1v$means, col=6, lwd=2)
lines(X0, PImv95[,1], col=6, lwd=2, lty=3)
lines(X0, PImv95[,2], col=6, lwd=2, lty=3)
lines(X0, CImv95[,1], col=6, lwd=2, lty=2)
lines(fit$X0, CImv95[,2], col=6, lwd=2, lty=2)
legend("topright", c("Vecchia A", "Vecchia B"), lty=1, lwd=2, col=c(4,6), cex=1.4)
legend("topleft", c("PI", "CI/rep", "full CI"), lty=c(3,2,4), lwd=2, cex=1.4)
dev.off()


legend("topright", c("hetGP", "laGP moments", "vecchia moments"), lty=1, lwd=2, col=c(2,3,4))
legend("top", c("PI", "CI/rep", "full CI"), lty=c(3,2,4), lwd=2)
## window dressing
legend("topright", c("hetGP", "laGP moments", "vecchia moments", "vecchia moments 95th"),
       lty=1, lwd=2, col=c(2,3,4,6))
legend("top", c("PI", "CI/rep", "full CI"), lty=c(3,2,4), lwd=2)

## summarize coverages
covs <- rbind(hetGP=c(PIcov, CIcov, CIfcov),
              laGP=c(PImcov, CImcov, CImfcov),
              vecc_a=c(PImvcov, CImvcov, CImfvcov),
              vecc_b=c(PImvcov95, CImvcov95, CImfvcov95))
colnames(covs) <- c("PI", "CI/rep", "full CI")
print(covs)
xtable(covs)
