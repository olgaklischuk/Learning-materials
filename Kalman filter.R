
alpha(t + 1) = d(t) + T(t) alpha(t) + H(t) * eta(t),

y(t) = c(t) + Z(t) alpha(t) + G(t) * epsilon(t),

d(t)=.1
T(t)=1
h(t)=.5
c(t)=.5
Z(t)=1
G(t)=.5
a(0)=1
P(0)=2 or min(real.gdp)

Iteration:

 Let i be the loop variable. The filter iterations are implemented the following way (in case of no NA's):

 Initialization:
 if(i == 1){
 at[, i] = a0
 Pt[,, i] = P0
 }

 Updating equations:
 vt[, i] = yt[, i] - ct[, i] - Zt[,,i] %*% at[, i]
 Ft[,, i] = Zt[,, i] %*% Pt[,, i] %*% t(Zt[,, i]) + GGt[,, i]
 Kt[,, i] = Pt[,, i] %*% t(Zt[,, i]) %*% solve(Ft[,, i])
 att[, i] = at[, i] + Kt[,, i] %*% vt[, i]
 Ptt[, i] = Pt[,, i] - Pt[,, i] %*% t(Zt[,, i]) %*% t(Kt[,, i])

 Prediction equations:
 at[, i + 1] = dt[, i] + Tt[,, i] %*% att[, i]
 Pt[,, i + 1] = Tt[,, i] %*% Ptt[,, i] %*% t(Tt[,, i]) + HHt[,, i]

 Next iteration:
 i <- i + 1
 goto “Updating equations”.

 NA-values:

 NA-values in the observation matrix yt are supported. If particular observations yt[,i] contain NAs, the NA-values are removed and the measurement equation is adjusted accordingly. When the full vector yt[,i] is missing the Kalman filter reduces to a prediction step.

 Parameters:

 The parameters can either be constant or deterministic time-varying. Assume the number of observations is n (i.e. y = y[,1:n]). Then, the parameters admit the following classes and dimensions:

 dt	 either a m * n (time-varying) or a m * 1 (constant) matrix.
 Tt	 either a m * m * n or a m * m * 1 array.
 HHt	 either a m * m * n or a m * m * 1 array.
 ct	 either a d * n or a d * 1 matrix.
 Zt	 either a d * m * n or a d * m * 1 array.
 GGt	 either a d * d * n or a d * d * 1 array.
 yt	 a d * n matrix.
 ## <--------------------------------------------------------------------------->
## Example 2: Local level model for the Nile's annual flow.
## <--------------------------------------------------------------------------->
## Transition equation:
## alpha[t+1] = alpha[t] + eta[t], eta[t] ~ N(0, HHt)
## Measurement equation:
## y[t] = alpha[t] + eps[t], eps[t] ~  N(0, GGt)

y <- Nile
y[c(3, 10)] <- NA  # NA values can be handled

## Set constant parameters:
dt <- ct <- matrix(0)
Zt <- Tt <- matrix(1)
a0 <- y[1]            # Estimation of the first year flow
P0 <- matrix(100)     # Variance of 'a0'

## Estimate parameters:
library(FKF)
fit.fkf <- optim(c(HHt = var(y, na.rm = TRUE) * .5,
                   GGt = var(y, na.rm = TRUE) * .5),
                 fn = function(par, ...)
                 -fkf(HHt = matrix(par[1]), GGt = matrix(par[2]), ...)$logLik,
                 yt = rbind(y), a0 = a0, P0 = P0, dt = dt, ct = ct,
                 Zt = Zt, Tt = Tt, check.input = FALSE)

## Filter Nile data with estimated parameters:
fkf.obj <- fkf(a0, P0, dt, ct, Tt, Zt, HHt = matrix(fit.fkf$par[1]),
               GGt = matrix(fit.fkf$par[2]), yt = rbind(y))

## Compare with the stats' structural time series implementation:
fit.stats <- StructTS(y, type = "level")#level epsilon

fit.fkf$par
fit.stats$coef

## Plot the flow data together with fitted local levels:
plot(y, main = "Nile flow")
lines(fitted(fit.stats), col = "green")
lines(ts(fkf.obj$att[1, ], start = start(y), frequency = frequency(y)), col = "blue")
legend("top", c("Nile flow data", "Local level (StructTS)", "Local level (fkf)"),
       col = c("black", "green", "blue"), lty = 1)
rm(fkf.obj,fit.fkf,fit.stats,dt,Zt,a0,P0,y)
