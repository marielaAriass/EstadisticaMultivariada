

## Pruebas de hipotesis

SCT <- t(y) -sum(y)**2 /nrow(datos)
SCT

SCE <- t(beta) %% t(x) %% y - sum(y)**2 / nrow(datos)
SCE

SSE <- SCT - SCE
SSE


# El estadistico F_0
F0 <- (sCE / (ncol(x) - 1)) / (SSE / (nrow(x) - ncol(x-1)-1) )
F0

# F_(0.05, 2, 22) = 3.44 |grados de libertad ; p = 2 
#F0 > F_(0.05, 2, 22)
# Se rechaza H0

#Usando el modelo M1
SCT.m <- sum ( (datos$y - mean(datos$y))**2)
SCT.m

summary(M1)

SCE.m <- sum (M1$fitted - mean(datos$y))**2)
SCE.m

SSE.m <- 
SSE.m

n <- nrow(y)
n

GLT <- n-1
GLT

GLRes <- df.residual(M1)
GLRes

GLR <- GLT - GLRes
GLR

CMR <- SCE / GLR
CMR

CMRes <- SSE / GLRes

F0 <- CMR / cMRes

alpha <- 0.05
df1 <- GLR
df2 <- GLRes

F_crit <- qf(1-alpha, df1, df2)
F_crit

pv <-1 - pf(F0, GLR, GLRes)
