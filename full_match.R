library(MatchIt)
library(lmtest)
library(sandwich)
#library(boot)
#library(survival)
library(optmatch)
library(tibble)

#Fullmatchig, efeitos contínuos, sem ajuste por co-variável.
fullmm.c = function(k = 1000, n = 2000) {
  estimativas = c()
  erros = c()
  
  for (i in 1:k) {
    X = gen_X(n)
    A = gen_A(X)
    
    Y_C = gen_Y_C(A, X)
    Y_B = gen_Y_B(A, X)
    
    d = data.frame(A, X, Y_C, Y_B)
    
    mF <- matchit(A ~ X1 + X2 + X3 + X4 + X5 + 
                    X6 + X7 + X8 + X9, data = d,
                  method = "full", estimand = "ATT")
    
    m.data = match.data(mF)
    
    ##Para resultados contínuos
    
    ###sem ajuste por covariável
    fit1md <- lm(Y_C ~ A, data = m.data, weights = weights)
    
    ####Usando cluster-robust standart error
    vl = coeftest(fit1md, vcov. = vcovHC, cluster = ~subclass)
    estimativas[i] = vl[2]
    erros[i] = vl[4]
    
  }
  
  valores  = list(estimativas, erros)
  return(valores)
  
}
a = fullmm.c(n = 150)
b = fullmm.c(n = 400)
c = fullmm.c(n = 800)
d = fullmm.c(n = 1500)

#Fullmatching, efeitos contínuos, com ajuste por co-variável.
fullmm.c.a = function(k = 1000, n = 2000) {
  estimativas = c()
  erros = c()
  
  for (i in 1:k) {
    X = gen_X(n)
    A = gen_A(X)
    
    Y_C = gen_Y_C(A, X)
    Y_B = gen_Y_B(A, X)
    
    d = data.frame(A, X, Y_C, Y_B)
    
    mF <- matchit(A ~ X1 + X2 + X3 + X4 + X5 + 
                    X6 + X7 + X8 + X9, data = d,
                  method = "full", estimand = "ATT")
    
    m.data = match.data(mF)
    covs_to_center = c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9")
    md.cen = m.data
    md.cen[covs_to_center] = scale(md.cen[covs_to_center], scale = FALSE)
    
    ##Para resultados contínuos
    
    ###sem ajuste por covariável
    fit1md <- lm(Y_C ~ A * (X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9),
                 data = md.cen, weights = weights)
    
    ####Usando cluster-robust standart error
    vl = coeftest(fit1md, vcov. = vcovHC, cluster = ~subclass)[1:2,]
    estimativas[i] = vl[2]
    erros[i] = vl[4]
    
  }
  
  valores  = list(estimativas, erros)
  return(valores)
  
}
a1 = fullmm.c.a(n = 150)
b1 = fullmm.c.a(n = 400)
c1 = fullmm.c.a(n = 800)
d1 = fullmm.c.a(n = 1500)

#Fullmatching, efeitos binários, sem ajuste por co-variável, na escala OR.
fullmm.b.or = function(k = 1000, n = 2000) {
  estimativas = c()
  erros = c()
  
  for (i in 1:k) {
    X = gen_X(n)
    A = gen_A(X)
    
    Y_C = gen_Y_C(A, X)
    Y_B = gen_Y_B(A, X)
    
    d = data.frame(A, X, Y_C, Y_B)
    
    mF <- matchit(A ~ X1 + X2 + X3 + X4 + X5 + 
                    X6 + X7 + X8 + X9, data = d,
                  method = "full", estimand = "ATT")
    
    m.data = match.data(mF)
    
    ##Para resultados contínuos
    
    ###sem ajuste por covariável
    fit1md <- glm(Y_B ~ A, data = m.data, weights = weights, 
                  family = quasibinomial(link = "logit"))
    
    ####Usando cluster-robust standart error
    vl = coeftest(fit1md, vcov. = vcovHC, cluster = ~subclass)
    estimativas[i] = vl[2]
    erros[i] = vl[4]
    
  }
  
  valores  = list(estimativas, erros)
  return(valores)
  
}
a2 = fullmm.b.or(n = 150)
b2 = fullmm.b.or(n = 400)
c2 = fullmm.b.or(n = 800)
d2 = fullmm.b.or(n = 1500)

#Fullmatching, efeitos binários, sem ajuste por co-variável, na escala RR.
fullmm.b.rr = function(k = 1000, n = 2000) {
  estimativas = c()
  erros = c()
  
  for (i in 1:k) {
    X = gen_X(n)
    A = gen_A(X)
    
    Y_C = gen_Y_C(A, X)
    Y_B = gen_Y_B(A, X)
    
    d = data.frame(A, X, Y_C, Y_B)
    
    mF <- matchit(A ~ X1 + X2 + X3 + X4 + X5 + 
                    X6 + X7 + X8 + X9, data = d,
                  method = "full", estimand = "ATT")
    
    m.data = match.data(mF)
    
    ##Para resultados contínuos
    
    ###sem ajuste por covariável
    fit1md <- glm(Y_B ~ A, data = m.data, weights = weights, 
                  family = quasibinomial(link = "log"))
    
    ####Usando cluster-robust standart error
    vl = coeftest(fit1md, vcov. = vcovHC, cluster = ~subclass)
    estimativas[i] = vl[2]
    erros[i] = vl[4]
    
  }
  
  valores  = list(estimativas, erros)
  return(valores)
  
}
a3 = fullmm.b.rr(n = 150)
b3 = fullmm.b.rr(n = 400)
c3 = fullmm.b.rr(n = 800)
d3 = fullmm.b.rr(n = 1500)

