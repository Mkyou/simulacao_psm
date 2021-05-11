library(MatchIt)
library(lmtest)
library(sandwich)
#library(boot)
#library(survival)
library(tibble)

##NN com reposição com efeito contínuo sem ajuste por co-variável.
nn.com_rep.c = function(k = 1000, n = 2000) {
  estimativas = c()
  erros = c()
  
  for (i in 1:k) {
    X = gen_X(n)
    A = gen_A(X)
    
    Y_C = gen_Y_C(A, X)
    Y_B = gen_Y_B(A, X)
    
    d = data.frame(A, X, Y_C, Y_B)
    
    mNNr = matchit(
      A ~ X1 + X2 + X3 + X4 + X5 +
        X6 + X7 + X8 + X9,
      data = d,
      link = "linear.logit",
      ratio = 3,
      replace = TRUE
    )
    
    m.data = match.data(mNNr)
    
    ##Para resultados contínuos
    
    ###sem ajuste por covariável
    fit1md <- lm(Y_C ~ A, data = m.data, weights = weights)
    
    ####Usando cluster-robust standart error
    vl = coeftest(fit1md, vcov. = vcovHC)
    estimativas[i] = vl[2]
    erros[i] = vl[4]
    
    
  }
  
  valores  = list(estimativas, erros)
  return(valores)
  
}
a = nn.com_rep.c(n = 150)
b = nn.com_rep.c(n = 400)
c = nn.com_rep.c(n = 800)
d = nn.com_rep.c(n = 1500)

##NN com reposição com efeito contínuo com ajuste por co-variável.
nn.com_rep.c.a = function(k = 1000, n = 2000) {
  estimativas = c()
  erros = c()
  
  for (i in 1:k) {
    X = gen_X(n)
    A = gen_A(X)
    
    Y_C = gen_Y_C(A, X)
    Y_B = gen_Y_B(A, X)
    
    d = data.frame(A, X, Y_C, Y_B)
    
    mNNr = matchit(
      A ~ X1 + X2 + X3 + X4 + X5 +
        X6 + X7 + X8 + X9,
      data = d,
      link = "linear.logit",
      ratio = 3,
      replace = TRUE
    )
    
    m.data = get_matches(mNNr)
    
    ##Para resultados contínuos
    
    ###sem ajuste por covariável
    fit1md <- lm(Y_C ~ A + X1 + X2 + X3 + X4 + X5 + 
                X6 + X7 + X8 + X9,
                data = m.data, weights = weights)
    
    ####Usando cluster-robust standart error
    vl = coeftest(fit1md, vcov. = vcovHC)
    estimativas[i] = vl[2]
    erros[i] = vl[4]
    
    
  }
  
  valores  = list(estimativas, erros)
  return(valores)
  
}
a1 = nn.com_rep.c.a(n = 150)
b1 = nn.com_rep.c.a(n = 400)
c1 = nn.com_rep.c.a(n = 800)
d1 = nn.com_rep.c.a(n = 1500)

##NN com reposição com efeito binário na escala do OR
nn.com_rep.b = function(k = 1000, n = 2000) {
  estimativas = c()
  erros = c()
  
  for (i in 1:k) {
    X = gen_X(n)
    A = gen_A(X)
    
    Y_C = gen_Y_C(A, X)
    Y_B = gen_Y_B(A, X)
    
    d = data.frame(A, X, Y_C, Y_B)
    
    mNNr = matchit(
      A ~ X1 + X2 + X3 + X4 + X5 +
        X6 + X7 + X8 + X9,
      data = d,
      link = "linear.logit",
      ratio = 3,
      replace = TRUE
    )
    
    m.data = get_matches(mNNr)
    
    ##Para resultados binários
    
    ###sem ajuste por covariável
    fit1md <- glm(Y_B ~ A, data = m.data, weights = weights,
                  family = quasibinomial("logit"))
    
    ####Usando cluster-robust standart error
    vl = coeftest(fit1md, vcov. = vcovHC, cluster = ~ subclass + id)
    estimativas[i] = vl[2]
    erros[i] = vl[4]
    
    
  }
  
  valores  = list(estimativas, erros)
  return(valores)
  
}
a2 = nn.com_rep.b(n = 150)
b2 = nn.com_rep.b(n = 400)
c2 = nn.com_rep.b(n = 800)
d2 = nn.com_rep.b(n = 1500)

##NN com reposição com efeito binário na escala do RR
nn.com_rep.b.rr = function(k = 1000, n = 2000) {
  estimativas = c()
  erros = c()
  
  for (i in 1:k) {
    X = gen_X(n)
    A = gen_A(X)
    
    Y_C = gen_Y_C(A, X)
    Y_B = gen_Y_B(A, X)
    
    d = data.frame(A, X, Y_C, Y_B)
    
    mNNr = matchit(
      A ~ X1 + X2 + X3 + X4 + X5 +
        X6 + X7 + X8 + X9,
      data = d,
      link = "linear.logit",
      ratio = 3,
      replace = TRUE
    )
    
    m.data = get_matches(mNNr)
    
    ##Para resultados binários
    
    ###sem ajuste por covariável
    fit1md <- glm(Y_B ~ A, data = m.data, weights = weights,
                  family = quasibinomial("log"))
    
    ####Usando cluster-robust standart error
    vl = coeftest(fit1md, vcov. = vcovHC, cluster = ~ subclass + id)
    estimativas[i] = vl[2]
    erros[i] = vl[4]
    
    
  }
  
  valores  = list(estimativas, erros)
  return(valores)
  
}
a3 = nn.com_rep.b.rr(n = 150)
b3 = nn.com_rep.b.rr(n = 400)
c3 = nn.com_rep.b.rr(n = 800)
d3 = nn.com_rep.b.rr(n = 1500)

nn.com_rep.b.rd = function(k = 1000, n = 2000) {
  estimativas = c()
  erros = c()
  
  for (i in 1:k) {
    X = gen_X(n)
    A = gen_A(X)
    
    Y_C = gen_Y_C(A, X)
    Y_B = gen_Y_B(A, X)
    
    d = data.frame(A, X, Y_C, Y_B)
    
    mNNr = matchit(
      A ~ X1 + X2 + X3 + X4 + X5 +
        X6 + X7 + X8 + X9,
      data = d,
      link = "linear.logit",
      ratio = 3,
      replace = TRUE
    )
    
    m.data = get_matches(mNNr)
    
    ##Para resultados binários
    
    ###sem ajuste por covariável
    fit1md <- glm(Y_B ~ A, data = m.data, weights = weights,
                  family = quasibinomial("identity"))
    
    ####Usando cluster-robust standart error
    vl = coeftest(fit1md, vcov. = vcovHC, cluster = ~ subclass + id)
    estimativas[i] = vl[2]
    erros[i] = vl[4]
    
    
  }
  
  valores  = list(estimativas, erros)
  return(valores)
  
}
a4 = nn.com_rep.b.rd(n = 150)
b4 = nn.com_rep.b.rd(n = 400)
c4 = nn.com_rep.b.rd(n = 800)
d4 = nn.com_rep.b.rd(n = 1500)

