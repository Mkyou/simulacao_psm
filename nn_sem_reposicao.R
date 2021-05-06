library(MatchIt)
library(lmtest)
library(sandwich)
#library(boot)
#library(survival)
library(tibble)

#NN sem reposição com efeito contínuo sem ajuste por co-variável.
nn.sem_rep.c = function(k = 1000, n = 2000) {
  estimativas = c()
  erros = c()
  
  for (i in 1:k) {
    X = gen_X(n)
    A = gen_A(X)
    
    Y_C = gen_Y_C(A, X)
    Y_B = gen_Y_B(A, X)
    
    d = data.frame(A, X, Y_C, Y_B)
    
    #NN sem reposição
    m.out = matchit(A ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9,
                    data = d)
    #m.out
    m.data = match.data(m.out)
    #head(m.data)
    
    ##Para resultados contínuos
    
    ###sem ajuste por covariável
    fit = lm(Y_C ~ A, data = m.data, weights = m.data$weights)
    
    ####Usando cluster-robust standart error
    vl = coeftest(fit, vcov. = vcovCL, cluster = ~ subclass)
    estimativas[i] = vl[2]
    erros[i] = vl[4]
    
    
  }
  
  valores  = list(estimativas, erros)
  return(valores)
  
}

a = nn.sem_rep.c(n = 150)
b = nn.sem_rep.c(n = 400)
c = nn.sem_rep.c(n = 800)
d = nn.sem_rep.c(n = 1500)

#NN sem reposição com efeito contínuo com ajuste por co-variável.
nn.sem_rep.c.a = function(k = 1000, n = 2000) {
  estimativas = c()
  erros = c()
  for (i in 1:k) {
    X = gen_X(n)
    A = gen_A(X)
    
    Y_C = gen_Y_C(A, X)
    Y_B = gen_Y_B(A, X)
    
    d = data.frame(A, X, Y_C, Y_B)
    
    #NN sem reposição
    m.out = matchit(A ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9,
                    data = d)
    #m.out
    m.data = match.data(m.out)
    #head(m.data)
    
    ##Para resultados contínuos
    ###Com ajuste por co-variável
    fit <- lm(Y_C ~ A + X1 + X2 + X3 + X4 + X5 + 
                 X6 + X7 + X8 + X9, data = m.data,
               weights = weights)
    vl = coeftest(fit, vcov. = vcovCL, cluster = ~subclass)["A",,drop=FALSE]
    estimativas[i] = vl[1]
    erros[i] = vl[2]
    
  }
  valores  = list(estimativas, erros)
  return(valores)
}

a1 = nn.sem_rep.c.a(n = 150)
b1 = nn.sem_rep.c.a(n = 400)
c1 = nn.sem_rep.c.a(n = 800)
d1 = nn.sem_rep.c.a(n = 1500)

nn.sem_rep.b.or = function(k = 1000, n = 2000){
  estimativas = c()
  erros = c()
  
  for (i in 1:k) {
    X = gen_X(n)
    A = gen_A(X)
    
    Y_C = gen_Y_C(A, X)
    Y_B = gen_Y_B(A, X)
    
    d = data.frame(A, X, Y_C, Y_B)
    
    #NN sem reposição
    m.out = matchit(A ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9,
                    data = d)
    m.data = match.data(m.out)

    ##Para resultados binários
    fit = glm(Y_B ~ A, data = m.data, weights = m.data$weights, 
              family = binomial(link = "logit"))
    
    ####Usando cluster-robust standart error
    vl = coeftest(fit, vcov. = vcovCL, cluster = ~ subclass)
    estimativas[i] = vl[2]
    erros[i] = vl[4]
    
    
  }
  
  valores  = list(estimativas, erros)
  return(valores)
  
}

a2 = nn.sem_rep.b.or(n = 150)
b2 = nn.sem_rep.b.or(n = 400)
c2 = nn.sem_rep.b.or(n = 800)
d2 = nn.sem_rep.b.or(n = 1500)

nn.sem_rep.b.rr = function(k = 1000, n = 2000){
  estimativas = c()
  erros = c()
  
  for (i in 1:k) {
    X = gen_X(n)
    A = gen_A(X)
    
    Y_C = gen_Y_C(A, X)
    Y_B = gen_Y_B(A, X)
    
    d = data.frame(A, X, Y_C, Y_B)
    
    #NN sem reposição
    m.out = matchit(A ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9,
                    data = d)
    m.data = match.data(m.out)
    
    ##Para resultados binários
    fit = glm(Y_B ~ A, data = m.data, weights = m.data$weights, 
              family = binomial(link = "log"))
    
    ####Usando cluster-robust standart error
    vl = coeftest(fit, vcov. = vcovCL, cluster = ~ subclass)
    estimativas[i] = vl[2]
    erros[i] = vl[4]
    
    
  }
  
  valores  = list(estimativas, erros)
  return(valores)
}

a3 = nn.sem_rep.b.rr(n = 150)
b3 = nn.sem_rep.b.rr(n = 400)
c3 = nn.sem_rep.b.rr(n = 800)
d3 = nn.sem_rep.b.rr(n = 1500)
