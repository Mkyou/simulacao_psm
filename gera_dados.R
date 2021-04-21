library(tibble)

#Gera base de dados binária pseudo-aleatória atribuindo-a 
#para a variável "data".
pgr_binaria = function(num = 1000, btreat = 0, b0treat = -3.5){
  
  #' @title PGR (Processo Gerador de Dados)
  #' @description Gera um conjunto de variáveis, efeitos e os vetores de 
  #' tratamento e resultado.
  #' @param num o valor que indicará o tamanho dos vetores, bem como dos laços.
  #' @param btreat o valor que indicará o efeito real que futuramente será 
  #' estimado - exp(btreat). Usar log(1), log(2), log(5) e log(10)
  #' @param b0treat o valor de influência no número de tratados;
  #' -3.5 implica em 50% T e 50% C;
  #' Valores aproximados.
  #' @return um data.frame com os dados gerados.
    
  #Criando variáveis explicativas
  x1 = rbinom(num, 1, 0.5)
  x2 = rbinom(num, 1, 0.5)
  x3 = rbinom(num, 1, 0.5)
  x4 = rbinom(num, 1, 0.5)
  x5 = rbinom(num, 1, 0.5)
  x6 = rbinom(num, 1, 0.5)
  x7 = rbinom(num, 1, 0.5)
  x8 = rbinom(num, 1, 0.5)
  x9 = rbinom(num, 1, 0.5)
  
  #Gerando efeitos de resultado
  b0out = -9.3
  a1 = log(5)
  a2 = log(5)
  a3 = log(5)
  a4 = log(2)
  a5 = log(2)
  a6 = log(2)
  
  #Gerando efeitos de tratamento
  b1 = log(5)
  b2 = log(2)
  b3 = log(5)
  b4 = log(2)
  b5 = log(5)
  b6 = log(2)
    
  #Criando vetores de tratamento e resultado
  for(i in 0:num){
    eta = b0treat + (b1*x1) + (b2*x2) + (b3*x4) + (b4*x5) + (b5*x7)
    + (b6*x8)
    
    pt = exp(eta)/(1+exp(eta))
    treat = rbinom(num, size = 1, prob = pt)
  }
  
  for (i in 0:num){
    eta1 = b0out + btreat*treat + (a1*x1) + (a2*x2) + (a3*x3)
    + (a4*x4) + (a5*x5) + (a6*x6)
    
    po = exp(eta1)/(1 + exp(eta1))
    result = rbinom(num, size = 1, prob = po)
  }
  
  #Criando base de dados 
  
  data = data.frame(cbind(x1, x2, x3, x4, x5, x6, x7, x8, x9, treat, result))
  data = as_tibble(data)
  
  return(data)
  
}

#somas =c()
#for(i in 1:500){
#  data = pgr_binaria()
#  somas[i] = sum(data$treat==1)
#}
#mean(somas)
