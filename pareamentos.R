library(MatchIt)

parea_simples = function(n, k, psmod, efeito, perc){
  
  soma = 0
  media = 0
  vies_pont = 0
  ic_n = 0
  
  for (j in 1:k){
    dados = pgr_binaria(num = n, btreat = efeito, b0treat = perc)
    m.out = matchit(formula = as.formula(psmod), data = dados, 
                    method = "nearest", caliper = 0.2, std.caliper = TRUE)
    res = lm(result ~ treat + x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9,
              data = dados, weights = m.out$weights)
    valor = summary(res)$coefficients[2]
    erro = summary(res)$coefficients[13]
    
    #obter soma
    soma = soma + valor
    
    #obter ic
    if(valor + 1.96*erro >= efeito && valor - 1.96*erro <= efeito){
      ic_n = ic_n+1
    }
    
    
  }
  
  #obter média
  media = soma/k
  
  #obter viés
  vies_pont = (100*(media - efeito))/efeito
  ic_n = (ic_n*100)/k
  
  return(cbind(n, media, vies_pont, ic_n))
  
}

parea_simples(n = 200, k = 500, psmod = "treat ~ x1 + x2 + x3 + x4 + x5 + x6", 
              efeito = 0, perc = -3.5)
