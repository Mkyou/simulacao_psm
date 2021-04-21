library(MatchIt)

parea_simples = function(k, psmod, efeito, perc){
  
  soma = c(0,0,0,0)
  media = c(0,0,0,0)
  vies_pont = c(0,0,0,0)
  ic_n = c(0,0,0,0)
  
  n = c(300, 600, 900, 1500)
  for (i in 1:4){
    
    for (j in 1:k){
      dados = pgr_binaria(num = n[i], btreat = efeito, b0treat = perc)
      m.out = matchit(formula = as.formula(psmod), data = dados, 
                      method = "nearest", caliper = 0.2, std.caliper = TRUE)
      res = glm(result ~ treat + x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9,
                data = dados, weights = m.out$weights)
      valor = summary(res)$coefficients[2]
      erro = summary(res)$coefficients[13]
      
      #obter soma
      soma[i] = soma[i] + valor
      
      #obter ic
      if(valor + 1.96*erro >= efeito && valor - 1.96*erro <= efeito){
        ic_n[i] = ic_n[i]+1
      }
      
      
      
    }
    
    #obter média
    media[i] = soma[i]/k
    
    #obter viés
    vies_pont[i] = (100*(media[i] - efeito))/efeito
    cat("Rodou simulação com n = ", n[i], " \n")
    
    ic_n[i] = ic_n[i]*100/k
  }
  return(cbind(n, media, vies_pont, ic_n))
  
}

parea_simples(200, "treat ~ x1 + x2 + x3 + x4 + x5 + x6", 0, -3.5)
