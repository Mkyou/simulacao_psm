library(MatchIt)


parea_simples = function(k, psmod, efeito = log(1), perc = -3.5){
  #seja numéro de amostras = k, e tamanho de amostras = n
  psmod = formula(psmod)
  
  soma = c(0,0,0,0)
  media = c(0,0,0,0)
  ics = c(0,0,0,0)
  vies = c(0,0,0,0)
  
  n = c(200, 500, 700, 1500)
  
  for (i in 1:length(n)){
    #pareamentos
    for(j in 1:k){
      #utilizaremos um conjunto de tamanho de amostras, um para cada pareamento.
      #geram-se bases de dados um pouco diferentes e pseudo aleatórias para cada 
      #simulação.
      data = pgr_binaria(n[i], btreat = efeito, b0treat = perc)
      
      #realiza-se o pareamento para cada modelo de escore descrito na literatura
      m.out = matchit(psmod, data = data, method = "nearest",
                      caliper = 0.2) 
      
      #da mesma forma, armazenam-se as respostas de cada um desses modelos
      res = glm(result ~ treat + x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, 
                data = data, weights = m.out$weights, 
                family = binomial(link = "logit"))
      
      #obtenção de erros e valores
      erro = summary(res)$coefficients[2,2]
      
      valor = summary(res)$coefficients[2,1]
      
      #obtenção de somas e ics
      soma[i] = soma[i] + valor
      
      #ics
      #1
      if(valor + 1.96*erro >= exp(efeito) && valor - 1.96*erro <= exp(efeito)){
        ics[i] = ics[i] + 1
      }
      
    }
    
    #médias
    media[i] = (soma[i]/k)
    vies[i] = (100*(media[i] - exp(efeito)))/(exp(efeito))
    print(i)
  } 
  
  return(cbind(media, vies, ics))
}

ps1 = "treat ~ x1 + x2 + x4 + x5 + x7 + x8"
parea_simples(50, as.formula(ps1))
