extrai_dados = function(lista, efeito){
  ic = mean(ifelse(efeito <= lista[[1]] + lista[[2]]*1.96 & 
                     efeito>= lista[[1]] - lista[[2]]*1.96, 1, 0))
  media = mean(lista[[1]])
  vies_media = (media-efeito)/media
  vies_quadrado = mean((efeito-lista[[1]])^2)
  return(round(c(ic, media, vies_media, vies_quadrado),5))
}

extrai_dados.or = function(lista, efeito){
  ic = mean(ifelse(efeito <= exp(lista[[1]] + lista[[2]]*1.96) & 
                     efeito >= exp(lista[[1]] - lista[[2]]*1.96), 1, 0))
  media = mean(exp(lista[[1]]))
  vies_media = (media-efeito)/media
  vies_quadrado = mean((efeito-exp(lista[[1]]))^2)
  return(round(c(ic, media, vies_media, vies_quadrado),5))
}

gera_tabela = function(a, b, c, d){
  tabela = as_tibble(data.frame(
    N = c(150, 400, 800, 1500),
    x_barra = c(a[[2]], b[[2]], c[[2]], d[[2]]),
    v_x_barra = c(a[[3]], b[[3]], c[[3]], d[[3]]),
    ics = c(a[[1]], b[[1]], c[[1]], d[[1]]),
    v_q_medio = c(a[[4]], b[[4]], c[[4]], d[[4]])
  ))
  return(tabela)
}


tabela1 = gera_tabela(extrai_dados(a, 2), extrai_dados(b, 2), extrai_dados(c, 2),
            extrai_dados(d, 2))
comment(tabela1) = 
"Resultados para pareamento NN, sem reposição, na estimativa
do ATT para efeito contínuo, sem ajuste por co-variável." 

tabela2 = gera_tabela(extrai_dados(a1, 2), extrai_dados(b1, 2), 
                      extrai_dados(c1, 2), extrai_dados(d1, 2))
comment(tabela2) =
  "Resultados para pareamento NN, sem reposição, na estimativa
do ATT para efeito contínuo, com ajuste por co-variável."

tabela3 = gera_tabela(extrai_dados.or(a2, 1.92), extrai_dados.or(b2, 1.92),
                      extrai_dados.or(c2, 1.92), extrai_dados.or(d2, 1.92))
comment(tabela3) = 
  "Resultados para pareamento NN, sem reposição, na estimativa
do ATT para efeito binário marginal, sem ajuste por co-variável, 
na escala do OR."

tabela4 = gera_tabela(extrai_dados.or(a3, 1.54), extrai_dados.or(b3, 1.54),
                      extrai_dados.or(c3, 1.54), extrai_dados.or(d3, 1.54))
comment(tabela4) = 
  "Resultados para pareamento NN, sem reposição, na estimativa
do ATT para efeito binário marginal, sem ajuste por co-variável, 
na escala do RR."

tabela5 = gera_tabela(extrai_dados(a4, .144), extrai_dados(b4, .144), 
                      extrai_dados(c4, .144), extrai_dados(d4, .144))
comment(tabela5) =
  "Resultados para pareamento NN, sem reposição, na estimativa
do ATT para efeito binário, sem ajuste por co-variável, na escala do RD."
