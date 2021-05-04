extrai_dados = function(lista, efeito){
  ic = mean(ifelse(2 <= lista[[1]] + lista[[2]]*1.96 & 
                     2>= lista[[1]] - lista[[2]]*1.96, 1, 0))
  media = mean(lista[[1]])
  vies_media = (media-efeito)/media
  vies_quadrado = mean((2-lista[[1]])^2)
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
