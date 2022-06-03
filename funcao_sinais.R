library(stringr)
library(dplyr)
library(tidyr)
library(rlang)

# Função Tabela Sinais 2k

tabsinais2k <- function(df,coluna='fatores'){
  
  novas_col <- df %>% select(as.name(coluna)) %>% filter(str_length(!!sym(coluna))==1) %>% unlist() # Pegando os fatores individualmente
  
  for(i in novas_col){
    df <- df %>% mutate( !!i := ifelse(str_detect(!!sym(coluna),i),1,-1)) # Gerando os sinais
  }
  
  return(df)
}

## Teste
# Função pra testar a tabela de sinais

geracaofatores <- function(num_fat = 5){
  # Função para gerar as combinações de fatores com o objetivo de testar a função tabsinais2k
  fatores = c()
  
  for(i in 1:num_fat){
    fatores = c(fatores,combn(letters[1:num_fat],i) %>% t() %>% data.frame() %>% unite('uni',sep='') %>% unlist() %>% unname())
  }
  return(data.frame(fatores))
}

## Saida funcao geracaofatores2k

geracaofatores(3)

## COnferindo

geracaofatores(2) %>% tabsinais2k()

geracaofatores(5) %>% tabsinais2k()

geracaofatores(8) %>% tabsinais2k()
