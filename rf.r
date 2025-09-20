library(readr)
# set.seed(123)

#Parametros
variavel_alvo <- "vis(m)"
mtry <- 3

# Carrega o dataset original
file_path <- "dataset-features-avancadas.csv"
if (!file.exists(file_path)) {
  stop("Erro: O arquivo 'dataset-definitivo.csv' não foi encontrado.")
}
df_raw <- read_csv(file_path)

# remove a variavel alvo para o dataframe treino

df_treino <- df_raw[, -which(names(df_raw) == variavel_alvo)] 

# pega o numero de variaveis para a amostra de acordo
# com a quantidade de colunas raiz(numero de colunas)

mtry <- floor(sqrt(ncol(df_treino)))

# coleta a um numero de variaveis definido pelo mtry
# aleatoriamente e salva na amostra


amostra <- sample(df_treino, mtry, replace = TRUE)


#print(colnames(amostra))


#Como o R trabalha com vetores, eu subistiui o 
#laço de repetição por uma função que calcula 
#os dados de uma só vez ao invez de percorrer os elementos. 

variancia <- function(amostra){
    #converte os valores para numerico para evitar erros
    amostra <- as.numeric(amostra)
    #calcula a media dos itens de cada variavel
    media <- mean(amostra[[2]], na.rm = TRUE)
    #print(media)

    #faz o calculo de desvio padrão da coluna
    return (sum((amostra-media)^2)/(length(amostra)-1))     
}

#A função sapply do R executa a fução dp 
#(calcular desvio padrão) para cada coluna da amostra

resultados_var <- sapply(amostra, variancia)
print(resultados_var)

