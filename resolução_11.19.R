# utiliza a mesma base de dados do exercício 11.6

d <- list(
  y = c(240, 236, 290, 274, 301, 316, 300, 296, 267, 276, 288, 261),
  x1 = c(25, 31, 45, 60, 65, 72, 80, 84, 75, 60, 50, 38),
  x2 = c(24, 21, 24, 25, 25, 26, 25, 25, 24, 25, 25, 23),
  x3 = c(91, 90, 88, 87, 91, 94, 87, 86, 88, 91, 90, 89),
  x4 = c(100, 95, 110, 88, 94, 99, 97, 96, 110, 105, 100, 98))

# (a) Teste a significância da regressão, usando a = 0,01. Qual
# é o valor P para esse teste?

m = lm(d$y ~ d$x1 + d$x2 + d$x3 + d$x4) # criando o modelo de regressão múltiplo
m
anova(m)
summary(m) # fornece o p-valor e a estatística F
 
X = model.matrix(m) # transformando em Matriz 
X
X1234= X[, -1] # criando uma submatriz sem a presença do intercepto
X1234
mA = lm(d$y ~ X1234) # modelo de regressão com a matriz
anova(mA) # anova fornecida vai ter os valores 'compressados'
valor_p = 0.0303 #retirado da anova

# teste para dizer se a regressão é ou não significativa
f_sign <- function(alpha, p_valor){
  if(p_valor >= alpha){
    print('A regressão NÃO é significativa!')
  }else{
    print('A regressão É significativa!')
  }
}

f_sign(0.01, 0.0303)
