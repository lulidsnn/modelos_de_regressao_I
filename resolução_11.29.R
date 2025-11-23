# utiliza a mesma base de dados do exercício 11.6

  d <- list(
    y = c(240, 236, 290, 274, 301, 316, 300, 296, 267, 276, 288, 261),
    x1 = c(25, 31, 45, 60, 65, 72, 80, 84, 75, 60, 50, 38),
    x2 = c(24, 21, 24, 25, 25, 26, 25, 25, 24, 25, 25, 23),
    x3 = c(91, 90, 88, 87, 91, 94, 87, 86, 88, 91, 90, 89),
    x4 = c(100, 95, 110, 88, 94, 99, 97, 96, 110, 105, 100, 98))

# a) Encontre os intervalos de confiança de 95% para B1, B2, B3; e В4

m = lm(d$y ~ d$x1 + d$x2 + d$x3 + d$x4) # criando o modelo de regressão múltiplo
m
anova(m)
summary(m)

# n -> 12 ; p -> k + 1 (k = 4) [5]

inter_conf_coef <- function(alpha, coef, erro_p, n, p){
  t = qt(1 - alpha/2, df = (n-p))
  li <- coef - t*erro_p
  ls <- coef + t*erro_p
  limites <- c(li, ls)
  print(limites)
}

b1 <- inter_conf_coef(0.05, 0.60537, 0.36890, 12, 5)
b2 <- inter_conf_coef(0.05, 8.92364, 5.30052, 12, 5)
b3 <- inter_conf_coef(0.05, 1.43746, 2.39162, 12, 5)
b4 <- inter_conf_coef(0.05, 0.01361, 0.73382, 12, 5)

confint(m)

# b) Encontre um intervalo de confiança de 95% para a média de Y, quando x1 = 75,x2 = 24, x3 = 90 e x4 = 98.

