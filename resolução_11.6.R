d <- list(
  y = c(240, 236, 290, 274, 301, 316, 300, 296, 267, 276, 288, 261),
  x1 = c(25, 31, 45, 60, 65, 72, 80, 84, 75, 60, 50, 38),
  x2 = c(24, 21, 24, 25, 25, 26, 25, 25, 24, 25, 25, 23),
  x3 = c(91, 90, 88, 87, 91, 94, 87, 86, 88, 91, 90, 89),
  x4 = c(100, 95, 110, 88, 94, 99, 97, 96, 110, 105, 100, 98))

# a) ajuste um modelo de regressão linear múltipla a esses dados 
m = lm(d$y ~ d$x1 + d$x2 + d$x3 + d$x4)
m
anova(m)
summary(m)

# b) Preveja o consumo de potência para um mês em que x1 = 75°F, x2 = 24 dias, x3 = 90%, x4 = 98 toneladas
Y = -102.71324 + 0.60537*75 + 8.92364*24 + 1.43746*90 + 0.01361*98
Y 
