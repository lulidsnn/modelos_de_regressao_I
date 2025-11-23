# Um engenheiro de uma companhia de semicondutores quer modelar a relação entre o 
# HFE (y) do equipamento e três parâmetros: RS do Emissor (x,), RS da Base (x₂) e 
# RS do Emissorpara-Base (x,).

d <- list(
  y = c(128.40, 52.62, 113.9, 98.01, 139.9, 102.6, 48.14, 109.6, 82.68, 112.6, 97.52, 59.06, 111.8, 89.09, 101, 171.9, 66.8, 157.1, 208.4, 133.4),
  x1 = c(14.620, 15.630, 14.620, 15.000, 14.500, 15.250, 16.120, 15.130, 15.500, 15.130, 15.500, 16.120, 15.130, 15.630, 15.380, 14.380, 15.500, 14.250, 14.500, 14.620),
  x2 = c(226, 220, 217.40, 220, 226.5, 224.1, 220.5, 223.5, 217.6, 228.5, 230.2, 226.5, 226.6, 225.6, 229.7, 234, 230, 224.3, 240.5, 223.7),
  x3 = c(7, 3.375, 6.375, 6, 7.625, 6, 3.375, 6.125, 5, 6.625, 5.750, 3.750, 6.125, 5.375, 5.875, 8.875, 4, 8, 10.870, 7.375)
)

# a) Ajuste o modelo de regressão linear múltipla para os dados.

m <- lm(d$y ~ d$x1 + d$x2 + d$x3)
m
summary(m)
anova(m)

# b) Preveja HFE quando x1, = 14,5, x2 = 220 e x3, = 5,0.

Y = m$coef[1] + m$coef[2]*14.5 + m$coef[3]*220 + m$coef[4]*5
Y

