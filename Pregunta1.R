# EP2 [3 preguntas]
# Maxima verisimildad (estimacion puntual)
# Intervalo de confianza 
# Prueba de hipotesis

#importante:
install.packages("BSDA")
library(BSDA)




# Pregunta 1 (practica 6)
x <- c(506, 508, 499, 503, 504, 510, 497, 512, 514, 505, 493, 496, 506, 502, 509, 496)

xbar <- mean(x)
sigma <- 5   #sigma^2=25
n <- length(x)
m <- qnorm(0.95)*sigma/sqrt(n)
IC <- c(xbar-m, xbar+m)
IC

z.test(x, sigma.x = 5, conf.level = 0.9)

# prueba de hipotesis
# H0: mu = 500 (hipotesis nula)
# H1: mu > 500 (hipotesis alternativa <- lo que queremos probar)

# rechazo la hipotesis nula porque mu0 no esta en el IC 

zc <- qnorm(0.90) # z critico = limite de rareza

mu0 <- 500
zobs <- (xbar - mu0)/(sigma/sqrt(n))

z.test(x, sigma.x = 5, conf.level = 0.9, alternative = "greater", mu = mu0)

# c)
#no sigma
xbar <- mean(x)
s <- sd(x)
n <- length(x)
m <- qt(0.99+0.005, n-1)*s/sqrt(n)
IC <- c(xbar-m, xbar+m)

t.test(x, conf.level = 0.99)

# H0 : mu = mu0 = 500
# H1 : mu > mu0 (cola superior)

mu0 <- 500
t.test(x, mu = mu0, alternative = "greater", conf.level = 0.99)

# t critico
qt(0.99, n-1)
tobs <- (xbar - mu0)/(s/sqrt(n))
p-valor <- 1-pt(tobs, n-1)


###
install.packages("EnvStats")
library(EnvStats)
varTest(x)
icvar <- c(s^2*(n-1)/qchisq(0.975, n-1), s^2*(n-1)/qchisq(0.025, n-1))
icsigma <- sqrt(icvar)
icsigma

varTest(c, conf.level = 0.95, sigma.squared = 5^2, alternative = "two.sided")


# ha faltado prop.test
prop.test(136, 400, correct = FALSE, 
          p = 0.3, alternative = "less")
prop.test(positivos, totales, ...)