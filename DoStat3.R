n <- rnorm(100)
plot(ecdf(n), col="green", main="Dystrybuanta empiryczna, a teoretyczna - N(0,1)",
     ylab = "y")
curve(pnorm(x), add=TRUE, col="blue")
legend(0.4, 0.5, legend=c("Dystrybuanta empiryczna", "Dystrybuanta teoretyczna"),
       fill = c("green", "blue"), border = "white", text.width = 1.7)

n <- rexp(100, 1)
plot(ecdf(n), col="green", main="Dystrybuanta empiryczna, a teoretyczna - Exp(1)",
     ylab = "y")
curve(pexp(x, 1), add=TRUE, col="blue")
legend(2, 0.5, legend=c("Dystrybuanta empiryczna", "Dystrybuanta teoretyczna"),
       fill = c("green", "blue"), border = "white", text.width = 1.3)

Symulator <- function(F, M=1000, alpha=0.05, n=100, R){
  eps <- (log(2/alpha)/(2*n))^(1/2)
  L <- function(x, E){
    max(E(x) - eps, 0)}
  U <- function(x, E){
    min(E(x) + eps, 1)}
  I <- 0
  for (b in 1:M){
    X <- R(n)
    G <- c(1:100)
    D <- c(1:100)
    E <- ecdf(X)
    x <- seq(-5, 5, length=100)
    l <- 0
    for (i in 1:100){
      if (L(x[i], E)<=F(x[i]) & F(x[i]) <= U(x[i], E)){
        l <- l +1}
    }
    if(l==100){
      I <- I +1}
    }
  return(I)
}
Symulator(pnorm, R=rnorm)
Symulator(pexp, R=rexp)

#zad3
n <- rnorm(500)a
plot(density(n, kernel="gaussian"), main="Estymatory jądrowe, a gęstość rozkładu N(0,1)", col= "purple")
lines( density(n, kernel="gaussian", bw=1), col="green")
lines( density(n, kernel="gaussian", bw=5), col="darkgreen")
lines( density(n, kernel="gaussian", bw=0.5), col="lightgreen")
curve(dnorm(x), -3, 4, add=TRUE, col="red")
legend("topright", legend=c("gęstość N(0,1)",expression("h"[n]*"=Silverman's roth"),expression("h"[n]*"=1"),expression("h"[n]*"=5"),expression("h"[n]*"=0.5"))
       , fill=c("red", "purple", "green", "darkgreen", "lightgreen"))
#DONE

#zad4

u <- runif(500)
y <- c(1:500)
for (i in 1:500){
  if (u[i]<(4/10)){
    y[i] <- rnorm(1)
  }
  else if(u[i]<(8/10)){
    y[i] <- rnorm(1, 2, 1)
  }
  else{
    y[i] <- rnorm(1, 4, 2)
  }
}

liczba_klas = ceiling((max(y) - min(y))/(2*IQR(y)*length(y)^(-1/3)))
curve(dnorm(x, 1.6, (0.48)^(1/2)), -4, 10, col="red", ylab="y")
hist(y, liczba_klas, freq=FALSE, add=TRUE, col="yellow")
lines(density(y, kernel="gaussian"), col="blue")
legend("topright", legend=c("gęstość", "histogram z regułą F-D", "estymator jądrowy z regułą Silvermana"), fill=c("red", "yellow", "blue"))



