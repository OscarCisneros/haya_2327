Bisection <- function(f, a, b, n = 1000, tol = 1e-7) {
  if (!(f(a) < 0) && (f(b) > 0)) {
    stop('The root does not exist within this interval')
  } else if (!(f(a) > 0) && (f(b) < 0)) {
    stop('The root does not exist within this interval')
  }
  for (i in 1:n) {
    c <- (a + b) / 2
    if ((f(c) == 0) || ((b - a) / 2) < tol) {
      return(c)
    }
    ifelse(sign(f(c)) == sign(f(a)), 
           a <- c,
           b <- c)
  }
  print('Too many iterations')
}

Bisection(f = func, 2,3)

df <- data.frame(d_ = seq(6,30)) %>% 
  mutate(n_d = sapply(d_, function(x) func_dens_weibull(x,param_[1],param_[2],param_[3]))* N_) %>%
  mutate(n_d = floor(n_d))

sapply(x=seq(6,30), funtion(x) func_dens_weibull(x,param_[1],param_[2],param_[3]))

Dg_ = 17.8
Dm_ = 17.3
var_ = Dg_^2 - Dm_^2
# parámetro a
a = 7.5
c=2.489

fun_1 <- function(x) {
  c=2.489
  ((Dm_ - x)^2 / (gamma(1+1/c))^2) *(gamma(1+2/c) - (gamma(1+1/c))^2)
  
}

curve(fun_1, xlim=c(5,8), col = 'red', lwd = 2, lty = 2, xlab = "x", ylab = "f(x)")

func = function(x){
  var_ -  ((Dm_ - a)^2 / (gamma(1+1/x))^2) *(gamma(1+2/x) - (gamma(1+1/x))^2)
}
curve(func, xlim=c(9,10), col = 'red', lwd = 2, lty = 2, xlab = "x", ylab = "f(x)")

BFfzero(func, 0.1, 4, num = 10, eps = 1e-05)

fun_1 <- function(x) {
  c=.7932867
  ((Dm_ - x)^2 / (gamma(1+1/c))^2) *(gamma(1+2/c) - (gamma(1+1/c))^2)
  
}

curve(fun_1, xlim=c(7,15), col = 'red', lwd = 2, lty = 2, xlab = "x", ylab = "f(x)")

############
a <-7.184
b <-11.222
c <- 2.598

df_weib <- data.frame(x = seq(a-1,100, by = 1)) %>%
  mutate(dens_weib = func_dens_weibull(x, a,b,c)) %>%
  na.omit()
ggplot(df_weib, aes(x=x, y = dens_weib))+geom_point()
hh <- df_weib %>% filter(!(is.infinite(dens_weib)))
sum(hh$dens_weib)
