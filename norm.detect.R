# This function detect whether single-parameter datasets obey normal distribution.
# Modified from book "Web Analysis and Mining With R" by LI MING (ISBN 978-7-111-45971-2).
norm.detect <- function(input.data, alpha = 0.05, pic = TRUE){
  if(pic){
    dev.new()
    par(mfrow = c(2,1))
    qqnorm(input.data, main = "QQ Graph")
    qqline(input.data)
    hist(input.data, freq = F, main = "Histogram and Density Estimation Curve")
    lines(density(input.data), col = "blue")
    x <- c(round(min(input.data)):round(max(input.data)))
    lines(x, dnorm(x, mean(input.data), sd(input.data)), col = "red")
  }
  sol <- shapiro.test(input.data)
  if(sol$p.value > alpha){
    print(paste("Data obeys normal distribution with p.value = ", sol$p.value))
  }else{
    print("Data does not obey normal distribution")
  }
}