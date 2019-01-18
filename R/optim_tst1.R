dat=data.frame(x=c(1,2,3,4,5,6), 
               y=c(1,3,5,6,8,12))

min.RSS <- function(data, par) {
  with(data, sum((par[1] + par[2] * x - y)^2))
}

result <- optim(par = c(1, 1,1,1, 1,1,1, 1,1), fn = min.RSS, data = dat)

plot(y ~ x, data = dat, main="Least square regression")
abline(a = result$par[1], b = result$par[2], col = "red")



a<-data.frame(leg=c("ap","bq","cr","aq","br","cp","ar","bp","cq"))
a$transfers<- rep(1,9)
a$aa<- 4
a$bb<- 5
a$cc<- 6
a$pp<- 3
a$qq<- 3
a$rr<- 3
a$cost_pr<- c(2,3,4,2,3,4,2,3,4)
a$capacity <-c(4,5,6,4,5,6,4,5,6)
a$cost_tr<- c(.2,.5,1.1,0.5,0.9,0.7,.7,0.5,0.9)
a

library(mosaic)
f<- makeFun(x^2+y~x&y)
g<- makeFun(x^2-y^2~x&y)
plotFun(f(x,y)~x&y,xlim = range(-2,2),ylim = range(-2,2),filled = F)
plotFun(g(x,y)~x&y,levels = 1,xlim = range(-2,2),ylim = range(-2,2),add = T,col="red",filled = F)


