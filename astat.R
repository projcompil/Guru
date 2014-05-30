#!/usr/bin/env Rscript

#print(args)

library(getopt)

spec = matrix(c(
'file', 'f', 1, "character",
'help' , 'h', 0, "logical",
'save' , 'w', 2, "character",
'nolines	' , NA, 2, "logical"
), byrow=TRUE, ncol=4);
opt = getopt(spec);

if ( !is.null(opt$help) ) {
	cat(getopt(spec, usage=TRUE));
	q(status=1);
}
x = read.csv(opt$f, comment.char = "#")

x <- subset(x, tests >=100)

#print("Error rates mean and standard deviation : ")
#print(mean(x[,1]))
#print(sd(x[,1]))
#print("Number of iterations : mean and standard eviation : ")
#print(mean(x[,2]))
#print(sd(x[,2]))
X11()
#plot(x[,2], x, ylab="errorrate", xlab="listsize")
#plot(x$n, x$errorate)
plot(x[,2], x[,1], ylab="errorrate", xlab="listsize")
temp <- data.frame(y = x[,1], x = x[,2])
# fit non-linear model
#mod <- nls(y ~ c/(1+exp(-b *(x-a))), data = temp, start = list(a = 5, b = 1, c=5 ))
#mod <- nls(y ~ exp(b * x + a), data = temp, start = list(a = 0, b = 0))
mod <- nls(y ~ log( 1 + exp(b * x + a)), data = temp, start = list(a = 0, b = 0))
#
coef = coef(mod)

a = coef[1]
b = coef[2]
print(coef)
print(-a/b)
c = coef[3]
# add fitted curve
#lines(temp$x, predict(mod, list(x = temp$x)))
#func <-function(x) c/(1+exp(-b *(x-a)))
#func <-function(x) exp(b * x + a)
func <-function(x) log(1+exp(b * x + a))
curve(func, from = 0, to = 100, n = 100001, add=TRUE, col="red")
#lines(temp$x, predict(mod, list(x = temp$x)))

if(!is.null(opt$save)) {
	dev.copy(pdf, opt$save)
	dev.off()
}
message("Press Return To Continue")
invisible(readLines("stdin", n=1))
