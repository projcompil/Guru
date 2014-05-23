#!/usr/bin/env Rscript

args = commandArgs()
#print(args)

x = read.csv(args[6], comment.char = "#")


#print("Error rates mean and standard deviation : ")
#print(mean(x[,1]))
#print(sd(x[,1]))
#print("Number of iterations : mean and standard eviation : ")
#print(mean(x[,2]))
#print(sd(x[,2]))
X11()
#plot(x[,2], x, ylab="errorrate", xlab="listsize")
plot(x$n, x$errorate)
temp <- data.frame(y = x[,1], x = x[,2])
# fit non-linear model
mod <- nls(y ~ exp(a + b * x), data = temp, start = list(a = 0, b = 0))
#
coef = coef(mod)

print(coef)
# add fitted curve
lines(temp$x, predict(mod, list(x = temp$x)))

message("Press Return To Continue")
invisible(readLines("stdin", n=1))
