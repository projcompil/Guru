#!/usr/bin/env Rscript

args = commandArgs()
#print(args)

x = read.csv(args[6], comment.char = "#")


print("Error rates mean and standard deviation : ")
print(mean(x[,1]))
print(sd(x[,1]))
print("Number of iterations : mean and standard eviation : ")
print(mean(x[,2]))
print(sd(x[,2]))
X11()
plot(x[,2], x[,1], ylab="errorrate", xlab="listsize")
message("Press Return To Continue")
invisible(readLines("stdin", n=1))
