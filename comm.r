#!/usr/bin/env Rscript

library(ggplot2)

library(getopt)

spec = matrix(c(
'help' , 'h', 0, "logical",
'save' , 'w', 2, "character",
'nocurve' , NA, 2, "logical",
'file', 'f', 1, "character"
), byrow=TRUE, ncol=4);
opt = getopt(spec);

if ( !is.null(opt$help) ) {
	cat(getopt(spec, usage=TRUE));
	q(status=1);
}


data = read.csv(opt$file, comment.char="#")

X11()

#ggplot(data, aes(factor(data$n), data$errorrate)) + geom_boxplot() + geom_jitter() + 
qpl <- qplot(factor(n), errorrate, data = data, color = tests, geom = c("boxplot", "point"), xlab = "n")
if (is.null(opt$nocurve)) { qpl <- qpl + geom_smooth(aes(group = 1)) }

qpl + xlab("Number of computers") + ylab("Error")


if (!is.null(opt$save)) {
	ggsave(file = opt$save)
} 

message("Press Return To Continue")
invisible(readLines("stdin", n=1))
