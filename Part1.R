require(ggplot2)

set.seed(1)

noSim = 1000
n <- 40
lambda <- 0.2
simulations <- data.frame(x=replicate(noSim, mean(rexp(n, lambda))))

mean <- mean(simulations$x)
stdev <- sd(simulations$x)

expectedMean <- 1 / lambda
expectedStdev <- 1 / lambda / sqrt(n)

g <- ggplot(simulations, aes(x = x)) + geom_histogram(binwidth=.3, colour = "black", fill = "lightblue", aes(y = ..density..)) 
g <- g + stat_function(fun = dnorm, arg=list(mean=expectedMean, sd=expectedStdev), size = 2, aes(colour = "darkblue"))
g <- g + scale_colour_manual(name="Legend", values=c("darkblue"), labels=c(parse(text="plain('    Normal Distribution ')*(mu==5)*plain(', ')*(sigma==5)")))
g <- g + theme(legend.position="bottom")
g <- g
print(g)