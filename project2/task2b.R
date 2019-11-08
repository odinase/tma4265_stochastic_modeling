source("common.R")
require( tikzDevice );
tex <- "task2b.tex"
tikz( tex , standAlone = TRUE);

pred.interval <- function(mean, var, alpha) {
    zq <- qnorm(alpha/2, lower.tail=TRUE);
    lower <- mean - zq*sqrt(var);
    upper <- mean + zq*sqrt(var);
    return([lower, upper]);
}

indexes <- 

plot(t, S, type='l', col='red', main = 'Evolution over time for $S$, $I$ and $R$', ylim=c(0, tot), ylab='Number of individuals', xlab='$n$', cex.lab=1.5, lwd = 3.5);
lines(t, I, type='l', col='green', lwd = 3.5);
lines(t, R, type='l', col='blue', lwd = 3.5);
grid(nx=4, ny=10, col='darkgrey');
legend(x=0.7*n, y=0.75*tot, legend=c('$S$', '$I$', '$R$'),
       col=c('red', 'green', 'blue'), lty=1:2, cex=1.5, bg='white', lwd = 3.5)
dev.off();

tools::texi2dvi(tex,pdf=T);
system(cat('xdg-open', tex));