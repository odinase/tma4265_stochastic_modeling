source("common.R")
system(paste('rm *.tex', sep=''));
require( tikzDevice );
task <- 'task2a'
task.tex <- paste(task, '.tex', sep='')
task.pdf <- paste(task, '.pdf', sep='')
tikz( task.tex , standAlone = TRUE);

get.pred.interval <- function(mean, var, alpha) {
    zq <- qnorm(alpha/2, lower.tail=FALSE);
    lower <- mean - zq*sqrt(var);
    upper <- mean + zq*sqrt(var);
    return(cbind(lower, upper));
}

conf.interval <- 90; # % 
alpha <- 1 - conf.interval / 100;
range <- get.pred.interval(mu.cond, var, alpha);
lower <- range[,1];
upper <- range[,2];

plot(NULL,NULL, xlim = c(0.25,0.5), ylim = c(0.2, 1.0), main = 'E[$Y(\\theta)]$ for different $\\theta$ enveloped in a $90\\%$ prediction interval',
     xlab = '$\\theta$', ylab = 'E[$Y(\\theta)$]', cex.lab = 1.5)
lines(theta.grid, mu.cond, col="black", lwd=1.2*2)
lines(theta.grid,upper,lty=2,col="blue", lwd=1.2*2)
lines(theta.grid,lower,lty=2,col="green", lwd=1.2*2)
points(theta.cond,y.cond,col = "red", pch = 19)
legend(0.35,0.9,legend = c("$\\mu_{\\mathrm{conditioned}}$","upper prediction bound", "lower prediction bound", "Values conditioned on"),
       col = c("black","blue","green", "red"), cex = 0.8, lty = c(1,2,2,NA), lwd=2.4, pch=c(NA, NA, NA, 19))
dev.off();
tools::texi2dvi(task.tex, pdf=T);
system(paste('xdg-open', task.pdf));
