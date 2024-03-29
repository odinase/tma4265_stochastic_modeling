source("common.R")
system(paste('rm *.tex', sep=''));
require( tikzDevice );
task <- 'task2b'
task.tex <- paste(task, '.tex', sep='')
task.pdf <- paste(task, '.pdf', sep='')
tikz( task.tex , standAlone = TRUE);

threshold <- 0.30;
probs <- pnorm(threshold, mean=mu.cond, sd=sqrt(var));

par(mar=c(5,6,4,1)+.1)
plot(NULL,NULL, xlim = c(0.25,0.5), ylim = c(0, 0.4), main = 'P$\\left(Y(\\theta)<0.30\\right)$ for different $\\theta$',
     xlab = '$\\theta$', ylab = 'P$\\left(Y(\\theta)\\right)$', cex.lab = 1.5)
lines(theta.grid, probs, col="blue", lwd=1.2*2)
legend(0.35,0.9,legend = "P$\\left(Y(\\theta)<0.30\\right)$", cex = 0.8, lty = 1, lwd=2.4, col="blue")
dev.off();
tools::texi2dvi(task.tex, pdf=T);
system(paste('xdg-open', task.pdf));