source("common.R")
system(paste('rm *.tex', sep=''));
require( tikzDevice );

# Recalculate new conditional mean and variance
theta.cond.updated <- as.matrix(c(as.vector(theta.cond), 0.33));
y.cond.updated <- as.matrix(c(as.vector(y.cond), 0.40));
theta <- rbind(theta.grid, theta.cond.updated)

# Useful constants for lengths

l.tg <- length(theta.grid);
l.tc <- length(theta.cond.updated);
N <- l.tg + l.tc;

mu <- as.matrix(rep(E.Y, N))
sigma <- 0.5^2;
covar.mat.updated <- create.Covar.Matrix(theta, phi.M, sigma);

mu.uncond <- mu[1:l.tg];
mu.cond.on <- mu[(l.tg + 1):N];
mu.cond.updated <- create.Mu.C(mu.uncond, mu.cond.on, y.cond.updated, covar.mat.updated);

covar.mat.cond.updated <- create.Covar.Matrix.Conditional(l.tg, l.tc, covar.mat.updated);

var.updated <- diag(covar.mat.cond.updated);
# Correct for floating point errors, som diagonal elements are ~ -1e-16
var.updated[var.updated < 0] <- 0;

conf.interval <- 90; # % 
alpha <- 1 - conf.interval / 100;
range <- get.pred.interval(mu.cond.updated, var.updated, alpha);
lower <- range[,1];
upper <- range[,2];

task <- 'task2c_pred'
task.tex <- paste(task, '.tex', sep='')
task.pdf <- paste(task, '.pdf', sep='')
tikz( task.tex , standAlone = TRUE);

plot(NULL,NULL, xlim = c(0.25,0.5), ylim = c(0.2, 1.0), main = 'E[$Y(\\theta)]$ for different $\\theta$ enveloped in a $90\\%$ prediction interval',
     xlab = '$\\theta$', ylab = 'E[$Y(\\theta)$]', cex.lab = 1.5)
lines(theta.grid, mu.cond.updated, col="black", lwd=1.2*2)
lines(theta.grid,upper,lty=2,col="blue", lwd=1.2*2)
lines(theta.grid,lower,lty=2,col="green", lwd=1.2*2)
points(theta.cond.updated, y.cond.updated, col = "red", pch = 19)
legend(0.35,0.9,legend = c("$\\mu_{\\mathrm{conditioned}}$","upper prediction bound", "lower prediction bound", "Values conditioned on"),
       col = c("black","blue","green", "red"), cex = 0.8, lty = c(1,2,2,NA), lwd=2.4, pch=c(NA, NA, NA, 19))
dev.off();
tools::texi2dvi(task.tex, pdf=T);
system(paste('xdg-open', task.pdf));

threshold <- 0.30;
probs.updated <- pnorm(threshold, mean=mu.cond.updated, sd=sqrt(var.updated));

task <- 'task2c_probs'
task.tex <- paste(task, '.tex', sep='')
task.pdf <- paste(task, '.pdf', sep='')
tikz( task.tex , standAlone = TRUE);

plot(NULL,NULL, xlim = c(0.25,0.5), ylim = c(0, 0.4), main = 'P$\\left(Y(\\theta)<0.30\\right)$ for different $\\theta$',
     xlab = '$\\theta$', ylab = 'P$\\left(Y(\\theta)<0.30\\right)$', cex.lab = 1.5)
lines(theta.grid, probs.updated, col="blue", lwd=1.2*2)
legend(0.35,0.9,legend = "P$\\left(Y(\\theta)<0.30\\right)$", cex = 0.8, lty = 1, lwd=2.4, col="blue")
dev.off();
tools::texi2dvi(task.tex, pdf=T);
system(paste('xdg-open', task.pdf));

y.best <- theta[match(max(probs.updated),probs.updated)]
print("Best y: ")
print(y.best)