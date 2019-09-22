require( tikzDevice );

tikz( 'problem1e.tex' , standAlone = TRUE);

beta <- function(I, tot) {
    return(0.5*I/tot);
}

S.0 <- 950;
I.0 <- 50;
R.0 <- 0;
S <- 1;
I <- 2;
R <- 3;
Y.0 <- c(S.0, I.0, R.0);
gamma <- 0.20;
tot <- 1000;
ns <- 3;
n <- 200;
N <- 1000;
Y <- matrix(c(Y.0, 1:(ns*(n - 1))), nrow = ns);
t.s <- 0;
t.i <- 0;
p <- 0;

cat(sprintf("Starting simulation, simulating %i time steps\n", n));

for (i in 2:n) {
    # Calculate number of people that gets sick
    dI = rbinom(1, Y[S, i - 1], beta(Y[I, i - 1], tot));
    # Calculate number of people that gets recovers
    dR = rbinom(1, Y[I, i - 1], gamma);
    # Update population
    Y[S, i] <- Y[S, i - 1] - dI;
    Y[I, i] <- Y[I, i - 1] + dI;
    
    Y[I, i] <- Y[I, i - 1] - dR;
    Y[R, i] <- Y[R, i - 1] + dR;
}

cat(sprintf("Simulations complete!\n"));

S <- Y[1,];
I <- Y[2,];
R <- Y[3,];
t <- c(1:n);

plot(t, S, type='l', col='red', main = 'Evolution over time for $S$, $I$ and $R$', ylim=c(0, tot), ylab='Number of individuals', xlab='$n$', cex.lab=1.5);
lines(t, I, type='l', col='green');
lines(t, R, type='l', col='blue');
grid(nx=4, ny=10, col='darkgrey');
legend(x=0.7*n, y=0.75*tot, legend=c('$S$', '$I$', '$R$'),
       col=c('red', 'green', 'blue'), lty=1:2, cex=1.5, bg='white')
dev.off();

tools::texi2dvi('problem1e.tex',pdf=T);
system('xdg-open problem1e.pdf');