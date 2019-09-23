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
I.max = c(1:N);
n.max = c(1:N);

cat(sprintf("Starting simulation, simulating %i time steps per simulation and %i simulations.\n", n, N));

for (j in 1:N) {

    for (i in 2:n) {
        # Calculate number of people that gets sick
        dI = rbinom(1, Y[S, i - 1], beta(Y[I, i - 1], tot));
        # Calculate number of people that recovers
        dR = rbinom(1, Y[I, i - 1], gamma);
        # Update population
        Y[S, i] <- Y[S, i - 1] - dI;
        Y[I, i] <- Y[I, i - 1] + dI - dR;
        Y[R, i] <- Y[R, i - 1] + dR;
    }

    I.max[j] <- max(Y[I,]);
    n.max[j] <- which.max(Y[I,]);
}

cat(sprintf("Simulations complete!\n"));
cat(sprintf("Estimated max of I: %f, rounded off: %i\n", mean(I.max), round(mean(I.max))));
cat(sprintf("Estimated number of time steps before max I: %f, rounded off: %i\n", mean(n.max), round(mean(n.max))));
