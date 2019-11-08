library(Matrix)
library(mvtnorm)

corr <- function(yt1, yt2, phi.M) {
    return((1+phi.M*abs(yt1 - yt2))*exp(-phi.M*abs(yt1 - yt2)));
}

covar.matrix <- function(theta, phi.M, sigma) {
    ones <- as.matrix(rep(1.0, length(theta)));
    H <- abs(theta %*% t(ones) - ones %*% t(theta));
    return(sigma*(1 + phi.M*H)*exp(-phi.M*H));
}

mu.c <- function(mu.1, mu.2, a, covar.matrix) {
    p <- length(mu.1);
    q <- length(mu.2);
    N <- p + q;
    covar.matrix.11 <- covar.matrix[1:p, 1:p];
    covar.matrix.12 <- covar.matrix[1:p, (p+1):N];
    covar.matrix.21 <- covar.matrix[(p+1):N, 1:p];
    covar.matrix.22 <- covar.matrix[(p+1):N, (p+1):N];
    return(mu.1 + (covar.matrix.12 %*% solve(covar.matrix.22)) %*% (a - mu.2))
}

covar.matrix.c <- function(p, q, covar.matrix) {
    N <- p + q;
    covar.matrix.11 <- covar.matrix[1:p, 1:p];
    covar.matrix.12 <- covar.matrix[1:p, (p+1):N];
    covar.matrix.21 <- covar.matrix[(p+1):N, 1:p];
    covar.matrix.22 <- covar.matrix[(p+1):N, (p+1):N];
    return(covar.matrix.11 - covar.matrix.12 %*% (solve(covar.matrix.22) %*% covar.matrix.21))
}


phi.M <- 15;

# Does initial comparisons in integers to avoid float derps
theta.cond <- c(300, 350, 390, 410, 450);
theta <- seq(from=250, to=500, by=5);
theta.uncond <- setdiff(theta, theta.cond);
nt <- length(theta)

# Needs to convert to matrix to use matrix multiplication and make the numbers floats
theta.cond <- as.matrix(theta.cond) / 1000;
theta.uncond <- as.matrix(theta.uncond) / 1000;
theta <- rbind(theta.uncond, theta.cond);

y <- as.matrix(c(0.50, 0.32, 0.40, 0.35, 0.60));
ny <- length(y);
mu.uncond <- as.matrix(rep(0.5, nt));
sigma <- 0.5^2;
c.m <- covar.matrix(theta, phi.M, sigma);
mu.1 <- mu.uncond[1:(nt-ny)];
mu.2 <- mu.uncond[(nt-ny+1):nt];
p <- length(mu.1);
q <- length(mu.2);
mu.cond <- mu.c(mu.1, mu.2, y, c.m);
c.m.cond <- covar.matrix.c(p, q, c.m);