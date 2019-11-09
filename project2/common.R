library(Matrix)

create.Covar.Matrix <- function(theta, phi.M, sigma) {
    ones <- as.matrix(rep(1.0, length(theta)));
    H <- abs(theta %*% t(ones) - ones %*% t(theta));
    return(sigma*(1 + phi.M*H)*exp(-phi.M*H));
}

create.Mu.C <- function(mu.1, mu.2, a, covar.matrix) {
    p <- length(mu.1);
    q <- length(mu.2);
    N <- p + q;
    covar.matrix.11 <- covar.matrix[1:p, 1:p];
    covar.matrix.12 <- covar.matrix[1:p, (p+1):N];
    covar.matrix.21 <- covar.matrix[(p+1):N, 1:p];
    covar.matrix.22 <- covar.matrix[(p+1):N, (p+1):N];
    return(mu.1 + (covar.matrix.12 %*% solve(covar.matrix.22)) %*% (a - mu.2))
}

create.Covar.Matrix.Conditional <- function(p, q, covar.matrix) {
    N <- p + q;
    covar.matrix.11 <- covar.matrix[1:p, 1:p];
    covar.matrix.12 <- covar.matrix[1:p, (p+1):N];
    covar.matrix.21 <- covar.matrix[(p+1):N, 1:p];
    covar.matrix.22 <- covar.matrix[(p+1):N, (p+1):N];
    return(covar.matrix.11 - covar.matrix.12 %*% (solve(covar.matrix.22) %*% covar.matrix.21))
}


phi.M <- 15;
E.Y <- 0.5;
theta.cond <- as.matrix(c(0.300, 0.350, 0.390, 0.410, 0.450));
theta.grid <- as.matrix(seq(from=0.250, to=0.500, by=0.005));
theta <- rbind(theta.grid, theta.cond)

# Useful constants for lengths

l.tg <- length(theta.grid);
l.tc <- length(theta.cond);
N <- l.tg + l.tc;

mu <- as.matrix(rep(E.Y, N))
y.cond <- as.matrix(c(0.500, 0.320, 0.400, 0.350, 0.600));
sigma <- 0.5^2;
covar.mat <- create.Covar.Matrix(theta, phi.M, sigma);
mu.uncond <- mu[1:l.tg];
mu.cond.on <- mu[(l.tg + 1):N];
mu.cond <- create.Mu.C(mu.uncond, mu.cond.on, y.cond, covar.mat);
covar.mat.cond <- create.Covar.Matrix.Conditional(l.tg, l.tc, covar.mat);
