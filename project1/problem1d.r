library(ggplot2)

beta <- 0.05;
gamma <- 0.20;
N <- 1000000;
T.s <- c(1:N);
T.i <- c(1:N);
p <- 0;

cat(sprintf("Starting simulation, simulating %i times\n\n--------------------\n\n", N));

for (i in 1:N) {
    # if (!(i %% 100)) {
    #     cat(sprintf("Running simulation %i...\n", i));
    # }

    t.s <- 1;
    t.i <- 1;

    p = runif(1);

    while (p > beta) {
        t.s <- t.s + 1;
        p <- runif(1);
    }
    
    p = runif(1);

    while (p > gamma) {
        t.i <- t.i + 1;
        p <- runif(1);
    }
    
    T.s[i] <- t.s;
    T.i[i] <- t.i;
}

cat(sprintf("\n----------------------\nSimulations complete!\n\n"));
cat(sprintf("Average time in state S: %f\n", mean(T.s)));
cat(sprintf("Average time in state I: %f\n", mean(T.i)));
cat(sprintf("Standard deviation of time in state S: %f\n", sd(T.s)));
cat(sprintf("Standard deviation of time in state I: %f\n", sd(T.i)));

t <- seq(from=0, to=360, by=0.1);
y <- exp(t);
dev.new();
ggplot(data.frame(T.s), aes(x=T.s)) + geom_histogram(aes(y=..density..), binwidth=1);
lines(t, y, type='l', col='blue', lwd = 3.5);
dev.off();