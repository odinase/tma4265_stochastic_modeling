##############
### Task 2 ###
##############

### Calculate probability directly
lambda = 1.5;
days = 59;
mu = lambda*days;
claims = 100;

prob = ppois(claims, mu, lower.tail = FALSE)

### Simulate realizations and estimate probability/expectation/variance in 2a/2b
N = 1000;                  #Repeat simulation N times to get a representative estimate
numSim = 1000;             #Number of realizations
probs = numeric(N);        #Vector of estimated probabilites for each simulation in 2a)
expectations = numeric(N); #Vector of estimated expected values for each simulation in 2b)
variances = numeric(N);    #Vector of estimated variances for each simulation in 2b)
beta = 10;

for (i in 1:N){
  sim = rpois(numSim, mu);
  probs[i] = sum(sim > claims)/numSim;
  expectations[i] = sim/beta;     #Formula for expectation after law of total expectation
  variances[i] = 2*sim/(beta^2);  #Formula for variance after law of total variance
}

cat(sprintf("Probability std: %f\n", sd(probs)));
cat(sprintf("Probability mean: %f\n", mean(probs)));

cat(sprintf("Expectation std: %f\n", sd(expectations)));
cat(sprintf("Expectation mean: %f\n", mean(expectations)));

cat(sprintf("Variance std: %f\n", sd(variances)));
cat(sprintf("Variance mean: %f\n", mean(variances)));

### Figure with realizations of X(t)
realizations = 10;
nP = matrix(, nrow = realizations, ncol = 60);

# Generate data
T = 0:59;
for (i in 1:realizations) {
  for (t in T) {
    nP[i,(t+1)] = rpois(1, t*lambda);
  }
}

# Plot data
plot(NULL, NULL, xlim = c(0, 59), ylim = c(0, max(nP)), xlab = "Time", ylab = "Events", main = "10 realizations of X(t)")
for (i in 1:realizations) {
  for (t in T) {
    lines(T[(t+1):((t+1)+1)], rep(nP[i,t+1],2), col = 31+i)

  }
}
