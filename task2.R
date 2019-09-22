##############
### Task 2 ###
##############

### Calculate probability directly
lambda = 1.5;
days = 59;
mu = lambda*days;
claims = 100;

prob = ppois(claims, mu, lower.tail = FALSE)

### Simulate realizations and estimate probability
N = 1000;         #Repeat simulation N times to get a representative estimate
numSim = 1000;    #Number of realizations
probs = 0;        #Vector of estimated probabilites for each simulation in 2a)
expectations = 0; #Vector of estimated expected values for each simulation in 2b)
variances = 0;    #Vector of estimated variances for each simulation in 2b)
beta = 10;

for (i in 1:N){
  sim = rpois(numSim, mu);
  probs[i] = sum(sim > claims)/numSim;
  expectations[i] = sim/beta;
  variances[i] = 2*sim/(beta^2);
}

upperProb = max(probs);
lowerProb = min(probs);
expectedProb = mean(probs);

upperExpectation = max(expectations);
lowerExpectation = min(expectations);
expectedExpectation = mean(expectations);

upperVariance = max(variances);
lowerVariance = min(variances);
expectedVariance = mean(variances);

cat(sprintf("Upper probability: %f\n", upperProb))
cat(sprintf("Lower probability: %f\n", lowerProb))
cat(sprintf("Mean probability: %f\n",  expectedProb))

cat(sprintf("Upper expectation: %f\n", upperExpectation))
cat(sprintf("Lower expectation: %f\n", lowerExpectation))
cat(sprintf("Mean expectation: %f\n",  expectedExpectation))

cat(sprintf("Upper variance: %f\n", upperVariance))
cat(sprintf("Lower variance: %f\n", lowerVariance))
cat(sprintf("Mean variance: %f\n",  expectedVariance))

#HOW TO MAKE FIGURE SHOWING 10 realizations??

