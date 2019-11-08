theta_cond = [300, 350, 390, 410, 450]';
theta = [250:5:500]';
theta_uncond = setdiff(theta, theta_cond);
theta = [theta_uncond; theta_cond] / 1000;
n = size(theta, 1);
one = ones(n, 1);
sigma = 0.5^2;
phiM = 15;
H = abs(theta * one' - one * theta');
Cov = sigma*(1 + phiM*H) .* exp(-phiM*H);

p = 46;
q = 5;
N = p + q;
CovCond = Cov(1:p,1:p) - Cov(1:p,(p+1):N) *...
                        inv(Cov((p+1):N, (p+1):N)) *...
                     Cov((p+1):N, 1:p);
disp(CovCond)

vars = diag(CovCond)
