theta = c(0, .125, .250, .375, .500, .625, .750, .875, 1)
prior_theta = c(.001, .001, .950, .008, .008, .008, .008, .008, .008)
prior_prob = sum(prior_theta[theta > 0.5])

n = 5
y = 3

likelihood_data = dbinom(y, n, theta)
posterior_theta = (likelihood_data * prior_theta) / sum(likelihood_data * prior_theta)
names(posterior_theta) = theta
round(posterior_theta, 2)
plot(theta, posterior_theta,col="green", type="b", xlab = expression(theta), ylab = "probability", ylim=range(c(0,1.3)))
par(new=TRUE)
plot(theta, prior_theta,col="blue", type="b", xlab = expression(theta), ylab = "probability", ylim=range(c(0,1.3)))
legend(0.3, 1.2, legend=c(expression(paste("posterior distribution for p(", theta, "| x = 3)")), expression(paste("prior distribution for p(", theta, ")"))),
       col=c("green", "blue"), lty=1:2, cex=0.8)

#prob 4
library(RColorBrewer)
df <- read.csv("basketball.csv", header = T)
theta = seq(0, 1, by=0.05)
a = 1; b = 1;
prior_theta = dbeta(theta, a, b)
color = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
nsample = nrow(df)
colors = sample(color, nsample)
probs = c(0.05, 0.25, 0.5, 0.75, 0.95)
S = 100000
data_summary = matrix(nrow =nsample, ncol = 6)
plot(1,type='n', xlab = expression(theta), ylab = "probability",xlim = range(c(0, 1.0)), ylim=range(c(0, 1.2)))
prob_greater_than_overall = c()
for (idx in seq(1, nsample))
{
  y = df$cmake[idx]
  n = df$cattempts[idx]
  A = y + a;
  B = n - y + b;
  theta_hat = A / (A + B)
  theta_star = rbeta(S, A, B)
  posterior_theta = dbeta(theta, A, B)
  posterior_theta = posterior_theta/sum(posterior_theta)
  lines(theta, posterior_theta, col=colors[idx])
  qtl = quantile(theta_star, probs= probs)
  mn = mean(theta_star)
  data_summary[idx, ] = c(mn, qtl)
  theta_cdf = ecdf(theta_star)
  prob_greater_than_overall = c(prob_greater_than_overall, theta_cdf(1.) - theta_cdf(df$overall_proportion[idx]))
}
prob_greater_than_overall
dt = as.data.frame(sapply(data_summary, round, 2), as.character(df$players))
colnames(dt) = c("Mean", "5% percentile", "25% percentile", "50% percentile", "75% percentile", "95% percentile")

legend(0., 1.2, legend=c(as.character(df$players)),
       col=colors, lty=1:1, cex=0.8)


prior_expectation = sum(theta * (prior_theta / sum(prior_theta)))
posterior_expectation = sum(theta * posterior_theta)

