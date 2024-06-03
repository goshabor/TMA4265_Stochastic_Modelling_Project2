#Here you can find the code for project 2.
#Used libraries:
library(latex2exp) #to be able to write in LaTeX in the plots

#Task 1b)
lambda_rate <- 5 # patients per hour
mu_rate <- 6 # patients per hour (1/μ = 10 minutes)
simulation_time <- 50 * 24 # 50 days in hours
num_simulations <- 30 # Number of simulations for CI

simulate_UCC <- function(lambda_rate, mu_rate, simulation_time) {
  time <- 0
  num_in_system <- 0
  times <- c(time)
  states <- c(num_in_system)
  
  while (time < simulation_time) {
    if (num_in_system == 0) {
      time_to_next_event <- rexp(1, lambda_rate)
      num_in_system <- num_in_system + 1
    } else {
      time_to_next_event <- rexp(1, lambda_rate + mu_rate)
      if (runif(1) < lambda_rate / (lambda_rate + mu_rate)) {
        num_in_system <- num_in_system + 1
      } else {
        num_in_system <- num_in_system - 1
      }
    }
    time <- time + time_to_next_event
    times <- c(times, time)
    states <- c(states, num_in_system)
  }
  list(times = times, states = states)
}

#simulating and estimating the time
simulation <- simulate_UCC(lambda_rate, mu_rate, simulation_time)
mean_num_patients <- mean(simulation$states)
estimated_time <- mean_num_patients / lambda_rate

#computing the confidence interval
estimated_times <- numeric(num_simulations)
for (i in 1:num_simulations) {
  sim <- simulate_UCC(lambda_rate, mu_rate, simulation_time)
  estimated_times[i] <- mean(sim$states) / lambda_rate
}
ci <- quantile(estimated_times, c(0.025, 0.975))

#calculating the results
print(paste("Estimated time: ", estimated_time))
print(paste("95% CI: ", ci[1], "-", ci[2]))

simulation_time <- 12
plot_data <- simulate_UCC(lambda_rate, mu_rate, simulation_time)
times <- plot_data$times
states <- plot_data$states

#creating an empty plot
plot(0, type="n", xlim=c(0, max(times)), ylim=c(0, max(states)), xlab="Time (hours)", 
     ylab="Number of Patients in UCC", main="Number of Patients in UCC over Time (First 12 Hours)")

#adding horizontal lines for each state
for(i in 2:length(times)) {
  # Draw a horizontal line from the previous time to the current time
  if(states[i-1] > 0) {
    segments(x0 = times[i-1], y0 = states[i-1], x1 = times[i], y1 = states[i-1])
  }
}

#task 1f)
lambda_rate <- 5
mu_rate <- 6

calculate_WU <- function(p, lambda_rate, mu_rate) {
  return(1 / (mu_rate - p * lambda_rate))
}

calculate_WN <- function(p, lambda_rate, mu_rate) {
  return(mu_rate / ((mu_rate - lambda_rate) * (mu_rate - p * lambda_rate)))
}

#creating a sequence of p-values
p_values <- seq(0, 1, by = 0.01)

plot(calculate_WN(p_values, lambda_rate, mu_rate)~p_values, type="l", ylim=c(0, max(states)), xlab="p", 
     ylab="time (h)", main=expression(W[U] ~ "and" ~ W[N] ~ "as functions of" ~ p), col="blue")
lines(calculate_WU(p_values, lambda_rate, mu_rate)~p_values, col="red")
legend("topright", legend=c("WN (Normal Patients)", "WU (Urgent Patients)"), 
       col=c("blue", "red"), lty=1)

#Problem 1g)

lambda_rate <- 5      # patients per hour
mu_rate <- 6          # patients per hour (1/μ = 10 minutes)
p_urgent <- 0.80      # probability of a patient being urgent
simulation_time <- 50 * 24 # 50 days in hours

simulate_UCC <- function(lambda_rate, mu_rate, p_urgent, simulation_time) {
  time <- 0
  num_urgent <- 0
  num_normal <- 0
  times <- c(time)
  urgent_states <- c(num_urgent)
  normal_states <- c(num_normal)
  
  while (time < simulation_time) {
    is_urgent <- runif(1) < p_urgent
    if (num_urgent == 0 && num_normal == 0) {
      time_to_next_event <- rexp(1, lambda_rate)
      if (is_urgent) {
        num_urgent <- num_urgent + 1
      } else {
        num_normal <- num_normal + 1
      }
    } else {
      rate_sum <- mu_rate + lambda_rate
      time_to_next_event <- rexp(1, rate_sum)
      if (runif(1) < lambda_rate / rate_sum) {
        if (is_urgent) {
          num_urgent <- num_urgent + 1
        } else {
          num_normal <- num_normal + 1
        }
      } else {
        if (num_urgent > 0) {
          num_urgent <- num_urgent - 1
        } else if (num_normal > 0) {
          num_normal <- num_normal - 1
        }
      }
    }
    time <- time + time_to_next_event
    times <- c(times, time)
    urgent_states <- c(urgent_states, num_urgent)
    normal_states <- c(normal_states, num_normal)
  }
  list(times = times, urgent_states = urgent_states, normal_states = normal_states)
}

#simulating for 50 days
simulation_result <- simulate_UCC(lambda_rate, mu_rate, p_urgent, simulation_time)

estimate_expected_times <- function(simulation_result) {
  total_time_urgent <- sum(simulation_result$urgent_states)
  total_time_normal <- sum(simulation_result$normal_states)
  
  total_arrivals_urgent <- length(which(simulation_result$urgent_states > 0))
  total_arrivals_normal <- length(which(simulation_result$normal_states > 0))
  
  expected_time_urgent <- total_time_urgent / total_arrivals_urgent
  expected_time_normal <- total_time_normal / total_arrivals_normal
  
  return(c(urgent = expected_time_urgent, normal = expected_time_normal))
}

#computing the confidence interval
urgent_times <- numeric(num_simulations)
normal_times <- numeric(num_simulations)

for (i in 1:num_simulations) {
  sim_result <- simulate_UCC(lambda_rate, mu_rate, p_urgent, simulation_time)
  expected_times <- estimate_expected_times(sim_result)
  urgent_times[i] <- expected_times['urgent']
  normal_times[i] <- expected_times['normal']
}

ci_urgent <- quantile(urgent_times, c(0.025, 0.975))
ci_normal <- quantile(normal_times, c(0.025, 0.975))

#calculating the results
print(paste("95% CI for Urgent Patients: ", ci_urgent[1], "-", ci_urgent[2]))
print(paste("95% CI for Normal Patients: ", ci_normal[1], "-", ci_normal[2]))

simulation_time <- 12 # 12 hours
simulation_short <- simulate_UCC(lambda_rate, mu_rate, p_urgent, simulation_time)

times <- simulation_short$times
urgent_states <- simulation_short$urgent_states
normal_states <- simulation_short$normal_states
total_states <- urgent_states + normal_states

#creating an empty plot
plot(0, type="n", xlim=c(0, max(times)), ylim=c(0, 14), 
     xlab="Time (hours)", ylab="Number of Patients", 
     main="Patients in UCC over Time (First 12 Hours)")

#adding horizontal lines for urgent and total patients
for(i in 2:length(times)) {
  if(urgent_states[i-1] > 0) {
    segments(x0 = times[i-1], y0 = urgent_states[i-1], x1 = times[i], y1 = urgent_states[i-1], col="red")
  }
  if(total_states[i-1] > 0) {
    segments(x0 = times[i-1], y0 = total_states[i-1], x1 = times[i], y1 = total_states[i-1], col="blue")
  }
}

#adding a legend
legend("topright", legend=c("Total Patients", "Urgent Patients"), col=c("blue", "red"), lty=1)

#Task 2a)
#Function to calculate the covariance for a given theta1 and theta2 using the given 
#formula for Matern type correlation
covariance <- function(theta_a, theta_b){
  cov_matrix <- matrix(0, nrow = length(theta_a), length(theta_b), byrow = TRUE)
  for(i in 1:length(theta_a)){
    for(j in 1:length(theta_b)){
      correlation = (1+15*abs(theta_a[i]-theta_b[j]))*exp(-15*abs(theta_a[i]-theta_b[j]))
      cov_matrix[i,j] = correlation*0.25
    }
  }
  return(cov_matrix) 
}

#given constants
n <- 51 #number of segments
thetaA <- seq(from = 0.25, to = 0.5, length.out = n) #sequence with delta_theta=0.05
mean_vectorA <- rep(0.5, n) #array with the mean for theta of length n=51

thetaB <- c(0.3, 0.35, 0.39, 0.41, 0.45) #measured theta
YB <- c(0.5, 0.32, 0.4, 0.35, 0.6) #measured Y(theta)
mean_vectorB <- rep(0.5, length(YB)) #array with the mean for theta of length 5

#calculating the differen covariances
cov_A <- covariance(thetaA, thetaA)
cov_AB <- covariance(thetaA, thetaB)
cov_BA <- covariance(thetaB, thetaA)
cov_B <- covariance(thetaB, thetaB)

#using the formula for the conditional mean and variance
E_conditional <- mean_vectorA + cov_AB %*% solve(cov_B) %*% (YB - mean_vectorB)
Var_conditional <- cov_A - cov_AB %*% solve(cov_B) %*% cov_BA

#calculating the 90%-confidence interval (CI)
CI_lower <- rep(0, n)
CI_upper <- rep(0, n)

for(i in 1:n){
    CI_lower[i] = E_conditional[i] - 1.645*sqrt(Var_conditional[i,i])
    CI_upper[i] = E_conditional[i] + 1.645*sqrt(Var_conditional[i,i]) 
}

#plotting the conditional mean with its confidence interval
plot(thetaA, E_conditional, type = "l", col = "blue", ylim = range(c(CI_lower, CI_upper)),
     xlab = "Theta", ylab = "Conditional Mean and 90% CI", main = "Conditional Mean and Confidence Intervals")
polygon(c(thetaA, rev(thetaA)), c(CI_lower, rev(CI_upper)), col = "deepskyblue", border = NA)
lines(thetaA, E_conditional, col ="blue")

#plotting the measurements provided
for(i in 1:length(thetaB)){
  points(x = thetaB[i], y = YB[i], col = "red", pch = 19, cex = 1) 
}

legend("topright", 
       legend = c("Conditional Mean", "90% confidence interval", "Observations"),
       col = c("blue", "deepskyblue", "red"), 
       lty = c(1, 2, NA),
       pch = c(NA, NA, 19),
       cex=0.8)

#Task 2b)
P <- numeric(n)

#calculating the conditional probabilities
for(i in 1:n){
  Z <- (0.3-E_conditional[i])/sqrt(Var_conditional[i,i])
  P[i] <- pnorm(Z)
}

plot(thetaA, P, type = "l", col = "blue", ylim = range(c(0,max(P))),
     xlab = "Theta", ylab = TeX("$P(Y(\\theta)<0.3 | Y_B)$"), 
     main = TeX("Conditional Probability for $Y<0.3$ given the 5 evalutation points"))

#Oppgave 2c)
#adding the new findings to our arrays with measurements and updating the mean vector
new_thetaB <- c(0.3, 0.35, 0.33, 0.39, 0.41, 0.45)
new_YB <- c(0.5, 0.32, 0.4, 0.4, 0.35, 0.6)
new_mean_vectorB <- rep(0.5, length(new_YB))

#calculating the new covariances
new_cov_A <- covariance(thetaA, thetaA)
new_cov_AB <- covariance(thetaA, new_thetaB)
new_cov_BA <- covariance(new_thetaB, thetaA)
new_cov_B <- covariance(new_thetaB, new_thetaB)

#calcualting new conditional mean and variance
new_E_conditional <- mean_vectorA + new_cov_AB %*% solve(new_cov_B) %*% (new_YB - new_mean_vectorB)
new_Var_conditional <- new_cov_A - new_cov_AB %*% solve(new_cov_B) %*% new_cov_BA

#calculating the new 90%-confidence interval
new_CI_lower <- rep(0, n)
new_CI_upper <- rep(0, n)

for(i in 1:n){
  print(new_Var_conditional[i,i])
  new_CI_lower[i] = new_E_conditional[i] - 1.645*sqrt(abs(new_Var_conditional[i,i]))
  new_CI_upper[i] = new_E_conditional[i] + 1.645*sqrt(abs(new_Var_conditional[i,i])) 
}

#plotting
plot(thetaA, new_E_conditional, type = "l", col = "blue", ylim = range(c(new_CI_lower, new_CI_upper)),
     xlab = "Theta", ylab = "Conditional Mean and 90% CI", main = "Conditional Mean and Confidence Intervals")
polygon(c(thetaA, rev(thetaA)), c(new_CI_lower, rev(new_CI_upper)), col = "deepskyblue", border = NA)
lines(thetaA, new_E_conditional, col ="blue")

#plotting the measurements
for(i in 1:length(new_thetaB)){
  points(x = new_thetaB[i], y = new_YB[i], col = "red", pch = 19, cex = 1)
}

legend("topright", 
       legend = c("Conditional Mean", "90% confidence interval", "Observations"),
       col = c("blue", "deepskyblue", "red"), 
       lty = c(1, 2, NA), 
       pch = c(NA, NA, 19),
       cex=0.75)

#calculating the new conditional probability
new_P <- numeric(n)
for(i in 1:n){
  Z <- (0.3-new_E_conditional[i])/sqrt(abs(new_Var_conditional[i,i]))
  new_P[i] <- pnorm(Z)
}

plot(thetaA, new_P, type = "l", col = "blue", ylim = range(c(0,max(new_P))),
     xlab = "Theta", ylab = TeX("$P(Y(\\theta)<0.3 | Y_B)$"), 
     main = TeX("Conditional Probability for $Y<0.3$ given the 6 evalutation points"))