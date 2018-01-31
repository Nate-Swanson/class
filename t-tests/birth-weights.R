# Motivaing question:
# Is there an observable difference in baby weight for smokers / non-smokers?

# Install/library the `openintro package
# install.packages('openintro')
library(openintro)
library(dplyr)

# You should now be able to `View` the `births` dataset
by.smoker <- births %>% 
  group_by(smoke) %>% 
  summarize(mean.weight = mean(weight))

# What are the mean birth weights for smokers/non-smokers
smokers <- births %>% filter(smoke == "smoker") %>% select(weight)
non.smokers <- births %>% filter(smoke == "nonsmoker") %>% select(weight)

# Store smokers/non-smokers in separate variables for easier access
smokers <- births %>% filter(smoke == "smoker") %>% select(weight)
non.smokers <- births %>% filter(smoke == "nonsmoker") %>% select(weight)

# Make an overlapping histogram of the birth weights of smokers/non-smokers
hist(non.smokers$weight, col=rgb(0,1,0,0.3))
hist(smokers$weight, col=rgb(0,0,1,0.3), add=T)

# Pre-t-test conceptaul question: is this data paired?

# Implement a t-test to assess the significance of this difference
t.test(smokers, non.smokers)


# Let's calculate the t value and confidence intervals manually

diff.means <- mean(smokers$weight) - mean(non.smokers$weight)
se <- sqrt(
  (sd(smokers$weight)^2/length(smokers$weight)) +
    (sd(non.smokers$weight)^2/length(non.smokers$weight))
)

t.score <- diff.means/se
t.score

# Compute CIs using t threshold

t <- 2.009
ci.lower <- diff.means - t * se
ci.upper <- diff.means + t * se

# Implement a t-test to confirm our results




