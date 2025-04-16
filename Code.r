library(readxl)
library(rstudioapi)

# Auto set working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Read the dataset using a relative path
data <- read_excel("dataset.xlsx")

# Check and print column names
print(colnames(data))

#---------------------------------------------
# Q1 (a)

# Assigning variables from the dataset
Physical_activity_frequency <- data$"Physical Activity Frequency"
Age <- data$Age
Gender <- data$Gender
Hand_dominance <- data$"Hand Dominance"
Reaction_times <- data$"Reaction Time (ms)"

# Calculate mean and standard deviation
mean_rt <- mean(Reaction_times)
sd_rt <- sd(Reaction_times)
n <- length(Reaction_times)

# Margin of error for a 95% confidence interval
alpha <- 0.05
t_value <- qt(1 - alpha/2, df = n-1)
margin_of_error <- t_value * (sd_rt / sqrt(n)) 
print(margin_of_error)

# Confidence Interval
lower_bound <- mean_rt - margin_of_error
upper_bound <- mean_rt + margin_of_error
conf_interval <- c(lower_bound, upper_bound)
print(conf_interval)


# (c)
# Calculate the sample size for a 90% confidence level
confidence_level <- 0.90
margin_of_error <- 0.05
z_value <- qnorm((1 + confidence_level) / 2)
sample_size <- (z_value * sd_rt / margin_of_error)^2
print(sample_size)

#---------------------------------------------------------

# Q2 (a)
# Load ggplot2 package
library(ggplot2)

# Plot to explore reaction time by hand dominance
ggplot(data, aes(x = Hand_dominance, y = Reaction_times)) +
    geom_boxplot(fill = "lightblue", color = "blue") +
    labs(title = "Reaction Time by Hand Dominance",
        x = "Hand Dominance",
        y = "Reaction Time (ms)")
ggsave("Q2(a)_boxplot.png")

# (b)
# Calculate sample variances
var_left <- var(Reaction_times[Hand_dominance == "Left"])
var_right <- var(Reaction_times[Hand_dominance == "Right"])

# Calculate the F-statistic
F_statistic <- var_left / var_right
print(F_statistic)

# Define degrees of freedom
df_left <- length(Reaction_times[Hand_dominance == "Left"]) - 1
df_right <- length(Reaction_times[Hand_dominance == "Right"]) - 1

# Calculate critical values
alpha <- 0.03
F_critical_lower <-qf(alpha / 2, df_left, df_right)
print(F_critical_lower)
F_critical_upper <- qf(1 - alpha / 2, df_left, df_right)
print(F_critical_upper)

# Calculate confidence interval
confidence_level <- c((F_statistic / F_critical_upper), F_statistic / F_critical_lower)
print(confidence_level)

# (c)
t_test <- t.test(Reaction_times ~ Hand_dominance, data=data)
print(t_test)

t_stat <- t_test$statistic
print(t_stat)

p_value <- t_test$p.value
print(p_value)

#---------------------------------------------------------

# Q3 (a)
# Construct a linear regression model
lm_model <- lm(Reaction_times ~ Age + Gender + Hand_dominance +
                Physical_activity_frequency, data = data)
print(summary(lm_model))

# (b)
# Extract coefficient for Age variable
coef_age <- coef(lm_model)["Age"]
# Calculate change in reaction time for an additional 10 years in age
change_in_reaction_time <- coef_age * 10
print(change_in_reaction_time)

# (c)
# Perform ANOVA test
anova_result <- anova(lm_model)
print(anova_result) 

# (d)
# Plot diagnostic plots
par(mfrow = c(2,2))
plot(lm_model)