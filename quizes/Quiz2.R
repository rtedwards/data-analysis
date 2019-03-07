## Data Analysis
## Quiz 2
## 

## Question 1
Cor(X1, X3) = 0.18


## Question 2
# X1 and X3

## Question 3
# Model 2 ---> 510.90 is the smallest value

## Question 4
# Simpsons paradox

## Question 5
# Yes there is an interaction term because the slopes are not parallel

## Question 6
X <- 52
intercept <- 84.5
GroupB <- 0 # Group A
Y <- intercept + 0.556*X + -17.9*GroupB
Y

## Question 7
X <- 47
intercept <- 79.7
GroupB <- 1 # Group B
X.GroupB <- 0.594*X - 54.4*GroupB
Y <- intercept - 54.4*GroupB + 0.196*X + 0.594 *X*GroupB
Y2 <- (intercept - 54.4*GroupB) + (0.196 + 0.594*GroupB)*X 
Y
Y2

## Question 8
\bar{x}_1 - \bar{x}_2


## Question 9
sample %>%
  rep_sample_n(size = 100, replace = TRUE, reps = 40)


## Question 10
x.bar <- 14.77
std.err <- 2.98

lower <- x.bar - 1.96*std.err
upper <- x.bar + 1.96*std.err

## Question 11
p_A <- 0.46
p_B <- 0.39

calculate(stat = "diff in props", order = c("A", "B"))

## Question 12
se <- (16.78 - 12.44)/2
se

mu <- 12.44 + se
mu

## Question 13
(0.028, 0.363) # check this answer


## Question 14
It is highly likely that, on average, the response of members of Group B is between 16.2 and 19.5 units lower than members of Group A when X is taken into account.


## Question 15
# lm(score ~ age + gender, data = teaching)

teaching <- evals %>%
  select(score, age, gender)

reg <- lm(score ~ age + gender, data = evals)

coeff=coefficients(reg) 
eq = paste0("y = ", round(coeff[2],1), "*x + ", round(coeff[1],1)) # Equation of the line 

# Plot
ggplot(data = teaching, aes(x = age, y = score, color = gender)) + 
  geom_point() +
  geom_abline(intercept = coeff[1], slope = coeff[2], color="red") +
  geom_abline(intercept = coeff[1] + coeff[3], slope = coeff[2], color="cyan") 
  ggtitle(eq)

ggplot(data = teaching, aes(x = age, y = score, color = gender)) + 
  geom_boxplot()







