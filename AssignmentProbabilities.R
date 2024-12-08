library(dplyr)

# Define total sample size 
N = 1000
p = 6

nk = N / (sqrt(p) + p)
message("Sample size in kth treatment condition: ", ceiling(nk))

n0 = nk * sqrt(p)
message("Sample size in control condition: ", ceiling(n0))

# Should sum to N
p * nk + n0

assignment_p = function(N, p){
  
  n_k = N/(sqrt(p) + p)
  n_0 = n_k * sqrt(p)
  N = n_0 + p * n_k
  
  data.frame(
    p_0 = n_0 / N,
    p_k = n_k / N
  )
}
probs = assignment_p(N, p)

tr = tribble(
  ~ Condition,  ~ Probability,
  "P(control)", probs$p_0,
  "P(predistribution)", probs$p_k,
  "P(predistribution + moral)", probs$p_k,
  "P(redistribution from)", probs$p_k,
  "P(redistribution from + moral)", probs$p_k,
  "P(redistribution to)", probs$p_k,
  "P(redistribution to + moral)", probs$p_k,
) %>%  
  mutate(across(where(is.numeric), ~ round(.,3)))

tr
sum(tr$Probability)

