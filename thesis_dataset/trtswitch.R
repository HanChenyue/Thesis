# Prof's code
# Sample Size
n = 100
# Per segment treatment effects for A and B
treateff = c(0.5, 0.0)
# error SD
sderror = 0.5
# Initial states for subjects
yinit = rnorm(n, mean=0, sd=1)
# Random allocation of first treatment
treatinit = ifelse(runif(n) < 0.5,"A","B")
# First stage outcomes
ymid = yinit + 
  ifelse(treatinit == "A",treateff[1],treateff[2]) + 
  rnorm(n, mean=0, sd=sderror)
# Switch treatments if response is negative
othertreat = ifelse(treatinit == "A","B","A")
treatmid = ifelse(ymid <0, othertreat, treatinit)
yfinal = ymid +
  ifelse(treatmid == "A",treateff[1],treateff[2]) +
  rnorm(n, mean=0, sd=sderror)
# Create a data frame
gendata = data.frame(yinit,ymid,yfinal,treatinit,treatmid)

# Sample size
n <- 10
# Per segment treatment effects for A, B, C, D, and E
treateff <- c(0.5, 0.0, 0.2, 0.3, 0.1)

# error SD
sderror <- 0.5

# Initial states for subjects
stage_0 <- rnorm(n, mean = 0, sd = 1)

# All possible treatments
treatments <- c("A", "B", "C", "D", "E")

# Random allocation of first treatment
treat_1 <- sample(treatments, n, replace = TRUE)

# Store the treatment each subject received in the first stage
received_treatments_1 <- split(treat_1, seq_along(treat_1))

# First stage outcomes
stage_1 <- stage_0 +
          ifelse(treat_1 == "A", treateff[1],
          ifelse(treat_1 == "B", treateff[2],
          ifelse(treat_1 == "C", treateff[3],
          ifelse(treat_1 == "D", treateff[4],
          ifelse(treat_1 == "E", treateff[5], NA))))) +
          rnorm(n, mean = 0, sd = sderror)

# Switch to another treatment selected randomly if first stage outcome is negative # nolint: line_length_linter.
#treat_2 <- ifelse(stage_1 < 0, sample(treatments, n, replace = TRUE), treat_1)

## Switch to another treatments select randomly if response is negative
treat_2 <- mapply(function(s1, trts) {
  if (s1 < 0) {
    # Get treatments not yet received
    not_received <- setdiff(all_treatments, trts)
    # Sample a new treatment
    new_treatment <- sample(not_received, 1)
    # Add the new treatment to the list of received treatments
    trts <- c(trts, new_treatment)
  }
  trts
}, stage_1, received_treatments)

# The last treatment received is the current treatment
current_treatment <- sapply(treat_2, tail, n = 1)