# Overview ################################################

# This R code calculates an incremental cost-effectiveness ratio from state transition events, costs and other paramaters using a Markov chain model. 
# Custom functions included in this code are
# * Trace(), which calculates a Markov trace
# * Disc(), which applies a discount rate per Markov cycle
# * and RRisk(), which applies a relative risk to a transition probability matrix.
# Parameters, calculations and outcomes are based on exercise 2.5 of 'Decision modelling for health economic evaluation' by Briggs et al.


# Parameters #############################################

# --- Transition probabilities

# Transition events for monotherapy (arm 1)
EventsArm1 <- matrix(c(1251, 350, 116 , 17 ,  # From A
                       0   , 731, 512 , 15 ,  # From B
                       0   , 0  , 1312, 437,  # From C
                       0   , 0  , 0   , 1  ), # From D
                     4,4, byrow = TRUE, dimnames = list(c("A", "B", "C", "D"), c("2A", "2B", "2C", "2D")))
TPArm1 <- EventsArm1/rowSums(EventsArm1) # Transition probabilities for arm 1


# --- Costs

# Costs per health state
Costs <- matrix(c(1701, 1774, 6948, 0,  # Direct medical costs
                  1055, 1278, 2059, 0), # Community care costs
                2,4, byrow = TRUE, dimnames = list(c("Direct medical", "Community"),c("A","B","C","D")))

# Drug costs and other parameters
NCycles <- 20      # Number of model cycles
CAZT    <- 2278    # Zidovudine drug cost
CLam    <- 2086.50 # Lamivudine drug cost
RR      <- 0.509   # treatment effect (RR) of arm 2
CDR     <- 0.06    # Annual discount rate - costs
ODR     <- 0       # Annual discount rate - benefits


# Markov model ############################################

# --- Functions

# The Trace() function applies transition probabilities (Trans) to the baseline population per health state (P0) and consecutive cycle populations for the total number of model cycles (NCycles). 
# It accepts both a single matrix and a time-dependent list of matrices.
Trace <- function(Trans, NCycles, P0 = c(1,0,0,0)){ 
  # Header - Check that input is in the expected format
  if      (length(dim(Trans)) == 3) {TP <- function(n) Trans[,,n]} 
  else if (length(dim(Trans)) == 2) {TP <- function(n) Trans}
  else    stop("Trans must be either a matrix or list of matrices.")
  # Body
  Trace <- matrix(nrow = NCycles, ncol = ncol(TP(1)))
  Trace[1,] = colSums(P0 * TP(1))
  for(n in 2:NCycles){Trace[n,] <- colSums(Trace[n - 1,] * TP(n))}
  # Footer - Check that output is in the expected format
  if(!all(round(rowSums(Trace),10) == 1)) stop("Markov trace does not sum to 1 for all cycles")
  Trace
}

# The Disc() function applies a discount rate (DR) to an outcome trace (Trace), such as costs or life years.
Disc <- function(Trace, DR){
  # Header - check that arguments are in the expected format
  if(!is.vector(Trace) & all(Trace >= 0))                   stop("Trace must be a positive vector")
  if(!is.numeric(DR) & length(DR) == 1 & RR >= 0 & RR <= 1) stop("DR must be a single numerical between 0 and 1")
  # Body
  Disc <- 1:length(Trace)
  for(n in 1:length(Trace)){Disc[n] <- Trace[n]/(1 + DR) ^ n}
  # Footer - Check that output is in the expected format
  if(!all(Disc <= Trace)) stop("Discounted trace is not always smaller or equal to original trace")
  Disc}

# The RRisk() function applies a relative risk (RR) to a transition probability matrix (TP).
RRisk <- function(TP, RR) {
  # Header - check that input is in the expected format
  if(!is.matrix(TP) & all(rowSums(TP) == 1) & all(TP >= 0) & ncol(TP) == nrow(TP)) stop("TP must be a positive, square matrix with rows summing to 1")
  if(!is.numeric(RR) & length(RR) == 1 & RR >= 0 & RR <= 1)                        stop("RR must be a single numerical between 0 and 1")
  #Body
  TP = TP * RR
  for (n in 1:ncol(TP)) {
    TP[n,n] = 0
    TP[n,n] = 1 - sum(TP[n,])}
  TP}


# --- Arm 1

TraceMarkovArm1   <- Trace(TPArm1,NCycles)
TraceLYsArm1      <- rowSums(TraceMarkovArm1) - TraceMarkovArm1[,4]
TraceLYsArm1Disc  <- Disc(TraceLYsArm1, ODR)
TraceCostArm1     <- rowSums(t(t(TraceMarkovArm1) * colSums(Costs))) + TraceLYsArm1 * CAZT
TraceCostArm1Disc <- Disc(TraceCostArm1, CDR)


# --- Arm 2

TPArm2 <- replicate(NCycles,RRisk(TPArm1, RR))
TPArm2[,,3:20] = TPArm1
CLam = c(rep(CLam, 2),rep(0, 18))

TraceMarkovArm2   <- Trace(TPArm2,NCycles)
TraceLYsArm2      <- rowSums(TraceMarkovArm2) - TraceMarkovArm2[,4]
TraceLYsArm2Disc  <- Disc(TraceLYsArm2, ODR)
TraceCostArm2     <- rowSums(t(t(TraceMarkovArm2) * colSums(Costs))) + TraceLYsArm2 * (CAZT + CLam)
TraceCostArm2Disc <- Disc(TraceCostArm2, CDR)


# Analysis #################################################

LYsArm1 <- sum(TraceLYsArm1Disc)
CostArm1 <- sum(TraceCostArm1Disc)

LYsArm2 <- sum(TraceLYsArm2Disc)
CostArm2 <- sum(TraceCostArm2Disc)

LYG <- LYsArm2 - LYsArm1
IncCost <- CostArm2 - CostArm1
 
ICER = IncCost / LYG

# Print results
LYsArm1 
CostArm1
LYsArm2
CostArm2
LYG
IncCost
ICER