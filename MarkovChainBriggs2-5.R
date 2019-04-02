# Parameters #############################################

# --- Transition probabilities
EventsMono <- matrix(c(1251, 350, 116 , 17 ,  # From A
                       0   , 731, 512 , 15 ,  # From B
                       0   , 0  , 1312, 437,  # From C
                       0   , 0  , 0   , 1  ), # From D
                     4,4, byrow = TRUE, dimnames = list(c("A", "B", "C", "D"), c("2A", "2B", "2C", "2D")))
TPMono <- EventsMono/rowSums(EventsMono)


# --- Costs (C)
Costs <- matrix(c(1701, 1774, 6948, 0,  # Direct medical costs
                  1055, 1278, 2059, 0), # Community care costs
                2,4, byrow = TRUE, dimnames = list(c("Direct medical", "Community"),c("A","B","C","D")))

# --- Drug costs and other parameters
NCycles <- 20      # Number of model cycles
CAZT    <- 2278    # Zidovudine drug cost
CLam    <- 2086.50 # Lamivudine drug cost
RR      <- 0.509   # treatment effect (RR)
CDR     <- 0.06    # Annual discount rate - costs
ODR     <- 0       # Annual discount rate - benefits


# Markov model ############################################

# Formulas
Trace <- function(Trans, NCycles, c0 = c(1,0,0,0)){ 
  if (length(dim(Trans)) == 3) {TP <- function(n) Trans[,,n]} 
  else if (length(dim(Trans)) == 2) {TP <- function(n) Trans}
  else stop("Incompatible transition matrix provided. Should be either 2 or 3 dimensional.")
  Trace <- matrix(nrow = NCycles, ncol = ncol(TP(1)))
  Trace[1,] = colSums(c0 * TP(1))
  for(n in 2:NCycles){Trace[n,] <- colSums(Trace[n - 1,] * TP(n))}
  if(sum(round(rowSums(Trace),10) == 1) != nrow(Trace)) stop("Markov trace does not sum to 1 for all cycles")
  Trace
}

Disc <- function(Trace, DR){
  Disc <- 1:length(Trace)
  for(n in 1:length(Trace)){Disc[n] <- Trace[n]/(1 + DR) ^ n}
  Disc}

TPComb <- replicate(NCycles,(matrix(c(1 - (TPMono["A","2B"] + TPMono["A", "2C"] + TPMono["A", "2D"]) * RR, TPMono["A", "2B"] * RR, TPMono["A", "2C"] * RR, TPMono["A", "2D"] * RR,
                                      0, 1 - (TPMono["B", "2C"] + TPMono["B", "2D"]) * RR, TPMono["B", "2C"] * RR, TPMono["B", "2D"] * RR,   
                                      0, 0, 1 - TPMono["C", "2D"] * RR, TPMono["C", "2D"] * RR,
                                      0, 0, 0, 1),
                                    nrow(TPMono), ncol(TPMono), byrow = TRUE)))
TPComb[,,3:20] = TPMono
CLam = c(rep(CLam, 2),rep(0, 18))

# Monotherapy
TraceMarkovMono   <- Trace(TPMono,NCycles)
TraceLYsMono      <- rowSums(TraceMarkovMono) - TraceMarkovMono[,4]
TraceLYsMonoDisc  <- Disc(TraceLYsMono, ODR)
TraceCostMono     <- rowSums(t(t(TraceMarkovMono) * colSums(Costs))) + TraceLYsMono * CAZT
TraceCostMonoDisc <- Disc(TraceCostMono, CDR)

# Combination therapy
TraceMarkovComb   <- Trace(TPComb,NCycles)
TraceLYsComb      <- rowSums(TraceMarkovComb) - TraceMarkovComb[,4]
TraceLYsCombDisc  <- Disc(TraceLYsComb, ODR)
TraceCostComb     <- rowSums(t(t(TraceMarkovComb) * colSums(Costs))) + (TraceLYsComb * (CAZT + CLam))
TraceCostCombDisc <- Disc(TraceCostComb, CDR)


# Analysis #################################################

LYsMono <- sum(TraceLYsMonoDisc)
CostMono <- sum(TraceCostMonoDisc)

LYsComb <- sum(TraceLYsCombDisc)
CostComb <- sum(TraceCostCombDisc)

LYG <- LYsComb - LYsMono
IncCost <- CostComb - CostMono
 
ICER = IncCost / LYG
