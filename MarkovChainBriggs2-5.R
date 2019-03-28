# Parameters #############################################

# --- Transition probabilities (TP)
TransitionEvents <- matrix(c(1251, 350, 116 , 17 ,
                             0   , 731, 512 , 15 ,
                             0   , 0  , 1312, 437,
                             0   , 0  , 0   , 1  ),
                           4,4, byrow = TRUE, dimnames = list(c("From A", "From B", "From C", "From D"), c("To A", "To B", "To C", "To D")))

TP <- TransitionEvents/rowSums(TransitionEvents)


# --- Costs (C)
C <- matrix(c(dmca = 1701, dmcb = 1774, dmcc = 6948, 0,
              ccca = 1055, cccb = 1278, cccc = 2059, 0),
            2,4, byrow = TRUE, dimnames = list(c("Direct Medical", "Community"),c("A","B","C","D")))

c
# --- Drug costs (DC)
DC <- c(CAZT = 2278, CLam = 2086.50)

# --- Other parameters (OP)
OP <- c(RR = 0.509, cDR = 0.06, ODR = 0)


# Markov model ############################################

NCycles <- 20
TraceMarkov = matrix(nrow = NCycles, ncol = 4)
TraceMarkov[1,]=colSums(c(1,0,0,0) * TP)
for(n in 2:NCycles){
  TraceMarkov[n,] <- colSums(TraceMarkov[n - 1,] * TP)
}

if(sum(round(rowSums(TraceMarkov),10) == 1) != nrow(TraceMarkov)) stop("Markov trace does not sum to 1 for all cycles")

TraceLYsHono <- rowSums(TraceMarkov) - TraceMarkov[,4]


# Analysis #################################################

LYsMono <- sum(TraceLYsMono)
# CostsMono <- sum(TraceCostsMono)
#
# LYsComb <- sum(TraceLYsComb)
# CostComb <- sum(TraceCostComb)
#
# LYG <- LYsComb - LYsMono
# IncCost <- CostComb - CostMono
# 
# ICER = IncCost / LYG

