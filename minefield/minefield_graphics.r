guess_exponential_dependance <- function() {
  d <- read.csv("most_dense_steps.csv")
  
  # least squares fit
  lsf <- function(par) {
    s1 <- par[1]
    s2 <- par[2]
    
    a = (s2 * exp(-s1 * d$pvec))
    b = d$steps
    sum((a - b) ^ 2)
  }
  
  ores <- optim(par=c(0, 0), fn = lsf, gr = NULL, method=c("BFGS"), 
                lower = -Inf, upper = Inf, control = list(), hessian = T)
  
  s1opt <- ores$par[1]
  s2opt <- ores$par[2]
  
  # 4.408363 83.024338 for 9x9 grid
  print ("Params are:")
  print (ores$par)
  
  # 59.85338 for 9x9 grid
  print ("SSE is :")
  print(ores$value)
  
  exp_curve <- sapply(d$pvec, function(x) { s2opt * exp(-s1opt * x) })
  
  par(mfrow=c(1,1))
  plot(d$pvec, d$steps, col='green', xlab='probabilities', ylab='steps till destruction')
  lines(d$pvec, exp_curve, col='red', 
        xlab='probabilities', ylab='steps till destruction', main=ores$par)
}

# Distribution of number of steps for differents probabilities
# Lot of them look like Gaussian
plot_hists <- function(am) 
{
  par(mar   = rep(2, 4))
  par(mfrow = c(3,3))
  
  pvec = seq(0.1, 0.9, by = 0.1)
  
  for (i in seq(along=pvec)) 
  {
    p <- pvec[i]
    s <- simulate_many(am, p, 500)
    hist(s, col = sample(colours(), 1), 
         main   = paste("P:", p," M: ", mean(s)))
  }
}

describe_graph <- function(am) {
  nverts <- dim(am)[1]
  nedges <- sum(as.vector(am)) # counts with self loops and directed (i->j and j->i are both counted)
  nedges2 <- (nedges - nverts) / 2
  
  print (sprintf("Graph is (V, E) = (%d, %d)", nverts, nedges))
  print (sprintf("Undirected graph has %d edges", nedges2))
  
  tneighs <- sapply(1:81, function(v) { length(neighbours(am, v)) })
  cts     <- hist(tneighs, plot = F)$counts
  cts     <- cts[-(which(cts == 0))]
  
  print ("Number of neighbours distribution")
  print (c(4, 6, 9))
  print (cts)
}
