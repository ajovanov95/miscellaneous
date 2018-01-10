library(igraph)

# adjacency matrix from grid of size m x m
# graph has m x m vertices
# 3x3 neighbourhood with full (including self loops) conectivity
graph_from_grid <- function (m) {
  v   <- m * m
  adj <- matrix(rep(0, v * v), nrow = v)
  
  for (i in 0 : (m - 1)) 
  {
    for (j in 0 : (m - 1)) 
    {
        vind <- 1 + j + i * m;
        
        for (row in c(i - 1, i, i + 1)) 
        {
            for (col in c(j - 1, j, j + 1)) 
            {
                if (col >= 0 & col < m && row >= 0 && row < m) 
                {
                    vneigh <- 1 + col + row * m;
                  
                    # self loops are absolutely mandatory.
                    # do not think of removing them
                    adj[vind, vneigh] <- 1;
                }
            }
        }
    }
  }
  
  graph_from_adjacency_matrix(adj, "directed")
}

most_dense_vertex <- function(g) {
  which.max(degree(g, mode='in'))
}

neighbours <- function(g, v) {
  neighborhood(g, 1, nodes=c(v))[[1]]
}

# remove all inbound and all outbound links
explode_vertex <- function(g, v) {
  am <- as_adjacency_matrix(g, type="both", sparse = F)
  am[v, ]  <- rep(0, vcount(g)) # REMOVE outbound links
  am[, v]  <- rep(0, vcount(g)) # REMOVE inbound links
  graph_from_adjacency_matrix(am, mode="directed")
}

find_edge_index <- function(g, u, v) {
  el <- as_edgelist(g)
  
  winner <- -1
  
  N <- ecount(g) - 1
  
  if (N == 0)
  {
    winner <- 1  
  }
  else 
  {
    for (i in 1 : N) 
    {
      if (el[i, 1] == u && el[i, 2] == v)
        winner <- i
    }
  }
  
  winner
}

# Union conflict resolution is fcfs
# Maximumum dg in-degree is 1 for every vertex
# Edge weight is explosion propagation probability
generate_desctruction_graph <- function(mg, v0) {
  dg <- make_empty_graph(n = vcount(mg), directed = T)
  
  queue <- c(v0)
  
  while (length(queue) > 0) 
  {
      q     <- queue[1]
      
      queue <- queue[-1]
      
      neighs <- neighbours(mg, q)
    
      for (nv in neighs) 
      {
        u <- runif(1, 0.0, 1.0)
        
        e <- find_edge_index(mg, q, nv)
        
        p <- edge_attr(mg, "weight", index=e)
        
        indeg <- degree(dg, v=c(nv), mode="in")
        
        if (indeg == 0 && u < p) 
        {
          dg <- add_edges(dg, c(q, nv))
          
          queue <- append(queue, nv)
        }
      }
  }
  
  dg
}

explosion_propagation <- function(mg, v0) 
{
  dg <- generate_desctruction_graph(mg, v0)
  
  nexp <- 1
  
  # it can only destroy 6, 10 or 16 links
  # but sometimes it destroys 2 links only?
  mg <- explode_vertex(mg, v0)
  
  for (v in 1 : vcount(mg)) 
  {
    if (v != v0 && degree(dg, v, mode="in") > 0) {
      mg <- explode_vertex(mg, v)
      nexp <- nexp + 1  
    }
  }

  mg
}

simulate <- function(g, p) 
{
    nsteps <- 0
    
    while (ecount(g) > 0) 
    {
        v0       <- most_dense_vertex(g)
      
        # explode vertex deletes this attributes
        g        <- set.edge.attribute(g, "weight", value = rep(p, ecount(g)))     
        
        g        <- explosion_propagation(g, v0)
        
        nsteps   <- nsteps + 1
    }
  
    nsteps
}

# SLOW SLOW SLOW
simulate_many <- function(g, p, n) {
  sapply (1:n, function (x) { simulate (g, p) })
}

expected_number_of_steps <- function(g, p) {
  # simulate_many is slow, take care with this parameter
  niter <- 50
  
  mean(simulate_many(g, p, niter))
}

# generate link between p and expected steps
generate_link_p_expected_steps <- function(g, ofname = NULL) {
  svl   <- 20
  pstep <- 1.0 / svl 
  pvec  <- seq(0.0, 1.0, pstep)
  
  steps <- rep(0, svl)
  
  for (i in seq(along=pvec)) 
  {
      p        <- pvec[i]
      
      steps[i] <- expected_number_of_steps(g, p)
      
      print (sprintf("Processing step [%d]", i))
  }
  
  if (is.null(ofname) == F)
    write.csv(file=ofname, cbind(pvec, steps), row.names = F)
  
  cbind(pvec, steps)
}

# d = cbind(pvec, steps)
get_params_for_exp_curve <- function(d) {
  # least squares fit
  lsf <- function(par) {
    s1 <- par[1]
    s2 <- par[2]
    
    a = (s2 * exp(-s1 * d[,1]))
    b = d[,2]
    sum((a - b) ^ 2)
  }
  
  ores <- optim(par=c(0, 0), fn = lsf, 
                gr = NULL, method=c("BFGS"), 
                lower = -Inf, upper = Inf, 
                control = list(), hessian = T)
  
  s1opt <- ores$par[1]
  s2opt <- ores$par[2]
  
  sse   <- ores$value
  
  c(s1opt, s2opt, sse)
}

# Question 1: Is the p-nsteps curve exponential for every n
test_question1 <- function(N) {
  # 3 koloni: s1, s2, sse
  s1s  <- 1:N
  s2s  <- 1:N
  sses <- 1:N
  
  for (n in 1:N) 
  {
      print(sprintf ("Question 1 [%d] of [%d]", n, N))
    
      g <- graph_from_grid(n)
    
      d <- generate_link_p_expected_steps(g, ofname=sprintf("minefield_steps/steps-%d.csv", n))
      
      o <- get_params_for_exp_curve(d)
      
      s1s [n] <- o[1]
      s2s [n] <- o[2]
      sses[n] <- o[3]
  }
  
  r <- cbind(s1s, s2s, sses)
  colnames(r) <- c("S1", "S2", "SSE")
  
  write.csv(r, file = sprintf('question1-%d.csv', N), row.names = F)
}

# Open questions:
# 1.0. Is it always s2 * e^(-s1 * x) for quadratic minefields of any size n?
# 1.1. Is there a way to find/estimate s1 and s2 from graph characterstics?

# 3. What if we use least dense instead of most dense?
# 4. Will it be same exponential for any graph in case p = const?
# 5. What if probability of explosion propagation is not fixed?
