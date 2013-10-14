
# undocumented function for creating datasets
generate_correlated_data <- function(n= 100, r_set1=.1, r_set2=.5, r_set1_set2=.3) {
    library(MASS) 
    r <- matrix(NA, ncol=6, nrow=6)
    r[1:3, 1:3] <- r_set1
    r[4:6, 4:6] <- r_set2
    r[1:3, 4:6] <-  r_set1_set2
    r[4:6, 1:3] <- r_set1_set2
    diag(r) <- 1
    colnames(r) <- c('a1', 'a2', 'a3', 'b1', 'b2', 'b3')
    x <- 1:3
    simulated_data <- mvrnorm(n=n, mu=rep(0, 6), Sigma=r)
    colnames(simulated_data) <- colnames(r)
    v <- list()
    v$set1 <-  c('a1', 'a2', 'a3')
    v$set2 <- c('b1', 'b2', 'b3')
    v$data <- data.frame(simulated_data)
    v
}

# set.seed(1234)
# twosetsim <- generate_eecorrelated_data()
# save(twosetsim, file='data/twosetsim.rda')
