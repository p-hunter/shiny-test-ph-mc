suppressWarnings(library(tidyverse)); library(magrittr); 

options(repr.plot.width = 10, repr.plot.height = 5)

mh_target <- function(theta) {
  dt(theta, 1)
}

plot_mh <- function(df) {
  
  plot_out <- df %>%
    ggplot(aes(x = Iteration, y = `Simulated Value`, colour = Draw)) +
    geom_line() +
    labs(title = "Beep Boop Computerr ahahaha")
  plot_out
}


Draw_MH <- function(N, K) {
  X <- matrix(1, nrow = N, ncol = K)
  accepted <- rep(1, 3)
  U <- runif(N * K, -10, 10) %>% matrix(nrow = N, ncol = K)
  for (i in 2:N) {
    for(j in 1:K) {
      q <- rnorm(1, X[i - 1, j], 1)
      rejection <- mh_target(q) / mh_target(X[i - 1, j])
      a <- min(1, rejection)
      if(U[i, j] < a) {
        X[i, j] <- q
        accepted[j] <- accepted[j] + 1            
      } else {
        X[i, j] <- X[i - 1, j]
      }
    }
  }
  message("\n\nAcceptance Rate:\n\n", paste0(100 * accepted / N, "%, "), "\n\n")
  X %>%
    as.data.frame() %>%
    set_colnames(paste0("Draw ", seq_len(K))) %>%
    mutate(Iteration = 1:10000, .before = `Draw 1`) %>%
    pivot_longer(-Iteration, names_to = "Draw", values_to = "Simulated Value") 
}


test  <- Draw_MH(10000, 3)

test %>% plot_mh()