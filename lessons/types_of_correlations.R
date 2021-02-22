### Load Libraries
library(tidyverse)

### Generate correlation data frames (from https://stats.stackexchange.com/questions/15011/generate-a-random-variable-with-a-defined-correlation-to-an-existing-variables/15035#15035)
getBiCop <- function(n, rho, mar.fun=rnorm, x = NULL, label = NULL, ...) {
  if (!is.null(x)) {X1 <- x} else {X1 <- mar.fun(n, ...)}
  if (!is.null(x) & length(x) != n) warning("Variable x does not have the same length as n!")

  C <- matrix(rho, nrow = 2, ncol = 2)
  diag(C) <- 1

  C <- chol(C)

  X2 <- mar.fun(n)
  X <- cbind(X1,X2)

  # induce correlation (does not change X1)
  df <- X %*% C

  ## if desired: check results
  #all.equal(X1,X[,1])
  #cor(X)

  # produce a clean x, y data frame
  df <- df %>% as.data.frame() %>% rename(x = V1, y = V2)
  if (!is.null(label)) { df <- df %>% mutate(label = label) }

  return(df)
}


### Generate Data Frames
p_nocor <- getBiCop(100, 0, x=seq(1, 100, 1), label="No Correlation") # No correlation
p_lowcor <- getBiCop(100, 0.05, x=seq(1, 100, 1), label="Low Correlation") # Low correlation
p_highcor <- getBiCop(100, 0.2, x=seq(1, 100, 1), label="High Correlation") # High correlation
p_perfcor <- getBiCop(100, 0.9999999, x=seq(1, 100, 1), label="Perfect Correlation") # Perfect correlation
p_positive <- getBiCop(100, 0.9999999, x=seq(1, 100, 1), label="Positive Correlation") # Positive correlation
p_negative <- getBiCop(100, -0.9999999, x=seq(1, 100, 1), label="Negative Correlation") # Negative correlation
p_curved <- p_positive %>% mutate(y=x*x*x, x=seq(1, 100, 1), label="Curvilinear Correlation") # Curvilinear correlation
#p_partial <- bind_rows(getBiCop(50, 0.9999999, x=seq(1, 50, 1)), getBiCop(25, 0.8, x=seq(51, 75, 1)), getBiCop(25, 0.4, x=seq(76, 100, 1))) %>% mutate(label="Partial Correlation")
p_partial <- bind_rows(p_perfcor %>% filter(x <= 25) %>% mutate(y=(y*(runif(25, 0.95, 1.05)))), p_perfcor %>% filter(x > 25 & x <= 75) %>% mutate(y=(y*(runif(50, 0.9, 1.3)))), p_perfcor %>% filter(x > 75) %>% mutate(y=(y*(runif(25, 0.8, 1.6))))) %>% mutate(label="Partial Correlation") # Partial correlation


### Combine data frames
plot <- mget(ls(pattern="^p_")) %>%
  bind_rows()

plot %>%
  filter(label %in% c("No Correlation", "Low Correlation", "High Correlation", "Perfect Correlation")) %>%
  mutate(label=factor(label, levels=c("No Correlation", "Low Correlation", "High Correlation", "Perfect Correlation", "Positive Correlation", "Negative Correlation", "Curvilinear Correlation", "Partial Correlation"))) %>%
  ggplot(aes(x=x, y=y, group=label)) +
  facet_wrap(~label, scales="free", ncol=4, ) +
  geom_point() +
  labs(x=element_blank(), y=element_blank()) +
  theme_bw() +
  theme(axis.text = element_blank())
ggsave("/tmp/strength-of-correlation.png", width=9, height=2)

plot %>%
  filter(label %in% c("Positive Correlation", "Negative Correlation", "Curvilinear Correlation", "Partial Correlation")) %>%
  mutate(label=factor(label, levels=c("No Correlation", "Low Correlation", "High Correlation", "Perfect Correlation", "Positive Correlation", "Negative Correlation", "Curvilinear Correlation", "Partial Correlation"))) %>%
  ggplot(aes(x=x, y=y, group=label)) +
  facet_wrap(~label, scales="free", ncol=4, ) +
  geom_point() +
  labs(x=element_blank(), y=element_blank()) +
  theme_bw() +
  theme(axis.text = element_blank())
ggsave("/tmp/type-of-correlation.png", width=9, height=2)
