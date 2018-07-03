library('rbi')
library(ggplot2)

model_file_name <- "LorenzGenerate.bi"

Lorenz <- bi_model(model_file_name)

T <- 10.0
nObs <- 100
init_parameters <- list(X = 1, Y = 1, Z = 1)

## path0 <- ggplot() +
##     theme(legend.position="bottom") +
##     ggtitle("Lorenz") +
##     theme(plot.title = element_text(hjust = 0.5)) +
##     xlab("Time") +
##     ylab("Value")


## set.seed(42)

## for (i in c("red", "blue")) {
##     init_parameters <- list(X = 1 + rnorm(1,0.0,0.01),
##                             Y = 1 + rnorm(1,0.0,0.01),
##                             Z = 1 + rnorm(1,0.0,0.01))

##     synthetic_dataset <- bi_generate_dataset(end_time=T, model=Lorenz,
##                                              init=init_parameters,
##                                              noutputs = nObs)

##     synthetic_data <- bi_read(synthetic_dataset)
##     synthetic_df <- as.data.frame(synthetic_data)

##     path0 <- path0 +
##         geom_line(data = synthetic_df, aes(x = X.time, y = X.value), color = i)
## }

ggplot(synthetic_df, aes(X.time)) +
    geom_path(aes(y = X.value, colour="X.value")) +
    geom_path(aes(y = X_obs.value, colour="X_obs.value")) +
    theme(legend.position="bottom") +
    ggtitle("Lorenz") +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlab("Time") +
    ylab("Value")

set.seed(42)

init_parameters <- list(X = 1 + rnorm(1,0.0,0.01),
                        Y = 1 + rnorm(1,0.0,0.01),
                        Z = 1 + rnorm(1,0.0,0.01))

synthetic_dataset <- bi_generate_dataset(end_time=T, model=Lorenz,
                                         init=init_parameters,
                                         noutputs = nObs)

synthetic_data <- bi_read(synthetic_dataset)
synthetic_df <- as.data.frame(synthetic_data)

init_parameters1 <- list(X = 1 + rnorm(1,0.0,0.01),
                         Y = 1 + rnorm(1,0.0,0.01),
                         Z = 1 + rnorm(1,0.0,0.01))

synthetic_dataset1 <- bi_generate_dataset(end_time=T, model=Lorenz,
                                          init=init_parameters1,
                                          noutputs = nObs)

synthetic_data1 <- bi_read(synthetic_dataset1)
synthetic_df1 <- as.data.frame(synthetic_data1)

model_file_name_state <- "LorenzState.bi"

LorenzState <- bi_model(model_file_name_state)

bi_state <- libbi(model=LorenzState)
bi_state

bi_state <- filter(bi_state, nparticles = 8196, nthreads = 1, end_time = T, obs = synthetic_dataset, init = init_parameters, ess_rel = 1, sample_obs = TRUE)

bi_file_summary(bi_state$output_file_name)
bi_state
summary(bi_state)

bi_state1 <- filter(bi_state, nparticles = 8196, nthreads = 1, end_time = T, obs = synthetic_dataset1, init = init_parameters1, ess_rel = 1, sample_obs = TRUE)

bi_file_summary(bi_state1$output_file_name)
bi_state1
summary(bi_state1)

output <- bi_read(bi_state)
logw <- xtabs(value ~ time + np, data = output$logweight, addNA = TRUE)
X <- output$X$value
Y <- output$Y$value
Z <- output$Z$value
A <- output$ln_alpha$value

output1 <- bi_read(bi_state1)
logw1 <- xtabs(value ~ time + np, data = output1$logweight, addNA = TRUE)
X1 <- output1$X$value
Y1 <- output1$Y$value
Z1 <- output1$Z$value
A1 <- output1$ln_alpha$value

log2normw <- function(lw){
  w <- exp(lw - max(lw))
  return(w / sum(w))
}

w = t(apply(X=logw, MARGIN=1, FUN=log2normw))
Xmeans = apply(X = X*w, MARGIN=1, FUN=sum)
Ymeans = apply(X = X*w, MARGIN=1, FUN=sum)
Zmeans = apply(X = Z*w, MARGIN=1, FUN=sum)
Ameans = apply(X = A*w, MARGIN=1, FUN=sum)

w1 = t(apply(X=logw1, MARGIN=1, FUN=log2normw))
X1means = apply(X = X1*w1, MARGIN=1, FUN=sum)
Y1means = apply(X = X1*w1, MARGIN=1, FUN=sum)
Z1means = apply(X = Z1*w1, MARGIN=1, FUN=sum)
A1means = apply(X = A1*w1, MARGIN=1, FUN=sum)

# Finally retrieve the original values used to generate the data
synthetic_data <- bi_read(synthetic_dataset)
X_original <- synthetic_data$X$value
Y_original <- synthetic_data$Y$value
Z_original <- synthetic_data$Z$value

# And now plot the estimated states along with the original "unknown" states
# taken from the synthetic dataset

synthetic_df <- as.data.frame(synthetic_data)
synthetic_df$Xmeans <- Xmeans
synthetic_df$Ymeans <- Ymeans
synthetic_df$Zmeans <- Zmeans
synthetic_df$Ameans <- Ameans
synthetic_df$X1means <- X1means
synthetic_df$Y1means <- Y1means
synthetic_df$Z1means <- Z1means
synthetic_df$A1means <- A1means


ggplot(synthetic_df, aes(X.time)) +
    geom_path(aes(y = X.value, colour="X.value")) +
    geom_path(aes(y = Xmeans, colour="Xmeans")) +
    geom_path(aes(y = X_obs.value, colour="X_obs.value")) +
    theme(legend.position="bottom") +
    ggtitle("Lorenz") +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlab("Time") +
    ylab("Value")

ggplot(synthetic_df1, aes(X.time)) +
    geom_path(aes(y = X.value, colour="X.value")) +
    geom_path(aes(y = X1means, colour="X1means")) +
    geom_path(aes(y = X_obs.value, colour="X_obs.value")) +
    theme(legend.position="bottom") +
    ggtitle("Lorenz1") +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlab("Time") +
    ylab("Value")

ggplot(synthetic_df, aes(X.time)) +
    ## geom_path(aes(y = X.value, colour="X.value")) +
    geom_path(aes(y = exp(Ameans), colour="Ameans")) +
    geom_path(aes(y = exp(A1means), colour="A1means")) +
    ## geom_path(aes(y = X_obs.value, colour="X_obs.value")) +
    theme(legend.position="bottom") +
    ggtitle("Lorenz") +
    theme(plot.title = element_text(hjust = 0.5)) +
    ylim(0.0, max(exp(Ameans), exp(A1means))) +
    xlab("Time") +
    ylab("Value")
