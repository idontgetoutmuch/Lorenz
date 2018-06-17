### This demo shows how to perform filtering on a simple
### synthetic dataset using libbi.
model_file_name <- "/Users/dom/Lorenz96/LorenzGenerate.bi"

# assign model variable
Lorenz <- bi_model(model_file_name)
# look at the model
Lorenz

T <- 0.01
nObs <- 100
init_parameters <- list(X = 1, Y = 1, Z = 1)

# First let's generate a dataset from the model
synthetic_dataset <- bi_generate_dataset(end_time=T, model=Lorenz,
                                         init=init_parameters,
                                         noutputs = nObs)

synthetic_data <- bi_read(synthetic_dataset)
synthetic_df <- as.data.frame(synthetic_data)

ggplot(synthetic_df, aes(x=synthetic_df$X.value, y=synthetic_df$Y.value)) +
    geom_path() +
    theme(legend.position="bottom") +
    ggtitle("Lorenz X Vs. Y") +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlab("X") +
    ylab("Y")

set.seed(42)
l <- length(synthetic_data$X$value)
synthetic_data$X$value <- rnorm(n=l, mean = synthetic_data$X$value, sd = 0.1)
synthetic_data$Y$value <- rnorm(n=l, mean = synthetic_data$Y$value, sd = 0.1)
synthetic_data$Z$value <- rnorm(n=l, mean = synthetic_data$Z$value, sd = 0.1)

model_file_name_infer <- "/Users/dom/Lorenz96/LorenzInfer.bi"

# assign model variable
LorenzInfer <- bi_model(model_file_name_infer)
# look at the model
LorenzInfer
# Settings
bi_object <- libbi(model=LorenzInfer)
bi_object

# Once happy with the settings, launch bi.
bi_object <- filter(bi_object, nparticles = 8192, nthreads = 1, end_time = T, noutputs = nObs, obs = synthetic_dataset, init = init_parameters)

# It can be a good idea to look at the result file
bi_file_summary(bi_object$output_file_name)
bi_object
summary(bi_object)

## read mu variable
bi_read(bi_object, vars = "mu")

# Let's have a look at the filtering means
# First, get the particles
output <- bi_read(bi_object)
logw <- xtabs(value ~ time + np, data = output$logweight)
X <- output$X$value
Y <- output$Y$value
Z <- output$Z$value
A <- output$alpha$value

# Then compute the filtering means
log2normw <- function(lw){
  w <- exp(lw - max(lw))
  return(w / sum(w))
}

w = t(apply(X=logw, MARGIN=1, FUN=log2normw))
Xmeans = apply(X = X*w, MARGIN=1, FUN=sum)
Ymeans = apply(X = X*w, MARGIN=1, FUN=sum)
Zmeans = apply(X = Z*w, MARGIN=1, FUN=sum)
Ameans = apply(X = A*w, MARGIN=1, FUN=sum)

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

ggplot(synthetic_df, aes(X.time)) +
    geom_path(aes(y = X.value, colour="X.value")) +
    geom_path(aes(y = Xmeans, colour="Xmeans")) +
    theme(legend.position="bottom") +
    ggtitle("Lorenz") +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlab("Time") +
    ylab("Value")
