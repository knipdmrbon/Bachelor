

#########################################################################
# Author:  Fabian Pribahsnik
# Date:    25.08.2018
# Purpose: Plot activation functions as described in the chapter
            "Multi-Layer Neural Networks"
#########################################################################
library(ggplot2)


# Sigmoid activation function
sigmoidFun <- function(x) {
   1 / (1 + exp(-x))
}

# Tanh activation function
tanhFun <- function(x){
  (exp(x) - exp(-x)) / (exp(x) + exp(-x))
}

# Rectified linear activation function
reluFun <- function(x){
  pmax(0,x)
}

# Softmax activation function
softmaxFun <- function(x){
  log(1 + exp(x))
}

plotOverview <- ggplot(data.frame(x = c(-4, 4)), aes(x = x)) +
  stat_function(fun = sigmoidFun,   n = 500, size = 0.8, aes(colour = "Sigmoid")) +
  stat_function(fun = tanhFun,      n = 500, size = 0.8, aes(colour = "tanh")) +
  stat_function(fun = reluFun,      n = 500, size = 0.8, aes(colour = "Rectified linear")) +
  stat_function(fun = softmaxFun,   n = 500, size = 0.8, aes(colour = "Softmax")) +
  theme(legend.position = "top",
        legend.title=element_blank()) +
  scale_y_continuous(name = "f(x)")

plotOverview
