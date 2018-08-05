#############################################################################################
### Authors: Dominik Maruszczak & Fabian Pribahsnik                                       ###
### Purpose: Bachelor Thesis                                                              ###
#############################################################################################


#############################################################################################
############################### Variable description ########################################
#############################################################################################
# DT.header --------------------> Header for the raw-data except last column (is spam or not)
# DT.rawData -------------------> Raw data, read directly from the data-repository. (http://archive.ics.uci.edu/ml/datasets/Spambase?ref=datanews.io)
# DT.train ---------------------> 90% of the raw-data used for training the different models. 
# DT.test ----------------------> 10% of the raw-data used for testing the accuracy of the different models. 
# 
#
#
#
#############################################################################################



############## load all required packages ##############
library(rstudioapi) # for setting the working directory to the path of the script
library(data.table) 
library(caret)
library(ggplot2)
library(corrplot)
library(viridis) # new colors
library(gridExtra) # arrange more ggplot-objects on one page
library(MASS) # needed for LDA and QDA

############## Read and prepare the raw-data ##############
set.seed(115) # set the seed to get similar results for the random-parts
# set the working directory to the path of the script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# read the header of the data
DT.header <- fread("spambase/header.txt", header = FALSE)
# read the raw-data
DT.rawData <- fread("spambase/spambase.data", sep = ",", header = FALSE)
# create header for the data
names(DT.rawData) <- c(as.vector(DT.header$V1), "is_spam_flag")
# define the flag for spam as factor
DT.rawData$is_spam_flag <- factor(DT.rawData$is_spam_flag, labels = c("No", "Yes"))

# Find randomly selected rownumbers of the raw data so that we get 90% of data for training
train_rows <- createDataPartition(DT.rawData$is_spam_flag, p = 0.9, list = FALSE, times = 100)
# create list of data.tables for training purposes
DT.train <- apply(train_rows, 2, function(x){DT.rawData[x,]})
# create list of data.tables for testing purposes
DT.test <- apply(train_rows, 2, function(x){DT.rawData[-x,]})

####################################  Data - analysis ####################################

# calculate all column means, based on is_spam_flag
DT.col_means <- DT.rawData[, lapply(.SD, mean), by = is_spam_flag]
# bring the values into a "column-based" format for plotting
DT.col_means <- melt(DT.col_means, id.vars = c("is_spam_flag"),value.name = 'mean')

############################################################################################################
##################### mean - plots #########################################################################
############################################################################################################

# plot all means for the variables starting with "word_*"
ggplot(data = DT.col_means[DT.col_means$variable %like% 'word',] ,
       aes(x = variable, y = mean, color = is_spam_flag)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), # Beschriftung x-Achse um 90° drehen
        axis.title.x = element_blank()) + # delete x-labels
  scale_color_manual(values=c("green", "red")) +
  labs(color = "Spam") # Legende beschriften

# plot all means for the variables starting with "char_*"
ggplot(data = DT.col_means[DT.col_means$variable %like% 'char',] ,
       aes(x = variable, y = mean, color = is_spam_flag)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), # Beschriftung x-Achse um 90° drehen
        axis.title.x = element_blank()) + # delete x-labels
  scale_color_manual(values=c("green", "red")) +
  labs(color = "Spam") # Legende beschriften

# plot all means for the variables starting with "cap_*"
ggplot(data = DT.col_means[DT.col_means$variable %like% 'cap',] ,
       aes(x = variable, y = mean, color = is_spam_flag)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), # Beschriftung x-Achse um 90° drehen
        axis.title.x = element_blank()) + # delete x-labels
  scale_color_manual(values=c("green", "red")) +
  labs(color = "Spam") # Legende beschriften

############################################################################################################
##################### correlation - plots ##################################################################
############################################################################################################

# calculate the correlation between all columns except the Spam-flag
corrplot(cor(DT.rawData[,.SD, .SDcols = DT.header$V1]), method = "ellipse")
# "zoom-in" and just take columns with "higher" correlation
corrplot(cor(DT.rawData[,.SD, .SDcols = c(which(DT.header$V1 == 'word_freq_money') : which(DT.header$V1 == 'word_freq_cs'),
                                          which(DT.header$V1 == 'char_freq_#') : which(DT.header$V1 == 'capital_run_length_total'))]),
         method = "ellipse")

############################################################################################################
##################### QQ-plots #############################################################################
############################################################################################################
# create a function that takes a data.frame / data.table and gives back a list of qqnorm-ggplot elements
create_qqplot <- function(x, ...) {
  # define a list with the number of element equal to the column-number of the passed data.frame
  gg_object <- vector(mode = "list", length = dim(x)[2])
  for(i in 1:dim(x)[2])
  {
    # we need some quotes around the columnnames because some columns have special characters
    # and so we have to supply the column-name in the form: "`char_freq_(`" 
    quoted_name <- paste('`', colnames(x)[i], '`', sep = "") # needed for special charac
    gg_object[[i]] <- ggplot(x, aes_string(sample = quoted_name)) + 
                        stat_qq() +
                        theme(axis.title.x = element_blank(),
                              axis.title.y = element_blank())
                        #ggtitle(colnames(x)[i]) + # take column-name as title 
                        #theme(plot.title = element_text(margin = margin(t = 10, b = -20))) # move title into plotting area
  }
  return(gg_object)
}

# create a list of qq-plots for all input-data except the spam flag
qq_plots <- create_qqplot(DT.rawData[,!c("is_spam_flag")])
# draw all qq-plots on one page
do.call("grid.arrange", c(qq_plots, ncol = 4))

############################################################################################################
##################### transform the data ###################################################################
############################################################################################################

# Transform the data with log if it is not a factor
DT.transformed <- lapply(DT.rawData,function(x){
  if(!is.factor(x)){
    log(x) # also possible: log(x + 0.00000001)
  } else {
    x
  }
})

# transform the result of lapply (list) to a data.table
DT.transformed <- as.data.table(DT.transformed)

# create a function that takes a data.frame / data.table and gives back a list of hist-ggplot elements
create_hist <- function(x, ...) {
  # define a list with the number of element equal to the column-number of the passed data.frame
  gg_object <- vector(mode = "list", length = dim(x)[2])
  for(i in 1:dim(x)[2])
  {
    # we need some quotes around the columnnames because some columns have special characters
    # and so we have to supply the column-name in the form: "`char_freq_(`" 
    quoted_name <- paste('`', colnames(x)[i], '`', sep = "") # needed for special charac
    gg_object[[i]] <- ggplot(x, aes_string(x = quoted_name)) + 
      geom_histogram() +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank())
    #ggtitle(colnames(x)[i]) + # take column-name as title 
    #theme(plot.title = element_text(margin = margin(t = 10, b = -20))) # move title into plotting area
  }
  return(gg_object)
}

# create a list of hist-plots for all input-data except the spam flag
hist_plots <- create_hist(DT.transformed)
# draw all qq-plots on one page
do.call("grid.arrange", c(hist_plots, ncol = 4))


#density plots for all variables
create_dens <- function(x, ...) {
  # define a list with the number of element equal to the column-number of the passed data.frame
  gg_object1 <- vector(mode = "list", length = dim(x)[2])
  for(i in 1:dim(x)[2])
  {
    # we need some quotes around the columnnames because some columns have special characters
    # and so we have to supply the column-name in the form: "`char_freq_(`" 
    quoted_name <- paste('`', colnames(x)[i], '`', sep = "") # needed for special charac
    gg_object1[[i]] <- ggplot(x, aes_string(x = quoted_name)) + 
      geom_density() +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank())
    #ggtitle(colnames(x)[i]) + # take column-name as title 
    #theme(plot.title = element_text(margin = margin(t = 10, b = -20))) # move title into plotting area
  }
  return(gg_object1)
}

# create a list of hist-plots for all input-data except the spam flag
dens_plots <- create_dens(DT.transformed)
# draw all qq-plots on one page
do.call("grid.arrange", c(dens_plots, ncol = 4))

############################################################################################################
##################### Try to apply PCA on the data ###################################################################
############################################################################################################

ir.pca <- prcomp(DT.rawData[,!c("is_spam_flag")],
                 center = TRUE,
                 scale. = TRUE) 
plot(ir.pca, type = "l")
summary(ir.pca) # with non-transformed data we need 43 variables to describe more than 90% of the variance

# Transform the data with log if it is not a factor
DT.transformed <- lapply(DT.rawData,function(x){
  if(!is.factor(x)){
    log(x + 0.00000001)
  } else {
    x
  }
})
# transform the result of lapply (list) to a data.table
DT.transformed <- as.data.table(DT.transformed)

ir.pca <- prcomp(DT.transformed[, !c("is_spam_flag")],
                 center = TRUE,
                 scale. = TRUE) 
plot(ir.pca, type = "l")
summary(ir.pca) # with transformed data we need 40 variables to describe more than 90% of the variance


################################################################
###################Analysis#############################
################################################################

## Make LDA - Analysis
source("01_LDA.R")
ergebnis_LDA <- learnLDA(DT.train, DT.test)

## Make QDA - Analysis
source("02_QDA.R")
ergebnis_QDA <- learnQDA(DT.train, DT.test)