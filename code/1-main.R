# SETUP ########################################################################

# Library ----------------------------------------------------------------------

library(rstudioapi) # setup
library(tidyverse)    # data processing
library(magrittr)
library(reshape2)
library(data.table)
library(png)
library(bnlearn)      # bn
library(parallel)     # parallel computing
library(future)
library(Rgraphviz)    # plotting
library(egg)
library(cowplot)
library(ggplotify) 


# Set Working Directory --------------------------------------------------------

# set current directory as working directory
dir_path <- dirname(getActiveDocumentContext()$path)
setwd(dir_path)


# Functions --------------------------------------------------------------------

# list of functions
functions <- list.files(path = 'functions/', pattern = "[.]R$", 
                        full.names=TRUE, recursive = TRUE)
# load
for (i in (1:length(functions))) {source(functions[i])}


