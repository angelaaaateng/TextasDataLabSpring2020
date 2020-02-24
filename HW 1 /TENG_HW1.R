# Angela V Teng (Amber)
# Course: Text as Data
# Due Date: March 4, 2020

# Leslie's TIPS from Lab 1:
# you should always (always!) annotate your code
# use version control (GitHub)
# DEBUGGING: rubber duck it
# Google is your friend. Type your question and add "R" to the end of it.
# knitr is useful for problem sets that require showing your code
# for bigger projects: use a dependency manager (packrat) for projects (see below)

#-----------------------------
# 0 SETTING UP
#-----------------------------

# 0.1 Clearing environment
rm(list = ls())

# 0.2 Working directory

getwd()  # returns current working directory
# working_dir <- "~./NYU_GoogleDrive/NYU ACADEMICS/Spring 2020/Text as Data/TAD HOMEWORK/HW 1"
# setwd(working_dir)
# getting errors here :( 
# setwd(choose.dir())


# 0.3 Installing and loading some useful packages from CRAN
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("xtable")
# install.packages("devtools")
# 
# install.packages("stargazer")

# Installing packages from GitHub
#devtools::install_github("quanteda/quanteda.corpora")
# update.packages() # update packages (careful, some codes may not run -> use packrat)

# Installing packages from GitHub
#devtools::install_github("quanteda/quanteda.corpora")
# update.packages() # update packages (careful, some codes may not run -> use packrat)

library(dplyr)
library(ggplot2)
library(xtable)

# Loading multiple packages
libraries <- c("foreign", "stargazer")
lapply(libraries, require, character.only=TRUE)

# 0.5 Managing dependencies

# If you want to ensure that your code will run with specific package dependencies, I recommend using a dependency manager for R called packrat so that you can specify which version of libraries that you use.
# Find out about setting up packrat here: https://rstudio.github.io/packrat/walkthrough.html

# For R packages that are actively being developed, functions and function names can change and this can break your code if you update the package but not your code! (More about this next week.)

# 0.6 Setting up Quanteda from Lab 2

set.seed(100)

# 0.7 Installing quanteda -----------------------

# Install the latest stable version of quanteda from CRAN
# install.packages("quanteda") # run this if you don't have quanteda already installed

library(quanteda)

# 1.3 Devtools and the quanteda corpus -----------------------

# Install the package "devtools" which is used to install packages directly from Github
# install.packages("devtools")
library("devtools")

# Use devtools to install some sample data
# devtools::install_github("quanteda/quanteda.corpora")

# Load it into our environment
library(quanteda.corpora)
# library(quanteda)
# install.packages("readtext")
library(readtext)
# install.packages("spacyr")
library(spacyr)
# Read about the data available: https://github.com/quanteda/quanteda.corpora

### Note: Quanteda is still under development so it is changing! New features are being added but sometimes functions or function parameters are deprecated or renamed.

# 1.4 Versions of quanteda -----------------------

# to check version
packageVersion("quanteda")

# note that I'm using package 1.5.2

# How would you get an older version of quanteda? (For example, if you accidentally installed the dev version from GitHub but you want to go back to the last stable release, or you want a legacy version to support old code.)

# - Check the CRAN archive
# use the install_version function, e.g.:
# devtools::install_version("quanteda", version = "0.99.12", repos = "http://cran.us.r-project.org")

# If you want the latest dev version of quanteda, it's on GitHub, but we will use the latest version from CRAN for stability/sanity reasons
# devtools::install_github("quanteda/quanteda") 

#-----------------------------
# QUESTION 1
# First we'll use the data from the U.S. inaugural addresses available in quanteda. Let's first
# look at the inaugural addresses given by Richard Nixon in 1969 and 1973.
#-----------------------------

# Load the presidential inaugural address texts (https://rdrr.io/cran/quanteda/man/data_corpus_inaugural.html)
inaug <- data_corpus_inaugural
data_corpus_inaugural

summary(data_corpus_inaugural)
head(docvars(data_corpus_inaugural), 10)
# do basic data exploration
head(inaug)
# a corpus consists ofs: (1) documents: text + doc level data (2) corpus metadata (3) extras (settings)
head(docvars(inaug))  # document-level variables
metacorpus(inaug)  # corpus-level variables

# ndoc identifies the number of documents in a corpus
ndocs <- ndoc(inaug)
ndocs

# summary of the corpus (provides some summary statistics on the text combined with the metadata)
corpusinfo <- summary(inaug, n = ndocs)  # note n default is 100
head(corpusinfo)
# does tokens >= types always hold?

# quick visualization
token_plot <- ggplot(data = corpusinfo, aes(x = Year, y = Tokens, group = 1)) + geom_line() + geom_point() + theme_bw()
token_plot

# subset corpus to include only Nixon ---------------------
summary(corpus_subset(inaug, President == "Nixon"))
nixon_inaug <- corpus_subset(inaug, President == "Nixon")
head(docvars(nixon_inaug))

# (a) Calculate the TTR of each of these speeches and report your findings.

# We can apply Heap's Law to better understand: 
# Token-type relationship in corpus
# How might pre-processing affect this relationship? 
# Think about reducing the dimensionality of the problem.

#     M = kT^b

# M = vocab size (num of types)
# T = number of tokens

# k, b are constants
# 30 <= k <= 100
# 0.4 <= b <= 0.6

# 2.1 Example using data from the corpus of inaugural speeches
tokens <- tokens(nixon_inaug, remove_punct = TRUE) 
tokens
num_tokens <- sum(lengths(tokens))
num_tokens

inaug_nixon_dfm <- dfm(nixon_inaug)
inaug_nixon_dfm

M <- nfeat(inaug_nixon_dfm)  # number of features = number of types
M

# Let's check using parameter values from MRS Ch. 5 for a corpus with more than 100,000 tokens

# k <- 54
k <- 15.832 # based on calculations that assume M = 992, T = 3926, b = 0.5
# b <- 0.49
b <- 0.5

k * (num_tokens)^b

M

# this is close! :-) good job! 

# Let's think about why (what types of texts are these?)

# New parameters

k <- 16
b <- 0.455

k * (num_tokens)^b

M

# gets further 

# You can solve mathematically for k and b or fit a model to find k and b -- relationship between log(collection size) and log(vocab size) is linear

# this is the ttr for BOTH speeches. now, let's calculate the TTR of each speech. 

# keep only the text of the the 2018 SOTU
(nixon_inaug)[1]
texts(nixon_inaug)[1]
nixon_1969_text <- texts(nixon_inaug)[1]
nixon_1969_text
summary(corpus_subset(inaug, President == "Nixon", Year == "1969"))
nixon_1973_text <- texts(nixon_inaug)[2]
nixon_1973_text
# summary(corpus_subset(inaug, President == "Nixon",  Year == 1969))


# NIXON 1969

tokens_1969 <- tokens(nixon_1969_text, remove_punct = TRUE) 
tokens_1969

num_tokens_1969 <- sum(lengths(tokens_1969))
num_tokens_1969
# ntoken(num_tokens_1969)

inaug_nixon_dfm_1969 <- dfm(nixon_1969_text)
inaug_nixon_dfm_1969

M <- nfeat(inaug_nixon_dfm_1969)  # number of features = number of types
M
# M = 714 for nixon 1969

# k <- 54
k <- 15.492 # based on calculations that assume M = 714, T = 2124, b = 0.5
# b <- 0.49
b <- 0.5

k * (num_tokens_1969)^b

M

# QUESTION FOR LESLIE: Are we supposed to compute the TTR per corpus, or for the two corpora? 
# What M should we use? Should we use the same M for both, as in the total number of features for BOTH corpora?


# (b) Create a document feature matrix of the two speeches, with no pre-processing other than
# to remove the punctuation{be sure to check the options on \dfm" in R as appropriate.
#  Calculate the cosine similarity between the two documents with quanteda. Report your
#  ndings.







#-----------------------------
# QUESTION 2
#-----------------------------




#-----------------------------
# QUESTION 3
#-----------------------------



#-----------------------------
# QUESTION 4
#-----------------------------




#-----------------------------
# QUESTION 5
#-----------------------------





#-----------------------------
# QUESTION 6
#-----------------------------




#-----------------------------
# QUESTION 7
#-----------------------------







#-----------------------------
# QUESTION 8
#-----------------------------



#-----------------------------
# QUESTION 9
#-----------------------------



#-----------------------------
# * FINISHING UP
#-----------------------------

# *.1 Save workspace after running it -- all objects, functions, etc  (e.g. if you have run something computationally intensive and want to save the object for later use)
# Similar to pickle() in Python

save.image("workspace_hw1.RData")

# *.2 Pick up where you left off (but note that the workspace does not include packages. You need packrat for that)

rm(list = ls())

load("workspace_hw1.RData")
