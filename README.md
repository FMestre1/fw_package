# FWebs (v. 0.1.0)

Beta version of package to download, process and derive metrics from food web datasets. 

#DISCLAIMER 
This is a very early release. The code will be subject to improvements. A few errors might occur when running these functions. Any suggestions are welcomed!

#EXAMPLE USE (07 OCT. 2020)

In the blog Geekcologist I provided an example of the use of some of these functions: https://geekcologist.wordpress.com/2020/02/06/function-to-download-biotic-interaction-datasets/

# 1. Load required packages
library(RCurl)
library(XML)
library(plyr)
library(stringr)
library(NCmisc)
library(sf)
library(igraph)
#install.packages("remotes")
#remotes::install_github("mangal-wg/rmangal")
library(rmangal)
library(randomcoloR)
library(ggplot2)

# 2. Create
mg1 <- create.fw.list(db="mg", ref=TRUE, spatial = TRUE)

# 3. Which matrices are adjacency (0 and 1 matrices)?
is.adjacency.matrix(mg1)

# 3.1.Need to convert to adjacency (not needed here)?
#mg2 <- convert2adjacency(mg1)
#is.adjacency.matrix(mg2)

# 4. Whih are square matrices (same number of columns and rows)?
is.sq.matrix(mg1) 

# 4.1. Need to convert to square matrix (not needed here)?
#mg3 <- rec2square(mg2)
#is.sqmatrix(mg3) 

# 5. Plot degree distribution
dd.fw(mg1, log=TRUE, cumulative=TRUE)



