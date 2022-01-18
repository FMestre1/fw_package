# FWebs (v. 0.1.0)

Beta version of package to download, process and derive metrics from food web datasets. 

DISCLAIMER: This is a very early release. The code will be subject to improvements. A few errors might occur when running these functions. Any suggestions are welcomed!

In the blog Geekcologist I provided an example of the use of some of these functions: https://geekcologist.wordpress.com/2020/02/06/function-to-download-biotic-interaction-datasets/

# 1. Install the package  
```{r}
library(devtools) 
install_github("FMestre1/fw_package")
library(FWebs)
```

# 2. Create food web list (from the mangal database as an example)
```{r}
mg1 <- create.fw.list(db="mg", ref=TRUE, spatial = TRUE)
```

# 3. Which are  adjacency matrices (0 and 1 matrices)?
```{r}
is.adjacency.matrix(mg1)
```

## 3.1.Need to convert to adjacency (not needed here)?
```{r}
mg2 <- convert2adjacency(mg1)
is.adjacency.matrix(mg2)
```

# 4. Which are square matrices (same number of columns and rows)?
```{r}
is.sq.matrix(mg1) 
```

## 4.1. Need to convert to square matrix (not needed here)?
```{r}
mg3 <- rec2square(mg2)
is.sq.matrix(mg3)
```

# 5. Deriving Food Web metrics
```{r}
metrics <- fw.metrics(mg1)

metrics
```

# 6. Plot the degree distribution of all food webs in the dataset
```{r}
dd.fw(mg1, log=TRUE, cumulative=TRUE)
```

