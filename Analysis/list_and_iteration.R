# load libraries  
library(tidyverse)

# Use readRDS() and a relative path to read this data into R.
data <- readRDS("../data/fpout.RDS")

# Use appropriate functions to obtain the length of the list and then the vector of the names from the list.
length_list <- length(data)  
names_list <- ls(data)
length_list
names_list
is.vector(names_list) # check 

# The `diploscores` element does not provide any information. Remove it from the list.
data[names(data) %in% "diploscores" == FALSE]  -> data_rm  
ls(data_rm) # check

# another method 
data_rm1 <- data
data_rm1[['diploscores']] <- NULL

# 4. The `scores` element contains the output most users would want. 
# The variables in `scores` called `P0`, `P1`, `P2`, `P3`, `P4`, `P5`, and `P6` contain 
# "posterior probabilities" for each individual for values    `0`, `1`, `2`, `3`, `4`, `5`, and `6` (respectively).  
# - A quantity useful in Bayesian analysis is called the "posterior mean," which in this case is calculated as follows:
#  + posterior_mean = `(P0 * 0) + (P1 * 1) + (P2 * 2) + (P3 * 3) + (P4 * 4) + (P5 * 5) + (P6 * 6)`.
# - Calculate the posterior mean for each individual and add this as a new variable in the `scores` data frame.
data_rm %>%  
  pluck(4) %>%  # pluck function helps to retrieve objects from data 
  # pluck() is inside purrr package which is included inside tidyverse, (methods for iteration)
  mutate(posterior_mean = (P0 * 0) + (P1 * 1) + (P2 * 2) + (P3 * 3) + (P4 * 4) + (P5 * 5) + (P6 * 6)) -> scores

# Use a `map*()` function to identify the names of the variables in the `scores` dataframe that are * not* of type double
not_double <- map(scores, is.double) != TRUE 
not_double 

which(not_double, arr.ind = FALSE) %>%  
  names()

# Create a new element called `col_means` in the list that contains just the column means of all the *double* variables in the `scores` data frame.
scores %>%  
  keep(is.double) %>%  
  colMeans(na.rm = TRUE) -> data_rm$col_means

data_rm$col_means

# assign data_rm to a new variable full_data 
full_data <- data_rm

# Demonstrate three different ways to extract the col_means element from the list
# Extracted element should not be a list 
full_data$col_means # 1 way 
full_data[[7]] # 2 way 
full_data[["col_means"]] # 3 way 

# Show two ways to extract the third element of col_means
full_data[[7]][3] # 1 way 
data[["col_means"]][3] # 2 way 

# Second Section 
# For Loops
# Consider recursive sequence defined by Xn = Xn-1 + |Xn-3 - Xn-2|/4
# Write a function called calcn() that takes as input a vector x containing the first 
# three elements of this sequence and an integer n denoting the final element of the sequence to calculate.  
# - `calcn()` should return element `n`.
# - Include error checking to to ensure the inputs are of the correct length and type and n is greater than 0.

calcn <- function(x,n){
  stopifnot(length(x) ==3 & is.numeric(x))
  stopifnot(n > 0 & is.integer(n))
  
  for (i in 4:n){
    result = x[i-1] +abs(x[i-3]-x[i-2])/4
    x = c(x, result)
  }
  return (x[n]) 
}

calcn(c(11,1,130),1L)
calcn(c(11,1,130),1000L)
calcn(c(7, 3, 20), 8L)

# checking the function with another method
a <- vector(mode = "double", length = 1000)

for (i in seq_along(a)){
  if(i ==1) {
    a[[i]] <- 11
  } else if ( i ==2){
    a[[i]] <- 1 
  } else if (i == 3){
    a[[i]] <- 130
  }
  else {
    a[[i]] <- a[[i-1]]+ abs(a[[i-3]] - a[[i-2]])/4
  }
}
a[[1L]]
a[[1000L]]

b <- vector(mode = "double", length = 1000)

for (i in seq_along(b)){
  if(i ==1) {
    b[[i]] <- 7
  } else if (i ==2){
    b[[i]] <- 3
  } else if (i == 3){
    b[[i]] <- 20
  }
  else {
    b[[i]] <- b[[i-1]]+ abs(b[[i-3]] - b[[i-2]])/4
  }
}
b[[8L]]

# Lists, For-loops, and `map_*()`
# Lists are used to save simulation output -> Extract elements from lists using for loops
# Consider the t-test, used to test whether or not the mean of some observations is 0
x <- rnorm(n = 10, mean = 0, sd = 1) # simulate data from a Normal (0,1) distribution
tout <- t.test(x) # use a t-test to test if the true mean is 0
str(tout) # to see how many elements are in the list along their names and types


# 3.3.2:  - 0.5 Did not initialize the list of proper length so is inefficient
set.seed(1)
# Create empty list to store vectors
tlist <- vector(mode = "list", length = 1000)

for (i in 1:1000){
  tlist[[i]] <- t.test(rnorm(n = 10, mean = 0, sd = 2))
}

tlist %>% 
  map_dbl(~.$estimate[[1]]) %>% 
  tibble(x=.) %>% 
  ggplot(aes(x=x))+
  geom_histogram(bins = 10)+
  theme_bw()

# Use for loop to extract the p-values from each test  
pvec_f <- vector(mode = "double", length = 1000)

for (i in seq_along(pvec_f)) {
  pvec_f[i] <-tlist[[i]]$p.value
}
head(pvec_f)

# Use appropriate map function to extract p-values from each test 
map_dbl(tlist, ~.$p.value) -> pvec_m  # map_dbl outputs double vector
pvec_m %>%  
  head()

# If the null hypothesis is true (the mean of the observations is actually 0) then the p-values follow the uniform distribution 
pvec_m %>%
  as_tibble() %>% 
  ggplot()+
  geom_qq(geom = "point", position = "identity", distribution = stats::qunif, mapping = aes(sample = pvec_m))+
  geom_abline(color = "red", linetype = "dashed", size = 2)+  
  ggtitle("QQ_Plot")+
  labs(x = "Theoretical_Quantiles", y = "Sample_Quantiles")+
  theme_bw()
# p-value follow the uniform distribution, because the points are well aligned with the red dotted line. 


