## data structure
# scalar 
a <- 2.0 # numeric
b <- 4L # numeric integer
d <- "aquatic" # charecter

a # run
b # run
d # run

## vector
va <- c(1.0, 2.3, 3) # numeric vector
vb <- c( "a", "b", "c") # charecter vector
vc <- c("1", "2", "3.5")
vd <- c(1.0, "b") # don't do this, because it is error prone and not ideal for coding

va # run
vb # run
vc # run
vd # run

## matrix
va <- c(1.0, 2.3, 3) # numeric vector
vb<- c( 3, 2, 5.6) # numeric vector

ma<- cbind( va, vb)
mb <- rbind( va, vb)
ma # run
mb # run 

mc<- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
ma # run
mb # run
mc # run
