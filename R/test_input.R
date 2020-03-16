

X1 <- seq(0, 10, 2)
set.seed(1241235)
X2 <- rnorm(6, 0, 1)


X <- data.frame(X1, X2)

update_X <- function(X){
  q1 <- readline("Press 1 to update X or any other key to exit: ")
  q1 <- as.integer(q1)
  
  if(q1 == 1){
    
    new.X1 <- readline(prompt = "Enter new value for X1:")
    new.X1 <- as.integer(new.X1)
    
    if(new.X1 %in% X$X1){
      print(paste("WARNING: X1 already has value:", new.X1, sep = " "))
    }
    
    new.X2 <- readline(prompt = "Enter new value for X2:")
    new.X2 <- as.numeric(new.X2)
    
    new.X <- data.frame(new.X1, new.X2)
    names(new.X) <- c("X1", "X2")
    
    X <- rbind(X, new.X)
    
    X
    
  } else {
    print("X will not be updated")
  }
  
}

# new.row <- readline(prompt = "Enter new row for X: ")


