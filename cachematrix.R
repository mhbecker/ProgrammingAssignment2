## Put comments here that give an overall description of what your
## functions do

#This function creates a special "matrix" that is really a list containing a function to 
# 1 - set the value of the matrix
# 2 - get the value of the matrix
# 3 - set the value of the inverse of the matrix
# 4 - get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<-y
                m <<-NULL
        }
        get<-function() x
        setSolve <- function(solve) m <<- solve
        getSolve <- function() m
        list(set = set, get = get, 
             setSolve = setSolve,
             getSolve = getSolve)
        
}


## The following function solves (calculates the inverse) of the special "matrix"
## created with the above function. However, it first checks to see if the 
## inverse has already been calculated. If so, it 'get's the solution from the
## cache and skips the computation. Otherwise, it calculates the solution of 
## the data and sets the value of the solution in the cache via the 'setSolve' function. 


cacheSolve <- function(x, ...) {
        m<- x$getSolve()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data<-x$get()
        m<-solve(data,...)
        x$setSolve(m)
        m
        ## Return a matrix that is the inverse of 'x'
}

##The lines below (commented out) allow for this function to be tested easily
#m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
#aMatrix<-makeCacheMatrix(x=m1)
#aMatrix$get()
#aMatrix$getSolve()
#cacheSolve(aMatrix)
