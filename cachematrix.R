## The following functions cache the inverse of a invertible matrix. 
## When the matrix for which the inverse is being computed is not changing
## the following functions will cache the the inverse rather than recomputing.

## makeCacheMatrix creates a matrix that is a list of functions used to set the value of 
## matrix, get the value of the matrix, compute the inverse of the matrix
## and get the inverse of the matrix.

makeCacheMatrix<-function(X=matrix()){
        I<-NULL
        set<-function(Y){
                X<<-Y
                I<<-NULL
        }
        get <-function() X
        setinv <- function(solve) I<<-solve
        getinv <- function() I
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve computes the inverse of the matrix from makeCacheMatrix but 
## first checks if the inverse has already been calculated. If so, it gets
## the inverse from the cahce. Otherwise, it calculates the mean of the data 
##and sets the value of the mean in the cache via the setinv function.

cacheSolve <- function(X, ...) {
        I<-X$getinv()
        if(!is.null(I)){
                message("getting cached data")
                return(I)
        }
        data<-X$get()
        I<-solve(X,...)
        X$setinv(I)
        I
        ## Return a matrix that is the inverse of 'X'
}
