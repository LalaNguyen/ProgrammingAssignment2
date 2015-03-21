## makeCacheMatrix will create list of function that can be used to manipulate
# the original Matrix with cache option and cacheSolve will convert original matrix
# into inverse and store it in cache.

## Create list of function that caches matrix
## Input : invertible matrix
## Output : List of functions including:
##              + get() # return original matrix
##              + set() # store original matrix in cache
##              + getInverseMatrix() # return inverse matrix
##              + setInverseMatrix() # store inverse matrix in cache

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        set <- function(m) {
                x <<- m
                im <<- NULL
        }
        get <- function() x
        setInverseMatrix <- function(inv_matrix) im <<- inv_matrix
        getInverseMatrix <- function() im
        list(set = set, get = get,
             setInverseMatrix = setInverseMatrix,
             getInverseMatrix = getInverseMatrix)
}


## Invert the cached matrix
## Input : makeCacheMatrix list of functions
## Output : Inverted matrix in cache (can be access via list)
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverseMatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        # perform inversion
        inv_matrix<-solve(data)
        # store the inverted matrix in cache
        x$setInverseMatrix(inv_matrix)
        inv_matrix
}

# Sample run
# > x <- matrix(1:4,2,2)
# > l <- makeCacheMatrix(x)
# > l$get()
#        [,1] [,2]
#[1,]    1    3
#[2,]    2    4

## Receiving inverted matrix from cache
# > cacheSolve(l)
#        [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

## Test if the result is correct
# > l$getInverseMatrix()%*%l$get()
#       [,1] [,2]
#[1,]    1    0
#[2,]    0    1
