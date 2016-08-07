## These functions work with matrix objects and together result in reducing the need for computing power
## if the inverse of the matrix already exists.

## setter and getter function for 2 matrices (base and inverse)

makeCacheMatrix <- function(x = matrix()) {
    mat <- NULL
    
    set <- function(y){
        x <<- y
        mat <<- NULL ## Nulls the second matrix/inverse if the new matrix has already been assigned 
    }
    get <- function(){
        x
    }
    setMatrix <- function(matrix){
        mat <<- matrix
    }
    getMatrix <- function(){
        mat
    }
    
    list(set = set, get = get, setMatrix = setMatrix, getMatrix = getMatrix)
}


## Solves for the inverse of the matrix if the inverse was not calculated in the past, otherwise
## returns the inverse which was calculated and stored before

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    mat <- x$getMatrix()
    if(!is.null(mat)){
        message("getting cache data")
        return(mat)
    }
    mat <- solve(x$get())
    x$setMatrix(mat)
    mat
    
}
