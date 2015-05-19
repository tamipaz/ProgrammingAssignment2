## The pair functions "makeCacheMatrix" and "cacheSolve" compute the inverse of a matrix
## and cache the inverse matrix result. This enable to caching the inverse of a matrix
## rather than compute it repeatedly.

## "makeCacheMatrix" function creates a special list that stores 
## 4 functions: "set"", ""get,"" ""set_inverse" and "get_inverse"
## The input of "makeCacheMatrix" is a matrix "x"

makeCacheMatrix <- function(x = matrix()) {
        
        im <- NULL                      ## define the inverse matrix "im" as NULL
        
        set <- function(y) {            ## "set" is a function that changes the matrix stored in the main function "makeCacheMatrix"
                
                x <<- y                 ## replace the matrix "x" with "y" (the input of function "set")                                   
                im <<- NULL             ## restores to null the value of the inverse matrix "im"
        }
        get <- function() x             ## get is a function that returns the matrix "x"  
                                        ## stored in the main function. Doesn't require any input.
        
        
        set_inverse <- function(solve) im <<- solve     ## set_inverse is a function that changes the stored matrix "im"
                                                        ## with the new matrix "solve" that is the input of the "set_inverse" function
                                                        ## this "im" matrix is cached in the main function "makeCacheMatrix"
        
        get_inverse <- function() im                   ## get_inverse is a function that returns the matrix "im" 
                                                       ## stored in the main function "makeCacheMatrix". Doesn't require any input.
        
        list(set = set, get = get,                     ## list() store the 4 functions in the function makeCacheMatrix
             set_inverse = set_inverse,                ## when makeCacheMatrix is assigned to an object,
             get_inverse = get_inverse)                ## the object has all the 4 functions
        
}


## The cacheSolve function calculates the inverse of the special "matrix" created by makeCacheMatrix.
## However, it first checks to see if the inverse matrix has already been calculated.
## If so, it gets the inverse matrix from the cache and skips the computation.
## Otherwise, it calculates the inverse matrix and sets it in the cache via the set_inverse function
## The input of cacheSolve is the object that makeCacheMatrix(x) was assigned to


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        
        im <- x$get_inverse()                           ## get the inverse matrix cached by makeCacheMatrix and assigned it to "im"
        if(!is.null(im)) {                              ## if matrix "im" exist (not null)
                message("getting cached data")          ## return message
                return(im)                              ## exit progrm and return cached matrix "im"
                #
        }
        data <- x$get()                                 ## otherwise, get metrix cached by makeCacheMatrix and assigned it to "data" 
        im <- solve(data, ...)                          ## calculate inverse matrix by solve() and assigned it to "im"
        x$set_inverse(im)                               ## cache the inverse matrix result by function set_inverse()
        im                                              ## return the inverse matrix result
}
