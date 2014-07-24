# The following two functions, when used together, calculate and cache the inverse of
# an invertible square matrix.


# The makeCacheMatrix() function:
# -------------------------------
# 1) Takes a matrix and generates a cache for storing the inverse of that matrix.
# 2) The matrix passed to the function must be an invertible square matrix.

# e.g. matrix1 <- makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
# e.g. matrix2 <- makeCacheMatrix(matrix(c(1,2,3,4,4,1,2,5,6), nrow=3, ncol=3))


makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {                                    # the set() function is used to modify the matrix e.g. matrix1$set(matrix(c(2,3,4,5), nrow=2, ncol=2)).
                x <<- y
                i <<- NULL
        }
        get <- function() {x}                                   # the get() function returns the original matrix e.g. matrix1$get().
        setinverse <- function(inverse) {i <<- inverse}         # the setinverse() function is used by the cacheSolve() function to cache the inverse of the matrix.
        getinverse <- function() {i}                            # the getinverse() function returns the cached inverse of the matrix (if it has already been cached) e.g. matrix1$getinverse().
        list(set = set,... =
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)                           # the makeCacheMatrix() function is a list of 4 functions: set(), get(), setinverse(), getinverse().
}


#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# The cacheSolve() function:
# --------------------------
# 1) Computes, caches and returns the inverse of an invertible square matrix.
# 2) If the inverse has already been computed and cached to the makeCacheMatrix() function, then the inverse is retrieved without repeating the computation.
# 3) The argument x passed to the cacheSolve() function is an instance of the makeCacheMatrix() function e.g. matrix1


cacheSolve <- function(x, ...) {
        i <- x$getinverse()                                     # the getinverse() function is used to check if the inverse of the matrix has already been computed and cached to x.
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        } 
        data <- x$get()                                         # if the inverse of the matrix has not yet been cached, the get() function is used to retrieve the original matrix from x.
        i <- solve(data, ...)                                   # the solve() function computes the inverse of the original matrix.
        x$setinverse(i)                                         # the setinverse() function is used to cache the inverse of the matrix to x.
        i   
}

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



