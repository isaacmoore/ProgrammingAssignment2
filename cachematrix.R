## Put comments here that give an overall description of what your
## functions do
#----------------------------------------------------
# When creating a matrix using the "MakeCacheMatrix" 
# function, the inverse of the matrix is cached and 
# then can be recalled using the "cacheSolve" function.
#----------------------------------------------------


## Write a short comment describing this function
#----------------------------------------------------
# The "makeCacheMatrix" function creates a cached
# inverse of the matrix created.
#----------------------------------------------------

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function
#----------------------------------------------------
# The "cacheSolve" function returns "inv" which is the
# "getInverse" value set from the "makeCacheMatrix" function 
# above. The second time the inverse is called, using the 
# data, the "cacheSolve" function is retun the value 
# from the cache.
#----------------------------------------------------

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}

#------------------Results---------------------------
# > my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2)) #Setting the matrix
# > my_matrix$get() #viewing the values passed into the matrix.
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > cacheSolve(my_matrix) #retrieveing the inverse.
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(my_matrix) #retrieveing the cached inverse.
# getting cached data
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5