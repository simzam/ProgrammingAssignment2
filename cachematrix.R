## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# START by copying the example in the assignment text
makeCacheMatrix <- function(x = matrix()) {
        library(matlib)
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<-solve
        getinv <- function() m
        list(set = set, get = get,
                setinv = setinv,
                getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        print(x)
        print(class(x))
        m <- solve(data, ...)
        x$setinv(m)
        m
}


#aMatrix <- makeCacheMatrix(matrix(runif(9), 3, 3))
#print(class(aMatrix$get()))
#print(aMatrix$getinv())
#print(aMatrix$getinv())
#print(aMatrix$get() %*% aMatrix$getinv())
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
print(m1)

I2 <- matrix(c(1,0,0,1), nrow = 2, ncol = 2)
print(I2)

n1 <- matrix(c(6,2,8,4), nrow = 2, ncol = 2)
print(n1)

print(n1 %*% m1)
print(m1 %*% n1)

print(solve(m1))
print(solve(n1))

myMatrix_object <- makeCacheMatrix(m1)
print(cacheSolve(myMatrix_object))
print(cacheSolve(myMatrix_object))

n2 <- matrix(c(5/8, -1/8, -7/8, 3/8), nrow = 2, ncol = 2)
myMatrix_object$set(n2)
print(cacheSolve(myMatrix_object))
print(cacheSolve(myMatrix_object))
