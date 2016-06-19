#Script by Carina Palumbo
#Prepared for R programming course, Coursera, Assignment week 3.

#The first function in the assignment: it creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(amatrix = matrix()) {
    s <- NULL
    set <- function(y) {
        amatrix <<- y
        s <<- NULL
    }
    get <- function() amatrix
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set,
    get = get,
    setsolve = setsolve,
    getsolve = getsolve)
}

#The 2nd function in the assignment: it computes the inverse of the special "matrix" returned by makeCacheMatrix.
#If the inverse has already been calculated (and the matrix has not changed), then it will retrieve the inverse from the cache.
cacheSolve <- function(amatrix, ...) {
    s <- amatrix$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    message("Inverted matrix has not been cached. I'll cache it now.")
    data <- amatrix$get()
    s <- solve(data, ...)
    amatrix$setsolve(s)
    s
}

#If you want to try out the functions:
#Use this invertible matrix as an example (or any other invertible matrix):
imatrix <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
mymatrix <- imatrix(8)

#First, run this to create the special matrix object:
myspecialmatrix1 <- makeCacheMatrix(mymatrix)
#Then, run this to get the cached inverted matrix:
cacheSolve(myspecialmatrix1) # This will return the inverted of mymatrix, if previously set using makeCacheMatrix
#To check that the output matrix is the inverted one, you can use this:
invertedmatrix <- cacheSolve(myspecialmatrix1)
#And then run:
mymatrix %*% invertedmatrix # The resulting matrix has 1 in the diagonal and (close to) 0 elsewhere, proving that the outcome of cacheSolved is the inverted matrix of the original one entered.

#To try out the else scenario in the 2nd function
#Set the special matrix, without calculating the inverse matrix
myspecialmatrix2$set(mymatrix)
#Run the second function, which will now calculate the inverted matrix cached
cacheSolve(myspecialmatrix2)
