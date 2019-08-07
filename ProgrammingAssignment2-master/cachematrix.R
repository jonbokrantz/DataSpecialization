## The purpose of the program is to cache the inverse of a matrix in order to reduce computational cost
## The program executes two functions: makeCacheMatrix and cacheSolve

## The function makeCacheMatrix creates a special "matrix" object that can cache its inverse
	makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	
	#set the value of the matrix
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	
	#get the value of the matrix
	get <- function() x
	
	#set the value of the inverse
	setinverse <- function(inverse) i <<- inverse
	
	#get the value of the inverse
	getinverse <- function() i
	
	#create the "special matrix"
	list(set = set, get = get, 
	setinverse = setinverse, 
	getinverse = getinverse)
}

## The function cacheSolve computes the inverse of the matrix crated by makeCacheMatrix
## If the inverse has already been calculated, then cacheSolve retrieves the inverse from the cache
	cacheSolve <- function(x, ...) {	
	i <- x$getinverse()
	
	#if inverse has already been created, the inverse is retrieved from the cache
	if (!is.null(i)){
		message("inverse retrieved from cache")
		return(i)
	}
	
	#compute the inverse of the matrix produced by makeCacheMatrix
	matrixdata <-x$get()
	i <- solve(matrixdata, ...)
	x$setinverse(i)
    
    ## Return a matrix that is the inverse of 'x'
    i
}
