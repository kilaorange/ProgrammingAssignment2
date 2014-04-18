##    Author:  Andrew Szwec
##    Date:    2014-04-18
##
##    Coursera: R Programming, Assignment 2
##
##    Program Description:
##    These 2 functions (makeCacheMatrix and cacheSolve) will implement a program to calculate the inverse matrix    
##    given an invertible matrix.
##    
##    Function 1:
##    makeCacheMatrix() will create a special matrix object that will cache the inverse matrix
##
##    Function 2:    
##    cacheSolve() will conpute the inverse of the special matrix returned by makeCacheMatrix(). If the inverse has 
##    already been calculated and the matrix has not changed, then cacheSolve() will return the inverse from cache.
##
##

##
## makeCacheMatrix() will create a special matrix object that will cache the inverse matrix
##
makeCacheMatrix <- function(x = matrix()) {
      ## Initiate inverse to NULL matrix
      inverse <- NULL
      
      ## Reset vector 'x' to new value from set() function and reset inverse to NULL
      set <- function(y) {
            x <<- y
            inverse <<- NULL
      }
      
      ## returns matrix
      get <- function() x
      
      ## Once inverse is calculated, set the inverse attribute of the vector to the value
      setInverse <- function(i) { 
            inverse <<- i
      }
      
      ## return the value of the mean
      getInverse <- function() inverse
      
      ## List methods inside makeCacheMatrix
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}

##
##    cacheSolve() will conpute the inverse of the special matrix returned by makeCacheMatrix(). If the inverse has 
##    already been calculated and the matrix has not changed, then cacheSolve() will return the inverse from cache.
##
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      ## Once you've made a vector 'x' with some "special properties" 
      ## you can call on these special properties, like getInverse(), etc...
      inv <- x$getInverse()
      
      ## if the inverse is not null then return the inverse
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      
      ## else, if the inverse has not been stored in getInverse() yet then 
      ## get the matrix you stored in the vector and call it data
      data <- x$get()
      
      ## now calculate the inverse of the matrix
      inv <- solve(data)
      
      ## store the mean in the special vector using the method setmean()
      x$setInverse(inv)
      
      ## Print Inverse Matrix!
      inv
}



## Unit test cases

## Choose an invertible matrix called 'a'
a <- rbind(c(2,3),c(2,2))
b <- rbind(c(1,2,3),c(0,1,4), c(5,6,0))

## Check its invertible. YES!
solve(a)
solve(b)

## Make a matrix with special properties
x <- makeCacheMatrix(a)

## Check methods of Special Matrix 'x'

######################################
##    Check get()
######################################
x$get()

# Returns a 2x2
> x$get()
[,1] [,2]
[1,]    2    3
[2,]    2    2

######################################
##    Check set()
######################################
x$set(b)
x$get()
# Returns a 3x3
> x$get()
[,1] [,2] [,3]
[1,]    1    2    3
[2,]    0    1    4
[3,]    5    6    0

######################################
##    Check getInverse()
######################################
x$getInverse()


######################################
##    Check setInverse()
######################################
x$setInverse( solve(a) )

x$getInverse()


######################################
##    Check cacheSolve()
######################################
## Calculate Inverse Matrix and return it
cacheSolve(x)









