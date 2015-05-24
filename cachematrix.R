#makeCacheMatrix: function creates a special "matrix" object that can cache its inverse.

#setM sets the matrix
#getM gets the matrix
#setInv sets the inverse matrix
#getInv gets the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
   
    #matrix inverse set to NULL initially 
    inverseM <- NULL
    #create a NEW matrix x using a variable y from another environment. As it's a matrix, the potentially cached inverse has to be nullified
    setM <- function(y) {
      x <<- y
      inverseM <<- NULL
    }
    #get (return)matrix x 
    getM <- function() x
    
    #store (cache) the inv value. In this case -  the inverse of the new matix  
    setInv <- function(inv) inverseM <<- inv
    
    #get the cached the matrix's invesion 
    getInv <- function() inverseM
    
    #return the list of the above functions
    list(setM = setM, getM = getM,
         setInv = setInv,
         getInv = getInv)
  
}


#cacheSolve: function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  #get the inverse of the matrix x  
  inv <- x$getInv()
  
  #if cached inverse exists, return it & exist function
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  #get the original matrix 
  data <- x$getM()
  #compute the inverse of matrix x
  inv <- solve(data)
  #cache the inverse
  x$setInv(inv)
  #return the current inversion of the x matrix
  inv
}

#need help understanding the above ? Check the existing info:
#https://github.com/DanieleP/PA2-clarifying_instructions
#https://class.coursera.org/rprog-014/forum/thread?thread_id=576