
## makeCacheMatrix - this function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix){
        
        ## initialize the objects
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## get the value of the matrix
        get <- function() x
                
        ## set the value of the inverse
        set_inv_matrix <- function(solve) m <<- solve
        
        ## get the value of the inverse
        get_inv_matrix <- function() m
        list(set = set, get = get, 
				set_inv_matrix = set_inv_matrix, 
				get_inv_matrix = get_inv_matrix)
        
        
}


## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cacheSolve should retrieve the
## inverse from the cache.

cacheSolve <- function(x, ...) {
        
	##get the matrix
	m <- x$get_inv_matrix()
		
	##if the inverse has already been calculated, then get the cached data
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
		
	##if the inverse has not been calculated, calculate the inverse with solve...
        data <- x$get()
        m <- solve(data, ...)
        x$set_inv_matrix(m)
        m
}
