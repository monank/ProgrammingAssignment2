## These functions will calculate the inverse matrix of a given matrix
## and cache it for future use.

## makeCacheMatrix will create a cache of matrices and its inverse matrices.

makeCacheMatrix <- function(x = matrix()) {   
    m<-NULL
    set<-function(y){
        x<<-y 
        m<<-NULL
    }
    get<-function() x
    setm<-function(solve) m<<- solve
    getm<-function() m
    list(set=set, get=get,
         setm=setm,
         getm=getm)
}


## cacheSolve will give the inverse matrix of x

cacheSolve <- function(x, ...) {
    
    m<-x$getm()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setm(m)
    m
}