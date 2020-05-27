## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse
makeCacheMatrix<-function(x=matrix()){
        m <- NULL
        set <- function(y) {
            x<<-y
            m<<-NULL
        }
        get<-function()x
        setinverse<-function(inverse)m<<-solve
        getinverse<-function()m
        list(set=set,get=get,
            setinverse=setinverse,
            getinverse=getinverse)
   }
## the above function calculates the inverse of the special matrix
cacheSolve<-function(x, ...){
        ## Return a matrix that is the inverse of 'x'
    m<-x$getinverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data<-x$get()
    m<-solve(data, ...)
    x$setinverse(m)
    m
}
