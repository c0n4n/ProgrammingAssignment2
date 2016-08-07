### Two functions to calculate the inverse of a matrix, which can be a costly computation. Uses cache to avoid computing repeadetly.

### Set & get the matrix & its inverse (setinmat),(getinmat). Returns a list needed as input to solve-function below.

makeCacheMatrix<- function(x=matrix()) {
        cac<-NULL
        set<-function(y) {
                x<<-y
                i<<-NULL
        }
        get <-function() x
        setinmat<-function(inv) cac<<-inv
        getinmat<-function() cac
        list(set=set, get=get, setinmat=setinmat, getinmat=getinmat)
}

###Computes the inverse of a matrix created with above function.
###Returns cached value cac if it is there, otherwise calculates it with solve -function from matrix created above.
cacheSolve<- function(x, ...){
        cac<-x$getinmat()
        if(!is.null(cac)) {
                message("getting cached data")
                return(cac)
        }
        
        mat<-x$get()
        cac<-solve(mat,...)
        x$setinmat(cac)
        return(cac)
}

