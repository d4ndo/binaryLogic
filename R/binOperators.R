#' Binary Addition (+)
#' 
#' @description Adds two binary numbers. (x + y)
#' @details Big Endian is supported only at the moment.
#' @usage binAdd(x, y, signed=FALSE, size=0)
#' @param x summand 1 (logical vector)
#' @param y summand 2 (logical vector)
#' @param signed TRUE or FALSE. Returns a signed number. see example
#' @param size in Byte. Is needed if signed is set. 0 = auto
#' @return The sum of x and y. Returns a logical vector.
#' @examples
#' binAdd(as.logical(c(0,1)),as.logical(c(1,0)))
#' @seealso base::as.logical , base::is.logical, base::as.integer base::raw
#' @export
binAdd <- function(x, y, signed=FALSE, size=0) {
    if (length(x) >= length(y))
    {
        MAX <- length(x)
        a <- rep(FALSE,length(x)-length(y))
        y <- c(a,y)
    } else {
        MAX <- length(y)
        a <- rep(FALSE,length(y)-length(x))
        x <- c(a,x)
    }

    ret = logical(MAX)
    temp = logical(MAX+1)
    ret[MAX] <- xor(x[MAX],y[MAX])
    if(isTRUE(x[MAX]) & isTRUE(y[MAX])) temp[MAX+1] = TRUE

    if(MAX > 2)
    {
        for(i in (MAX-1):1)
        {
            ret[i] <- xor(x[i],y[i])
            ret[i] <- xor(ret[i],temp[i+2])
            if((isTRUE(x[i]) & isTRUE(y[i])) | 
                (isTRUE(x[i]) & isTRUE(temp[i+2])) | 
                (isTRUE(y[i]) & isTRUE(temp[i+2]))) temp[i+1] = TRUE
        }
    }
    if(temp[2] & !signed) ret <- c(T,ret)
    return(ret)
}

#' Binary Negation (!)
#' 
#' @description Negates the binary number x. Negation x -> -x or -x -> x
#' @usage negate(x)
#' @param x The number to be negated. A logical vector is expected.
#' @return The negated number of x. Returns a logical vector with signed=TRUE
#' @examples
#' negate(c(TRUE,FALSE,TRUE))
#' bin2dec(negate(dec2bin(-5, signed=TRUE)))
#' @seealso base::as.logical , base::is.logical, base::as.integer base::raw
#' @export
negate <- function(x) {
    if (missing(x)) stop("x is missing.")
    if(length(x)%%Byte() != 0) {
        MAX <- (trunc((length(x)/Byte())) +1) * Byte()
        a <- rep(FALSE, MAX - length(x))
        x <- c(a,x)
    }
    x <- !x
    return(binAdd(x,TRUE))
}


#' Binary Left Shift (<<)
#' 
#' @description Logical left shift x << size
#' @usage shiftLeft(x, size)
#' @param x The binary number to shift. (logical vector).
#' @param size The number of places to shift.
#' @return Pushes 0's(FALSE) to the vector from right(LSB) to left(MSB).
#' Everything on right(MSB) side drops out. Returns a logical vector
#' @examples
#' x <- c(TRUE,FALSE,FALSE,TRUE,TRUE,TRUE,FALSE,TRUE); x
#' shiftLeft(x,1)
#' shiftLeft(x,2)
#' @seealso base::as.logical , base::is.logical, base::as.integer base::raw
#' @export
shiftLeft <- function(x, size) {
    if (missing(x)) stop("x is missing.")
    stopifnot(is.logical(x))
    stopifnot(size != 0)
    if(size > length(x)) stop("size is larger than length of x")
    delta <- length(x)-size
    for(i in 1:size)
    {
        for(i in 1:length(x)){
        x[i] <- x[i+1]
        }
    }
    x[(delta+1):length(x)] <- FALSE

    return(x)
}

#' Binary Right Shift (>>)
#' 
#' @description Logical right shift 1 >> size
#' @usage shiftRight(x, size)
#' @param x The binary number to shift. (logical vector).
#' @param size The number of places to shift.
#' @return Pushes 0's(FALSE) to the vector from left(MSB) to right(LSB).
#' Everything on right(LSB) side drops out. Returns a logical vector
#' @examples
#' x <- c(TRUE,FALSE,FALSE,TRUE,TRUE,TRUE,FALSE,TRUE); x
#' shiftRight(x,1)
#' shiftRight(x,2)
#' @seealso base::as.logical , base::is.logical, base::as.integer base::raw
#' @export
shiftRight <- function(x, size) {
    if (missing(x)) stop("x is missing.")
    stopifnot(is.logical(x))
    stopifnot(size != 0)
    if(size > length(x)) stop("size is larger than length of x")
    delta <- length(x)-size
    x <- x[length(x):1]
    for(i in 1:size)
    {
        for(i in 1:length(x)){
        x[i] <- x[i+1]
        }
    }
    x[(delta+1):length(x)] <- FALSE

    return(x[length(x):1])
}

#' Rotate no carry ()
#' 
#' @description A circular shift
#' @usage rotate(x, size)
#' @param x The binary number to rotate. (logical vector).
#' @param size The number of places to rotate.
#' @return rotates the vector from left to right. 
#' The value from MSB is used to fill up the vector at LSB. Returns a logical vector.
#' @examples
#' x <- c(TRUE,FALSE,FALSE,TRUE,TRUE,TRUE,FALSE,TRUE); x
#' rotate(x,1)
#' rotate(x,2)
#' @seealso base::as.logical , base::is.logical, base::as.integer base::raw
#' @export
rotate <- function(x, size) {
    if (missing(x)) stop("x is missing.")
    stopifnot(is.logical(x))
    stopifnot(size != 0)
    if(size > length(x)) stop("size is larger than length of x")
    delta <- length(x)-size

    tmp <- x[1:size]
    for(i in 1:size)
    {
        for(i in 1:length(x)){
        x[i] <- x[i+1]
        }
    }
    x[(delta+1):length(x)] <- tmp[1:size]

    return(x)
}