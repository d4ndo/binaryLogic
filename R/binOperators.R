#' Binary Negation (!)
#' 
#' # !c(rep(0,Byte()-length(x)),x)
#' @description Negates the binary number x. Negation x -> -x or -x -> x
#' @details No floating point supported.
#' @usage negate(x)
#' @param x The number to be negated. A binary or logical vector is expected.
#' @return The negated number of x. Returns a binary/logical vector with signed=TRUE
#' @examples
#' negate(dec2bin(2, signed=TRUE))
#' bin2dec(negate(dec2bin(-5, signed=TRUE)))
#' @seealso base::as.logical , base::is.logical, base::raw
#' @export
negate <- function(x) {
    stopifnot(is.binary(x))
    signed <- attributes(x)$signed
    #if (!signed) warning("Trying to negate an unsigned digit. treated as signed value. Returns a signed value")
    littleEndian <- attributes(x)$littleEndian

    if(littleEndian) x <- rev(x)
    
    if (length(x)%%Byte() != 0) {
        MAX <- (trunc((length(x)/Byte())) +1) * Byte()
        a <- rep(FALSE, MAX - length(x))
        a <- as.binary(a, littleEndian=littleEndian)
        # 'c.binary'(x,y) needs to be implemented
        x <- as.binary(c(a,x))
    }
    x <- !x
    x <- binAdd(as.binary(x),as.binary(TRUE))

    if(littleEndian) x <- rev(x)
    attr(x,"signed") <- TRUE
    attr(x,"littleEndian") <- littleEndian
    class(x) <- c("binary", "logical")
    return(x)
}

#' Binary Left Shift (<<)
#' 
#' @description Logical left shift x << n
#' @usage shiftLeft(x, n)
#' @param x The binary number to shift. (binary or logical vector).
#' @param n The number of bits to shift.
#' @return Pushes 0's(FALSE) to the vector from right(LSB) to left(MSB).
#' Everything on right(MSB) side drops out. Returns a binary/logical vector
#' @examples
#' x <- as.binary(c(1,0,0,1,1,1,0,1)); x
#' shiftLeft(x,1)
#' shiftLeft(x,2)
#' @seealso \link{shiftRight} and \link{rotate} 
#' @export
shiftLeft <- function(x, n) {
    stopifnot(is.logical(x) || is.binary(x))
    stopifnot(n > 0)
    if (n > length(x)) return(binary(length(x)))
    delta <- length(x) - n
    for(i in seq(1, n))
    {
        for(j in seq_len(length(x))) {
            x[j] <- x[j+1]
        }
    }
    x[(delta+1):length(x)] <- FALSE
    return(x)
}

#' Binary Right Shift (>>)
#' 
#' @description Logical right shift 1 >> n
#' @usage shiftRight(x, n)
#' @param x The binary number to shift. (binary or logical vector).
#' @param n The number of bits to shift.
#' @return Pushes 0's(FALSE) to the vector from left(MSB) to right(LSB).
#' Everything on right(LSB) side drops out. Returns a binary/logical vector
#' @examples
#' x <- as.binary(c(1,0,0,1,1,1,0,1)); x
#' shiftRight(x,1)
#' shiftRight(x,2)
#' @seealso \link{shiftLeft} and \link{rotate} 
#' @export
shiftRight <- function(x, n) {
    stopifnot(is.logical(x) || is.binary(x))
    stopifnot(n > 0)
    if (n > length(x)) return(binary(length(x)))
    delta <- length(x) - n
    x <- rev(x)
    for(i in seq_len(n))
    {
        for(j in seq_len(length(x))){
        x[j] <- x[j+1]
        }
    }
    x[(delta+1):length(x)] <- FALSE
    x <- rev(x)
    return(x)
}

#' Rotate no carry ()
#'
#' @description A circular shift
#' @usage rotate(x, n)
#' @param x The binary number to rotate. (binary or logical vector).
#' @param n The number of bits to rotate.
#' @return rotates the vector from left to right. 
#' The value from MSB is used to fill up the vector at LSB. Returns a binary/logical vector.
#' @examples
#' x <- as.binary(c(1,0,0,1,1,1,0,1)); x
#' rotate(x,1)
#' rotate(x,2)
#' @seealso \link{shiftLeft} and \link{shiftRight} 
#' @export
rotate <- function(x, n) {
    stopifnot(is.binary(x) || is.logical(x))
    stopifnot(n > 0)
    #save and loadAttributes needs to be done, until I find a way to overload »combine« c(...)
    l <- saveAttributes(x)
    if (n > length(x)) stop("n is larger than length of x")
    loadAttributes(c(x[-seq(n)],x[seq(n)]), l)
}

#' Fill Bits (000..)
#' !c(rep(0,Byte()-length(x)),x)
#' @description Fills the number with Bits to the size in Byte.
#' @details No floating point supported.
#' @usage fillBits(x, value=FALSE, size=0)
#' @param x The binary number to fill with bits. (Any binary vector).
#' @param value Fill with FALSE or fill with TRUE.
#' @param size in Byte. 0 = auto (smallest possible byte).
#' @return binary number. A binary vector with the desired size.
#' @examples
#' fillBits(as.binary(c(1,1)), size=2)
#' fillBits(as.binary(c(1,0,1)), value=FALSE, size=2)
#' @seealso \link{bytesNeeded} or \link{binPrefix2Bytes} or \link{Byte}
#' @export 
fillBits <- function(x, value=FALSE, size=0) {
    stopifnot(is.binary(x))
    l <- saveAttributes(x)    
    if (size == 0 && length(x) %% Byte() == 0) return(x)
    if (size > 0 && length(x) >= size * Byte()) return(x)

    if (size == 0) {
        append <- binary(((trunc((length(x) / Byte())) +1) * Byte()) - length(x))
    } else {
        append <- binary(size * Byte() - length(x))
    }
    append[1:length(append)] <- value

    if (attributes(x)$littleEndian) {
        x <- c(x,append)
    } else {
        x <- c(append,x)
    }
    x <- loadAttributes(x,l)    
    return(x)
}

#' Switch Endianess.
#' 
#' @description switch little-endian to big-endian and vice versa.
#' @usage switchEndianess(x)
#' @param x binary number. Any binary vector. switchEndianess(y)
#' @return switch little-endian to big-endian and vice versa.
#' @examples
#' x <- as.binary(c(1,1,0,0)); print(x); attributes(x);
#' y <- switchEndianess(x); print(y); attributes(y);
#' @seealso /link{as.binary} binaryLogic::is.binary
#' @export
switchEndianess <- function(x) {
    stopifnot(is.binary(x))
    
    attr(x,"littleEndian") <- !attributes(x)$littleEndian
    return(rev(x))
}