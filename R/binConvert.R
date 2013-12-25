#' Converts a decimal(Base10) or hex(Base16) number to a binary(Base2) digit.
#'
#' @description Converts a decimal(Base10) or hex(Base16) number to a binary(Base2) number.
#' @details The binary number is represented by a logical vector.
#' The Bit order usually follows the same endianness as the byte order.
#' No floating-point support.
#' \itemize{
#' \item Little Endian    (LSB) ---> (MSB)
#' \item Big Endian       (MSB) <--- (LSB)
#' }
#' Auto switch to signed if num < 0.
#' @usage dec2bin(num, signed=FALSE, littleEndian=FALSE, size=2)
#' @param num     integer (hex) or (decimal) number.
#' @param signed  TRUE or FALSE. Unsigned by default. (two's complement) 
#' @param littleEndian if TRUE. Big Endian if FALSE.
#' @param size in Byte. Needed if »signed« is set. (by default 2 Byte)
#' @return The binary number. Returns a (binary/logical) vector.
#' @seealso \link{bin2dec} and \link{binary} or \link{logical}
dec2bin <- function(num, signed=FALSE, littleEndian=FALSE, size=2) {
    # Very slow with negative numbers. (maybe binAdd)
    if (signed && (((num > ((2^(size*Byte())/2)-1))) || 
                    (num < ((-1)*(2^(size*Byte())/2)))))
    stop("Out of Range. Please increase the size[Byte]")
    
    l <- list(class=c("binary","logical"),
              signed=signed,
              littleEndian=littleEndian)
    
    #needed for hex input
    num <- as.numeric(num)
    
    neg = FALSE
    if (num < 0) {
        l$signed <- TRUE
        neg = TRUE
        num = abs(num)
    }
    
    #global read only for function h(num)
    ret <- h(num)

    if(l$signed) b <- logical(size*Byte()) else b <- logical(max(ret)-1)
    #do some optimization here.
    for (i in seq(length(b))) b[i] <- any(ifelse((ret-1)==i, TRUE, FALSE))
    
    if (neg)
    {
        b <- !rev(b)
        b <- rev(binAdd(as.binary(b, signed=TRUE), as.binary(TRUE)))
    }
    if(!l$littleEndian) b <- rev(b)
    loadAttributes(b, l)
    return(loadAttributes(b, l))
}

# Helper function to convert decimal to binary
h <- function(x) {
    # v should be global.
    v <- c(0,  2^(0:1016))
    ret <- numeric(1)
    for(i in 1:length(v)) {
        if (v[i] > x) { ret <- c(i-1, h(x-v[i-1])); break }
        if (v[i] == x) return(ret <- c(i, ret))
    }
    return(ret)
}

#' Converts a binary(Base2) number to a decimal(Base10) number.
#' 
#' @description Converts a binary(Base2) number to a decimal(Base10) number.
#' @details No floating-point support.
#' \itemize{
#' \item Little Endian    (LSB) ---> (MSB)
#' \item Big Endian       (MSB) <--- (LSB)
#' }
#' @usage bin2dec(bin)
#' @param bin      binary number. Any logical or binary vector.
#' @return The decimal(Base10) number (numeric/double).
#' @seealso \link{dec2bin} and \link{binary} or \link{logical}
bin2dec <- function(bin) {
    #could be implemented in C. But it is not that slow.
    signed <- attributes(bin)$signed
    littleEndian <- attributes(bin)$littleEndian
    
    if(!littleEndian) { bin <- rev(bin) }
    
    bin <- as.integer(as.logical(bin))
    i = length(bin)-1
    numeric = 0
    first <- TRUE

    for(d in rev(bin))
    {
        if ((signed) & (first)) {
            numeric <- (-1 * d * (2^i))
            i <- i - 1
            first <- FALSE
        } else {
            numeric = (numeric + d * (2^i))
            i <- i - 1
        }
    }
    return(numeric)
}