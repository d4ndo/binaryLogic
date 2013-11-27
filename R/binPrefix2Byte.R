#' Binary prefix to Byte. (KiB,MiB,..)
#' 
#' @description Amount of Bytes needed to fit in n * KiB, MiB ..etc.
#' @usage binPrefix2Byte(n, prefix="KiB")
#' @param n vector of numeric values
#' @param prefix binary prefix * Byte. Expeting a »string«
#' @return The number of bytes fitting in n * binary prefix * Byte
#' @examples
#' binPrefix2Byte(c(0.5,1:10),"KiB")
#' as.integer(dec2bin(binPrefix2Byte(1,"KiB")*Byte()))
#' @export
binPrefix2Byte <- function(n, prefix="KiB") {
    if(missing(n)) stop("Missing n")
    stopifnot(all(is.numeric(n) | is.na(n)))
    stopifnot(all(n >= 0 | is.na(n)))
    ret <- NULL
    
    if (prefix=="KiB") ret = n * (2^10)
    if (prefix=="MiB") ret = n * (2^20)
    if (prefix=="GiB") ret = n * (2^30)
    if (prefix=="TiB") ret = n * (2^40)
    if (prefix=="PiB") ret = n * (2^50)
    if (prefix=="EiB") ret = n * (2^60)
    if (prefix=="ZiB") ret = n * (2^70)
    if (prefix=="YiB") ret = n * (2^80)
    
    if (any(is.null(ret))) stop("Unknown binary prefix")
    if (isTRUE(all.equal(ret, round(ret)))) {
        return(ret)
    } else {
        message("ceiling called: returns the smallest integer not less than the corresponding element of ret")
        return(ceiling(ret))
    }
}

#' A simple helper function to return the size of one Byte
#' 
#' @description Used to increase readabilaty
#' @usage Byte()
#' @return The size of one Byte (8)
#' @export
Byte <- function() {
    return(8)
}