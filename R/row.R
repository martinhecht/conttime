## Changelog:
# MH 0.0.44 2024-05-31 if M is scalar, put in matrix
# MH 0.0.1 2022-05-31

## Documentation
#' @title
#' @description the row operator puts the elements of a matrix row-wise in a column vector
#' @param
#' @param
#' @param
#' @return

## Function definition
row <- function( M ){

	# MH 0.0.44 2024-05-31 if M is scalar, put in matrix
	if( is.null( dim(M) ) ) M <- matrix( M, nrow=1, ncol=1 )
	
	if( ! ( is.matrix(M) && dim(M)[1] == dim(M)[2] && dim(M)[1]>0 ) ){
        rowM <- NULL
    } else {
        F <- dim(M)[1]
        rowM <- matrix( NA, nrow=F^2, ncol=1)
        for ( r in (1:F) ) {
            for( c in (1:F) ) {
                rowM[ c+F*(r-1), 1 ] <- M[r,c]
            }
        }
    }
    return( rowM )
}

# potential TODO:
# -- arrays that have dimensions with just 1, reduce to matrix

### development

# user.profile <- shell( "echo %USERPROFILE%", intern=TRUE )
# Rfiles.folder <- file.path( user.profile,
                                    # "Dropbox/96_mutualism/mutualism/R" )
# Rfiles <- list.files( Rfiles.folder , pattern="*.R" )
# Rfiles <- Rfiles[ !Rfiles %in% c("row.R") ]
# for( Rfile in Rfiles ){
	# source( file.path( Rfiles.folder, Rfile ) )
# }

### test
# require( testthat )
# test_file("../tests/testthat/XXXXX.R")
