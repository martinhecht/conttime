## Changelog:
# MH 0.0.1 2024-05-29: set up

# https://chatgpt.com/c/c811aa70-1021-48ea-bf82-a0eed8e3f23e (2024-05-29)
# In R, checking for positive definiteness of a matrix typically applies to symmetric matrices, as positive definiteness is a property usually defined for such matrices. However, for a non-symmetric matrix, you can check for positive definiteness in terms of its symmetric part.

## Documentation
#' @title
#' @description
#' @param
#' @param
#' @param
#' @return

## Function definition
is_positive_definite <- function( m ){

	# Compute the symmetric part of m
	S <- (m + t(m)) / 2
	
	# Check for positive definiteness using Cholesky decomposition
	is_positive_definite2 <- function(matrix) {
	tryCatch({
		chol(matrix)
		return(TRUE)
	}, error = function(e) {
		return(FALSE)
	})
	}
	
	# Check if the symmetric part is positive definite
	is_positive_definite2(S)
}


### development

# user.profile <- shell( "echo %USERPROFILE%", intern=TRUE )
# Rfiles.folder <- file.path( user.profile,
                                    # "Dropbox/96_mutualism/mutualism/R" )
# Rfiles <- list.files( Rfiles.folder , pattern="*.R" )
# Rfiles <- Rfiles[ !Rfiles %in% c("is_positive_definite.R") ]
# for( Rfile in Rfiles ){
	# source( file.path( Rfiles.folder, Rfile ) )
# }

### test
# require( testthat )
# test_file("../tests/testthat/XXXXX.R")
