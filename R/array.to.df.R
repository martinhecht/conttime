## Changelog:
# MH 2024-04-29: set up

## Documentation
#' @title
#' @description
#' @param
#' @param
#' @param
#' @return

## Function definition
array.to.df <- function( array, array.name, value.name="value" ) {

	if (! is.list( array ) ) array <- list( array )

	one.array.to.df <- function( array, array.name ){

		# Generate row names based on array dimensions
		dims <- dim(array)
		indices <- expand.grid(lapply(seq_along(dims), function(d) seq_len(dims[d])))
		row_names <- apply(indices, 1, function(idx) paste( array.name, paste(idx, collapse = ","), sep = "["))
		par <- paste(row_names, "]", sep="")
		
		# Convert the array to a vector and then to a data.frame
		df <- data.frame( par=par, value=as.vector(array) )
		colnames( df )[ncol(df)] <- value.name
		rownames( df ) <- seq( along=rownames( df ) )
	
		return( df )
	}
	df.list <- mapply( one.array.to.df, array, array.name, SIMPLIFY=FALSE )
	
	# rbind
	df <- do.call( "rbind", df.list )
	rownames( df ) <- seq( along=rownames( df ) )	

	return( df )
}


### development
# user.profile <- shell( "echo %USERPROFILE%", intern=TRUE )
# Rfiles.folder <- file.path( user.profile,
                                    # "Dropbox/139_conttime/conttime/R" )
# Rfiles <- list.files( Rfiles.folder , pattern="*.R" )
# Rfiles <- Rfiles[ !Rfiles %in% c("array.to.df.R") ]
# for( Rfile in Rfiles ){
	# source( file.path( Rfiles.folder, Rfile ) )
# }

# array.to.df( array(1:24, dim = c(2, 3, 4)), "A" )

### test
# require( testthat )
# test_file("../tests/testthat/XXXXX.R")
