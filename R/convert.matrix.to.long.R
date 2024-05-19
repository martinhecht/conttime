## Changelog:
# MH 0.0.1 2024-05-19: set up

## Documentation
#' @title
#' @description
#' @param
#' @param
#' @param
#' @return

## Function definition
convert.matrix.to.long <- function( matrices ){
	
	if( !is.list( matrices ) ) matrices <- list( matrices )
	
	.convert.matrix.to.long <- function( no ){
	
		# Convert the matrix to a data frame
		df <- as.data.frame(matrices[no])
		
		# Convert column names to numbers
		colnames(df) <- seq_len(ncol(df))
		
		# Convert the matrix to a long data frame
		df_long <- as.data.frame(as.table(as.matrix(df)))
		
		# Rename the columns appropriately
		colnames(df_long) <- c("row","column","start.value")
		
		# Convert row and column identifiers to numeric
		df_long$row <- as.numeric(df_long$row)
		df_long$column <- as.numeric(df_long$column)
	
		# matrix name
		df_long$matrix <- names( matrices )[no]
		
		# column sort
		df_long <- df_long[, c( "matrix", colnames( df_long )[!colnames(df_long) %in% "matrix"] )]
			
		return( df_long )
	}	
	
	long.list <- sapply( seq(along=matrices), .convert.matrix.to.long, simplify=FALSE )
	df_long <- do.call( "rbind", long.list )
	rownames( df_long ) <- seq( along=rownames( df_long ) )

	return( df_long )
}


### development

# user.profile <- shell( "echo %USERPROFILE%", intern=TRUE )
# Rfiles.folder <- file.path( user.profile,
                                    # "Dropbox/96_mutualism/mutualism/R" )
# Rfiles <- list.files( Rfiles.folder , pattern="*.R" )
# Rfiles <- Rfiles[ !Rfiles %in% c("convert.matrix.to.long.R") ]
# for( Rfile in Rfiles ){
	# source( file.path( Rfiles.folder, Rfile ) )
# }

### test
# require( testthat )
# test_file("../tests/testthat/XXXXX.R")
