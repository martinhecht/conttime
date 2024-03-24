
check.input <- function ( env ) {
		
		# get variables from env
		eval( parse ( text=paste0( "assign( '",ls(envir=env), "' , get('",ls(envir=env),"', envir=env ) )" ) ) )
		
		# error vector for console output
		error <- character(0)
		
		# if verbose is not logical set to FALSE
		if (!is.logical( verbose ) ) verbose <- FALSE
		# overwrite in environment
		obj <- c( "verbose" )
		eval( parse ( text=paste0( "assign( '",obj, "' , get('",obj,"') , envir=env )" ) ) )		
		
		# console output
		if ( verbose ) cat("checking input\n\n")
# browser()
		# d, data.frame
		if ( verbose ) cat( paste0( "                    d is data.frame (or matrix): "  ) )
		if ( is.data.frame( d ) || is.matrix( d ) ) { if ( verbose ) cat( "OK" ) } 
		   else { if ( verbose ) cat( "FAIL\n" ); error[length(error)+1] <- paste0( "d is not a data.frame (or matrix) | check d" ) }
		if ( is.matrix( d ) ) { d <- as.data.frame( d ); if ( verbose ) cat( " (matrix converted to data.frame)\n" ) } else { if ( verbose ) cat( "\n" ) }
# browser()
		# d, non-empty
		if ( verbose ) cat( paste0( "                                 d is not empty: "  ) )
		if ( nrow(d)>0 & ncol(d)>0 ) { if ( verbose ) cat( "OK\n" ) } 
		   else { if ( verbose ) cat( "FAIL\n" ); error[length(error)+1] <- paste0( "d is empty | check d" ) }
# browser()		
		# id, character
		if ( verbose ) cat( paste0( "                id is character and of length 1: ") )
		if ( is.character( id ) && length( id ) == 1 ) { if ( verbose ) cat( "OK\n" ) }
		   else { if ( verbose ) cat( "FAIL\n" ); error[length(error)+1] <- paste0( "id is not character or not of length 1 | check id" ) }
		# time, character
		if ( verbose ) cat( paste0( "              time is character and of length 1: ") )
		if ( is.character( time ) && length( time ) == 1 ) { if ( verbose ) cat( "OK\n" ) }
		   else { if ( verbose ) cat( "FAIL\n" ); error[length(error)+1] <- paste0( "time is not character or not of length 1 | check time" ) }
	    # timepoint_sep, character vector
		# if ( verbose ) cat( paste0( "              timepoint.sep is character vector: "  ) )
		# if ( is.character(timepoint.sep) ) { if ( verbose ) cat( "OK\n" ) }
		   # else { if ( verbose ) cat( "FAIL\n" ); error[length(error)+1] <- paste0( "timepoint.sep is not character | check timepoint.sep" ) }
		# lag.names, NULL or character vector
		# if ( verbose ) cat( paste0( "          lag.names is NULL or character vector: "  ) )
		# if ( is.null(lag.names) || is.character(lag.names) ) { if ( verbose ) cat( "OK\n" ) }
		   # else { if ( verbose ) cat( "FAIL\n" ); error[length(error)+1] <- paste0( "lag.names is not NULL or character vector | check lag.names" ) }
		# Lambda, NULL or two-dimensional matrix
		if ( verbose ) cat( paste0( "       Lambda is NULL or two-dimensional matrix: "  ) )
		if ( is.null(Lambda) || (is.matrix(Lambda) & length(dim(Lambda))==2) ) { if ( verbose ) cat( "OK\n" ) }
		   else { if ( verbose ) cat( "FAIL\n" ); error[length(error)+1] <- paste0( "Lambda is not NULL or two-dimensional matrix | check Lambda" ) }
		# measurement.model
		if ( verbose ) cat( paste0( "                 measurement.model is supported: "  ) )
		if ( ( measurement.model$family == "gaussian" & measurement.model$link == "identity" ) || ( measurement.model$family == "binomial" & measurement.model$link == "logit" ) || ( measurement.model$family == "poisson" & measurement.model$link == "log" ) ) { if ( verbose ) cat( "OK\n" ) }
		   else { if ( verbose ) cat( "FAIL\n" ); error[length(error)+1] <- paste0( "measurement.model is not gaussian(link='identity'), binomial(link='logit'), or poisson(link='log') | check measurement.model" ) }
		# priors, NULL or list
		if ( verbose ) cat( paste0( "                         priors is NULL or list: "  ) )
		if ( is.null( priors ) || is.list( priors ) ) { if ( verbose ) cat( "OK\n" ) }
		   else { if ( verbose ) cat( "FAIL\n" ); error[length(error)+1] <- paste0( "priors is not NULL or list | check priors" ) }
		# A is two-dimensional matrix
		if ( exists( "A", inherits=FALSE ) ) {
				if ( verbose ) cat( paste0( "                    A is two-dimensional matrix: "  ) )
				if ( is.matrix(A) & length(dim(A))==2 ) { if ( verbose ) cat( "OK\n" ) }
				   else { if ( verbose ) cat( "FAIL\n" ); error[length(error)+1] <- paste0( "A is not two-dimensional matrix | check A" ) }
		}
		# Q is two-dimensional matrix
		if ( exists( "Q", inherits=FALSE ) ) {
				if ( verbose ) cat( paste0( "                    Q is two-dimensional matrix: "  ) )
				if ( is.matrix(Q) & length(dim(Q))==2 ) { if ( verbose ) cat( "OK\n" ) }
				   else { if ( verbose ) cat( "FAIL\n" ); error[length(error)+1] <- paste0( "Q is not two-dimensional matrix | check Q" ) }
		}
		# b is (column) vector
		if ( exists( "b", inherits=FALSE ) ) {
				if ( verbose ) cat( paste0( "                           b is (column) vector: "  ) )
				# if is vector convert to column vector
				if ( is.vector(b) ) {
						b <- matrix( b, ncol=1 )
						# write to environment
						eval( parse ( text=paste0( "assign( 'b' , get('b') , envir=env )" ) ) )
				}
				if ( is.matrix(b) & dim(b)[2]==1 ) { if ( verbose ) cat( "OK\n" ) }
				   else { if ( verbose ) cat( "FAIL\n" ); error[length(error)+1] <- paste0( "b is not a (column) vector | check b" ) }
		}
		# E is two-dimensional matris
		if ( exists( "E", inherits=FALSE ) ) {
				if ( verbose ) cat( paste0( "              E is two-dimensional matrix: "  ) )
				if ( is.matrix(E) & length(dim(E))==2 ) { if ( verbose ) cat( "OK\n" ) }
				   else { if ( verbose ) cat( "FAIL\n" ); error[length(error)+1] <- paste0( "E is not two-dimensional matrix | check E" ) }
		}

		
		# independent of verbose, output errors to console
		if( length(error)>0 ) {
				cat( paste0( "\nERRORS occured:\n" ) )
				cat( paste0( paste( paste0("[",seq(along=error),"] ",error), collapse="\n" ), "\n" ) )
				
				# stop program
				stop("\nprogram aborted\n", call. = FALSE)
		} else {
				if ( verbose ) cat("\n")
		}
		
		# if any errors return FALSE	
		# if( length(error)>0 ) return( FALSE ) else return( TRUE )
		# return
		TRUE
}
