
check.consistency <- function ( env ) {
		
		# get variables from env
		eval( parse ( text=paste0( "assign( '",ls(envir=env), "' , get('",ls(envir=env),"', envir=env ) )" ) ) )
		
		# error vector for console output
		error <- character(0)
		
		# console output
		if ( verbose ) cat("checking consistency\n\n")


		# F must be 1 or 2 for jags
		if ( engine %in% c("jags") ) {
				if ( verbose ) cat( paste0( "            number of latent variables (F) is 2: "  ) )
				if ( F %in% c(1,2) ) { if ( verbose ) cat( "OK\n" ) } 
				   else { if ( verbose ) cat( "FAIL\n" ); error[length(error)+1] <- paste0( "number of latent variables (F) is not 1 or 2, the currently only supported option for engine=\"jags\" | check Lambda" ) }
		}
		
		### measurement stuff ###
		# Lambda
		if ( verbose ) cat( paste0( "             loading matrix Lambda is IxF (", I, "x", F, "): "  ) )
		if ( identical( dim(Lambda), c(I,F) ) ) { if ( verbose ) cat( "OK\n" ) } 
		   else { if ( verbose ) cat( "FAIL\n" ); error[length(error)+1] <- paste0( "loading matrix Lambda is not IxFxT (", I, "x", F, ") | check Lambda matrix" ) }		

		# beta
		if ( verbose ) cat( paste0( "             item easiness vector beta is I (", I, "): "  ) )
		if ( length(beta)==I ) { if ( verbose ) cat( "OK\n" ) } 
		   else { if ( verbose ) cat( "FAIL\n" ); error[length(error)+1] <- paste0( "     item easiness vector beta is not of length I (", I, ") | check data and/or beta vector" ) }

	   # E
		if ( measurement.model$family == "gaussian" ) {		
				# IxI
				if ( verbose ) cat( paste0( "        measurement error matrix E is IxI (", I, "x", I, "): "  ) )
				if ( identical( dim(E), c(I,I) ) ) { if ( verbose ) cat( "OK\n" ) } 
				   else { if ( verbose ) cat( "FAIL\n" ); error[length(error)+1] <- paste0( "        measurement error matrix E is not IxI (", I, "x", I, ") | check data and/or E matrix" ) }						
				# symmetric matrix
				if ( verbose ) cat( paste0( "                                 E is symmetric: "  ) )
				if ( identical( E[lower.tri( E )], E[upper.tri( E )] ) ) { if ( verbose ) cat( "OK\n" ) } 
				   else { if ( verbose ) cat( "FAIL\n" ); error[length(error)+1] <- paste0( "measurement error matrix E is not symmetric | check E" ) }
				   # uncorrelated errors
				if ( verbose ) cat( paste0( "                uncorrelated measurement errors: "  ) )
				if ( all( c( E[lower.tri( E )], E[upper.tri( E )] ) == 0 ) ) { if ( verbose ) cat( "OK\n" ) } 
				   else { if ( verbose ) cat( "FAIL\n" ); error[length(error)+1] <- paste0( "off-diagonal elements of measurement error matrix are not 0 (correlated errors are currently not supported) | check E matrix" ) }		
		}
		
		### continuous time stuff ###
		# A
		if ( verbose ) cat( paste0( "                    drift matrix A is FxF (", F, "x", F, "): "  ) )
		if ( identical( dim(A), c(F,F) ) ) { if ( verbose ) cat( "OK\n" ) } 
		   else { if ( verbose ) cat( "FAIL\n" ); error[length(error)+1] <- paste0( "drift matrix A is not FxF (", F, "x", F, ") | check Lambda and/or A matrix" ) }
		
# browser()		
		## process error matrix Q
		# in jags/ctsem Q
		# in ctstan cholQ
		if ( engine %in% c("jags") ) {
				# Q, FxF
				if ( verbose ) cat( paste0( "            process error matrix Q is FxF (", F, "x", F, "): "  ) )
				if ( identical( dim(Q), c(F,F) ) ) { if ( verbose ) cat( "OK\n" ) } 
				   else { if ( verbose ) cat( "FAIL\n" ); error[length(error)+1] <- paste0( "drift matrix Q is not FxF (", F, "x", F, ") | check Lambda and/or Q matrix" ) }
				# Q, symmetric matrix
				if ( verbose ) cat( paste0( "                                 Q is symmetric: "  ) )
				if ( identical( Q[lower.tri( Q )], Q[upper.tri( Q )] ) ) { if ( verbose ) cat( "OK\n" ) } 
				   else { if ( verbose ) cat( "FAIL\n" ); error[length(error)+1] <- paste0( "process error matrix Q is not symmetric | check Q matrix" ) }		
		}
		if ( engine %in% c("ctstan","ctsem") ) {
				# cholQ, FxF
				if ( verbose ) cat( paste0( "        process error matrix cholQ is FxF (", F, "x", F, "): "  ) )
				if ( identical( dim(cholQ), c(F,F) ) ) { if ( verbose ) cat( "OK\n" ) } 
				   else { if ( verbose ) cat( "FAIL\n" ); error[length(error)+1] <- paste0( "drift matrix cholQ is not FxF (", F, "x", F, ") | check Lambda and/or cholQ matrix" ) }
				# cholQ, symmetric matrix
				# if ( verbose ) cat( paste0( "                             cholQ is symmetric: "  ) )
				# if ( identical( cholQ[lower.tri( cholQ )], cholQ[upper.tri( cholQ )] ) ) { if ( verbose ) cat( "OK\n" ) } 
				   # else { if ( verbose ) cat( "FAIL\n" ); error[length(error)+1] <- paste0( "process error matrix cholQ is not symmetric | check cholQ matrix" ) }		
		}
		
		
		# b
		if ( verbose ) cat( paste0( "    continuous time intercept vector b is F (", F, "): "  ) )
		if ( identical( length(b), F ) ) { if ( verbose ) cat( "OK\n" ) } 
		   else { if ( verbose ) cat( "FAIL\n" ); error[length(error)+1] <- paste0( "continuous time intercept vector b is not of length F (", F, ") | check Lambda and/or b vector" ) }		
		# at the end line break
		if ( verbose ) cat( paste0( "\n" ) )

	
		return( error )

}
