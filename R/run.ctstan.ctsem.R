
run.ctstan.ctsem <- function ( env, mode ) {
		
		# package
		# requireNamespace( "ctsem" )
		
		# get variables from env
		eval( parse( text=paste0( "assign( '",ls(envir=env), "' , get('",ls(envir=env),"', envir=env ) )" ) ) )
			
		# get variables from data.env
		eval( parse( text=paste0( "assign( '",ls(envir=data.env), "' , get('",ls(envir=data.env),"', envir=data.env ) )" ) ) )
		
		# get variables from par.env
		eval( parse( text=paste0( "assign( '",ls(envir=par.env), "' , get('",ls(envir=par.env),"', envir=par.env ) )" ) ) )

		if( mode %in% "ctstan" ) {
		
				# defaults fuer ctstan
				if (!exists("iter",mode="numeric")) iter <- 10
				if (!exists("chains",mode="numeric")) chains <- 2
				# if (!exists("adapt",mode="numeric")) adapt <- 0
				# if (!exists("thin",mode="numeric")) thin <- 1

				# output
				if ( verbose ){
						cat( paste0( "Running with...", "\n" ) )
						# cat( paste0( "   adaption iterations: ", adapt, "\n" ) )
						cat( paste0( "            iterations: ", iter, "\n" ) )
						cat( paste0( "                chains: ", chains, "\n" ) )
						# cat( paste0( "     thinning interval: ", thin, "\n" ) )
						cat( paste0( "\n" ) )
						flush.console()
				}
		}

		### run syntax

		# identify ctstan model block
		m1 <- which( grepl( "^m <- ", call ) )
		m2 <- which( grepl( "^\\s*\\)\\s*$", call ) )[1]
		
		# identify run block
		r1 <- which( grepl( "^r <- ", call ) )
		r2 <- which( grepl( "^\\s*\\)\\s*$", call ) )[2]
	
		# run call
		for (z in 1:(m1-1)){
				if( verbose ) { cat( paste0( call[z,], "\n" ) ); flush.console() }
				eval( parse( text= call[z,] ) )
		}
# browser()
		# prec.t1[ 1,2 ] <- NA
		# prec.t1 <- t( prec.t1 )
		
		if( verbose ) { cat( paste0( call[m1:m2,], "\n" ) ); flush.console() }
		eval( parse( text= call[m1:m2,] ) )
		for (z in (m2+1):(r1-1)){
				if( verbose ) { cat( paste0( call[z,], "\n" ) ); flush.console() }
				eval( parse( text= call[z,] ) )
		}
		if( verbose ) { cat( paste0( call[r1:r2,], "\n" ) ); flush.console() }
		eval( parse( text= call[r1:r2,] ) )
		for (z in (r2+1):nrow(call)){
				if( verbose ) { cat( paste0( call[z,], "\n" ) ); flush.console() }
				eval( parse( text= call[z,] ) )
		}		
		
	
		# return list
		ret <- list()

		# first entry: model name
		ret$model.name <- model.name
		
		# second entry: engine
		ret$engine <- engine

		# third entry: runtime
		ret$runtime <- runtime		
		
		# fourth entry: results
		ret$results <- r
		names( ret$results )

		## fifth entry: parameters
		# for better usability parameter from par.env will be put into list
		pl <- list()
		# do <- paste0( "pl$'", ls( envir=get("par.env",env) ) , "' <- get( '", ls( envir=get("par.env",env) ) ,"', envir=get('par.env',env) )" )
		do <- paste0( "pl$'", ls( envir=par.env ) , "' <- get( '", ls( envir=par.env ) ,"', envir=par.env )" )
		eval( parse( text=do ) )
		ret$parameters <- pl
		names( ret$parameters )

		## entry: seeds
		# ret$seeds <- seeds	
		
		## sixth entry: run parameter
		# ret$runpar <- list( "adapt"=adapt, "iter"=iter, "chains"=chains, "thin"=thin )
		if( mode %in% "ctstan" ) ret$runpar <- list( "iter"=iter, "chains"=chains )
		
		## sortieren von parameters
		first <- c("A","Q","b","beta","mu.beta","prec.beta")
# browser()
		# sortieren von parameters (third entry)
		ord <- match( first, names(ret$parameters) )
		ord <- ord[!is.na(ord)]
		if( !identical( ord, integer(0) ) ) ret$parameters <- c( ret$parameters[ord], ret$parameters[ !ret$parameters %in% ret$parameters[ord] ] )
		
		# return
		return( ret )
}
