
results.ctsem <- function ( env ) {
# browser()		
		# requireNamespace packages
		# requireNamespace( "ggplot2" )
		# requireNamespace( "shinystan" )
		# requireNamespace( "coda" )
		requireNamespace( "ctsem" )
		
		# get variables from env
		eval( parse( text=paste0( "assign( '",ls(envir=env), "' , get('",ls(envir=env),"', envir=env ) )" ) ) )
		
		# output
		if ( verbose ){
				cat( paste0( "Extracting results from ctsem/OpenMx object...", "\n" ) )
				# cat( paste0( "            iterations: ", iter, "\n" ) )
				# cat( paste0( "                chains: ", chains, "\n" ) )
				# cat( paste0( "     thinning interval: ", thin, "\n" ) )
				cat( paste0( "\n" ) )
				cat( paste0( "   fetching results:\n\n" ) )
				flush.console()
		}		
		
		# make list of all parameters
		pars.l <- mapply( get.par.list, r$parameters, names( r$parameters ), MoreArgs=list(mode="ctsem"), SIMPLIFY=FALSE )
		pars <- do.call( "rbind", pars.l )
		rownames( pars ) <- seq( along=rownames( pars ) )
# browser()
		extr <- function( z ) {
# browser()
				if ( verbose ) cat( paste0( "      ", z["parameter"], "\n" ) ); flush.console()
				ret <- data.frame( "name"=z["name"], "variable"=z["parameter"], "value"=eval( parse( text=z["call"] ) ), stringsAsFactors=FALSE )
				
				return( ret )
		}
		est.l <- apply( pars, 1, extr )
		est <- do.call( "rbind", est.l )
# browser()
		# rename parameters
		if( verbose ){
				cat( paste0( "\n" ) )
				cat( paste0( "   renaming results:\n\n" ) )
				if ( any( est$name %in% "chol.var.t1" ) ) cat( paste0( "      chol.var.t1 -> var.t1   (Note: ctModel requires chol.var.t1 as input, but ctFit/ctsem outputs var.t1)\n" ) )
				if ( any( est$name %in% "chol.var.b" ) ) cat( paste0( "      chol.var.b -> var.b   (Note: ctModel requires chol.var.b as input, but ctFit/ctsem outputs var.b)\n" ) )
				if ( any( est$name %in% "cholQ" ) ) cat( paste0( "      cholQ -> Q   (Note: ctModel requires cholQ as input, but ctFit/ctsem outputs Q)\n" ) )
		}
		
		# if ( any( est$name %in% "chol.var.t1" ) ) est$name[ est$name %in% "chol.var.t1" ] <- ""
		est$name <- sub( "^chol", "", est$name )
		est$name <- sub( "^\\.", "", est$name )
		est$variable <- sub( "^chol", "", est$variable )
		est$variable <- sub( "^\\.", "", est$variable )
		est$engine <- engine
		
		# return
		rownames( est ) <- seq( along=rownames( est ) )
		return( est )
}
