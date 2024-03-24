
run.jags <- function ( env ) {

		# get variables from env
		eval( parse( text=paste0( "assign( '",ls(envir=env), "' , get('",ls(envir=env),"', envir=env ) )" ) ) )
		
		# defaults fuer jags
		if (!exists("iter",mode="numeric")) iter <- 10
		if (!exists("chains",mode="numeric")) chains <- 2
		if (!exists("adapt",mode="numeric")) adapt <- 0
		if (!exists("thin",mode="numeric")) thin <- 1

		# output
		if ( verbose ){
				cat( paste0( "Running with...", "\n" ) )
				cat( paste0( "   adaption iterations: ", adapt, "\n" ) )
				cat( paste0( "            iterations: ", iter, "\n" ) )
				cat( paste0( "                chains: ", chains, "\n" ) )
				cat( paste0( "     thinning interval: ", thin, "\n" ) )
				cat( paste0( "\n" ) )
				flush.console()
		}
		
		# probably better to push those to data.env
		# assign( "iter", iter, envir=globalenv() )
		# assign( "chains", chains, envir=globalenv() )
		# assign( "adapt", adapt, envir=globalenv() )
		# assign( "thin", thin, envir=globalenv() )
		
		# write bugs file to disk
		dir <- file.path( work.dir, model.name )
		if ( !dir.exists( dir ) ) dir.create( dir )
		bugs.file <- file.path( dir, paste0( model.name, ".bug" ) )
		write.table( syntax, file=bugs.file, quote=FALSE, row.names = FALSE, col.names = FALSE )
# browser()	
		# rjags has problems with environments
		# hard set variables
		# ini.row <- which(grepl("^ini", call[,1]))
		# call[ini.row,1] <- sub( "=chains", paste0("=",chains), call[ini.row,1] )
		# call[ini.row,1] <- sub( "=adapt", paste0("=",adapt), call[ini.row,1] )
		# run.row <- which(grepl("^res", call[,1]))
		# call[run.row,1] <- sub( "=iter", paste0("=",iter), call[run.row,1] )
		# call[run.row,1] <- sub( "=thin", paste0("=",thin), call[run.row,1] )
		
		### run syntax
		
		# identify jags parallel model block
# browser()		
		m1 <- which( grepl( "^# parallel chains", call ) )
		if( !identical( m1, integer(0) ) ){
				m2 <- which( grepl( "^}$", call ) )[1]		
# browser()			
				# before
				for (z in 1:(m1-1)){
						if( verbose ) { cat( paste0( call[z,], "\n" ) ); flush.console() }
						eval( parse( text= call[z,] ) )
				}
				
				# block
				if( verbose ) { cat( paste0( call[m1:m2,], "\n" ) ); flush.console() }
				eval( parse( text= call[m1:m2,] ) )
				
				# after
				for (z in (m2+1):nrow(call)){
						if( verbose ) { cat( paste0( call[z,], "\n" ) ); flush.console() }
						eval( parse( text= call[z,] ) )
				}
		} else {
				## no parallel run
				# run call step by step
				for (z in 1:nrow(call)){
						if( verbose ) { cat( paste0( call[z,], "\n" ) ); flush.console() }
						eval( parse( text= call[z,] ) )
				}
		}
		
# browser()		
		# return list
		ret <- list()

		# first entry: model name
		ret$model.name <- model.name		
		
		# second entry: engine
		ret$engine <- engine

		# third entry: runtime
		ret$runtime <- runtime		
		
		# fourth entry: results
		ret$results <- res
		names( ret$results )

		## fifth entry: parameters
		# for better usability parameter from par.env will be put into list
		pl <- list()
		# do <- paste0( "pl$'", ls( envir=get("par.env",env) ) , "' <- get( '", ls( envir=get("par.env",env) ) ,"', envir=get('par.env',env) )" )
		do <- paste0( "pl$'", ls( envir=par.env ) , "' <- get( '", ls( envir=par.env ) ,"', envir=par.env )" )
		eval( parse( text=do ) )
		ret$parameters <- pl
		names( ret$parameters )

		## sixth entry: seeds
		ret$seeds <- seeds	
		
		## seventh entry: run parameter
		ret$runpar <- list( "adapt"=adapt, "iter"=iter, "chains"=chains, "thin"=thin )
		
		## sortieren von results/parameters
		first <- c("A","Q","b","beta","mu.beta","prec.beta")
			
		# sortieren von results (second entry)
		ord <- match( first, names(ret$results) )
		ord <- ord[!is.na(ord)]
		if( !identical( ord, integer(0) ) ) ret$results <- c( ret$results[ord], ret$results[ !ret$results %in% ret$results[ord] ] )
		rm("ord")
# browser()
		# sortieren von parameters (third entry)
		ord <- match( first, names(ret$parameters) )
		ord <- ord[!is.na(ord)]
		if( !identical( ord, integer(0) ) ) ret$parameters <- c( ret$parameters[ord], ret$parameters[ !ret$parameters %in% ret$parameters[ord] ] )

		
		# return
		return( ret )
}
