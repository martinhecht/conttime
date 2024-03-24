
ctglm.syntax <- function ( m, model.name="model", cores=detectCores(), ..., verbose=TRUE ) {
		
# browser()
		# cores modden
		if( !exists("cores") ) cores <- 1L
		cores <- get.cores( cores )
		
		# new environment
		env <- new.env()
		
		# put all variables (values of list m) into environment
		eval( parse ( text=paste0( "assign( '",names(m), "' , m$'",names(m),"' , envir=env )" ) ) )
		# put additional vars into environment
		vars <- c("model.name","cores","verbose")
		eval( parse ( text=paste0( "assign( '",vars, "' , get('",vars,"') , envir=env )" ) ) )
		
		# track.include / track.exclude zu Liste falls noch nicht
		if( exists( "track.include", env ) ){
				track.include <- get("track.include",envir=env)
				if ( !is.list( track.include ) ) track.include <- eval( parse( text=paste0("list(", paste( paste0("'",track.include,"'=NULL"), collapse="," ) , ")") ) )
				assign( "track.include", track.include, envir=env )
				rm( "track.include" )
		}
		
		# additional arguments from ...
		if( length( list(...) ) > 0 ) {
				# merge user track.include / track.exclude with potential t.i/t.e from m
				if( "track.include" %in% names(list(...)) ){
						track.include1 <- list(...)$'track.include'
						if ( !is.list( track.include1 ) ) track.include1 <- eval( parse( text=paste0("list(", paste( paste0("'",track.include1,"'=NULL"), collapse="," ) , ")") ) )
						if ( !exists( "track.include", envir=env ) ){
								assign( "track.include", track.include1, envir=env )
						} else {
								# merge
								track.include2 <- get("track.include",envir=env)
								track.include.new <- c( track.include2, track.include1 )
								track.include.new <- track.include.new[ !duplicated( names( track.include.new ) ) ]
								assign( "track.include", track.include.new, envir=env )
						}
				}
				if( "track.exclude" %in% names(list(...)) ){
						track.exclude1 <- list(...)$'track.exclude'
						if ( !exists( "track.exclude", envir=env ) ){
								assign( "track.exclude", track.exclude1, envir=env )
						} else {
								# merge
								track.exclude2 <- get("track.exclude",envir=env)
								track.exclude.new <- unique( c( track.exclude2, track.exclude1 ) )
								assign( "track.exclude", track.exclude.new, envir=env )
						}
				}
# browser()		
				# further arguments from ...
				furth.args <- names( list(...) )[ !names( list(...) ) %in% c("track.include","track.exclude") ]
				if ( length( furth.args ) > 0 ) {
						eval( parse ( text=paste0( "assign( '",furth.args, "' , list(...)$'",furth.args,"' , envir=env )" ) ) )
				}
		}

		# make matrices and prior matrices consistent
		### MAY CAUSE PROBLEMS
		invisible( make.matrices.consistent( env , mods = c(1,2,3,4,5,6) ) )
# browser()	
		### call software specific syntax/call generator
		if( get( "engine", envir=env ) %in% "jags" ) {
				s <- create.jags.syntax( env )
		} else if( get( "engine", envir=env ) %in% "ctstan" ) {
				s <- create.ctstan.ctsem.syntax( env, mode="ctstan" )
		} else if( get( "engine", envir=env ) %in% "ctsem" ) {
				s <- create.ctstan.ctsem.syntax( env, mode="ctsem" )
		}
# browser()
# get("Q",envir=env)

		if (verbose) cat( paste0( "SYNTAX SUCCESSFULLY CREATED | proceed with ctglm.run() \n" ) )
		
		# return
		return( s )
}

get.cores <- function(cores){
		if( !is.numeric(cores) ) cores <- 1L
		if( is.numeric(cores) && !is.integer(cores) ) cores <- as.integer( floor( cores ) )
		if( is.numeric(cores) && cores < 1 ) cores <- 1L
		if( is.numeric(cores) && ( cores.max <- detectCores() ) < cores ) cores <- cores.max
		return(cores)
}
