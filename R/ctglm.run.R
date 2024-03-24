
ctglm.run <- function ( s, work.dir=getwd(), verbose=TRUE, ... ) {
		
# browser()
		# new environment
		env <- new.env()
		
		# put values of list s into environment
		eval( parse ( text=paste0( "assign( '",names(s), "' , s$'",names(s),"' , envir=env )" ) ) )
		# put all variables (values of arguments of function) into environment
		vars <- ls()[ !ls() %in% "s" ]
		eval( parse ( text=paste0( "assign( '",vars, "' , get('",vars,"') , envir=env )" ) ) )
		# additional arguments from ...
		if( length( list(...) ) > 0 ) {
				eval( parse ( text=paste0( "assign( '",names(list(...)), "' , list(...)$'",names(list(...)),"' , envir=env )" ) ) )
		}
# browser()			
		### call software specific run
		if( get( "engine", envir=env ) %in% "jags" ) {
				r <- run.jags( env )
		}
		if( get( "engine", envir=env ) %in% "ctsem" ) {
				r <- run.ctstan.ctsem( env, mode="ctsem" )
		}
		if( get( "engine", envir=env ) %in% "ctstan" ) {
				r <- run.ctstan.ctsem( env, mode="ctstan" )
		}
		
		if (verbose) cat( paste0( "\nMODEL SUCCESSFULLY RAN | proceed with ctglm.results() \n" ) )
		
		# return
		return( r )
}
