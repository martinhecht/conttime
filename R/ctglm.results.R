
ctglm.results <- function ( r, plot.dir=NULL, plot.person.par=FALSE, cores=detectCores(), value=c("mode","median","mean"), verbose=TRUE, ... ) {
		
# browser()
		# warnings loeschen
		assign("last.warning", NULL, envir = baseenv())
		
		# create plot.dir if not exists
		if( !is.null( plot.dir ) && !dir.exists( plot.dir ) ) dir.create( plot.dir )

		# value auf 1 beschraenken
		if( length( value ) > 1 ) value <- value[1]
		
		# new environment
		env <- new.env()

		# cores modden
		if( !exists("cores") ) cores <- 1L
		cores <- get.cores( cores )
		assign( "cores", cores, envir=env )
		
		# put values of list r into environment
		eval( parse ( text=paste0( "assign( '",names(r), "' , r$'",names(r),"' , envir=env )" ) ) )
		# put all variables (values of arguments of function) into environment
# browser()		
		vars <- ls()[ !ls() %in% "r" ]
		eval( parse ( text=paste0( "assign( '",vars, "' , get('",vars,"') , envir=env )" ) ) )
		# additional arguments from ...
		if( length( list(...) ) > 0 ) {
				eval( parse ( text=paste0( "assign( '",names(list(...)), "' , list(...)$'",names(list(...)),"' , envir=env )" ) ) )
		}
# browser()			
		### call software specific results preparation
		if( get( "engine", envir=env ) %in% c("jags","ctstan") ) {
				e <- results.jags.ctstan( env, mode=get( "engine", envir=env ) )
		} else if( get( "engine", envir=env ) %in% c("ctsem") ) {
				e <- results.ctsem( env )
		} else {
				e <- NULL
		}

	
		## mod person results with original ids
		if( "original.ids" %in% names(list(...)) ){
				original.ids <- list(...)$original.ids
	
				# ct intercepts bj
				bj <- e$variable[grepl("^bj_",e$variable)]
				if( length( bj ) > 0 ){
						if( verbose ) cat( paste0("   modding person parameters bj with original ids\n") )
						bjd <- data.frame( "variable"=bj, "new.id" = sub("bj_","",bj) )
						bjd <- merge( bjd, original.ids, by="new.id", sort=FALSE )
						bjd$new.variable <- paste0( "bj_", bjd$original.id )
						cn <- colnames(e)
						e$nr <- 1:nrow(e)
						e <- merge( e, bjd[,c("variable","new.variable")], by="variable", all.x=TRUE, sort=FALSE )
						e$variable[e$variable %in% bjd$variable] <- e$new.variable[e$variable %in% bjd$variable]
						e <- e[ order( e$nr ) , ]
						rownames( e ) <- seq( along=rownames( e ) )
						e <- e[, cn, drop=FALSE ]
				}
				
				# mu.t1.j
				mu.t1.j <- e$variable[grepl("^mu\\.t1\\.j_",e$variable)]
				if( length( mu.t1.j ) > 0 ){
						if( verbose ) cat( paste0("   modding person parameters mu.t1.j with original ids\n") )
# browser()						
						mtd <- data.frame( "variable"=mu.t1.j, "new.id" = sub("mu\\.t1\\.j_","",mu.t1.j) )
						mtd <- merge( mtd, original.ids, by="new.id", sort=FALSE )
						mtd$new.variable <- paste0( "mu.t1.j_", mtd$original.id )
						cn <- colnames(e)
						e$nr <- 1:nrow(e)
						e <- merge( e, mtd[,c("variable","new.variable")], by="variable", all.x=TRUE, sort=FALSE )
						e$variable[e$variable %in% mtd$variable] <- e$new.variable[e$variable %in% mtd$variable]
						e <- e[ order( e$nr ) , ]
						rownames( e ) <- seq( along=rownames( e ) )
						e <- e[, cn, drop=FALSE ]
				}				
		}
		
# browser()		
		if ( !is.null( e ) ) {
				# add model names
				e$model.name <- get( "model.name", envir=env )
				vorn <- c("model.name","engine")
				e <- e[ , c(vorn, colnames(e)[!colnames(e) %in% vorn]) ]
		}
# browser()		
		if ( verbose ) {
				cat( paste0( "\nDONE | :-)\n" ) )				
		}
		
		# return
		return( e )
}
