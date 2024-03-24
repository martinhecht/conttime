
ctglm.save.syntax <- function( s, dir, ... ) {
# browser()
		# new environment
		env <- new.env()
		
		# put values of list s into environment
		eval( parse ( text=paste0( "assign( '",names(s), "' , s$'",names(s),"' , envir=env )" ) ) )
		# put all variables (values of arguments of function) into environment
		vars <- ls()[ !ls() %in% "s" ]
		eval( parse ( text=paste0( "assign( '",vars, "' , get('",vars,"') , envir=env )" ) ) )
		# additional arguments from ...
		# if( length( list(...) ) > 0 ) {
				# eval( parse ( text=paste0( "assign( '",names(list(...)), "' , list(...)$'",names(list(...)),"' , envir=env )" ) ) )
		# }
		# put values from data.env into environment
		eval( parse( text=paste0( "assign( '",ls(envir=s$data.env), "' , get('",ls(envir=s$data.env),"', envir=s$data.env ), envir=env )" ) ) )
		
		# put values from data.env into environment
		eval( parse( text=paste0( "assign( '",ls(envir=s$par.env), "' , get('",ls(envir=s$par.env),"', envir=s$par.env ), envir=env )" ) ) )		
		
		### Rdata
		path.rdata <- file.path( dir, paste0( get( "model.name", envir=env ), ".Rdata" ) )
		
		# save
		save( list=ls(envir=env), file=path.rdata, envir=env )
		
		### Model call
		# modify to include load of objects
		# i.e. load objects from dir/file.name
		call <- get( "call", envir=env )
		
		# if jags, remove # in #require(ctglm)
		if( s$engine %in% "jags" ){
				if ( length( indd <- grep( "#require( 'ctglm' )", call, fixed=TRUE ) ) == 1 ) {
						call[ indd ] <- sub( "#", "", call[ indd ] )
				}
		}
		
		# where is ### engine
		ind <- grep( "### engine:", call[,1] )
		# add load
		add <- matrix( paste0( "# load data/objects" ), ncol=1 )
		add <- rbind( add, paste0( "( load( '", path.rdata, "' ) )" ) )
		add <- rbind( add, "" )
		# generate/add bugs file for jags
		if( s$engine %in% "jags" ){
				bugs.file <- file.path( dir, paste0( get( "model.name", envir=env ), ".bugs" ) )
				write.table( s$syntax, bugs.file, quote=FALSE, row.names=FALSE, col.names=FALSE )
				add <- rbind( add, paste0( "# bugs file" ) )
				add <- rbind( add, paste0( "bugs.file <- '", bugs.file, "'" ) )
				add <- rbind( add, "" )				
		}
		# at this point no run parameters are usually not set, default
		add <- rbind( add, '# run parameters defaults' )
		add <- rbind( add, 'if (!exists("iter",mode="numeric")) iter <- 10' )
		add <- rbind( add, 'if (!exists("chains",mode="numeric")) chains <- 2' )
# browser()		
		# if jags, add adapt and thin
		if( s$engine %in% "jags" ){
				add <- rbind( add, 'if (!exists("adapt",mode="numeric")) adapt <- 0' )
				add <- rbind( add, 'if (!exists("thin",mode="numeric")) thin <- 1' )
		}
		
		add <- rbind( add, "" )

		# modified call
		call2 <- do.call( "rbind", list( call[1:(ind+1),,drop=FALSE], add, call[(ind+2):nrow(call),,drop=FALSE] ) )		

		# write call
		path.rcall <- file.path( dir, paste0( get( "model.name", envir=env ), ".R" ) )		
		write.table( call2, file=path.rcall, row.names=FALSE, col.names=FALSE, quote=FALSE )
		
		## save stan syntax
		if( s$engine %in% "ctstan" ){
				
				call3 <- call2
	# browser()			
				# create dry run (fit=FALSE) 
				call3[ grep( "fit=TRUE", call3[,1] ) ] <- "fit=FALSE,"
				# no output with print
				call3[ grep( "print", call3[,1], fixed=TRUE ) ] <- ""
				# no output of ctStanFit
				call3[ grep( "m <- ctModel( Tpoints=T,", call3[,1], fixed=TRUE ) ] <- "m <- invisible( ctModel( Tpoints=T,"
				# somewhat dangerous here with the ) , might match other line too, might be accidently changed in create syntax
				call3[ grep( "            )", call3[,1], fixed=TRUE ) ] <- "            ))"
								
				# no output of ctStanFit
				call3[ grep( "r <- ctStanFit( datalong=d,", call3[,1], fixed=TRUE ) ] <- "r <- invisible( ctStanFit( datalong=d,"
				# somewhat dangerous here with the ) , might match other line too, might be accidently changed in create syntax
				call3[ grep( "              )", call3[,1], fixed=TRUE ) ] <- "              ))"
				
				
				# identify ctstan model block
				m1 <- which( grepl( "m <- invisible( ctModel( Tpoints=T,", call3, fixed=TRUE ) )
				m2 <- which( grepl( "^\\s*\\))\\s*$", call3 ) )[1]
				
				# identify run block
				r1 <- which( grepl( "r <- invisible( ctStanFit( datalong=d,", call3, fixed=TRUE ) )
				r2 <- which( grepl( "^\\s*\\))\\s*$", call3 ) )[2]
	
				# run call
				for (z in 1:(m1-1)){
						# if( TRUE ) { cat( paste0( call3[z,], "\n" ) ); flush.console() }
						eval( parse( text= call3[z,] ) )
				}
				eval( parse( text= call3[m1:m2,] ) )
				for (z in (m2+1):(r1-1)){
						# if( TRUE ) { cat( paste0( call[z,], "\n" ) ); flush.console() }
						eval( parse( text= call3[z,] ) )
				}
				# if( TRUE ) { cat( paste0( call[r1:r2,], "\n" ) ); flush.console() }
				eval( parse( text= call3[r1:r2,] ) )
				for (z in (r2+1):nrow(call3)){
						# if( TRUE ) { cat( paste0( call[z,], "\n" ) ); flush.console() }
						eval( parse( text= call3[z,] ) )
				}		
# browser()				
				path.stan.syntax <- file.path( dir, paste0( get( "model.name", envir=env ), ".stan" ) )		
				write.table( r, file=path.stan.syntax, row.names=FALSE, col.names=FALSE, quote=FALSE )
				
		}

		
		return( TRUE )
	
}
