
merge.results <- function( ..., consistent=TRUE, noNAcol=TRUE, pattern="\\.\\b(Rdata|rda)\\b$", results.identifier=NULL ) {
		
		# packages
		requireNamespace( "plyr" ) # rbind.fill
		
		# default results.identifier (for ctglm results)
		if( is.null( results.identifier ) ) results.identifier <- 'is.data.frame( d ) && all( c( "model.name","engine","name","variable","mode","median","mean" ) %in% colnames( d ) )'
		
		# arguments from ...
		# if( length( list(...) ) > 0 ) {
				# eval( parse ( text=paste0( "assign( '",names(list(...)), "' , list(...)$'",names(list(...)),"' , envir=env )" ) ) )
		# }
		arg <- list(...)
# browser()		
		## identify if
		# folder
		fols <- arg[ sapply( arg, dir.exists ) ]
		# TODO: results
		# TODO: list of results
		
		
		### folders
		if ( length( fols ) > 0 ) {
# browser()				
				fls.l <- sapply( fols, list.files, pattern=pattern, full.names=TRUE, simplify=FALSE )
				fls <- do.call( "rbind", fls.l )

				load.fls <- function( fl, verbose=TRUE ) {
				# browser()
						# output
						if( verbose ){
								cat( paste0( fl, "" ) ); flush.console()
						}
						
						# load file
						d.nam <- load( fl )
						# TODO: check all objects in file, not only first
						d <- get( d.nam[1] )
						
						# identify if its results object
						if( eval( parse( text=results.identifier ) ) ) {
								ret <- d
								if( verbose ) cat( " -> added to results\n" )
						} else {
								ret <- NULL
								if( verbose ) cat( " -> NO results\n" )
						}
						
						return( ret )
				}
				d.l <- sapply( fls, load.fls, simplify=FALSE )
				d <- do.call( "rbind.fill", d.l )
				rownames( d ) <- seq( along=rownames( d ) )

				# keep only parameter that are in all analyses
				if ( consistent ) {
						var.nams <- sapply( d.l, function( d ) unique( d$variable ), simplify=FALSE )
						var.nams <- var.nams[!sapply(var.nams,is.null)]
						vars <- Reduce(function(x, y) intersect(x, y),var.nams,accumulate=FALSE )

						d <- d[ d$variable %in% vars, ]
				}

				# delete columns with NA
				if ( noNAcol ){
						del <- sapply( d, function( cl ) any( is.na( cl ) ) )
						d <- d[,!del,drop=FALSE]
				}
		} else {
				d <- NULL
		} ### end of folders
		
		
		
		return( d )
}
