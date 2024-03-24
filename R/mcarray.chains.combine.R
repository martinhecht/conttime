
mcarray.chains.combine <- function( x ){
		
		requireNamespace( "abind" )
		
		f1 <- function( nr ) {

				r <- sapply( x, "[[", nr, simplify=FALSE )

				y <- do.call( "abind", r )
				attr( y, "dimnames" ) <- NULL

				return( array2mcarray( y, nam=attributes(r[[1]])$varname ) )
				
		}
		y <- sapply( 1:length( x[[1]] ), f1, simplify=FALSE )
# browser()		
		names( y ) <- names( x[[1]] )
		return( y )
}

array2mcarray <- function( x, nam="" ) {
		Ndim <- length( attributes(x)$dim ) - 2 
		attr(x,"class") <- "mcarray"
		attr(x,"varname") <- nam
		#attributes(y)$dim <- c( dim(x)[-c(length(dim(x))-1,length(dim(x)))], chainlength-burnin, dim(x)[length(dim(x))] )
		names( attributes(x)$dim ) <- eval( parse( text = paste0 ( "c(",paste(rep("''",Ndim),collapse=","),",'iteration','chain')" ) ) )
		return( x )
}

