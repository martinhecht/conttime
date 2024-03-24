
ctglm.model <- function ( d, id="id", time="time", person.var=c("b"=TRUE,"mu.t1"=TRUE), track.person.par=NULL, Lambda=NULL, measurement.model=binomial(link="logit"), engine=c("jags","ctstan","ctsem"), ..., priors=NULL, verbose=TRUE ) {
# browser()		
		# put all arguments from ... here
		# if( length( list(...) ) > 0 ) {
				# eval( parse ( text=paste0( "assign( '",names(list(...)), "' , list(...)$'",names(list(...)),"' )" ) ) )
		# }

		# engine
		if ( is.character( engine ) && length( engine ) > 1 ) engine <- engine[1] else if( !is.character(engine) || !any(engine %in% c("jags","ctstan","ctsem")) ) { if(verbose) cat(paste0("engine='",engine,"' is not correctly specified | engine set to 'jags'\n\n")); engine <- "jags" }
		
		# person.var
		# if( person.var["b"] && !person.var["mu.t1"] ){
				# if( verbose ) cat( paste0( "if 'b' varies over persons, 'mu.t1' should also vary over persons | person.var['mu.t1'] has been set to TRUE\n\n" ) )
				# person.var["mu.t1"] <- TRUE
		# }

		# track.person.par
		# if someone puts "b" in, convert to "bj"
		if( "b" %in% track.person.par ) track.person.par[ track.person.par %in% "b" ] <- "bj"
		# if someone puts "mu.t1" in, convert to "mu.t1.j"
		if( "mu.t1" %in% track.person.par ) track.person.par[ track.person.par %in% "mu.t1" ] <- "mu.t1.j"
		
		# new environment
		env <- new.env()
		
		# put all variables (values of arguments of function) into environment
		eval( parse ( text=paste0( "assign( '",ls(), "' , get('",ls(),"') , envir=env )" ) ) )
		# additional arguments from ...
		if( length( list(...) ) > 0 ) {
				eval( parse ( text=paste0( "assign( '",names(list(...)), "' , list(...)$'",names(list(...)),"' , envir=env )" ) ) )
		}

		# make user set matrices and prior matrices consistent
		invisible( make.matrices.consistent( env=env, mods=c(1,2,4,5) ) )
		
# browser()
		# test
		# get("Q",envir=env)
		# get("Q.prior",envir=env )			
		
		# check input
		invisible( check.input( env ) )
		
		# data preparation
		invisible( prep.data( env ) )
		
		# check consistency
		error <- check.consistency( env )
		
		# set priors
		invisible( set.priors( env ) )
		
		# make user set matrices and prior matrices consistent again
		# this time include mod 3 (constraint transformed to parameter; contraint)
# browser()
		invisible( make.matrices.consistent( env=env, mods=c(1,2,3,4,5,6) ) )		
		# invisible( make.matrices.consistent( env=env, mods=c(3,6) ) )	
		
		# independent of verbose, output errors to console
		if( length(error)>0 ) {
				cat( paste0( "ERRORS:\n" ) )
				cat( paste0( paste( paste0("[",seq(along=error),"] ",error), collapse="\n" ), "\n\n" ) )
				cat( paste0( "DO NOT RUN THE MODEL!\n" ) )
		} else {
				if (verbose) cat( paste0( "MODEL SUCCESSFULLY CREATED | proceed with ctglm.syntax() \n" ) )
		}
		
		# create jags syntax
		# invisible( create.jags.syntax( env ) )
		
		### return list ###
		
		# overwrite user specified prior list
		# assign( "priors", get( "prior", envir=env ), envir=env )
		
		# list elements
		el <- ls( envir=env )
		
		# remove arguments of ctirt.model and other objects
		el <- el[ !el %in% c("timepoint.sep","verbose","prior","env","lag.names") ]

# browser()
		
		## order of elements
		el <- el[ order(el) ]
		first <- c("d","col.id","col.item","col.time","col.y","A",ifelse(exists("A.prior",envir=env),"A.prior",NA),ifelse(exists("Q",envir=env),"Q",NA),ifelse(exists("Q.prior",envir=env),"Q.prior",NA),ifelse(exists("cholQ",envir=env),"cholQ",NA),ifelse(exists("cholQ.prior",envir=env),"cholQ.prior",NA),"b",ifelse(exists("b.prior",envir=env),"b.prior",NA),"beta",ifelse(exists("beta.prior",envir=env),"beta.prior",NA),ifelse(exists("mu.beta",envir=env),"mu.beta",NA),ifelse(exists("prec.beta",envir=env),"prec.beta",NA),ifelse(exists("prec.beta.prior",envir=env),"prec.beta.prior",NA),ifelse(exists("E",envir=env),"E",NA),ifelse(exists("mu.t1",envir=env),"mu.t1",NA),ifelse(exists("mu.t1.prior",envir=env),"mu.t1.prior",NA),ifelse(exists("prec.t1",envir=env),"prec.t1",NA),ifelse(exists("prec.t1.prior",envir=env),"prec.t1.prior",NA),"priors","F","I","J","T","R","P","Lambda","measurement.model")
		first <- first[!is.na(first)]
		last <- c("engine")
		el <- el[ c( match( first, el ) , which( !el %in% c( first, last ) ), match( last, el ) ) ]
	
		# create return list
		ret <- list()
		do <- paste0( "ret$'", el, "' <- get( '", el, "', envir=env )" )
		eval( parse( text = do ) )
		
		# remove environment
		rm( env )
		
		return( ret )
		
}

make.matrices.consistent <- function( env, known.matrices=NULL, mods=c(1,2,3,4,5,6,7) ) {
# browser()
		# known matrices
		if ( is.null( known.matrices ) ) known.matrices <- c("A","Q","cholQ","b","bj","Lambda","beta","E","prec.beta","mu.t1","mu.t1.j","prec.mu.t1.j","prec.t1","chol.var.t1","prec.b","chol.var.b")

		## prior mods functions
		# x ~ distr  zu  distr
		mod1 <- function(m) { m[ is.prior.formula( m ) & !is.na( m ) ] <- paste0( gsub( "\\s", "", sub( "^[^~].*~(.*)$", "\\1",m[ is.prior.formula( m ) & !is.na( m ) ]) ) ); return(m) }
		# set all parameters in M.prior to NA if not prior
		# cat(paste0(nam,"\n")); if(nam=="beta") browser(); 
		mod6 <- function(m,nam) { if( exists( paste0( nam, "" ), envir=matrix.env, mode="character" ) ) m[ is.parameter( get( paste0( nam, "" ), envir=matrix.env ) ) & is.code ( get( paste0( nam, "" ), envir=matrix.env ) ) ] <- NA; return( m ) }
		
		## matrices mods functions
		# x ~ distr  zu x
		mod2 <- function(m) { m[ is.prior.formula( m ) & !is.na( m )  ] <- paste0( gsub( "\\s", "", sub( "^([^~].*)~.*$", "\\1", m[ is.prior.formula( m ) & !is.na( m )  ] ) ) ); return(m) }
		# x <- constr  zu x; x <- constr
		mod3 <- function(m) { m[ is.constraint( m ) & !is.na( m )  ] <- paste0( gsub( "\\s", "", sub( "^([^<-].*)<-.*$", "\\1", m[ is.constraint( m ) & !is.na( m )  ] ) ), "; ", m[ is.constraint( m ) & !is.na( m )  ] ); return(m) }
		# priors in M zu NA
		mod4 <- function(m) { m[ is.prior( m ) & !is.na( m )  ] <- NA; return(m) }
		# x; x ~ distr   zu   x ~ distr
		mod5 <- function(m) { m[ is.par.plus.priorformula(m) ] <- paste0( gsub( "\\s", "", sub( "^[^;]*;(.*)$", "\\1", m[ is.par.plus.priorformula(m) ] ) ) ); return(m) }
		# delete code in parameters
		mod7 <- function(m) { m[ is.parameter(m) & is.code(m) ] <- paste0( gsub( "\\s", "", sub( "^([^;]*);(.*)$", "\\1", m[ is.parameter(m) & is.code(m) ] ) ) ); return(m) }
			
		# matrix.env
		matrix.env <- new.env()
		userset.matrices <- ls(envir=env)[ ls(envir=env) %in% known.matrices ]
		if ( !identical( userset.matrices, character(0) ) ) {
				# delete NULL
				# del <- sapply( userset.matrices, function( x ) is.null ( get ( x, envir=parent.env(environment()) ) ) )
				del <- sapply( userset.matrices, function( x ) is.null ( get ( x, envir=env ) ) )
				if ( any ( del ) ) {
						userset.matrices <- userset.matrices[!del]
				}
				# put all user set matrices in matrix.env
				if ( !identical( userset.matrices, character(0) ) ) {
						eval( parse ( text=paste0( "assign( '",userset.matrices, "' , get('",userset.matrices,"',envir=env) , envir=matrix.env )" ) ) )
				}
		}

		# prior.env
		prior.env <- new.env()
		userset.priors <- ls(envir=env)[ ls(envir=env) %in% paste0( known.matrices, ".prior" ) ]
		if ( !identical( userset.priors, character(0) ) ) {
				# delete NULL
				# del <- sapply( userset.priors, function( x ) is.null ( get ( x, envir=parent.env(environment()) ) ) )
				del <- sapply( userset.priors, function( x ) is.null ( get ( x, envir=env ) ) )
				if ( any ( del ) ) {
						userset.priors <- userset.priors[!del]
				}
				# put all user set priors in prior.env
				if ( !identical( userset.priors, character(0) ) ) {
						eval( parse ( text=paste0( "assign( '",userset.priors, "' , get('",userset.priors,"', envir=env) , envir=prior.env )" ) ) )
				}
		}
# browser()		

		
		# create M.prior for all userset M if not exists and M contains a prior definition
		if ( length( ls(envir=matrix.env) ) > 0 ) {
				notex <- !sapply( paste0(ls(envir=matrix.env),".prior"), exists, envir=prior.env )
				
				# is.prior.formula( get("Q",envir=matrix.env) )
				# is.prior( get("Q",envir=matrix.env) )
				
				priordef <- sapply( ls(envir=matrix.env), function(m) any( is.prior.formula( get(m,envir=matrix.env) ) ) | any( is.prior( get(m,envir=matrix.env) ) ) )
				create <- notex & priordef
				if ( any ( create ) ){
						eval( parse( text=paste0( "x<-get('",sub(".prior$","",names(create)[create]),"', envir=matrix.env); x[] <- NA; assign( '", names(create)[create] , "', x, envir=prior.env ); rm('x') " ) ) )
				}
		
				# set userdefined priors from M in M.prior
				## TODO, das hier nur machen wenn M und M.prior gleiche Struktur
				## sonst sinnlos
				## bzw. ermöglicht dies für ne ganze Matrix nen prior (z.B. Wishart) zu setzen
				if( any ( priordef ) ){
						eval( parse( text=paste0( "",names(priordef)[priordef],".prior[ is.prior.formula( get( '",names(priordef)[priordef],"', envir=matrix.env ) ) | is.prior( get( '",names(priordef)[priordef],"', envir=matrix.env ) ) ] <- get( '",names(priordef)[priordef],"', envir=matrix.env )[ is.prior.formula( get( '",names(priordef)[priordef],"', envir=matrix.env ) ) | is.prior( get( '",names(priordef)[priordef],"', envir=matrix.env ) ) ]" ) ), envir=prior.env )
				}
			
				## mods in matrices
				if( any( mods %in% 2 ) ) eval( parse( text=paste0( "assign('",ls(envir=matrix.env), "', mod2(get('",ls(envir=matrix.env),"',envir=matrix.env)), envir=matrix.env)" ) ) )
				if( any( mods %in% 3 ) ) eval( parse( text=paste0( "assign('",ls(envir=matrix.env), "', mod3(get('",ls(envir=matrix.env),"',envir=matrix.env)), envir=matrix.env)" ) ) )
				if( any( mods %in% 4 ) ) eval( parse( text=paste0( "assign('",ls(envir=matrix.env), "', mod4(get('",ls(envir=matrix.env),"',envir=matrix.env)), envir=matrix.env)" ) ) )
				if( any( mods %in% 5 ) ) eval( parse( text=paste0( "assign('",ls(envir=matrix.env), "', mod5(get('",ls(envir=matrix.env),"',envir=matrix.env)), envir=matrix.env)" ) ) )
				if( any( mods %in% 7 ) ) eval( parse( text=paste0( "assign('",ls(envir=matrix.env), "', mod7(get('",ls(envir=matrix.env),"',envir=matrix.env)), envir=matrix.env)" ) ) )
# browser()
				# m[ !is.prior( o ) ] <- NA
				# ( m <- get("Q.prior",envir=prior.env) )
				# ( o <- get("Q",envir=matrix.env) )
				# test
				# get("Q",envir=matrix.env)
				# get("Q.prior",envir=prior.env )	
				
		}
		if ( length( ls(envir=prior.env) ) > 0 ) {
				## mods in priors
				if( any( mods %in% 1 ) ) eval( parse( text=paste0( "assign('",ls(envir=prior.env), "', mod1(get('",ls(envir=prior.env),"',envir=prior.env)), envir=prior.env)" ) ) )
				if( any( mods %in% 6 ) ) eval( parse( text=paste0( "assign('",ls(envir=prior.env), "', mod6(get('",ls(envir=prior.env),"',envir=prior.env),'",sub("\\.prior$","",ls(envir=prior.env)),"'), envir=prior.env)" ) ) )
		}
# browser()		
		# test
		# get("Q",envir=matrix.env)
		# get("Q.prior",envir=prior.env )		
		
		# put all matrix.env and prior.env objects into env too
		if( length( ls(envir=matrix.env) ) > 0 ) {
				eval( parse ( text=paste0( "assign( '",ls(envir=matrix.env), "' , get( '",ls(envir=matrix.env),"', envir=matrix.env ) , envir=env )" ) ) )
		}
		if( length( ls(envir=prior.env) ) > 0 ) {
				eval( parse ( text=paste0( "assign( '",ls(envir=prior.env), "' , get( '",ls(envir=prior.env),"', envir=prior.env ) , envir=env )" ) ) )
		}
		
		return( TRUE )
}		


is.fixed <- function( m ){
		# m2 <- !is.na( suppressWarnings( as.numeric( m ) ) )
		# http://www.regular-expressions.info/floatingpoint.html
		m2 <- grepl( "^\\s*[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?.*", m )
		dim( m2 ) <- dim( m )
		is.na( m2 ) <- is.na( m )
		return( m2 )
}
is.parameter <- function( m ){
# browser()
		# m2 <- grepl( "^\\s*[a-zA-Z]+[a-zA-Z0-9]*.*$", m ) & !grepl( "^.*<-.*;.*$", m ) & !grepl( "^.*~.*;.*$", m )
		# m2 <- grepl( "^\\s*[a-zA-Z]+[a-zA-Z0-9]*.*$", m ) & !grepl( "^\\s*[a-zA-Z]+[a-zA-Z0-9]*.*<-.*$", m ) & !grepl( "^\\s*[a-zA-Z]+[a-zA-Z0-9]*.*~.*$", m )
		# m2 <- grepl( "^\\s*[a-zA-Z]+[a-zA-Z0-9]*[\\s|;]*.*$", m ) 
		m2 <- grepl( "^\\s*[a-zA-Z]+[a-zA-Z0-9]*.*$", m ) & !grepl( "^\\s*[a-zA-Z]+[a-zA-Z0-9]*[^;]*<\\-.*$", m )
		dim( m2 ) <- dim( m )
		# is.na( m2 ) <- is.na( m )
		if( any( is.na( m2 ) ) ) m2[ is.na ( m2 ) ] <- FALSE 		
		return( m2 )
}
is.constraint <- function( m ){
		# m2 <- grepl( "^[^;]*<-.*$", m ) | grepl( "^[^;]*<-.*;.*$", m )
		m2 <- grepl( "^[^;]*<-.*$", m )
		dim( m2 ) <- dim( m )
		# is.na( m2 ) <- is.na( m )
		if( any( is.na( m2 ) ) ) m2[ is.na ( m2 ) ] <- FALSE 		
		return( m2 )
}
is.code <- function( m ){
		m2 <- grepl( ";", m )
		dim( m2 ) <- dim( m )
		# is.na( m2 ) <- is.na( m )
		if( any( is.na( m2 ) ) ) m2[ is.na ( m2 ) ] <- FALSE 
		return( m2 )
}
is.prior.formula <- function( m ){
# browser()		
		m2 <- grepl( "^[^;]*~.*$", m )
		dim( m2 ) <- dim( m )
		# is.na( m2 ) <- is.na( m )
		if( any( is.na( m2 ) ) ) m2[ is.na ( m2 ) ] <- FALSE 		
		return( m2 )
}
is.prior <- function( m ){
		priors <- c("dbin","dbinom","dchisqr","dchisq","dgen.gamma","dggamma","dnegbin","dnbinom","dweib","dweibull","ddirch","ddirich","dmulti","dmt","dwish","dmnorm","dpois","dhyper","dcat","dbern","dbetabin","dunif","dt","dpar","dnorm","dnchisqr","dlnorm","dlogis","dgamma","df","dexp","ddexp","dbeta")
# browser()		
		m2 <- grepl( paste0("^[^;~]*[",paste(paste0("\\b",priors,"\\b"),collapse="|"),"]\\s*\\(.*\\)*$"), m )
		m2 <- grepl( paste0("^[^;~|<-]*[",paste(paste0("\\b",priors,"\\b"),collapse="|"),"]\\s*\\(.*\\)*$"), m )
		dim( m2 ) <- dim( m )
		# is.na( m2 ) <- is.na( m )
		if( any( is.na( m2 ) ) ) m2[ is.na ( m2 ) ] <- FALSE 
		return( m2 )
}
is.par.plus.priorformula <- function( m ){
		# m2 <- is.parameter( sub( "^([^;]);.*", "\\1", m ) ) & is.prior.formula( sub( "^[^;];(.*)", "\\1", m ) )
		m2 <- is.parameter( sub( "^([^;]*);.*", "\\1", m ) ) & is.prior.formula( sub( "^[^;]*;(.*)", "\\1", m ) )
		dim( m2 ) <- dim( m )
		# is.na( m2 ) <- is.na( m )
		if( any( is.na( m2 ) ) ) m2[ is.na ( m2 ) ] <- FALSE 
		m2 <- m2 & is.parameter( m ) & is.code ( m )
		return( m2 )
}
# is parameter that is redefined,  par; par <- 
is.par.plus.redefined <- function( m ){
# browser()		
		# potential parameter
		( potpar <- sub( "^([^;]*);.*$", "\\1", m ) )
		
		# gucken ob nicht redefined
		if ( !is.null ( dim ( m ) ) ) nr <- prod( dim ( m ) ) else nr <- length( m )
		m3 <- sapply( 1:nr , function( nr ) grepl( paste0( "^[^;]*;.*",gsub(".","\\.",potpar[nr],fixed=TRUE),"[^;]*<-.*$" ), m[nr] ) )
		dim( m3 ) <- dim( m )
		
		# is parameter und nicht redefined
		m2 <- is.parameter( potpar ) & m3
		# dim( m2 ) <- dim( m )
		# is.na( m2 ) <- is.na( m )
		if( any( is.na( m2 ) ) ) m2[ is.na ( m2 ) ] <- FALSE 
		# m2 <- m2 & is.parameter( m ) & is.code ( m )
		return( m2 )
}

# m <- matrix( c(1,1.2,1.2,1), 2, 2 )
# m <- matrix( c("1","1.2"," 1.2 "," 1   ; "), 2, 2 )
# is.fixed( m )
# is.parameter( m )
# is.constraint( m )
# is.code( m )
# is.prior( m )

# m <- matrix( c(" x "," yyy ","  x555 ;    ","x123"), 2, 2 )
# is.fixed( m )
# is.parameter( m )
# is.constraint( m )
# is.code( m )
# is.prior( m )

# m <- matrix( c("1; <- "," <- ","  x555 ; <-   ","x <- 1 ;  "), 2, 2 )
# is.fixed( m )
# is.parameter( m )
# is.constraint( m )
# is.code( m )
# is.prior( m )

# m <- matrix( c("1.2; ~ "," ~ ","  x555 ; ~   ","x ~ 1 ;  "), 2, 2 )
# is.fixed( m )
# is.parameter( m )
# is.constraint( m )
# is.code( m )
# is.prior( m )
