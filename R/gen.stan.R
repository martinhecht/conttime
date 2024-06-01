## Changelog:
# MH 2024-04-29: renamed from estimate()

## Documentation
#' @title Generate Stan code
#' @description Generate Stan code and starting values for a time-varying continuous-time model.
#' @param data.env an environment containing objects created by the \code{gen.data()} and \code{gen.design()} functions
#' @param syntax.dir the directory where the Stan syntax file is written
#' @param model_name name of the model, used to name the Stan file as <model_name>.stan
#' @param model.parameters.env an environment containing declarations of model parameters in the matrices A0, Achange, and Q0; if \code{NULL}, the default parameterization is used
#' @param prior.env an environment containing priors for the elements of the matrices A0, Achange, and Q0; if \code{NULL}, default priors are used
#' @param chains Argument chains is used only for start value creation. Use \code{start.values()} for optional manual start value creation or to change defaults. If \code{NULL}, default start values are used. If a numeric value >= 1 is provided, jittered default start values are used.
#' @param KF use Kalman Filter estimation
#' @param verbose a logical value indicating whether to print detailed messages and progress updates during the execution of the function
#' @return A list is returned containing the elements TODO CHECK ADAPT \code{syntax.path}, \code{data}, \code{init}, \code{model.parameters}, \code{par.env}, \code{prior.env}, and \code{start.values.env}.

## Function definition
gen.stan <- function( data.env, syntax.dir=getwd(), model_name="model", model.parameters.env=NULL, prior.env=NULL, chains=NULL, KF=TRUE, verbose=TRUE ){

	# trigger for no between-(co)variances in mu
	between.mu <- FALSE
	# between.mu <- TRUE


	### based on tvct_v1.pdf (2024-04-04)
	
	## put all relevant data and model parameter elements from data environment here
	# all.elements <- c( data.structures, model.parameters, additional.structures )
	# for( i in 1:length( all.elements ) ){
		# eval( parse( text=paste0( all.elements[i], " <- get('", all.elements[i], "', envir=data.env, inherits=FALSE)" ) ) )
	# }
	# print( Tj ); flush.console()
	data.structures <- c( "F", "I", "N", "T", "Tunique", "Tj", "tunique", "ptuniquejp", "deltajp", "yjp" )
	F           <- get('F'          , envir=data.env, inherits=FALSE)
	I           <- get('I'          , envir=data.env, inherits=FALSE)
	N           <- get('N'          , envir=data.env, inherits=FALSE)
	T           <- get('T'          , envir=data.env, inherits=FALSE)
	Tunique     <- get('Tunique'    , envir=data.env, inherits=FALSE)
	Tj          <- get('Tj'         , envir=data.env, inherits=FALSE)
	tunique     <- get('tunique'    , envir=data.env, inherits=FALSE)
	ptuniquejp  <- get('ptuniquejp' , envir=data.env, inherits=FALSE)
	deltajp     <- get('deltajp'    , envir=data.env, inherits=FALSE)
	yjp         <- get('yjp'        , envir=data.env, inherits=FALSE)

	# no measurement model: F must be I
	if( F!=I ) stop( paste0( "F = ", F , " is NOT equal to I = ", I , " (this is required for no measurement model)" ) )

	# additional.structures <- c( "epsAt", "At", "epsQt", "Qt", "epsmut", "mut", "muj", "Ajp", "Qjp", "mujp", "Ahashjp", "Sigmawjp", "Astarjp", "Qstarjp", "thetajp", "omegajp", "epsjp", "zerovecF", "zerovecF2", "zerovecFF12", "zerovecI", "IF", "IF2", "S1", "S2", "S3"
	additional.structures <- c( "IF", "IF2", "S1", "S2", "zerovecF", "zerovecF2", "zerovecFF12" ) # , "S3"
	# epsAt       <- get('epsAt'      , envir=data.env, inherits=FALSE)
	# At          <- get('At'         , envir=data.env, inherits=FALSE)
	# epsQt       <- get('epsQt'      , envir=data.env, inherits=FALSE)
	# Qt          <- get('Qt'         , envir=data.env, inherits=FALSE)
	# epsmut      <- get('epsmut'     , envir=data.env, inherits=FALSE)
	# mut         <- get('mut'        , envir=data.env, inherits=FALSE)
	# muj         <- get('muj'        , envir=data.env, inherits=FALSE)
	# Ajp         <- get('Ajp'        , envir=data.env, inherits=FALSE)
	# Qjp         <- get('Qjp'        , envir=data.env, inherits=FALSE)
	# mujp        <- get('mujp'       , envir=data.env, inherits=FALSE)
	# Ahashjp     <- get('Ahashjp'    , envir=data.env, inherits=FALSE)
	# Sigmawjp    <- get('Sigmawjp'   , envir=data.env, inherits=FALSE)
	# Astarjp     <- get('Astarjp'    , envir=data.env, inherits=FALSE)
	# Qstarjp     <- get('Qstarjp'    , envir=data.env, inherits=FALSE)
	# thetajp     <- get('thetajp'    , envir=data.env, inherits=FALSE)
	# omegajp     <- get('omegajp'    , envir=data.env, inherits=FALSE)
	# epsjp       <- get('epsjp'      , envir=data.env, inherits=FALSE)
	zerovecF    <- get('zerovecF'   , envir=data.env, inherits=FALSE)
	zerovecF2   <- get('zerovecF2'  , envir=data.env, inherits=FALSE)
	zerovecFF12 <- get('zerovecFF12', envir=data.env, inherits=FALSE)
	# zerovecI    <- get('zerovecI'   , envir=data.env, inherits=FALSE)
	IF          <- get('IF'         , envir=data.env, inherits=FALSE)
	IF2         <- get('IF2'        , envir=data.env, inherits=FALSE)
	S1          <- get('S1'         , envir=data.env, inherits=FALSE)
	S2          <- get('S2'         , envir=data.env, inherits=FALSE)
	# S3          <- get('S3'         , envir=data.env, inherits=FALSE)

	# ids (j) need to be sorted by number of time points
	# resort.ind <- do.call( "c", sapply( unique( sort( Tj ) ), function( T ) which( Tj %in% T ), simplify=FALSE ) )
	# yjp <- yjp[,,resort.ind,,drop=FALSE]
	# Tj <- Tj[resort.ind]
	# deltajp <- deltajp[resort.ind,,drop=FALSE]
	# ptuniquejp <- ptuniquejp[resort.ind,,drop=FALSE]
	# temporary
	# mujp <- mujp[,,resort.ind,,drop=FALSE]
	### Ajp <- Ajp[,,resort.ind,,drop=FALSE]
	### Qjp <- Qjp[,,resort.ind,,drop=FALSE]
	# unique Tj
	uniqueTj <- unique( Tj ) # Tj should/needs to be sorted at this point
	# prepare indices vectors (lower/upper range) for persons with same number of time points
	Tj.list <- sapply( unique( Tj ), function( T ) which( Tj %in% T ), simplify=FALSE )
	Tjlow <- sapply( Tj.list, min )
	Tjup <- sapply( Tj.list, max )
	# number of different individual time points
	Tjn <- length( unique( Tj ) )
	# number of persons per T
	NperT <- sapply( Tj.list, length )
	# shifted cumulated number of persons per T (starting from 0 for 1st time point)
	if( Tjn > 1 ){
		NperTcum <- c( 0, cumsum( NperT )[1:(Tjn-1)] )
	} else if ( Tjn==1 ) {
		NperTcum <- 0
	}

	# split structure into a list with person groups with same number of time points and delete NAs due to missing time points
	yjpT <- mapply( function( js, Tj ) yjp[,,js,1:Tj,drop=FALSE], Tj.list, uniqueTj, SIMPLIFY=FALSE )
	names( yjpT ) <- paste0( "yjpT", uniqueTj )
	deltajpT <- mapply( function( js, Tj ) deltajp[js,1:Tj,drop=FALSE], Tj.list, uniqueTj-1, SIMPLIFY=FALSE )
	names( deltajpT ) <- paste0( "deltajpT", uniqueTj )
	
	ptuniquejpT <- mapply( function( js, Tj ) ptuniquejp[js,1:Tj,drop=FALSE], Tj.list, uniqueTj, SIMPLIFY=FALSE )
	# ptuniquejpT <- mapply( function( js, Tj ) ptuniquejp[js,1:(Tj-1),drop=FALSE], Tj.list, uniqueTj, SIMPLIFY=FALSE )
	names( ptuniquejpT ) <- paste0( "ptuniquejpT", uniqueTj )


	model.parameters <- c( "A0", "Achange", "Q0" ) # "Qchange", "SigmaepsQ", "mu0", "muchange", "Sigmaepsmu",  "Delta", "Sigmaeps", "SigmaepsA"
	if( between.mu ) model.parameters <- c( model.parameters, "Sigmamu" )
	
	# get empty structures
	str.env <- gen.empty.structures( env=data.env )
	
	A0          <- get('A0'         , envir=str.env, inherits=FALSE)
	Achange     <- get('Achange'    , envir=str.env, inherits=FALSE)
	# MH 0.0.30 2024-05-07, no epsAt
	# SigmaepsA   <- get('SigmaepsA'  , envir=data.env, inherits=FALSE)
	Q0          <- get('Q0'         , envir=str.env, inherits=FALSE)
	# MH 0.0.27 2024-05-04 Q0 is Q (not time-varying)		
	# Qchange     <- get('Qchange'    , envir=data.env, inherits=FALSE)
	# SigmaepsQ   <- get('SigmaepsQ'  , envir=data.env, inherits=FALSE)
	# 0.0.29 2024-05-06, no mean
	# mu0         <- get('mu0'        , envir=data.env, inherits=FALSE)
	# muchange    <- get('muchange'   , envir=data.env, inherits=FALSE)
	# Sigmaepsmu  <- get('Sigmaepsmu' , envir=data.env, inherits=FALSE)
	if( between.mu ) Sigmamu     <- get('Sigmamu'    , envir=str.env, inherits=FALSE)
	# 0.0.29 2024-05-06, no measurement model
	# Delta       <- get('Delta'      , envir=data.env, inherits=FALSE)
	# Sigmaeps    <- get('Sigmaeps'   , envir=data.env, inherits=FALSE)

	# Tests
	### Delta[2,1] <- "lambda21"
	# Delta[2,1] <- NA
	# Delta[4,2] <- NA
	### Delta[] <- "lambda"
	### Delta[] <- NA
	### Sigmaeps[] <- "sigmaeps"
	# Sigmawjp[1,1,1,1] <- NA
	
	# keep fixed model parameters from data generation for starting values
	# keep.fixed <- list( "A0"=A0, "Achange"=Achange, "Q0"=Q0 ) # , "SigmaepsQ"=SigmaepsQ, "Qchange"=Qchange, "mu0"=mu0, "muchange"=muchange, "Sigmaepsmu"=Sigmaepsmu, "Delta"=Delta, "Sigmaeps"=Sigmaeps, "SigmaepsA"=SigmaepsA
	# if( between.mu ) keep.fixed <- c( keep.fixed, list( "Sigmamu"=Sigmamu ) )
	
	# Defaults
	### TODO: ordentliche Defaults fuer Delta
	# 0.0.29 2024-05-06, no measurement model
	# Delta[2,1] <- NA
	# Delta[4,2] <- NA
	# Sigmaeps[] <- NA
	# Sigmaeps[ lower.tri( Sigmaeps ) ] <- 0
	# Sigmaeps[ upper.tri( Sigmaeps ) ] <- 0
	A0[] <- NA
	Achange[] <- NA
	# MH 0.0.30 2024-05-07, no epsAt
	# SigmaepsA[] <- 0
	# diag(SigmaepsA) <- NA
	# MH 0.0.24 2024-05-03 Q0 covariance now 0
	# Q0[] <- 0
	# diag(Q0) <- NA
	# MH 0.0.27 2024-05-04 Q0 is Q (not time-varying)
	Q0[] <- 0
	diag(Q0) <- NA
	# MH 0.0.28 2024-05-04 Q covariance fixed to 0
	# Q0[1,2] <- Q0[2,1] <- 0
	# MH 0.0.25 2024-05-03 Qchange covariance now 0
	# MH 0.0.27 2024-05-04 Q0 is Q (not time-varying)
	# Qchange[] <- 0
	# diag(Qchange) <- NA
	# Q0 <- matrix( c("q011","q021","q021","q022" ), F, F )
	# Qchange <- matrix( c("qchange11","qchange21","qchange21","qchange22" ), F, F )
	# MH 0.0.27 2024-05-04 Q0 is Q (not time-varying)
	# SigmaepsQ[] <- 0
	# diag(SigmaepsQ) <- NA
	# MH 0.0.26 2024-05-04 variance of Q-covariance errors now 0
	# SigmaepsQ[2,2] <- 0.0001
	# 0.0.29 2024-05-06, no mean
	# mu0[] <- NA
	# muchange[] <- NA
	# Sigmaepsmu[] <- 0
	# diag(Sigmaepsmu) <- NA
	if( between.mu ) Sigmamu[] <- NA

	# get elements of model.parameters.env and overwrite defaults
	if( !is.null( model.parameters.env ) ){
		if( "A0"      %in% ls(envir=model.parameters.env) )      A0      <- get('A0'         , envir=model.parameters.env, inherits=FALSE)
		if( "Achange" %in% ls(envir=model.parameters.env) )      Achange <- get('Achange'    , envir=model.parameters.env, inherits=FALSE)
		if( "Q0"      %in% ls(envir=model.parameters.env) )      Q0      <- get('Q0'         , envir=model.parameters.env, inherits=FALSE)
	}

	### determine structure type for model parameters: free, fixed, mixed (free parameters and fixed values)
	structure.type <- array( as.character(NA), dim=length(model.parameters) )
	names( structure.type ) <- model.parameters
	for( i in 1:length(structure.type) ){
		vals <- eval( parse( text=paste0( "matrix( ",model.parameters[i],", ncol=1 )[,1]" ) ) )
		if ( all ( is.numeric( vals ) & !is.na( vals ) ) ) type <- "fixed" else if ( all( is.na( suppressWarnings( as.numeric( vals ) ) ) ) | all( is.na( vals ) ) ) type <- "free" else type <- "mixed"
		structure.type[i] <- type
		type <- NULL
	}
	# structure dimensions (symbolic)
	structure.dim          <- c( "matrix[F,F] A0", "matrix[F,F] Achange", "cov_matrix[F] Q0" ) # , "cov_matrix[F] Qchange", "matrix[F*(F+1)/2,F*(F+1)/2] SigmaepsQ", "matrix[I,F] Delta", "cov_matrix[I] Sigmaeps", "matrix[F,1] mu0", "matrix[F,1] muchange", "matrix[F,F] Sigmaepsmu", "matrix[F*F,F*F] SigmaepsA"
	names( structure.dim ) <- c( "A0"            , "Achange"            , "Q0"               ) # , "Qchange"              , "SigmaepsQ"                            , "Delta"            , "Sigmaeps"              , "mu0"            , "muchange"            , "Sigmaepsmu"            , "SigmaepsA"                

	# 0.0.45, cov_matrix[F] crashes for F=1
	if( F==1 ) {
		structure.dim["Q0"] <- "matrix[F,F] Q0"
	}

	if( between.mu ){
		structure.dim <- c( structure.dim, "matrix[F,F] Sigmamu" )
		names( structure.dim ) <- c( names( structure.dim )[-length(structure.dim)], "Sigmamu" )
	}
	
	# parameter labels for each mixed structure
	par.lab.mixed.str          <- c( "a0", "achange", "q0" ) # "qchange", "sigmaepsQ", "lambda", "sigmaeps", "mu0", "muchange", "sigmaepsmu", "sigmaepsA"
	names( par.lab.mixed.str ) <- c( "A0", "Achange", "Q0" ) # "Qchange", "SigmaepsQ", "Delta" , "Sigmaeps", "mu0", "muchange", "Sigmaepsmu", "SigmaepsA"
	if( between.mu ){
		par.lab.mixed.str <- c( par.lab.mixed.str, "sigmamu" )
		names( par.lab.mixed.str ) <- c( names( par.lab.mixed.str )[-length(par.lab.mixed.str)], "Sigmamu" )
	}

	# get parameters of mixed structures
	parameters.of.mixed.structures <- character(0)
	if( length( wimis <- which( structure.type %in% "mixed" ) ) > 0 ){
		for( st in names( structure.type[wimis] ) ){
			# if NA in mixed structure, label with parameter name
			for( c in 1:eval(parse(text=paste0("ncol(",st,")"))) ) {
				for( r in 1:eval(parse(text=paste0("nrow(",st,")"))) ){
					if( !is.null( par.lab.mixed.str ) & (st %in% names( par.lab.mixed.str )) ) par.lab <- par.lab.mixed.str[st] else par.lab <- st
					eval(parse(text=paste0("if( is.na( ",st,"[",r,",",c,"] ) ) ",st,"[",r,",",c,"] <- '",par.lab,r,c,"'")))
				}
			}				
			# add parameters to vector
			parameters.of.mixed.structures <- c( parameters.of.mixed.structures, eval( parse( text=paste0( "matrix( ",st,", ncol=1 )[,1]" ) ) ) )
		}
	}
	if( length( parameters.of.mixed.structures ) > 0 ){
		parameters.of.mixed.structures <- unique( parameters.of.mixed.structures[ is.na( suppressWarnings( as.numeric( parameters.of.mixed.structures ) ) ) ] )
	}

	# remove completely fixed structures from model parameters
	if( any( tf <- structure.type %in% "fixed" ) ){
		model.parameters <- model.parameters[!model.parameters %in% names( structure.type )[tf]]
	}
	if( length( model.parameters ) == 0 ) model.parameters <- NULL

	# put model parameters on env to return
	par.env <- new.env()
	if( !is.null( model.parameters ) ){
		for( i in 1:length( model.parameters ) ){
			assign( model.parameters[i], eval( parse( text=model.parameters[i] ) ), envir = par.env, inherits = FALSE, immediate=TRUE )
		}
	}

	if( verbose ){
		cat( paste0( "parameterization:\n" ) )
		cat( paste0( "(free parameters: NA or label; fixed paramters: number)\n" ) )
		cat( paste0( "A0:\n" ) )
		print( A0 )
		cat( paste0( "Achange:\n" ) )
		print( Achange )
		cat( paste0( "Q0:\n" ) )
		print( Q0 )
		cat( paste0( "----------------------------------------------------------------------------\n" ) )
	}

	### stan syntax
	x <- paste0( "// Model name: ", model_name, " | ", date() )

	## functions
	x <- c( x, paste0( "functions {" ) )
    x <- c( x, paste0( "  vector flatten_matrix_rowwise(matrix mat) {" ) )
    x <- c( x, paste0( "    int rows = rows(mat); // Get the number of rows in the matrix" ) )
    x <- c( x, paste0( "    int cols = cols(mat); // Get the number of columns in the matrix" ) )
    x <- c( x, paste0( "    vector[rows * cols] vec; // Create a vector to hold all elements" ) )
    x <- c( x, paste0( "    int pos = 1;" ) )
    x <- c( x, paste0( "    for (i in 1:rows) {" ) )
    x <- c( x, paste0( "        for (j in 1:cols) {" ) )
    x <- c( x, paste0( "            vec[pos] = mat[i, j]; // Assign matrix elements to vector" ) )
    x <- c( x, paste0( "            pos = pos + 1; // Increment position index" ) )
    x <- c( x, paste0( "        }" ) )
    x <- c( x, paste0( "    }" ) )
    x <- c( x, paste0( "    return vec;" ) )
    x <- c( x, paste0( "  }" ) )
       x <- c( x, paste0( "  matrix unflatten_vector_to_matrix(vector vec, int rows, int cols) {" ) )
       x <- c( x, paste0( "      matrix[rows, cols] mat; // Create a matrix of specified dimensions" ) )
       x <- c( x, paste0( "      int pos = 1;" ) )
       x <- c( x, paste0( "      for (i in 1:rows) {" ) )
       x <- c( x, paste0( "          for (j in 1:cols) {" ) )
       x <- c( x, paste0( "              mat[i, j] = vec[pos]; // Assign vector elements back to matrix" ) )
       x <- c( x, paste0( "              pos = pos + 1; // Increment position index" ) )
       x <- c( x, paste0( "          }" ) )
       x <- c( x, paste0( "      }" ) )
       x <- c( x, paste0( "      return mat;" ) )
       x <- c( x, paste0( "  }" ) )
       x <- c( x, paste0( "  matrix kronecker_product(matrix X, matrix Y) {" ) )
       x <- c( x, paste0( "      int a_rows = rows(X);" ) )
       x <- c( x, paste0( "      int a_cols = cols(X);" ) )
       x <- c( x, paste0( "      int b_rows = rows(Y);" ) )
       x <- c( x, paste0( "      int b_cols = cols(Y);" ) )
       x <- c( x, paste0( "      matrix[a_rows * b_rows, a_cols * b_cols] result;" ) )
       x <- c( x, paste0( "      for (i in 1:a_rows) {" ) )
       x <- c( x, paste0( "          for (j in 1:a_cols) {" ) )
       x <- c( x, paste0( "              for (k in 1:b_rows) {" ) )
       x <- c( x, paste0( "                  for (l in 1:b_cols) {" ) )
       x <- c( x, paste0( "                      result[(i - 1) * b_rows + k, (j - 1) * b_cols + l] = X[i, j] * Y[k, l];" ) )
       x <- c( x, paste0( "                  }" ) )
       x <- c( x, paste0( "              }" ) )
       x <- c( x, paste0( "          }" ) )
       x <- c( x, paste0( "      }" ) )
       x <- c( x, paste0( "      return result;" ) )
       x <- c( x, paste0( "  }" ) )

	# end functions
	x <- c( x, paste0( "}" ) )
	
	## data
	x <- c( x, paste0( "data {" ) )
	x <- c( x, paste0( "  int<lower=1> F;             // ",paste(F,collapse=',')," | number of processes" ) )
	x <- c( x, paste0( "  int<lower=1> I;             // ",paste(I,collapse=',')," | number of items" ) )
	x <- c( x, paste0( "  int<lower=1> N;             // ",paste(N,collapse=',')," | number of persons" ) )
	x <- c( x, paste0( "  int<lower=2> T;             // ",paste(T,collapse=',')," | number of maximum time points of persons" ) )
	x <- c( x, paste0( "  int<lower=2> Tunique;       // ",paste(Tunique,collapse=',')," | number of all unique time points" ) )
	x <- c( x, paste0( "  real tunique[Tunique];      // ",paste(tunique,collapse=',')," | all unique time points" ) )
	x <- c( x, paste0( "  int<lower=2> ",ifelse(N>1,"Tj[N]","Tj"),";",ifelse(N>1,"","   "),"         // ",paste(Tj,collapse=',')," | number of individual time points" ) )
	x <- c( x, paste0( "  int<lower=1> Tjn;           // ",paste(Tjn,collapse=',')," | number of different individual time points" ) )
	x <- c( x, paste0( "  int<lower=1> ",ifelse(Tjn>1,"Tjlow[Tjn]","Tjlow"),";",ifelse(Tjn>1,"","     "),"    // ",paste(Tjlow,collapse=',')," | index vector with lower limit of persons with same number of time points" ) )
	x <- c( x, paste0( "  int<lower=1> ",ifelse(Tjn>1,"Tjup[Tjn]","Tjup"),";",ifelse(Tjn>1,"","     "),"     // ",paste(Tjup,collapse=',')," | index vector with upper limit of persons with same number of time points" ) )
	x <- c( x, paste0( "  int<lower=1> ",ifelse(Tjn>1,"NperT[Tjn]","NperT"),";",ifelse(Tjn>1,"","     "),"    // ",paste(NperT,collapse=',')," | number of persons per T" ) )
	x <- c( x, paste0( "  int<lower=0> ",ifelse(Tjn>1,"NperTcum[Tjn]","NperTcum"),";",ifelse(Tjn>1,"","     ")," // ",paste(NperTcum,collapse=',')," | shifted cumulated number of persons per T (starting from 0 for 1st time point)" ) )
	x <- c( x, paste0( "  matrix[F,F] IF;             // identity matrix of size ", F ) )
	x <- c( x, paste0( "  matrix[F*F,F*F] IF2;        // identity matrix of size ", F^2 ) )
	x <- c( x, paste0( "  matrix[F*F,F*F] S1;         // selection matrix 1" ) )
	x <- c( x, paste0( "  matrix[F*F,F*F] S2;         // selection matrix 2" ) )
	# x <- c( x, paste0( "  matrix[F*F,F*(F+1)/2] S3;   // selection matrix 3" ) )
	x <- c( x, paste0( "  matrix[F,1] zerovecF;       // zero column vector of size ", F ) )
	x <- c( x, paste0( "  matrix[F*F,1] zerovecF2;    // zero column vector of size ", F^2 ) )
	x <- c( x, paste0( "  matrix[F*(F+1)/2,1] zerovecFF12;  // zero column vector of size ", F*(F+1)/2 ) )

	# observed values of persons with same number of time points
	for( i in 1:length(uniqueTj) ){
		x <- c( x, paste0( "  real yjpT",uniqueTj[i],"[I,1,NperT",ifelse(Tjn>1,paste0('[',i,']'),''),",",uniqueTj[i],"];",ifelse(T<10000,paste(rep(" ",5-nchar(as.character(uniqueTj[i]))),collapse="")," "),"// observed values of ",NperT[i]," persons with ",uniqueTj[i]," time points" ) )
	}
	# time interval lengths of persons with same number of time points
	for( i in 1:length(uniqueTj) ){
		x <- c( x, paste0( "  real deltajpT",uniqueTj[i],"[NperT",ifelse(Tjn>1,paste0('[',i,']'),''),",",uniqueTj[i]-1,"];",ifelse(T<10000,paste(rep(" ",5-nchar(as.character(uniqueTj[i]))),collapse="")," "),"// time interval lengths of ",NperT[i]," persons with ",uniqueTj[i]," time points" ) )
	}		
	# indices of time points of persons in tunique vector
	for( i in 1:length(uniqueTj) ){
		# x <- c( x, paste0( "  int<lower=1> ptuniquejpT",uniqueTj[i],"[NperT",ifelse(Tjn>1,paste0('[',i,']'),''),",",uniqueTj[i]-1,"];",ifelse(T<10000,paste(rep(" ",5-nchar(as.character(uniqueTj[i]-1))),collapse="")," "),"// indices of time points in tunique vector of ",NperT[i]," persons with ",uniqueTj[i]," time points" ) )
		x <- c( x, paste0( "  int<lower=1> ptuniquejpT",uniqueTj[i],"[NperT",ifelse(Tjn>1,paste0('[',i,']'),''),",",uniqueTj[i],"];",ifelse(T<10000,paste(rep(" ",5-nchar(as.character(uniqueTj[i]))),collapse="")," "),"// indices of time points in tunique vector of ",NperT[i]," persons with ",uniqueTj[i]," time points" ) )
	}		
	# fixed structures go in as data
	if( length( wifis <- which( structure.type %in% "fixed" ) ) > 0 ){
		for( st in names( structure.type[wifis] ) ){
			eval( parse( text=paste0( "x <- c( x, paste0( '  ",structure.dim[st],";' ) )" ) ) )
		}
	}
	# end data
	x <- c( x, paste0( "}" ) )

	## transformed data
	# x <- c( x, paste0( "transformed data {" ) )
	# end transformed data
	# x <- c( x, paste0( "}" ) )
	
	## parameters
	x <- c( x, paste0( "parameters {" ) )
	if( !KF ){
		# latent values of persons with same number of time points
		for( i in 1:length(uniqueTj) ){
			x <- c( x, paste0( "  real thetajpT",uniqueTj[i],"[F,1,NperT",ifelse(Tjn>1,paste0('[',i,']'),''),",",uniqueTj[i],"]; // latent values of ",NperT[i]," persons with ",uniqueTj[i]," time points" ) )
		}
	}
	if( KF ){ # different structure for first time point, because first time point is still sampled in KF
		for( i in 1:length(uniqueTj) ){
			x <- c( x, paste0( "  real thetajpFirstTimepointT",uniqueTj[i],"[F,1,NperT",ifelse(Tjn>1,paste0('[',i,']'),''),",1]; // latent values at first time point of ",NperT[i]," persons with ",uniqueTj[i]," time points" ) )
		}
	}
	# free structures go in as parameters
	if( length( wifres <- which( structure.type %in% "free" ) ) > 0 ){
		x <- c( x, paste0( "  // free structures: ", paste( names( structure.type[wifres] ), collapse=", " ) ) )
		for( st in names( structure.type[wifres] ) ){
			# eval( parse( text=paste0( "x <- c( x, paste0( '  ",structure.dim[st]," ",st,";' ) )" ) ) )
			eval( parse( text=paste0( "x <- c( x, paste0( '  ",structure.dim[st],";' ) )" ) ) )
		}
	}		
	# parameters of mixed structures
	if( length( parameters.of.mixed.structures ) > 0 ){		
		x <- c( x, paste0( "  // parameters of mixed structures" ) )
		eval( parse( text=paste0("x <- c( x, '  real ",parameters.of.mixed.structures,";' )" ) ) )
	}
	# epsAt, epsmut
	# x <- c( x, paste0( "  // epsAt, epsQt, epsmut" ) )
	# x <- c( x, paste0( "  // epsAt, epsmut" ) )
	# MH 0.0.30 2024-05-07, no epsAt
	# x <- c( x, paste0( "  real epsAt[F*F,1,Tunique];" ) )
	# MH 0.0.27 2024-05-04 Q0 is Q (not time-varying)		
	# x <- c( x, paste0( "  real epsQt[F*(F+1)/2,1,Tunique];" ) )
	# 0.0.29 2024-05-06, no mean
	# x <- c( x, paste0( "  real epsmut[F,1,Tunique];" ) )
	if( between.mu ) x <- c( x, paste0( "  real muj[F,1,N];" ) )

	# end parameters
	x <- c( x, paste0( "}" ) )
	
	## transformed parameters
	x <- c( x, paste0( "transformed parameters {" ) )
	# mixed structures go in as transformed parameters
	if( length( wimis ) > 0 ){
		x <- c( x, paste0( "  // mixed structures: ", paste( names( structure.type[wimis] ), collapse=", " ) ) )
		for( st in names( structure.type[wimis] ) ){
			eval( parse( text=paste0( "x <- c( x, paste0( '  ",structure.dim[st],";' ) )" ) ) )
			for( c in 1:eval(parse(text=paste0("ncol(",st,")"))) ) {
				for( r in 1:eval(parse(text=paste0("nrow(",st,")"))) ){
					x <- c( x, paste0( "  ",st,"[",r,",",c,"] <- ",eval(parse(text=paste0(st,"[",r,",",c,"]"))),";" ) )
				}
			}
		}
	}
	x <- c( x, paste0( "  // Cholesky decompositions" ) )
	x <- c( x, paste0( "  matrix[F,F] Q0Chol = cholesky_decompose( Q0 );" ) )
	# MH 0.0.27 2024-05-04 Q0 is Q (not time-varying)
	# x <- c( x, paste0( "  matrix[F,F] QchangeChol = cholesky_decompose( Qchange );" ) )
	# MH 0.0.30 2024-05-07, no epsAt
	# x <- c( x, paste0( "  matrix[F*F,F*F] SigmaepsAChol = cholesky_decompose( SigmaepsA );" ) )
	# MH 0.0.27 2024-05-04 Q0 is Q (not time-varying)
	# x <- c( x, paste0( "  matrix[F*(F+1)/2,F*(F+1)/2] SigmaepsQChol = cholesky_decompose( SigmaepsQ );" ) )
	# MH 0.0.32 2024-05-10 no measurement model, both for Kalman Filter and regular estimation: set to small value
	# 0.0.45, crashes for F=1, positive definiteness check fails for Sigmaeps = 1e-10
	if( F==1 && I==1 ) { # in current implementation I is always 1
		x <- c( x, paste0( "  matrix[I,I] Sigmaeps;" ) )
		x <- c( x, paste0( "  Sigmaeps[1,1] = 1e-10;" ) )
	} else {
		x <- c( x, paste0( "  cov_matrix[I] Sigmaeps = diag_matrix(rep_vector(1e-10, I));" ) )
	}
	# 0.0.29 2024-05-06, no mean
	# x <- c( x, paste0( "  matrix[F,F] SigmaepsmuChol = cholesky_decompose( Sigmaepsmu );" ) )
	if( between.mu ) x <- c( x, paste0( "  matrix[F,F] SigmamuChol = cholesky_decompose( Sigmamu );" ) )
	# 0.0.29 2024-05-06, no measurement model <=== yjpT needs to be in, so no measurement model rather means setting measurement error to zero
	# 0.0.45, crashes for F=1, positive definiteness check fails for Sigmaeps = 1e-10
	# x <- c( x, paste0( "  matrix[I,I] SigmaepsChol = cholesky_decompose( Sigmaeps );" ) )
	# At, Qt, mut
	x <- c( x, paste0( "  // time-varying drift/diffusion/mu" ) )
	x <- c( x, paste0( "  real At[F,F,Tunique];  // time-varying drift matrices" ) )
	# x <- c( x, paste0( "  vector[F] Ac;" ) )
	x <- c( x, paste0( "  real Qt[F,F,Tunique];  // time-varying diffusion matrices" ) )
	# 0.0.29 2024-05-06, no mean
	# x <- c( x, paste0( "  real mut[F,1,Tunique]; // time-varying mean vectors" ) )
	x <- c( x, paste0( "  for( p in 1:Tunique ){" ) )
    x <- c( x, paste0( "    for (i in 1:F){" ) )
    x <- c( x, paste0( "      for (k in 1:F){" ) )
	x <- c( x, paste0( "        // Eq. 2" ) )
	# x <- c( x, paste0( "        At[i,k,p] = (  unflatten_vector_to_matrix( S1 * flatten_matrix_rowwise( A0 + Achange*tunique[p] + unflatten_vector_to_matrix( to_vector(epsAt[,1,p]),F,F ) ), F,F)   +   unflatten_vector_to_matrix( S2 * flatten_matrix_rowwise( A0 .* exp( -( Achange*tunique[p] + unflatten_vector_to_matrix( to_vector(epsAt[,1,p]),F,F ) ) ) ), F,F)   )[i,k];" ) )
	# MH 0.0.30 2024-05-07, no epsAt
	# MH 0.0.46 without exp
	# x <- c( x, paste0( "        At[i,k,p] = (  unflatten_vector_to_matrix( S1 * flatten_matrix_rowwise( A0 + Achange*tunique[p] ), F,F)   +   unflatten_vector_to_matrix( S2 * flatten_matrix_rowwise( A0 .* exp( -( Achange*tunique[p] ) ) ), F,F)   )[i,k];" ) )
	# x <- c( x, paste0( "        Ac = flatten_matrix_rowwise( A0 + Achange*tunique[p] );" ) )
	# x <- c( x, paste0( "        At[i,k,p] = unflatten_vector_to_matrix( S1*Ac + S2*Ac, F,F )[i,k];" ) )
	x <- c( x, paste0( "        At[i,k,p] = (A0 + Achange*tunique[p])[i,k];" ) )
	x <- c( x, paste0( "        // Eq. 3" ) )
	# MH 0.0.27 2024-05-04 Q0 is Q (not time-varying)
	# x <- c( x, paste0( "        Qt[i,k,p] = (  unflatten_vector_to_matrix( S1 * flatten_matrix_rowwise( Q0Chol*Q0Chol' + (QchangeChol*QchangeChol')*tunique[p] + unflatten_vector_to_matrix( S3 * to_vector(epsQt[,1,p]),F,F ) ), F,F)   +   unflatten_vector_to_matrix( S2 * flatten_matrix_rowwise( (Q0Chol*Q0Chol') .* exp( -( (QchangeChol*QchangeChol')*tunique[p] + unflatten_vector_to_matrix( S3 * to_vector(epsQt[,1,p]),F,F ) ) ) ), F,F)   )[i,k];" ) )
	x <- c( x, paste0( "        Qt[i,k,p] = Q0[i,k];" ) )
	x <- c( x, paste0( "      }" ) )
	x <- c( x, paste0( "    }" ) )
    # 0.0.29 2024-05-06, no mean
	# x <- c( x, paste0( "    for (i in 1:F){" ) )
	# x <- c( x, paste0( "      // Eq. 9" ) )
	# x <- c( x, paste0( "      mut[i,1,p] = (  to_vector(mu0) + to_vector(muchange)*tunique[p] + to_vector(epsmut[,1,p])  )[i];" ) )
	# x <- c( x, paste0( "    }" ) )
						# x <- c( x, paste0( "        print( At[,,p] );" ) )
						# x <- c( x, paste0( "        print( Qt[,,p] );" ) )
	x <- c( x, paste0( "  }" ) )
	# Ajp, Qjp, mujp
	# x <- c( x, paste0( "  // Ajp, Qjp ", ifelse( between.mu, ", mujp", "" ) ) )
	
	x <- c( x, paste0( "  // Ajp, Qjp" ) )
	if( !KF ){
		for( i in 1:length(uniqueTj) ){
			# x <- c( x, paste0( "  real ",c('AjpT','QjpT'),uniqueTj[i],"[F,F,NperT",ifelse(Tjn>1,paste0('[',i,']'),''),",",uniqueTj[i]-1,"];" ) )
			x <- c( x, paste0( "  real ",c('AjpT','QjpT'),uniqueTj[i],"[F,F,NperT",ifelse(Tjn>1,paste0('[',i,']'),''),",",uniqueTj[i],"];" ) )
			# 0.0.29 2024-05-06, no mean
			# x <- c( x, paste0( "  real mujpT",uniqueTj[i],"[F,1,NperT",ifelse(Tjn>1,paste0('[',i,']'),''),",",uniqueTj[i],"];",ifelse(T<10000,paste(rep(" ",5-nchar(as.character(uniqueTj[i]))),collapse="")," ") ) )
			
			x <- c( x, paste0( "  for (j in Tjlow",ifelse(Tjn>1,paste0('[',i,']'),''),":Tjup",ifelse(Tjn>1,paste0('[',i,']'),''),"){ // range: ",Tjlow[i],":",Tjup[i],", NperTcum=",NperTcum[i],", NperT=",NperT[i] ) )
			# x <- c( x, paste0( "    for (p in 1:(",ifelse(N>1,"Tj[j]","Tj"),"-1)){ // range: 1:",uniqueTj[i]-1 ) )
			x <- c( x, paste0( "    for (p in 1:",ifelse(N>1,"Tj[j]","Tj"),"){ // range: 1:",uniqueTj[i] ) )
							# x <- c( x, paste0( "print( ptuniquejpT",uniqueTj[i],"[j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p] );" ) )
							# x <- c( x, paste0( "print( \"j: \", j );" ) )
							# x <- c( x, paste0( "print( \"p: \", p );" ) )
							# x <- c( x, paste0( "print( \"ptuniquejp",uniqueTj[i],": \", ptuniquejpT",uniqueTj[i],"[j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p] );" ) )
			x <- c( x, paste0( "      for (i in 1:F){" ) )
			x <- c( x, paste0( "        for (k in 1:F){" ) )
			x <- c( x, paste0( "          // Eq. 11" ) )
			x <- c( x, paste0( "          ",c('AjpT','QjpT'),uniqueTj[i],"[i,k,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p] = to_matrix(",c('At','Qt'),"[,,ptuniquejpT",uniqueTj[i],"[j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p]])[i,k];" ) )
			x <- c( x, paste0( "        }" ) )
			x <- c( x, paste0( "      }" ) )
			# x <- c( x, paste0( "      for (i in 1:F){" ) )
			# x <- c( x, paste0( "        // Eq. 11" ) )
			# 0.0.29 2024-05-06, no mean
			# if( between.mu ) {
				# x <- c( x, paste0( "        mujpT",uniqueTj[i],"[i,1,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p] = (  to_vector( mut[,1,ptuniquejpT",uniqueTj[i],"[j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p]] ) + to_vector( muj[,1,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),"] )  )[i];" ) )
			# } else {
				# x <- c( x, paste0( "        mujpT",uniqueTj[i],"[i,1,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p] = (  to_vector( mut[,1,ptuniquejpT",uniqueTj[i],"[j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p]] ) )[i];" ) )
			# }
			# x <- c( x, paste0( "      }" ) )
			
			x <- c( x, paste0( "    }" ) )
			x <- c( x, paste0( "  }" ) )
							# x <- c( x, paste0( "        print( AjpT",uniqueTj[i]," );" ) )
							# x <- c( x, paste0( "        print( QjpT",uniqueTj[i]," );" ) )
		}
	} else if( KF ){
		for( i in 1:length(uniqueTj) ){
			x <- c( x, paste0( "  real ",c('AjpFirstTimepointT','QjpFirstTimepointT'),uniqueTj[i],"[F,F,NperT",ifelse(Tjn>1,paste0('[',i,']'),''),",1];" ) )
			x <- c( x, paste0( "  for (j in Tjlow",ifelse(Tjn>1,paste0('[',i,']'),''),":Tjup",ifelse(Tjn>1,paste0('[',i,']'),''),"){ // range: ",Tjlow[i],":",Tjup[i],", NperTcum=",NperTcum[i],", NperT=",NperT[i] ) )
			x <- c( x, paste0( "    for (p in 1:",ifelse(N>1,"Tj[j]","Tj"),"){ // range: 1:",uniqueTj[i] ) )
			x <- c( x, paste0( "      for (i in 1:F){" ) )
			x <- c( x, paste0( "        for (k in 1:F){" ) )
			x <- c( x, paste0( "          // Eq. 11" ) )
			x <- c( x, paste0( "          ",c('AjpFirstTimepointT','QjpFirstTimepointT'),uniqueTj[i],"[i,k,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",1] = to_matrix(",c('At','Qt'),"[,,ptuniquejpT",uniqueTj[i],"[j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p]])[i,k];" ) )
			x <- c( x, paste0( "        }" ) )
			x <- c( x, paste0( "      }" ) )
			x <- c( x, paste0( "    }" ) )
			x <- c( x, paste0( "  }" ) )
		}
	}

	# Astarjp, Qstarjp, Sigmawjp
	if( !KF ) {
		nms <- c( 'AstarjpT','QstarjpT','QstarjpCholT','SigmawjpT','SigmawjpCholT')
		for( i in 1:length(uniqueTj) ){
			x <- c( x, paste0( "  real ",nms,uniqueTj[i],"[F,F,NperT",ifelse(Tjn>1,paste0('[',i,']'),''),",",uniqueTj[i],"-1]; // matrices for ",NperT[i]," persons with ",uniqueTj[i]," time points" ) )
		}
	} else if( KF ) {
		nms <- c( 'QstarjpFirstTimepointT','QstarjpFirstTimepointCholT','SigmawjpFirstTimepointT','SigmawjpFirstTimepointCholT')		
		for( i in 1:length(uniqueTj) ){
			x <- c( x, paste0( "  real ",nms,uniqueTj[i],"[F,F,NperT",ifelse(Tjn>1,paste0('[',i,']'),''),",1]; // matrices for ",NperT[i]," persons with ",uniqueTj[i]," time points" ) )
		}
	}
	# Ahash
	if( !KF ) {
		for( i in 1:length(uniqueTj) ){
			x <- c( x, paste0( "  real ",c('AhashjpT'),uniqueTj[i],"[F*F,F*F,NperT",ifelse(Tjn>1,paste0('[',i,']'),''),",",uniqueTj[i],"-1]; // Ahash matrices for ",NperT[i]," persons with ",uniqueTj[i]," time points" ) )
		}
	} else if( KF ) {
		for( i in 1:length(uniqueTj) ){
			x <- c( x, paste0( "  real ",c('AhashjpFirstTimepointT'),uniqueTj[i],"[F*F,F*F,NperT",ifelse(Tjn>1,paste0('[',i,']'),''),",1]; // Ahash matrices for ",NperT[i]," persons with ",uniqueTj[i]," time points" ) )
		}
	}
	# Astarjp, Qstarjp, Ahashjp, Sigmawjp
	x <- c( x, paste0( "  // ##### loops over person groups with same number of time points T #########################################" ) )
	for( i in 1:length(uniqueTj) ){
		x <- c( x, paste0( "  // #",i,"# loop over time points of persons with T=",uniqueTj[i] ) )		
	    x <- c( x, paste0( "  for (j in Tjlow",ifelse(Tjn>1,paste0('[',i,']'),''),":Tjup",ifelse(Tjn>1,paste0('[',i,']'),''),"){ // range: ",Tjlow[i],":",Tjup[i],", NperTcum=",NperTcum[i],", NperT=",NperT[i] ) )		
	    if( !KF ){
			x <- c( x, paste0( "    for (p in 1:(",ifelse(N>1,"Tj[j]","Tj"),"-1)){ // range: 1:",uniqueTj[i]-1 ) )
			x <- c( x, paste0( "      // Astarjp, Eq. 12" ) )
			x <- c( x, paste0( "      for (i in 1:F){" ) )
			x <- c( x, paste0( "        for (k in 1:F){" ) )
			x <- c( x, paste0( "          AstarjpT",uniqueTj[i],"[i,k,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p] = matrix_exp(to_matrix(AjpT",uniqueTj[i],"[,,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p])*deltajpT",uniqueTj[i],"[j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p])[i,k];" ) )
			x <- c( x, paste0( "        }" ) )
			x <- c( x, paste0( "      }" ) )
			x <- c( x, paste0( "      // Ahashjp, Eq. 14" ) )
			x <- c( x, paste0( "      for (i in 1:(F*F)){" ) )
			x <- c( x, paste0( "        for (k in 1:(F*F)){" ) )
			x <- c( x, paste0( "          AhashjpT",uniqueTj[i],"[i,k,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p] = (kronecker_product(to_matrix(AjpT",uniqueTj[i],"[,,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p]),IF) + kronecker_product(IF,to_matrix(AjpT",uniqueTj[i],"[,,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p])))[i,k];" ) )
			x <- c( x, paste0( "        }" ) )
			x <- c( x, paste0( "      }" ) )
			x <- c( x, paste0( "      // Sigmawjp, Eq. 14" ) )
			x <- c( x, paste0( "      for (i in 1:F){" ) )
			x <- c( x, paste0( "        for (k in 1:F){" ) )
			x <- c( x, paste0( "          SigmawjpT",uniqueTj[i],"[i,k,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p] = unflatten_vector_to_matrix(-1*inverse(to_matrix(AhashjpT",uniqueTj[i],"[,,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p])) * flatten_matrix_rowwise(to_matrix(QjpT",uniqueTj[i],"[,,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p])),F,F)[i,k];" ) )
			x <- c( x, paste0( "        }" ) )
			x <- c( x, paste0( "      }" ) )
			x <- c( x, paste0( "      // Sigmawjp Cholesky" ) )
			x <- c( x, paste0( "      for (i in 1:F){" ) )
			x <- c( x, paste0( "        for (k in 1:F){" ) )
			x <- c( x, paste0( "          SigmawjpCholT",uniqueTj[i],"[i,k,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p] = cholesky_decompose( to_matrix(SigmawjpT",uniqueTj[i],"[,,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p]) )[i,k];" ) )
			x <- c( x, paste0( "        }" ) )
			x <- c( x, paste0( "      }" ) )			
			x <- c( x, paste0( "      // Qstarjp, Eq. 13" ) )
			x <- c( x, paste0( "      for (i in 1:F){" ) )
			x <- c( x, paste0( "        for (k in 1:F){" ) )
			x <- c( x, paste0( "          QstarjpT",uniqueTj[i],"[i,k,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p] = unflatten_vector_to_matrix( -1*( matrix_exp(to_matrix(AhashjpT",uniqueTj[i],"[,,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p])*deltajpT",uniqueTj[i],"[j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p]) - IF2 ) * flatten_matrix_rowwise(to_matrix(SigmawjpT",uniqueTj[i],"[,,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p])),F,F)[i,k];" ) )
			x <- c( x, paste0( "        }" ) )
			x <- c( x, paste0( "      }" ) )
			x <- c( x, paste0( "      // Qstarjp Cholesky" ) )
			x <- c( x, paste0( "      for (i in 1:F){" ) )
			x <- c( x, paste0( "        for (k in 1:F){" ) )
			x <- c( x, paste0( "          QstarjpCholT",uniqueTj[i],"[i,k,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p] = cholesky_decompose( to_matrix(QstarjpT",uniqueTj[i],"[,,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p]) )[i,k];" ) )
			x <- c( x, paste0( "        }" ) )
			x <- c( x, paste0( "      }" ) )			
			x <- c( x, paste0( "    }" ) )
		} else if( KF ){ # outside time loop because it's just for first time point!
			x <- c( x, paste0( "      // Ahashjp, Eq. 14" ) )
			x <- c( x, paste0( "      for (i in 1:(F*F)){" ) )
			x <- c( x, paste0( "        for (k in 1:(F*F)){" ) )
			x <- c( x, paste0( "          AhashjpFirstTimepointT",uniqueTj[i],"[i,k,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",1] = (kronecker_product(to_matrix(AjpFirstTimepointT",uniqueTj[i],"[,,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",1]),IF) + kronecker_product(IF,to_matrix(AjpFirstTimepointT",uniqueTj[i],"[,,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",1])))[i,k];" ) )
			x <- c( x, paste0( "        }" ) )
			x <- c( x, paste0( "      }" ) )
			x <- c( x, paste0( "      // Sigmawjp, Eq. 14" ) )
			x <- c( x, paste0( "      for (i in 1:F){" ) )
			x <- c( x, paste0( "        for (k in 1:F){" ) )
			x <- c( x, paste0( "          SigmawjpFirstTimepointT",uniqueTj[i],"[i,k,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",1] = unflatten_vector_to_matrix(-1*inverse(to_matrix(AhashjpFirstTimepointT",uniqueTj[i],"[,,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",1])) * flatten_matrix_rowwise(to_matrix(QjpFirstTimepointT",uniqueTj[i],"[,,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",1])),F,F)[i,k];" ) )
			x <- c( x, paste0( "        }" ) )
			x <- c( x, paste0( "      }" ) )
			x <- c( x, paste0( "      // Sigmawjp Cholesky" ) )
			x <- c( x, paste0( "      for (i in 1:F){" ) )
			x <- c( x, paste0( "        for (k in 1:F){" ) )
			x <- c( x, paste0( "          SigmawjpFirstTimepointCholT",uniqueTj[i],"[i,k,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",1] = cholesky_decompose( to_matrix(SigmawjpFirstTimepointT",uniqueTj[i],"[,,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",1]) )[i,k];" ) )
			x <- c( x, paste0( "        }" ) )
			x <- c( x, paste0( "      }" ) )			
			x <- c( x, paste0( "      // Qstarjp, Eq. 13" ) )
			x <- c( x, paste0( "      for (i in 1:F){" ) )
			x <- c( x, paste0( "        for (k in 1:F){" ) )
			x <- c( x, paste0( "          QstarjpFirstTimepointT",uniqueTj[i],"[i,k,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",1] = unflatten_vector_to_matrix( -1*( matrix_exp(to_matrix(AhashjpFirstTimepointT",uniqueTj[i],"[,,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",1])*deltajpT",uniqueTj[i],"[j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",1]) - IF2 ) * flatten_matrix_rowwise(to_matrix(SigmawjpFirstTimepointT",uniqueTj[i],"[,,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",1])),F,F)[i,k];" ) )
			x <- c( x, paste0( "        }" ) )
			x <- c( x, paste0( "      }" ) )
			x <- c( x, paste0( "      // Qstarjp Cholesky" ) )
			x <- c( x, paste0( "      for (i in 1:F){" ) )
			x <- c( x, paste0( "        for (k in 1:F){" ) )
			x <- c( x, paste0( "          QstarjpFirstTimepointCholT",uniqueTj[i],"[i,k,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",1] = cholesky_decompose( to_matrix(QstarjpFirstTimepointT",uniqueTj[i],"[,,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",1]) )[i,k];" ) )
			x <- c( x, paste0( "        }" ) )
			x <- c( x, paste0( "      }" ) )
		}
		x <- c( x, paste0( "  }" ) )

	}
	# MH 0.0.32/33 2024-05-10/11 Kalman Filter		
	if( KF ){
		x <- c( x, paste0( "  // Kalman Filter" ) )
		# latent values of persons with same number of time points
		for( i in 1:length(uniqueTj) ){
			x <- c( x, paste0( "  real thetajpT",uniqueTj[i],"[F,1,NperT",ifelse(Tjn>1,paste0('[',i,']'),''),",",uniqueTj[i],"]; // latent values of ",NperT[i]," persons with ",uniqueTj[i]," time points" ) )
		}
		# 0.0.45, crashes for F=1, positive definiteness check fails for P and other cov_matrices
		if( F==1 ) {
			x <- c( x, paste0( "  matrix[F,F] P[T];" ) )
			x <- c( x, paste0( "  matrix[F,F] P_pred;" ) )
		} else {
			x <- c( x, paste0( "  cov_matrix[F] P[T];" ) )
			x <- c( x, paste0( "  cov_matrix[F] P_pred;" ) )
		}
		x <- c( x, paste0( "  matrix[F,F] K;" ) )
		x <- c( x, paste0( "  vector[F] thetajp_pred;" ) )
		x <- c( x, paste0( "  matrix[F,F] Astarjp;" ) )
		x <- c( x, paste0( "  matrix[F*F,F*F] Ahashjp;" ) )
		x <- c( x, paste0( "  matrix[F,F] Sigmawjp;" ) )
		x <- c( x, paste0( "  matrix[F,F] SigmawjpChol;" ) )
		x <- c( x, paste0( "  matrix[F,F] Qstarjp;" ) )
		x <- c( x, paste0( "  matrix[F,F] QstarjpChol;" ) )
		x <- c( x, paste0( "  matrix[F,F] ",c('Ajp','Qjp'),";" ) )

		x <- c( x, paste0( "  // ##### loops over person groups with same number of time points T #########################################" ) )

		for( i in 1:length(uniqueTj) ){
			x <- c( x, paste0( "  // #",i,"# loop over time points of persons with T=",uniqueTj[i] ) )
			x <- c( x, paste0( "  for (j in Tjlow",ifelse(Tjn>1,paste0('[',i,']'),''),":Tjup",ifelse(Tjn>1,paste0('[',i,']'),''),"){ // range: ",Tjlow[i],":",Tjup[i],", NperTcum=",NperTcum[i],", NperT=",NperT[i] ) )
			
			x <- c( x, paste0( "    // initial state" ) )
			x <- c( x, paste0( "    thetajpT",uniqueTj[i],"[,1,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",1] = thetajpFirstTimepointT",uniqueTj[i],"[,1,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",1];" ) )
			x <- c( x, paste0( "    // initial variance" ) )
			# x <- c( x, paste0( "    P[1] = to_matrix( SigmawjpT",uniqueTj[i],"[,,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",1] );" ) )
			x <- c( x, paste0( "    P[1] = to_matrix( SigmawjpFirstTimepointT",uniqueTj[i],"[,,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",1] );" ) )
			
			x <- c( x, paste0( "    // loop beginning at second time points" ) )
			x <- c( x, paste0( "    for (p in 2:",ifelse(N>1,"Tj[j]","Tj"),"){ // range: 2:",uniqueTj[i] ) )

			x <- c( x, paste0( "      // Eq. 11" ) )
			x <- c( x, paste0( "      ",c('Ajp','Qjp')," = to_matrix(",c('At','Qt'),"[,,ptuniquejpT",uniqueTj[i],"[j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p-1]]);" ) )
			x <- c( x, paste0( "      // Astarjp, Eq. 12" ) )
			# x <- c( x, paste0( "      Astarjp = matrix_exp(to_matrix(AjpT",uniqueTj[i],"[,,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p-1])*deltajpT",uniqueTj[i],"[j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p-1]);" ) )
			x <- c( x, paste0( "      Astarjp = matrix_exp( Ajp * deltajpT",uniqueTj[i],"[j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p-1]);" ) )
			x <- c( x, paste0( "      // Ahashjp, Eq. 14" ) )
			# x <- c( x, paste0( "      Ahashjp = kronecker_product(to_matrix(AjpT",uniqueTj[i],"[,,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p]),IF) + kronecker_product(IF,to_matrix(AjpT",uniqueTj[i],"[,,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p]));" ) )
			x <- c( x, paste0( "      Ahashjp = kronecker_product(Ajp,IF) + kronecker_product(IF,Ajp);" ) )
			x <- c( x, paste0( "      // Sigmawjp, Eq. 14" ) )
			# x <- c( x, paste0( "      Sigmawjp = unflatten_vector_to_matrix(-1*inverse(Ahashjp) * flatten_matrix_rowwise(to_matrix(QjpT",uniqueTj[i],"[,,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p])),F,F);" ) )
			x <- c( x, paste0( "      Sigmawjp = unflatten_vector_to_matrix(-1*inverse(Ahashjp) * flatten_matrix_rowwise(Qjp) ,F,F);" ) )
			x <- c( x, paste0( "      // Sigmawjp Cholesky" ) )
			x <- c( x, paste0( "      SigmawjpChol = cholesky_decompose( Sigmawjp );" ) )
			x <- c( x, paste0( "      // Qstarjp, Eq. 13" ) )
			x <- c( x, paste0( "      Qstarjp = unflatten_vector_to_matrix( -1*( matrix_exp( Ahashjp * deltajpT",uniqueTj[i],"[j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p-1]) - IF2 ) * flatten_matrix_rowwise( Sigmawjp ),F,F);" ) )
			x <- c( x, paste0( "      // Qstarjp Cholesky" ) )
			x <- c( x, paste0( "      QstarjpChol = cholesky_decompose( Qstarjp );" ) )				
			
			x <- c( x, paste0( "      // time series, Eq. 15/16 " ) )
			x <- c( x, paste0( "      // predict phase" ) )
			# x <- c( x, paste0( "      to_vector( thetajpT",uniqueTj[i],"[,1,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p] ) = to_matrix(AstarjpT",uniqueTj[i],"[,,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p-1])*to_vector(thetajpT",uniqueTj[i],"[,1,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p-1]);" ) )
			# x <- c( x, paste0( "      thetajp_pred = to_matrix(AstarjpT",uniqueTj[i],"[,,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p-1]) * to_vector(thetajpT",uniqueTj[i],"[,1,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p-1]);" ) )
			x <- c( x, paste0( "      thetajp_pred = Astarjp * to_vector(thetajpT",uniqueTj[i],"[,1,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p-1]);" ) )
			# x <- c( x, paste0( "      P_pred = to_matrix(AstarjpT",uniqueTj[i],"[,,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p-1]) * P[p-1] * to_matrix(AstarjpT",uniqueTj[i],"[,,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p-1])' + to_matrix(QstarjpT",uniqueTj[i],"[,,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p-1]) ;" ) )
			# x <- c( x, paste0( "      P_pred = Astarjp * P[p-1] * Astarjp + to_matrix(QstarjpT",uniqueTj[i],"[,,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p-1]) ;" ) )
			x <- c( x, paste0( "      P_pred = Astarjp * P[p-1] * Astarjp + Qstarjp;" ) )
			x <- c( x, paste0( "      // update phase" ) )				
			x <- c( x, paste0( "      K = P_pred * inverse( P_pred + Sigmaeps ); // Kalman gain" ) )
			x <- c( x, paste0( "      for(k in 1:F){" ) )
			x <- c( x, paste0( "        thetajpT",uniqueTj[i],"[k,1,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p] = ( thetajp_pred + K * to_vector( yjpT",uniqueTj[i],"[,1,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p] ) - thetajp_pred )[k];" ) )
			x <- c( x, paste0( "      }" ) )				
			x <- c( x, paste0( "      P[p] = (IF - K) * P_pred;" ) )
			
			x <- c( x, paste0( "    }" ) )
			x <- c( x, paste0( "  }" ) )
		}
	}
	
	# end transformed parameters
	x <- c( x, paste0( "}" ) )
	
	## model
	x <- c( x, paste0( "model {" ) )
	# x <- c( x, paste0( "  // ##### errors of time-varying parameters ##################################################################" ) )
	# x <- c( x, paste0( "  for( p in 1:Tunique ){" ) )
	# MH 0.0.30 2024-05-07, no epsAt
	# x <- c( x, paste0( "    // epsAt, Eq. 4" ) )
	# x <- c( x, paste0( "    to_vector( epsAt[,1,p] )  ~ multi_normal_cholesky( to_vector(zerovecF2)  , SigmaepsAChol );" ) )
	# MH 0.0.27 2024-05-04 Q0 is Q (not time-varying)
	# x <- c( x, paste0( "    // epsQt, Eq. 5" ) )
	# x <- c( x, paste0( "    to_vector( epsQt[,1,p] )  ~ multi_normal_cholesky( to_vector(zerovecFF12), SigmaepsQChol );" ) )
	# 0.0.29 2024-05-06, no mean
	# x <- c( x, paste0( "    // epsmut, Eq. 10" ) )		
	# x <- c( x, paste0( "    to_vector( epsmut[,1,p] ) ~ multi_normal_cholesky( to_vector(zerovecF)   , SigmaepsmuChol );" ) )
	# x <- c( x, paste0( "  }" ) )
	if( between.mu ) x <- c( x, paste0( "  // ##### person-specific deviation from mean ################################################################" ) )		
	if( between.mu ) x <- c( x, paste0( "  for( j in 1:N ){" ) )
	if( between.mu ) x <- c( x, paste0( "    // muj, Eq. 8" ) )
	if( between.mu ) x <- c( x, paste0( "    to_vector( muj[,1,j] ) ~ multi_normal_cholesky( to_vector(zerovecF), SigmamuChol );" ) )
	if( between.mu ) x <- c( x, paste0( "  }" ) )
	x <- c( x, paste0( "  // ##### loops over person groups with same number of time points T #########################################" ) )
	for( i in 1:length(uniqueTj) ){
		x <- c( x, paste0( "  // #",i,"# loop over time points of persons with T=",uniqueTj[i] ) )
		x <- c( x, paste0( "  for (j in Tjlow",ifelse(Tjn>1,paste0('[',i,']'),''),":Tjup",ifelse(Tjn>1,paste0('[',i,']'),''),"){ // range: ",Tjlow[i],":",Tjup[i],", NperTcum=",NperTcum[i],", NperT=",NperT[i] ) )
		x <- c( x, paste0( "    // first time point, Eq. 17" ) )
							# x <- c( x, paste0( "        print( \"Sigmawjp: \", to_matrix(SigmawjpT",uniqueTj[i],"[,,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",1]) );" ) )
		# x <- c( x, paste0( "    to_vector( thetajpT",uniqueTj[i],"[,1,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",1] ) ~ multi_normal_cholesky(to_vector(mujpT",uniqueTj[i],"[,1,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",1]),to_matrix(SigmawjpCholT",uniqueTj[i],"[,,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",1]));" ) )
		# 0.0.29 2024-05-06, no mean
		if( !KF ) x <- c( x, paste0( "    to_vector( thetajpT",uniqueTj[i],"[,1,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",1] ) ~ multi_normal_cholesky(to_vector(zerovecF),to_matrix(SigmawjpCholT",uniqueTj[i],"[,,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",1]));" ) )
		# different structure for first time point, because first time point is still sampled in KF
		# if(  KF ) x <- c( x, paste0( "    to_vector( thetajpFirstTimepointT",uniqueTj[i],"[,1,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",1] ) ~ multi_normal_cholesky(to_vector(zerovecF),to_matrix(SigmawjpCholT",uniqueTj[i],"[,,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",1]));" ) )
		if(  KF ) x <- c( x, paste0( "    to_vector( thetajpFirstTimepointT",uniqueTj[i],"[,1,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",1] ) ~ multi_normal_cholesky(to_vector(zerovecF),to_matrix(SigmawjpFirstTimepointCholT",uniqueTj[i],"[,,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",1]));" ) )
		# MH 0.0.32 2024-05-10 Kalman Filter
		if( !KF ){
			x <- c( x, paste0( "    // loop beginning at second time points" ) )
			x <- c( x, paste0( "    for (p in 2:",ifelse(N>1,"Tj[j]","Tj"),"){ // range: 2:",uniqueTj[i] ) )
			x <- c( x, paste0( "      // time series, Eq. 15/16 " ) )
								# x <- c( x, paste0( "        print( \"QstarjpT: \", to_matrix(QstarjpT",uniqueTj[i],"[,,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p-1]) );" ) )
			# x <- c( x, paste0( "      to_vector( thetajpT",uniqueTj[i],"[,1,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p] ) ~ multi_normal_cholesky(to_matrix(AstarjpT",uniqueTj[i],"[,,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p-1])*to_vector(thetajpT",uniqueTj[i],"[,1,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p-1])+(IF - to_matrix(AstarjpT",uniqueTj[i],"[,,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p-1]))*to_vector(mujpT",uniqueTj[i],"[,1,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p]),to_matrix(QstarjpCholT",uniqueTj[i],"[,,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p-1]));" ) )
			# 0.0.29 2024-05-06, no mean
			x <- c( x, paste0( "      to_vector( thetajpT",uniqueTj[i],"[,1,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p] ) ~ multi_normal_cholesky(to_matrix(AstarjpT",uniqueTj[i],"[,,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p-1])*to_vector(thetajpT",uniqueTj[i],"[,1,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p-1]),to_matrix(QstarjpCholT",uniqueTj[i],"[,,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p-1]));" ) )
			x <- c( x, paste0( "    }" ) )
		}
		# 0.0.29 2024-05-06, no measurement model <=== yjpT needs to be in, so no measurement model rather means setting measurement error to zero
		if( !KF ) {
			x <- c( x, paste0( "    // loop over all time points" ) )
			x <- c( x, paste0( "    for (p in 1:",ifelse(N>1,"Tj[j]","Tj"),"){ // range: 1:",uniqueTj[i] ) )
			x <- c( x, paste0( "      // measurement model, Eq. 18/19 " ) )
			# x <- c( x, paste0( "      to_vector( yjpT",uniqueTj[i],"[,1,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p] ) ~ multi_normal_cholesky( Delta*to_vector(thetajpT",uniqueTj[i],"[,1,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p]),SigmaepsChol);" ) )
			# for no measurement model: Delta = IF
			x <- c( x, paste0( "      to_vector( yjpT",uniqueTj[i],"[,1,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p] ) ~ multi_normal_cholesky( IF * to_vector(thetajpT",uniqueTj[i],"[,1,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p]),SigmaepsChol);" ) )
			x <- c( x, paste0( "    }" ) )
		}			

		x <- c( x, paste0( "  }" ) )
	}
	
	# priors for parameters of mixed structures
	# if( length( parameters.of.mixed.structures ) > 0 ){		
		# x <- c( x, paste0( "  // ##### priors for parameters of mixed structures #################" ) )
		# eval( parse( text=paste0("x <- c( x, '  ",parameters.of.mixed.structures," ~ normal( 1, 0.25 );' )" ) ) )
	# }
	# priors for covariance matrices
	# x <- c( x, paste0( "  // priors for covariance matrices" ) )
	# x <- c( x, paste0( "  Sigmaeps ~ inv_wishart(I+3, scale_matrix);" ) )
	# x <- c( x, paste0( "  SigmaepsA ~ inv_wishart(F*F+3, scale_matrix);" ) )
	# x <- c( x, paste0( "  SigmaepsQ ~ inv_wishart(F*(F+1)/2+3, scale_matrix);" ) )
	# x <- c( x, paste0( "  Q0 ~ inv_wishart(F+3, scale_matrix_F);" ) )
	# x <- c( x, paste0( "  Qchange ~ inv_wishart(F+3, scale_matrix_F);" ) )
	
	# priors
	# x <- c( x, paste0( "  Q0[1,1] ~ inv_gamma(2, 1);" ) )
	# x <- c( x, paste0( "  Q0[2,2] ~ inv_gamma(2, 1);" ) )
	# x <- c( x, paste0( "  A0[1,1] ~ normal(-0.75, 0.25) T[, 0];" ) )
	# x <- c( x, paste0( "  A0[2,2] ~ normal(-0.75, 0.25) T[, 0];" ) )
	# x <- c( x, paste0( "  A0[1,2] ~ normal(0.1, 0.25);" ) )
	# x <- c( x, paste0( "  A0[2,1] ~ normal(0.1, 0.25);" ) )
	# x <- c( x, paste0( "  Achange[1,1] ~ normal(0,0.01);" ) )
	# x <- c( x, paste0( "  Achange[2,2] ~ normal(0,0.01);" ) )
	# x <- c( x, paste0( "  Achange[1,2] ~ normal(0,0.01);" ) )
	# x <- c( x, paste0( "  Achange[2,1] ~ normal(0,0.01);" ) )

	# defaults
	empty.str.env <- gen.empty.structures(env=data.env)
	A0.prior <- get( "A0", envir=empty.str.env, inherits=FALSE )
	Achange.prior <- get( "Achange", envir=empty.str.env, inherits=FALSE )
	Q0.prior <- get( "Q0", envir=empty.str.env, inherits=FALSE )
	
	A0.prior[] <- "normal(0,0.25)"
	diag( A0.prior ) <- "normal(-0.69,0.25) T[,-1e-3]"
	# Achange.prior[] <- "normal(0,0.0625)"
	# MH 0.0.43
	Achange.prior[] <- "normal(0,9)"
	Q0.prior[] <- "normal(0,1)"
	diag( Q0.prior ) <- "inv_gamma(3,1)"
	
	# get elements of prior.env and overwrite defaults
	if( !is.null( prior.env ) ){
		if( "A0"      %in% ls(envir=prior.env) ){
			A0.prior2      <- get('A0'         , envir=prior.env, inherits=FALSE)
			for (i in 1:dim(A0.prior2)[1]) {
				for (j in 1:dim(A0.prior2)[2]) {
					if( !is.na( A0.prior2[i,j] ) ) A0.prior[i,j] <- A0.prior2[i,j]
				}
			}
		}
		if( "Achange" %in% ls(envir=prior.env) ){
			Achange.prior2 <- get('Achange'    , envir=prior.env, inherits=FALSE)
			for (i in 1:dim(Achange.prior2)[1]) {
				for (j in 1:dim(Achange.prior2)[2]) {
					if( !is.na( Achange.prior2[i,j] ) ) Achange.prior[i,j] <- Achange.prior2[i,j]
				}
			}
		}
		if( "Q0"      %in% ls(envir=prior.env) ){
			Q0.prior2      <- get('Q0'         , envir=prior.env, inherits=FALSE)
			for (i in 1:dim(Q0.prior2)[1]) {
				for (j in 1:dim(Q0.prior2)[2]) {
					if( !is.na( Q0.prior2[i,j] ) ) Q0.prior[i,j] <- Q0.prior2[i,j]
				}
			}
		}
	}

	## alignment with fixed values
	# if NA or as.numeric=NA, then it's a free parameter, otherwise fixed
	# fixed parameters get NA as prior
	for (i in 1:dim(A0.prior)[1]) {
		for (j in 1:dim(A0.prior)[2]) {
			if( !is.na( suppressWarnings( as.numeric( A0[i,j] ) ) ) ) A0.prior[i,j] <- NA
		}
	}
	for (i in 1:dim(Achange.prior)[1]) {
		for (j in 1:dim(Achange.prior)[2]) {
			if( !is.na( suppressWarnings( as.numeric( Achange[i,j] ) ) ) ) Achange.prior[i,j] <- NA
		}
	}
	for (i in 1:dim(Q0.prior)[1]) {
		for (j in 1:dim(Q0.prior)[2]) {
			if( !is.na( suppressWarnings( as.numeric( Q0[i,j] ) ) ) ) Q0.prior[i,j] <- NA
		}
	}

	priors <- c( "A0.prior", "Achange.prior", "Q0.prior" )

	# put used priors on environment to return
	if( is.null( prior.env ) ){
		used.prior.env <- new.env()
		for( i in 1:length( priors ) ){
			assign( priors[i], eval( parse( text=priors[i] ) ), envir = used.prior.env, inherits = FALSE, immediate=TRUE )
		}
	} else {
		used.prior.env <- prior.env
	}
	
	if( verbose ){
		cat( paste0( "priors used for estimation:\n" ) )
		cat( paste0( "A0:\n" ) )
		print( A0.prior )
		cat( paste0( "Achange:\n" ) )
		print( Achange.prior )
		cat( paste0( "Q0:\n" ) )
		print( Q0.prior )
		cat( paste0( "----------------------------------------------------------------------------\n" ) )
	}
	
	# Stan code to set priors
	priors.dim1 <- c( dim(A0.prior)[1], dim(Achange.prior)[1], dim(Q0.prior)[1] )
	priors.dim2 <- c( dim(A0.prior)[2], dim(Achange.prior)[2], dim(Q0.prior)[2] )

	x <- c( x, paste0( "  // priors" ) )
	for( i in 1:length( priors ) ){
		for (j in 1:priors.dim1[i]) {
			for (k in 1:priors.dim2[i]) {
				if( !is.na( eval(parse(text=paste0( priors[i], "[",j,",",k,"]" ))) ) ) {
					# label or not
					lab1 <- eval(parse(text=paste0( sub(".prior","",priors[i],fixed=TRUE),"[",j,",",k,"]" ) ) )
					lab2 <- paste0( sub(".prior","",priors[i],fixed=TRUE),"[",j,",",k,"]" )
					if( is.na( eval(parse(text=paste0( "suppressWarnings(as.numeric(",sub(".prior","",priors[i],fixed=TRUE), "[",j,",",k,"]))" )))) & !is.na( eval(parse(text=paste0( sub(".prior","",priors[i],fixed=TRUE), "[",j,",",k,"]" )))) ){
						lab <- lab1
						labcomment <- paste0( " // ", lab2 )
					} else {
						lab <- lab2
						labcomment <- ""
					}
					x <- c( x, paste0( "  ",lab," ~ ",eval(parse(text=paste0( priors[i], "[",j,",",k,"]" ))),";",labcomment ) )
				}
			}
		}
	}
	
	# end model
	x <- c( x, paste0( "}" ) )
	
	## empty line
	x <- c( x, paste0( "" ) )
	
	## write file
	file.name <- paste0( model_name, ".stan" )
	syntax.path <- file.path( syntax.dir, file.name )
	write( x, file=syntax.path, sep="\n" )
	# remove rds
	rds.path <- file.path( syntax.dir, paste0( file.name, ".rds" ) )
	invisible( if( file.exists( rds.path ) ) file.remove( rds.path ) )

	## data
	dat <- list( "F"=F, "I"=I, "N"=N, "T"=T, "tunique"=tunique, "Tunique"=Tunique, "Tj"=Tj, "Tjn"=as.integer(Tjn), "Tjlow"=Tjlow, "Tjup"=Tjup, "NperT"=NperT, "NperTcum"=NperTcum, "IF"=IF, "IF2"=IF2, "S1"=S1, "S2"=S2, "zerovecF"=zerovecF, "zerovecF2"=zerovecF2, "zerovecFF12"=zerovecFF12 ) # "S3"=S3,
	# yjpT, deltajpT, ptuniquejpT
	dat <- c( dat, yjpT, deltajpT, ptuniquejpT ) # , AjpT, QjpT, mujpT,
	# add fixed structures
	if( length( wifis ) > 0 ){
		for( st in names( structure.type[wifis] ) ){
			eval( parse( text=paste0( "dat$", st, " <- ", st ) ) )
		}
	}

	# get default start values
	if( is.null( chains ) ){
		chains <- 1
		jitter <- FALSE
	} else { # chains>=1
		jitter <- TRUE
	}

	inits <- start.values( F=F, chains=chains, jitter=jitter, par.env=par.env, return.init.only=FALSE, verbose=verbose ) # always call with return.init.only=FALSE as this will return a list that is used in the next steps

	# return
	ret <- list( "data"=dat, "file"=syntax.path, "model_name"=model_name, "init"=inits$init, "model.parameters"=model.parameters, "par.env"=par.env, "prior.env"=used.prior.env, "sv"=inits$sv, "seed.jitter.sv"=inits$seed.jitter.sv ) # "start.values.env"=used.start.values.env

}




### development
# user.profile <- shell( "echo %USERPROFILE%", intern=TRUE )
# Rfiles.folder <- file.path( user.profile,
                                    # "Dropbox/139_conttime/conttime/R" )
# Rfiles <- list.files( Rfiles.folder , pattern="*.R" )
# Rfiles <- Rfiles[ !Rfiles %in% c("gen.stan.R") ]
# for( Rfile in Rfiles ){
	# source( file.path( Rfiles.folder, Rfile ) )
# }

# design.env <- gen.design()
# data.env <- gen.data( design.env=design.env )
# stn <- gen.stan( data.env=data.env, syntax.dir="C:/users/martin/Desktop/temp" )

## require packages
# require( rstan )
## options( mc.cores = (parallel::detectCores()-1) )
# options( mc.cores = 1 ); rstan_options(auto_write = TRUE)

## fit
# fit <- stan( file = stn$syntax.path, data = stn$data, init=stn$init, chains = 1, iter = 10 )

# est <- get.stan( fit=fit, stn=stn, true.env=data.env )


# ( summ <- summary(fit, pars = c("Delta", "A0"))$summary )

# m <- as.matrix(fit)
# colMeans( m[,grepl("^Delta.*$",colnames(m))] )
# colMeans( m[,grepl("^A0.*$",colnames(m))] )
# colMeans( m[,grepl("^Achange.*$",colnames(m))] )
# colMeans( m[,grepl("^Q0\\[.*$",colnames(m))] )
# colMeans( m[,grepl("^Qchange\\[.*$",colnames(m))] )
# colMeans( m[,grepl("^SigmaepsA.*$",colnames(m))] )
# colMeans( m[,grepl("^SigmaepsQ.*$",colnames(m))] )
# colMeans( m[,grepl("^Sigmaeps\\[.*$",colnames(m))] )



### test
# require( testthat )
# test_file("../tests/testthat/XXXXX.R")
