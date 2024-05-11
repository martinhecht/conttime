## Changelog:
# MH 2024-04-29:

# require packages
require( rstan )
# options( mc.cores = (parallel::detectCores()-1) )
options( mc.cores = 1 ); rstan_options(auto_write = TRUE)

# library( conttime ) # 0.0.32 2024-05-11

### development
library( mvtnorm )
library( expm )
user.profile <- shell( "echo %USERPROFILE%", intern=TRUE )
Rfiles.folder <- file.path( user.profile,
                                    "Dropbox/139_conttime/conttime/R" )
Rfiles <- list.files( Rfiles.folder , pattern="*.R" )
Rfiles <- Rfiles[ !Rfiles %in% c("") ]
for( Rfile in Rfiles ){
	source( file.path( Rfiles.folder, Rfile ) )
}

design.env <- gen.design()
# design.env <- gen.design(N=100,T=10,Tdiv=-2)
# A0 <- matrix( c(-0.80,0,0,-0.80), 2, 2 )
# value.env <- new.env()
# assign( "A0", A0, envir = value.env, inherits = FALSE, immediate=TRUE )

data.env <- gen.data( design.env=design.env, value.env=NULL )

# A0 <- matrix( c(-0.75,0,0,-0.75), 2, 2 )
# sv.env <- new.env()
# assign( "A0", A0, envir = sv.env, inherits = FALSE, immediate=TRUE )

stn <- gen.stan( data.env=data.env, KF=TRUE ) # , start.values.env=sv.env

                              
start <- Sys.time()
fit <- stan( file = stn$syntax.path, data = stn$data, init=stn$init, chains = 1, iter = 10 )
print( runtime <- Sys.time() - start )

save( fit, design.env, data.env, stn, runtime, file="c:/Users/martin/Dropbox/139_conttime/conttime/inst/examples/run3.Rdata" )
# load( file="c:/Users/martin/Dropbox/139_conttime/conttime/inst/examples/run1.Rdata" )

est <- get.stan( fit=fit, stn=stn, true.env=data.env )

# hist(est$bias)
est <- est[ order( abs( est$bias ), decreasing=TRUE ), ]


# print( fit )

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
