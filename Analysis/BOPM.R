##  Bivariate Ordered Probit Model

model {
	# LOOP OVER N RESPONDENTS
	for (i in 1:N) {        
	    # STRUCTURAL EQUATION FOR y1
	    mu[i,1] <- inprod(B[], X[i,])
	    
	    # STURCTURAL EQUATION FOR y2
	    mu[i,2] <- inprod(G[], Z[i,])

		# TWO LATENT OUTCOME VARIABLE
	    ystar[i,1:2] ~ dmnorm (mu[i,1:2], Tau[1:2,1:2])
	    
	    # DISTRIBUTIONS OF OBSERVED OUTCOME VARIABLES; dcat REQUIRES y1 TO BE CODED 1,2
	    y1[i] ~ dcat(p[i,1,])
	    y2[i] ~ dcat(p[i,2,])
		
		# LOOP OVER TWO OUTCOME VARIABLES
	    for (j in 1:2) {	    	
			# CUMULATIVE PROBABILITIES
	    	probit(Q[i,j,1]) <- gamma[j,1] - ystar[i,j]
	    	p[i,j,1] <- Q[i,j,1]

	    	# L CATEGORIES
	    	for (l in 2:(L-1)) {
	    		probit(Q[i,j,l]) <- gamma[j,l] - ystar[i,j]
	    		# TRICKS TO GET SLICE OF THE CDF
	    		p[i,j,l] <- Q[i,j,l] - Q[i,j,l-1]
	    	}
	    	p[i,j,L] <- 1 - Q[i,j,L-1]
	    }
    }    

	Tau[1:2,1:2] <- inverse(Sigma[,])
	Sigma[1,1] <- 1
	Sigma[2,2] <- 1
	Sigma[1,2] <- rho
	Sigma[2,1] <- rho
	
	# DISTRIBUTIONS FOR CORRELATION COEFFICIENT
    rho ~ dunif (-1, 1)
             
	# DISTRIBUTIONS FOR COEFFICIENTS
    for (k in 1:K) {
    	B[k] ~ dnorm (0, 0.01)
    }
    
    for (m in 1:M) {
    	G[m] ~ dnorm (0, 0.01)
    }

	# DISTRIBUTIONS FOR CUTPOINTS
    for (l in 1:(L-1)) {
    	gam[1,l] ~ dnorm (0, 0.01)
    	gam[2,l] ~ dnorm (0, 0.01)
    }    
    gamma[1,1:(L-1)] <- sort(gam[1,])
    gamma[2,1:(L-1)] <- sort(gam[2,])

} # END OF MODEL

