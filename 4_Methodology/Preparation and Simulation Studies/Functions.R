# ##############################################################################
# ### Title: Functions                                                       ### 
# ### Includes: Metrics; Probability matrices; Simulation.                   ###
# ##############################################################################

# Functions:
# Metric: ACC()
# Metric: F1m()
# Metric: MCC_matrix()
# Metric: MCC_cb()
# Probability matrix: PM()
# Simulation: S()

####### Metrics: ACC, F1m, MCC MCCcb 
# Accuracy = ACC
ACC <- function( cm ){
  # Input:  Confusion matrix with rows representing "predictions" and 
  #         the columns representing the "actuals".
  # Output: Accuracy: ratio between correctly predicted and total observations.
  
  ACC <- sum( diag( cm ) ) / sum( cm )
  return( ACC )
  
} # End ACC


# F1 macro averaged = F1m
F1m <- function( cm, positive.class = 1 ) {
  # Input: Confusion matrix with rows representing "predictions" and 
  #        the columns representing the "actuals".
  # Notes: "Positive class" is sensible only for binary matrices.         
  # Output is F1m: Class averaged F1 (i.e. the harmonic mean of the 
  #        precision and recall)  
  
  recall <- diag( cm ) / colSums( cm )
  precision <- diag( cm ) / rowSums( cm )
  f1 <-  ifelse( precision + recall == 0, 0, 
                 2 * ( ( precision * recall ) / ( precision + recall ) ) )
  f1[ is.na( f1 ) ] <- 0
  
  # Binary F1 or Multi-class macro-averaged F1
  ifelse( dim( cm )[ 1 ] == 2, f1[ positive.class ], mean( f1 ) )

} # End F1m


# Matthews correlation coefficient = MCC_matrix
MCC_matrix <- function( cm = confusionmatrix ) {
  # Input:     Confusion matrix with rows representing "predictions" and 
  #            the columns representing the "actuals".
  # Output:    Matthews correlation coefficient extention multi-class (MCC)
  # Reference: Rácz, A., Bajusz, D., & Héberger, K. (2019). Multi-Level 
  #            comparison of machine learning classifiers and their performance 
  #            metrics. Molecules (Basel, Switzerland), 24(15), 1–18. 
  #            https://doi.org/10.3390/molecules24152811
  
  # Initializing
  ncorrect = sum( diag( cm ) )
  n = as.numeric(sum(cm)) # Error without as.numeric()
  nlpred = rowSums( cm )
  nlactual = colSums( cm )
  numerator = ncorrect * n - sum( nlpred * nlactual )
  denominator = sqrt( n^2 - sum( nlpred^2 ) ) * sqrt( n^2 - sum( nlactual^2 ) )
  # Function
  mcc <- ifelse( ( numerator / denominator ) == "NaN", 0 , numerator / denominator )
  
  return(mcc)
  
} # End MCC_matrix


# Matthews correlation coefficient class balanced = MCC_cb
MCC_cb <- function( cm , nCl = 4){
  # Input:  Confusion matrix with rows representing "predictions" and 
  #         the columns representing the "actuals".
  # Notes:  The matrix is adjusted by the imbalance ratio of biggest and 
  #         smallest class. The MCC is calculated on this new matrix. 
  # Output: Matthews correlation coefficient class balanced (MCC_cb)
  
  IRj <- colSums( cm ) / ( max( colSums( cm ) ) ) # Class imbalance for j classes
  cm.adj <- ifelse( ( cm / ( matrix( 1, nCl, 1 ) %*% IRj ) ) == "NaN", 0, 
                    ( cm/( matrix(1, nCl, 1) %*% IRj ) ) )
  mcc <- MCC_matrix( cm.adj )
  return( mcc )
  
} # End MCC_cb


####### Probability matrices
PM <- function( IR, nMIN, MISCL, nCl = 4 ){
  # Input:  Imbalance ratio = IR, the number of minority classes = nMIN, 
  #         the type of misclassification = MISCL, and the number of 
  #         classes = nCl which is set to 4 because in this thesis only 4x4 
  #         matrices are considered. All but one numeric variables. 
  #         MISCL is a matrix.
  # Output: Probability matrix. Needed to draw samples from the multinomial 
  #         distribution.
  # Notes:  Order min/maj categories are fixed with all minority categories first. 
  
  # Empty matrix
  Mp <- matrix( 0, nCl, nCl )
  
  # Compute the max probability of minority and majority categories based on IR, 
  # nMIN, and nCL.
  min <- 1 / ( nMIN + ( nCl - nMIN ) * IR )
  maj <- IR * min 
  
  # Order min/maj categories are fixed with all minority categories first. 
  diag( Mp ) <- rep( c( min, maj ), times = c( nMIN, nCl - nMIN ) )
  
  # Probability matrix bij fout X (= MISCL).
  P <- MISCL %*% Mp # Remind the order in matrix multiplication
  
  # Check if probabilities add op to 1.
  if( as.factor( sum( P ) ) != 1 ) { stop( 'Probabilities dont add up to 1. 
                                           Reconsider your input' ) }
  
  return( P )
  
} # PM


######## Simulation
S <- function( n, p, nrep ) {
  # Input:  n = the number of samples that are draw from the multinomial distribution.
  #         p = the probability matrix that is calculated by p_vec().
  #         nrep = the number of repetitions.
  # Output: Dataframe with Accuracy, F1m, nMCC, nMCC_cb. 
  # Notes:  MCC and MCC_cb are normalized.
  #         n in replicate = number of replications = nrep = N
  
  cm <- replicate( n = nrep, expr = ( matrix( rmultinom( 1, n, p ), 4, 4) ) ) 
  
  # check if matrix is identical
  check <- list()
  
  for( i in 1:nrep ){ check[[ i ]] <- cbind( matrix( cm[ , , i ] ,4,4 ) ) }
  
  count <- 0
  
  while( any ( duplicated( check ) ) == TRUE ){ 
    count <- count + 1
    new   <- replicate( n = length( check[ duplicated( check ) == T ] ), 
                        expr = ( matrix( rmultinom( 1, n, p ), 4, 4 ) ) )
    for(i in length( check[ duplicated( check ) == T ] ) ) {
      check[ duplicated( check ) == T ][i] <- list( matrix( new[ , , i ] , 4, 4 ) )
    }
  }
  if( count != 0 ) {
    cat( count, 'extra round(s) for 100 unique matrices' ) }
  
  result <- list()
  for( i in 1:nrep ){
    result[[ i ]] <- cbind( ACC( cm[ , , i ] ), 
                            F1m( cm[ , , i ] ), 
                            .5*( MCC_matrix( cm[ , , i ] ) + 1 ), # Normalisation
                            .5*( MCC_cb( cm[ , , i ] ) + 1) )     # Normalisation
  }
  
  df <- data.frame( matrix( unlist( result), nrow = nrep, byrow = T), stringsAsFactors = F)
  names( df ) <- c( "Accuracy", "F1m", "MCC", "MCC.cb" )
  
  return( df )
} # End S()



