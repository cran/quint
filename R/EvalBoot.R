# Evaluation Function for Bootstrap Cis
# In: Results from Bootstrap
# Out: Bootstrap Confidence Intervals

EvalBoot <- function(dif_matrix, tree){

  CIs_leaf <- matrix(ncol = 2, nrow = ncol(dif_matrix))
  bias_leaf <-numeric(ncol(dif_matrix))
  sd_leaf <-numeric(ncol(dif_matrix))
  # confidence intervalls per leaf
  for ( i in 1:ncol(dif_matrix)){
    # find quantiles 2.5 and 97.5
    samp_var <- var(dif_matrix[,i])
    sd_leaf[i] <- sqrt(samp_var)
    est_dif <- tree$li[i,8]#tree$li$diff[i] Column 8 can be either d or diff
    bias_leaf[i] <- est_dif - mean(dif_matrix[,i])
    low_border <- est_dif - 1.96 * sd_leaf[i] #Addition Elise: also use 1.96 here (as with naive method)
    up_border <- est_dif + 1.96 * sd_leaf[i]
    CIs_leaf[i, 1] <- low_border
    CIs_leaf[i, 2] <- up_border
  }
  colnames(CIs_leaf) <- c('Lower_Boundary_CI', 'Upper-Boundary_CI')
  obj<-list(bias_est=bias_leaf, meanboot = apply(dif_matrix,2,mean),CIs=CIs_leaf, se_est=sd_leaf)
  return(obj)
}


