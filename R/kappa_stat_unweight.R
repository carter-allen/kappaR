#' Computes an unweighted kappa statistic
#' @param table A square confusion matrix
#' @param ci Return a confidence interval
#' @param alpha Confidence level for condifence interval
#' @examples
#' dgam_plot(a_square_matrix)

kappa_stat_unweight <- function(table, ci = FALSE, alpha = 0.05)
{
  library(usefulR)

  if(!is.matrix(table) || !(dim(table)[1] == dim(table)[2]))
  {
    stop("Pleast supply a square matrix")
  }

  n = sum(table)
  p_o = sum(diag(table))/n
  a_is = rowSums(table)/n
  b_is = colSums(table)/n
  p_e = sum(a_is * b_is)

  k = (p_o - p_e)/(1-p_e)
  se = sqrt((1/(n*(1-p_e)^2))*(p_e+p_e^2-sum(a_is*b_is*(a_is+b_is))))
  z_crit = qnorm(1-alpha/2)
  conf_int = usefulR::pm(k,z_crit*se)

  if(!ci)
  {
    return(k)
  }
  else
  {
    return(conf_int)
  }
}
