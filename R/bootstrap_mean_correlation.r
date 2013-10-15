mean_diff_correlation <- function(d, ind, set1, set2) {
    r1 <- cor(d[ind,set1])
    r2 <- cor(d[ind,set2])
    r_mean1 <- mean(r1[lower.tri(r1)])
    r_mean2 <- mean(r2[lower.tri(r2)])
    r_mean1 - r_mean2
}


#' @title Bootstrap difference in average correlation of two sets of variables
#' 
#' @description The function gets average correlation for set1 and set2 and obtains the difference
#' Bootstrapping is used to get confidence intervals on this difference
#' 
#' @param set1 character string of variable names in set 1 of data
#' @param set2 chracter string of variable names in set 2 of data
#' @param data data.frame or matrix containing variables set1 and set2
#' @param iterations positive integer indicating number of bootstrap iterations to run
#' @param ci number between 0 and 1 indicating size of bootstrap confidence interval. 
#' Default value is .95 representing a 95\% confidence interval. 
#' @return 
#' an object of class boot_mean_cor_diff
#' \item{boot_results}{object of class boot resulting from bootstrapping analysis}
#' \item{cor_matrix1}{numeric matrix: correlation matrix between set1 variables}
#' \item{cor_matrix2}{numeric matrix: correlation matrix between set2 variables}
#' \item{mean_cor1}{numeric scalar: average correlation between set1 variables}
#' \item{mean_cor2}{numeric scalar: average correlation between set2 variables}
#' \item{set1, set2, data, iterations, ci}{copy of corresponding arguments}
#' @seealso \code{\link{print.boot_mean_cor_diff}} for summaries of the returned object.
#' @export
#' @examples
#' data(twosetsim)
#' boot_mean_cor_diff(twosetsim$set1, twosetsim$set2, 
#' twosetsim$data, iterations=1000, ci=.95)
boot_mean_cor_diff <- function(set1, set2, data, iterations, ci=.95) {
    if (length(set1) < 2 | length(set2) < 2) 
        stop("There must be at least two variables in each set")
    if(iterations<1) 
        stop('iterations argument must be a positive integer')
    if(ci <= 0 | ci >= 1)
        stop('ci must be between 0 and 1')
    if( length( setdiff(c(set1), colnames(data)) ) != 0)
        stop('one or more variables in set1 not in data')
    if( length( setdiff(c(set2), colnames(data)) ) != 0)
        stop('one or more variables in set2 not in data')
    
    boot_results <- boot(data[,c(set1, set2)], statistic=mean_diff_correlation, R=iterations,
                         set1=set1, set2=set2)
    cor_matrix1 <- cor(data[,set1])
    cor_matrix2 <- cor(data[,set2])
    mean_cor1 <- mean(cor_matrix1[lower.tri(cor_matrix1)])
    mean_cor2 <- mean(cor_matrix2[lower.tri(cor_matrix2)])
    
    results <- list(boot_results=boot_results, cor_matrix1=cor_matrix1, cor_matrix2=cor_matrix2,
                    mean_cor1=mean_cor1, mean_cor2=mean_cor2, set1=set1, set2=set2, data=data, 
                    iterations=iterations, ci=ci)
    class(results) <- 'boot_mean_cor_diff'
    results
}

#' @title Print output for boot_mean_cor_diff object
#' 
#' @description Print output including descriptive statistics and bootstrap results
#' 
#' @param x object of class boot_mean_cor_diff
#' @param digits positive intenger: number of decimal points to display. Numbers are passed to round(x, digits)
#' @param verbose logical. Only print correlation matrices if TRUE.
#' @param ... further arguments passed to or from other methods (not currently used)
#' @method print boot_mean_cor_diff
#' @export 
#' @examples
#' data(twosetsim)
#' fit <- boot_mean_cor_diff(twosetsim$set1, twosetsim$set2, 
#'        twosetsim$data, iterations=1000)
#' print(fit, verbose=TRUE)
print.boot_mean_cor_diff <- function(x, digits=3, verbose=TRUE, ...) {
    cat('\nBOOTSTRAPPED TEST OF DIFFERENCE IN MEAN CORRELATIONS\n')
    
    cat('\nDESCRIPTION')
    cat('\nn =', nrow(x$data))
    cat('\nSet 1:', x$set1)
    cat('\nSet 2:', x$set2)    

    if(verbose) {
        cat('\nCORRELATION MATRICES')
        cat('\nSET 1:\n')
        print(round(x$cor_matrix1, digits))
        
        cat('\nSET 2:\n')
        print(round(x$cor_matrix2, digits))
    }
    
    cat('\n\nAVERAGE CORRELATIONS:\n')
    cat('Set1 mean r=', round(x$mean_cor1, digits),'\n')
    cat('Set2 mean r=', round(x$mean_cor2, digits),'\n')
    cat('Difference in mean r (set1 - set2)=', round(x$mean_cor1 - x$mean_cor2, digits), '\n')
    
    cat('\nBOOTSTRAP SUMMARY:\n')
    print(x$boot_results)
    cat('\n')
        
    cat('\nBootstrap Confidence Interval:', x$ci, '\n')
    print(boot.ci(x$boot_results, conf=x$ci))    
}
