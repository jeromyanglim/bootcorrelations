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
#' @param ci number between 0 and 1 indicating size of bootstrap confidence interval
#' @export
#' @example examples/bootstrap_mean_correlation.r
bootstrap_mean_correlation <- function(set1, set2, data, iterations=10000, ci=.95) {
    boot_results <- boot(data[,c(set1, set2)], statistic=mean_diff_correlation, R=iterations,
                         set1=set1, set2=set2)
    cor_matrix1 <- cor(data[,set1])
    cor_matrix2 <- cor(data[,set2])
    mean_cor1 <- mean(cor_matrix1[lower.tri(cor_matrix1)])
    mean_cor2 <- mean(cor_matrix2[lower.tri(cor_matrix2)])
    
    results <- list(boot_results=boot_results, cor_matrix1=cor_matrix1, cor_matrix2=cor_matrix2,
                    mean_cor1=mean_cor1, mean_cor2=mean_cor2, set1=set1, set2=set2, data=data, 
                    iterations=iterations, ci=ci)
    class(results) <- 'bootstrap_mean_correlation'
    results
}

#' @title Summary output for bootstrap_mean_correlation object
#' 
#' @description Summary output including descriptive statistics and bootstrap results
#' 
#' @param x object of class bootstrap_mean_correlation
#' @param digits positive intenger: number of decimal points to display. Numbers are passed to round(x, digits)
#' @param verbose logical. Only print correlation matrices if TRUE.
#' @method summary bootstrap_mean_correlation
#' @export 
#' @example examples/summary.bootstrap_mean_correlation.r
summary.bootstrap_mean_correlation <- function(x, digits=3, verbose=FALSE) {
    cat('\nBOOTSTRAPPED TEST OF DIFFERENCE IN AVERAGE CORRELATIONS\n')
    
    cat('nDESCRIPTION')
    cat('\nn =', nrow(x$data))
    cat('\nSet 1:', x$set1)
    cat('\nSet 2:', x$set2)    
    
    cat('\n\nAVERAGE CORRELATIONS:\n')
    cat('Set1 mean r=', round(x$mean_cor1, digits),'\n')
    cat('Set2 mean r=', round(x$mean_cor2, digits),'\n')
    cat('Difference in mean r (set1 - set2)=', round(x$mean_cor1 - x$mean_cor2, digits), '\n')
    
    cat('\nBOOTSTRAP SUMMARY:\n')
    print(x$boot_results)
    cat('\n')
        
    cat('\nBootstrap Confidence Interval:', x$ci)
    print(boot.ci(x$boot_results, conf=x$ci))
    
    
    if(verbose) {
        cat('\nCORRELATION MATRICES')
        cat('\nSET 1:\n')
        print(round(x$cor_matrix1, digits))
        
        cat('\nSET 2:\n')
        print(round(x$cor_matrix2, digits))
    }
}