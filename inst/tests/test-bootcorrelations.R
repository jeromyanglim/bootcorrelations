context("tests on inputs")

test_that("inputs",{
    data(twosetsim)
    expect_that(
        boot_mean_cor_diff(twosetsim$set1,  
                                   twosetsim$set2, data=twosetsim$data, 
                                   iterations=-10), 
        throws_error())
    
    expect_that(boot_mean_cor_diff(twosetsim$set1,  twosetsim$set2, 
                                           data=twosetsim$data, iterations=10, ci=100), 
                throws_error("ci must be between 0 and 1"))
    
    expect_that(boot_mean_cor_diff(c('asdf', 'stuff'),  twosetsim$set2, 
                                           data=twosetsim$data, iterations=10, ci=.95), 
                throws_error('one or more variables in set1 not in data'))
})
