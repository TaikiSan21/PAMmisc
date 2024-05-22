context('Test environmental data functions.')

test_that('Testing cache creation and deletion for CRAN policy', {
  tempDir <- hoard()$cache_path_set('PAMmisc')
})
