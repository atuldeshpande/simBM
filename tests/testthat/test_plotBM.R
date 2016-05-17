context("plot BM")
test_that("plot BM", {
  a<-genBM(100,1,1)
  expect_silent(plotBM(a,1))
  a<-genBM(100,2,1)
  expect_silent(plotBM(a,1))
})
