context("generate and plot BM")
test_that("generate and plot BM for", {
  expect_lte(Norm(as.vector(t(simBM(3, 1, 1))) - c(-0.6264538,-0.4428105,-1.278439),2),1e-6)
  expect_error(simBM(100, 1.01))
  expect_error(simBM(100, 1,1.03))
  expect_error(simBM(100.1, 1,1))
  expect_error(simBM(100, 3))
  expect_length(simBM(1000, 2),2000)
  expect_length(simBM(1000, 1),1000)
})
