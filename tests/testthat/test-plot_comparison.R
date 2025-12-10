test_that("shortest_time_diff works as expected", {
  t1 <- "00:46:00"
  t2 <- "23:50:00"
  diff_1 <- shortest_time_diff(t2, t1)
  diff_2 <- shortest_time_diff(t1, t2)
  expect_equal(diff_1, diff_2)
  expect_equal(diff_1, 56)
})
