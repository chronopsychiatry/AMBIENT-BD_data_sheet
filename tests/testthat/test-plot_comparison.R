test_that("shortest_time_diff works as expected", {
  t1 <- "23:50:00"
  t2 <- "00:46:00"
  diff_1 <- shortest_time_diff(t2, t1)
  diff_2 <- shortest_time_diff(t1, t2)
  expect_equal(diff_1, -diff_2)
  expect_equal(diff_1, -56)

  t3 <- "20:00:00"
  t4 <- "22:00:00"
  diff_3 <- shortest_time_diff(t4, t3)
  diff_4 <- shortest_time_diff(t3, t4)
  expect_equal(diff_3, -diff_4)
  expect_equal(diff_3, -120)
})

test_that("shortest_time_diff handles vectors", {
  tvec_1 <- c("23:30:00", "00:15:00", "12:00:00")
  tvec_2 <- c("00:15:00", "23:30:00", "14:00:00")
  diffs <- shortest_time_diff(tvec_1, tvec_2)
  expect_equal(diffs, c(45, -45, 120))
})
