context("create newdata")


test_that("creating newdata works on ungrouped data", {
  set.seed(123)
  iris2 <- iris %>%  group_by(Species) %>% sample_n(2) %>% ungroup()

  expect_data_frame(make_newdata(iris2), any.missing=FALSE, nrows=1, ncols=5)
  expect_equal(colnames(make_newdata(iris2)), colnames(iris2))
  expect_data_frame(make_newdata(iris2, Sepal.Length=c(5)), any.missing=FALSE,
    nrows=1, ncols=5)
  expect_equal(make_newdata(iris2, Sepal.Length=c(5))$Sepal.Length, 5)
  expect_data_frame(make_newdata(iris2, Sepal.Length=c(5, 6)),
    any.missing=FALSE, nrows=2, ncols=5)
  expect_data_frame(make_newdata(iris2, Sepal.Length=seq_range(Sepal.Length, 2)),
    any.missing=FALSE, nrows=2, ncols=5)
  expect_equal(make_newdata(iris2, Sepal.Length=seq_range(Sepal.Length, 2))$Sepal.Length,
    c(4.4, 7.1))
})


test_that("creating newdata fails on ungrouped data", {
  set.seed(123)
  iris2 <- iris %>% group_by(Species) %>% sample_n(2) %>% ungroup()

  expect_warning(make_newdata(iris2, Sepal.length=c(5)))
  expect_error(make_newdata(iris2, Sepal.Length=5))
  expect_error(make_newdata(iris2, Sepal.Length=seq_range(Sepal.length, 2)))
  expect_warning(make_newdata(iris2, Sepal.length=seq_range(Sepal.Length, 2)))

})


test_that("Errors are thrown", {

  expect_error(combine_df(data.frame(x=1), x=2))

})
