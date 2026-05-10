test_that("hockeystick_cache_list is callable", {
  expect_true(is.function(hockeystick_cache_list))
})

test_that("hockeystick_cache_delete is callable", {
  expect_true(is.function(hockeystick_cache_delete))
})

test_that("hockeystick_cache_delete_all is callable", {
  expect_true(is.function(hockeystick_cache_delete_all))
})

test_that("hockeystick_cache_details is callable", {
  expect_true(is.function(hockeystick_cache_details))
})

test_that("hockeystick_update_all is callable", {
  expect_true(is.function(hockeystick_update_all))
})

test_that("Cache functions have expected signatures", {
  expect_equal(length(formals(hockeystick_cache_list)), 0)
  expect_equal(length(formals(hockeystick_cache_delete)), 2)
  expect_equal(length(formals(hockeystick_cache_delete_all)), 1)
})
