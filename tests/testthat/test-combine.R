test_that("pdf_combine works with many files (no 'too many open files' error)", {
  pdf_file <- "pdf-example-password.original.pdf"
  pages_per_file <- pdf_length(pdf_file, password = "test")

  outfile_2 <- tempfile(fileext = ".pdf")
  on.exit(unlink(outfile_2), add = TRUE)
  pdf_combine(rep_len(pdf_file, 2), outfile_2, password = "test")
  expect_equal(pdf_length(outfile_2, password = "test"), 2 * pages_per_file)

  skip("May take a few minutes and creates a 455Mb temporary file")
  n <- 10000L
  outfile_n <- tempfile(fileext = ".pdf")
  on.exit(unlink(outfile_n), add = TRUE)
  pdf_combine(rep_len(pdf_file, n), outfile_n, password = "test")
  expect_equal(pdf_length(outfile_n, password = "test"), n * pages_per_file)
})
