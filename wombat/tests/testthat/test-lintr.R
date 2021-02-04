double_quotes_linter <- function(source_file) {
  lapply(lintr::ids_with_token(source_file, 'STR_CONST'), function(id) {
    parsed <- lintr::with_id(source_file, id)
    if (rex::re_matches(
      parsed$text,
      rex::rex(start, double_quote, any_non_single_quotes, double_quote, end)
    )) {
      lintr::Lint(
        filename = source_file$filename,
        line_number = parsed$line1,
        column_number = parsed$col1,
        type = 'style',
        message = 'Only use single-quotes.',
        line = source_file$lines[as.character(parsed$line1)],
        ranges = list(c(parsed$col1, parsed$col2)),
        linter = 'double_quotes_linter'
      )
    }
  })
}

if (requireNamespace('lintr', quietly = TRUE)) {
  context('lints')

  test_that('Package Style', {
    lintr::expect_lint_free(linters = lintr::with_defaults(
      cyclocomp_linter = NULL,
      object_name_linter = NULL,
      single_quotes_linter = NULL,
      object_length_linter = NULL,
      object_usage_linter = NULL,
      double_quotes_linter
    ))
  })
}
