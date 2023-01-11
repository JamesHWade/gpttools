# Commenting code has correct inputs [on CI]

    Code
      comment_code_addin()
    Output
      [1] "x is less than 24"

# Inserting roxygen works [on CI]

    Code
      add_roxygen_addin()
    Output
      [1] "#' @param x an int"

# Script to function works [on CI]

    Code
      script_to_function_addin()
    Output
      [1] "z <- \\(x, y) x + y"

# Suggesting a unit test works [on CI]

    Code
      suggest_unit_test_addin()
    Output
      [1] "add_xy(1, 2) == 3"

# Suggesting code improvements work [on CI]

    Code
      suggest_code_improvements()
    Output
      [1] "add_xy(1, 2) == 3"

