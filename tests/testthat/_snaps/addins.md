# Commenting code has correct inputs [on CI]

    Code
      comment_code_addin()
    Message <cliMessage>
      v API already validated in this session.
    Message <rlang_message>
      Asking GPT for help...
    Message <cliMessage>
      Status code: 200
    Message <rlang_message>
      Inserting text from GPT...
      Inserting text from GPT...
    Output
      [1] "x < 24 // this is a comment\n"

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

