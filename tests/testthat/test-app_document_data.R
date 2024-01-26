test_that("{shinytest2} recording: app_document_data", {
  skip_on_ci()
  skip_on_cran()
  skip_if(TRUE)
  withr::local_options("gpttools.api_valid" = TRUE)
  data(mtcars)
  mockr::local_mock(
    get_selection = function() "Here is some text.",
    openai_create_edit = function(model, input, instruction, temperature) {
      list(
        "choices" =
          data.frame("text" = "A mocked response from OpenAI")
      )
    }
  )
  shiny_app <- run_document_data()
  app <- shinytest2::AppDriver$new(shiny_app,
    name = "app_document_data",
    height = 900, width = 1400
  )
  app$set_inputs(temperature = 0.48)
  app$set_inputs(question = "Testing app")
  pp$click("button")
  app$expect_values()
})
