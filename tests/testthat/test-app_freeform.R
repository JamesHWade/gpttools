test_that("{shinytest2} recording: app_freeform", {
  skip_on_cran()
  withr::local_options("gpttools.api_valid"=TRUE)
  mockr::local_mock(
    get_selection = function() "Here is some text.",
    openai_create_edit = function(model, input, instruction, temperature) {
      list("choices" =
             data.frame("text" = "A mocked response from OpenAI"))
    }
  )
  shiny_app <- run_gpt_freeform()
  app <- shinytest2::AppDriver$new(shiny_app, name = "app_freeform",
                       height = 900, width = 1400)
  app$set_inputs(temperature = 0.48)
  app$set_inputs(model = "code-davinci-edit-001")
  app$set_inputs(question = "Testing app")
  app$click("button")
  app$expect_values()
})
