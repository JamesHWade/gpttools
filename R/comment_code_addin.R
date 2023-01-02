#' active voice Addin
#'
#' Call this function as a RStudio addin to ask GPT to add comments to your code
#'
#' @export
comment_code_addin <- function() {
  gpt_edit(
    model = "code-davinci-edit-001",
    instruction = "add comments to each line of code,
                   explaining what the code does",
    temperature = 0.1
  )
}


x <- 5
y <- 10
z <- x + y
z
