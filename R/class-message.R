new_gpttools_message <- function(role = character(),
                               content = character(),
                               datetime = character(),
                               model = character(),
                               service = character(),
                               temperature = double()) {
  if (!is_character(role)) cli_abort("`role` must be a character vector.")
  if (!is_character(content)) cli_abort("`content` must be a character vector.")
  if (!is_character(datetime, n = 1)) {
    cli_abort("`datetime` must be a character vector of length 1.")
  }
  if (!is_character(model, n = 1)) {
    cli_abort("`model` must be a character vector of length 1.")
  }
  if (!is_character(service, n = 1)) {
    cli_abort("`service` must be a character vector of length 1")
  }
  if (!is_double(temperature, n = 1)) {
    cli_abort("`temperature` must be a numeric vector of length 1.")
  }

  new_rcrd(
    fields =
      tibble::tibble(
      role = role,
      content = content,
    ),
    datetime = datetime,
    model = model,
    service = service,
    temperature = temperature,
    class = "gpttools_message"
  )
}

gpttools_message <- function(role = character(),
                           content = character(),
                           datetime = character(),
                           model = character(),
                           service = character(),
                           temperature = double()) {
  role        <- vec_cast(role, character())
  content     <- vec_cast(content, character())
  datetime    <- vec_cast(datetime, character())
  model       <- vec_cast(model, character())
  service     <- vec_cast(service, character())
  temperature <- vec_cast(temperature, double())

  new_gpttools_message(role, content, datetime, model, service, temperature)
}

#' @export
print.gpttools_message <- function(x, ...) {
  x_valid <- which(!is.na(x))

  role        <- field(x, "role")[x_valid]
  content     <- field(x, "content")[x_valid]
  datetime    <- attr(x, "datetime")
  model       <- attr(x, "model")
  service     <- attr(x, "service")
  temperature <- attr(x, "temperature")

  n <- length(field(x, "role"))
  for (i in seq_len(n)) {
    writeLines(col_silver(rule(stringr::str_to_title(role[i]))))
    writeLines(content[i])
  }
  writeLines(rule("Settings", col = "blue"))
  writeLines(col_blue(paste0("date: ", unique(datetime))))
  writeLines(col_blue(paste0("model: ", unique(model))))
  writeLines(col_blue(paste0("service: ", unique(service))))
  writeLines(col_blue(paste0("temperature: ", unique(temperature))))
  invisible(x)
}

#' @export
vec_ptype_full.gpttools_message <- function(x, ...) "gpttools_message"

#' @export
vec_ptype_abbr.gpttools_message <- function(x, ...) "msg"

#' @export
as_gpttools_message <- function(x, ...) {
  UseMethod("as_gpttools_message")
}

#' @export
as_gpttools_message.default <- function(x, ...) {
  vec_cast(x, "gpttools_message")
}

#' @export
vec_cast.gpttools_message.data.frame <- function(x, to, ...) {
  if (to == "data.frame") {
    tibble::tibble(
      role        = field(x, "role"),
      content     = field(x, "content"),
      datetime    = attr(x, "datetime"),
      model       = attr(x, "model"),
      service     = attr(x, "service"),
      temperature = attr(x, "temperature")
    )
  } else {
    cli_abort("Can't cast gpttools_message to ", to)
  }
}
