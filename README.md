
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gpttools <a href="https://jameshwade.github.io/gpttools/"><img src="man/figures/logo.png" align="right" height="125"/></a>

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![CRAN
status](https://www.r-pkg.org/badges/version/gpttools)](https://CRAN.R-project.org/package=gpttools)
[![gpttools status
badge](https://jameshwade.r-universe.dev/badges/gpttools)](https://jameshwade.r-universe.dev)
[![Codecov test
coverage](https://codecov.io/gh/JamesHWade/gpttools/branch/main/graph/badge.svg)](https://app.codecov.io/gh/JamesHWade/gpttools?branch=main)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/JamesHWade/gpttools/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/JamesHWade/gpttools/actions/workflows/R-CMD-check.yaml)
![Last
Commit](https://img.shields.io/github/last-commit/jameshwade/gpttools)
[![pre-commit](https://img.shields.io/badge/pre--commit-enabled-brightgreen?logo=pre-commit)](https://github.com/pre-commit/pre-commit)
<!-- badges: end -->

The goal of gpttools is to extend gptstudio for R package developers to
more easily incorporate use of large language models (LLMs) into their
project workflows. These models appear to be a step change in our use of
text for knowledge work, but you should carefully consider ethical
implications of using these models. Ethics of LLMs (also called
[Foundation Models](https://arxiv.org/abs/2108.07258)) is an area of
very active discussion.

## Installation

### Install from GitHub with `{pak}`

``` r
# install.packages("pak")
pak::pak("JamesHWade/gpttools")
```

### Install from [R-Universe](https://r-universe.dev/)

``` r
# Enable repository from jameshwade
options(repos = c(
  jameshwade = "https://jameshwade.r-universe.dev",
  CRAN = "https://cloud.r-project.org"
))
# Download and install gpttools in R
install.packages("gpttools")
# Browse the gpttools manual pages
help(package = "gpttools")
```

### Available AI Services and Models

| AI Service                                                                                           | Models                                                                                                                                                 | Documentation                                                                                                                           | Setup                                          |
|------------------------------------------------------------------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------|-----------------------------------------------------------------------------------------------------------------------------------------|------------------------------------------------|
| [OpenAI](https://platform.openai.com)                                                                | gpt-4-turbo, gpt-4, gpt-3.5-turbo ([latest models](https://platform.openai.com/docs/models))                                                           | [OpenAI API Docs](https://platform.openai.com/docs/api-reference)                                                                       | [OpenAI Setup](articles/openai.md)             |
| [HuggingFace](https://huggingface.co/)                                                               | various                                                                                                                                                | [HF Inference API Docs](https://huggingface.co/docs/hub/models-inference)                                                               | [HF Setup](articles/huggingface.md)            |
| [Anthropic](https://docs.anthropic.com/claude/docs/guide-to-anthropics-prompt-engineering-resources) | claude-2.1, claude-instant-1.2                                                                                                                         | [Anthropic API Docs](https://docs.anthropic.com/claude/reference/getting-started-with-the-api)                                          | [Anthropic Setup](articles/anthropic.md)       |
| [Ollama](https://ollama.ai/)                                                                         | mistral, llama2, mixtral, phi ([latest models](https://ollama.ai/library))                                                                             | [Ollama API Docs](https://github.com/jmorganca/ollama/blob/main/docs/api.md)                                                            | [Ollama Setup](articles/ollama.md)             |
| [Perplexity](https://www.perplexity.ai)                                                              | pplx-7b-chat, pplx-70b-chat, pplx-7b-online, pplx-70b-online, llama-2-70b-chat, codellama-34b-instruct, mistral-7b-instruct, and mixtral-8x7b-instruct | [Perplexity API Docs](https://docs.perplexity.ai/reference/post_chat_completions)                                                       | [Perplexity Setup](articles/perplexity.md)     |
| [Google AI Studio](https://ai.google.dev/tutorials/ai-studio_quickstart)                             | Gemini and Palm (legacy)                                                                                                                               | [Google AI Studio Docs](https://ai.google.dev/docs)                                                                                     | [Google AI Studio Setup](articles/googleai.md) |
| [Azure OpenAI](https://learn.microsoft.com/en-us/azure/ai-services/openai/overview)                  | gpt-4, gpt-3.5-turbo ([latest models](https://learn.microsoft.com/en-us/azure/ai-services/openai/concepts/models#gpt-4-and-gpt-4-turbo-preview))       | [Azure OpenAI API Docs](https://learn.microsoft.com/en-us/azure/ai-services/openai/quickstart?tabs=command-line,python&pivots=rest-api) | [Azure OpenAI Setup](articles/azure.md)        |

### Default AI Service: OpenAI

To get started, you must first set up an API service. The package is
configured to work with several AI service providers, allowing for
flexibility and choice based on your specific needs. The default
configuration is set to use OpenAI’s services. To use it you need:

1.  Make an OpenAI account. [Sign up
    here](https://platform.openai.com/).

2.  [Create an OpenAI API
    key](https://platform.openai.com/account/api-keys) to use with the
    package.

3.  Set the API key up in Rstudio. See the section below on configuring
    the API key.

#### Configuring OpenAI API Key

To interact with the OpenAI API, it’s required to have a valid
`OPENAI_API_KEY` environment variable. Here are the steps to configure
it.

You can establish this environment variable globally by including it in
your project’s .Renviron file. This approach ensures that the
environment variable persists across all sessions as the Shiny app runs
in the background.

Here is a set of commands to open the .Renviron file for modification:

``` r
require(usethis)
edit_r_environ()
```

For a persistent setting that loads every time you launch this project,
add the following line to .Renviron, replacing `"<APIKEY>"` with your
actual API key:

``` bash
OPENAI_API_KEY="<APIKEY>"
```

**Caution:** If you’re using version control systems like GitHub or
GitLab, remember to include .Renviron in your .gitignore file to prevent
exposing your API key!

**Important Note:** OpenAI API will not function without valid payment
details entered into your OpenAI account. This is a restriction imposed
by OpenAI and is unrelated to this package.

### Alternative AI Service Providers

While OpenAI is the default and currently considered one of the most
robust options, `gpttools` is also compatible with other AI service
providers. These include [Anthropic](articles/anthropic.md),
[HuggingFace](articles/huggingface.md), [Google AI
Studio](articles/googleai.md), [Azure OpenAI](articles/azure.md), and
[Perplexity](articles/perplexity.md). You can select any of these
providers based on your preference or specific requirements. You can
also run local models with [Ollama](articles/ollama.md). This requires
more setup but at the benefit of not sharing your data with any third
party.

To use an alternative provider, you will need to obtain the relevant API
key or access credentials from the chosen provider and configure them
similarly.

## Privacy Notice for gpttools

This privacy notice is applicable to the R package that uses popular
language models like gpt-4 turbo and claude-2.1. By using this package,
you agree to adhere to the privacy terms and conditions set by the API
service.

### Data Sharing with AI Services

When using this R package, any text or code you highlight/select with
your cursor, or the prompt you enter within the built-in applications,
will be sent to the selected AI service provider (e.g., OpenAI,
Anthropic, HuggingFace, Google AI Studio, Azure OpenAI) as part of an
API request. This data sharing is governed by the privacy notice, rules,
and exceptions that you agreed to with the respective service provider
when creating an account.

### Security and Data Usage by AI Service Providers

We cannot guarantee the security of the data you send via the API to any
AI service provider, nor can we provide details on how each service
processes or uses your data. However, these providers often state that
they use prompts and results to enhance their AI models, as outlined in
their terms of use. Be sure to review the terms of use of the respective
AI service provider directly.

### Limiting Data Sharing

The R package is designed to share only the text or code that you
specifically highlight/select or include in a prompt through our
built-in applications. No other elements of your R environment will be
shared unless you turn those features on. It is your responsibility to
ensure that you do not accidentally share sensitive data with any AI
service provider.

**IMPORTANT: To maintain the privacy of your data, do not highlight,
include in a prompt, or otherwise upload any sensitive data, code, or
text that should remain confidential.**

## Code of Conduct

Please note that the gpttools project is released with a [Contributor
Code of
Conduct](https://jameshwade.github.io/gpttools/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.
