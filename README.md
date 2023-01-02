
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gpttools <a href="https://jameshwade.github.io/gpttools/"><img src="man/figures/logo.png" align="right" height="139"/></a>

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![CRAN
status](https://www.r-pkg.org/badges/version/gpttools)](https://CRAN.R-project.org/package=gpttools)
[![Codecov test
coverage](https://codecov.io/gh/JamesHWade/gpttools/branch/main/graph/badge.svg)](https://app.codecov.io/gh/JamesHWade/gpttools?branch=main)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/JamesHWade/gpttools/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/JamesHWade/gpttools/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

The goal of gpttools is to extend gptstudio for R package developers to
more easily incorporate use of large language models (LLMs) into their
project workflows. These models appear to be a step change in our use of
text for knowledge work, but you should carefully consider ethical
implications of using these models. Ethics of LLMs (also called
[Foundation Models](https://arxiv.org/abs/2108.07258)) is an area of
very active discussion.

## Privacy Notice

These functions work by taking the text or code you have highlighted or
selected with the cursor, or your prompt if you use one of the built-in
apps, and send these to OpenAI as part of a prompt. Prompts fall under
the privacy notice, rules, or exceptions you agreed to when making an
OpenAI account. We cannot tell you or guarantee how secure these prompts
are when sent to OpenAI. We do not know what OpenAI does with your
prompts, but OpenAI is clear that they use prompts and results to
improve their model unless you opt out explicitly by contacting them.

The code is designed to ONLY share the highlighted or selected text, or
a prompt you build with the help of one of our apps and no other
elements of your R environment. Make sure you are aware of what you send
to OpenAI and do not accidentally share sensitive data with OpenAI.

**DO NOT HIGHLIGHT AND THEREFORE UPLOAD DATA, CODE, OR TEXT THAT SHOULD
REMAIN PRIVATE**

See OpenAI’s Terms of Use at <https://openai.com/terms>.

## Installation

``` r
require(remotes)
remotes::install_github("JamesHWade/gpttools")
```

## Prerequisites

1.  Make an OpenAI account. As of now, the free one will do.

2.  [Create an OpenAI API key](https://beta.openai.com/account/api-keys)
    to use `{openai}` package within RStudio

3.  Set the API key up in RStudio in one of two ways:

- By default, functions of `{openai}` will look for `OPENAI_API_KEY`
  environment variable. If you want to set a global environment
  variable, you can use the following command, where `"<APIKEY>"` should
  be replaced with your actual key:

``` r
Sys.setenv(OPENAI_API_KEY = "<OPENAI_API_KEY>")
```

- Alternatively, you can set the key in your .Renviron file.

Otherwise, you can add the key to the .Renviron file of the project. The
following commands will open .Renviron for editing:

``` r
require(usethis)
edit_r_environ(scope = "project")
```

You can add the following line to the file (again, replace
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx with your actual
key):

``` bash
OPENAI_API_KEY=xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
```

This now set the API key every time you start up this particular
project. Note: If you are using GitHub/Gitlab, do not forget to add
.Renviron to .gitignore!

## Usage

The package has four addins:

- Comment code: uses code-davinci-edit-001 model from OpenAI to add
  comments to your code with the prompt: “add comments to each line of
  code, explaining what the code does”

- Add roxygen: uses text-davinci-003 model from OpenAI to add and fill
  out a roxygen skeleton to your highlight code (should be a function)
  with the prompt: “insert roxygen skeleton to document this function”

- Convert script to function: uses code-davinci-edit-001 model from
  OpenAI to convert a highlighted script into a function with the
  prompt: “convert this R code into an R function”

- Write a unit test for a function with testthat: uses text-davinci-003
  model from OpenAI to suggest a unit test for a selected function with
  the prompt: “Suggest a unit text for this function using the testthat
  package”

- A freeform addins that let’s you specify the prompt using the “edit”
  functionality of ChatGPT

You can access these addins through the addin toolbar or use the command
pallet: `CMD/CTRL+SHIFT+P`. Examples of each of the addins in action is
below.

![](man/figures/image-1429395462.png)

### Comment Code

<video src="https://user-images.githubusercontent.com/6314313/209890944-3d6a00fa-2d8c-4df7-8a11-f5a5ec3a1391.mov" data-canonical-src="https://user-images.githubusercontent.com/6314313/209890944-3d6a00fa-2d8c-4df7-8a11-f5a5ec3a1391.mov" controls="controls" muted="muted" class="d-block rounded-bottom-2 width-fit" style="max-height:640px;">
</video>

### Add Roxygen

<video src="https://user-images.githubusercontent.com/6314313/209890939-ebd7afea-7d68-40b4-b482-b3fe51485ab1.mov" data-canonical-src="https://user-images.githubusercontent.com/6314313/209890939-ebd7afea-7d68-40b4-b482-b3fe51485ab1.mov" controls="controls" muted="muted" class="d-block rounded-bottom-2 width-fit" style="max-height:640px;">
</video>

### Convert a Script into Functions

<video src="https://user-images.githubusercontent.com/6314313/209890949-4da2bdd7-bcac-4769-9b11-7759b4abb760.mov" data-canonical-src="https://user-images.githubusercontent.com/6314313/209890949-4da2bdd7-bcac-4769-9b11-7759b4abb760.mov" controls="controls" muted="muted" class="d-block rounded-bottom-2 width-fit" style="max-height:640px;">
</video>

### Suggest a Unit Test for a Function

<video src="https://user-images.githubusercontent.com/6314313/209890959-fca623d9-5e8e-463c-ac64-80f3db9875d9.mov" data-canonical-src="https://user-images.githubusercontent.com/6314313/209890959-fca623d9-5e8e-463c-ac64-80f3db9875d9.mov" controls="controls" muted="muted" class="d-block rounded-bottom-2 width-fit" style="max-height:640px;">
</video>

## Code of Conduct

Please note that the gpttools project is released with a [Contributor
Code of
Conduct](https://jameshwade.github.io/gpttools/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.
