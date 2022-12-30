
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gpttools <a href="https://jameshwade.github.io/gpttools/"><img src="man/figures/logo.png" align="right" height="139"/></a>

<!-- badges: start -->

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

**Read the privacy note at the bottom, this is alpha software there is
no warranty for anything.**

## Installation

``` r
require(remotes)
remotes::install_github("JamesHWade/gpttools")
```

### R \>=4.2 dependency

The current version require R 4.2 or greater because of a current
dependency. If you are using an older version of R, you can get around
this dependency by installing `{openai}` from my fork of the [original
package](https://github.com/irudnyts/openai). Here’s how to do that:

``` r
require(remotes)
remotes::install_github("JamesHWade/openai")
```

## Privacy Notice

These functions work by taking the text or code you have highlighted or
selected with the cursor and send these to OpenAI as part of a prompt,
they fall under their privacy notice, rules, or exceptions you agreed to
with OpenAI when making an account. We do not know how secure these are
when sent to OpenAI, we also do not know what OpenAI does with them. The
code is designed to ONLY share the highlighted or selected text and no
other elements of your R environment (i.e. data) unless you have
highlighted it when running the addin. This may limit usability for now,
but I do not want people to accidentally share sensitive data with
OpenAI.

**DO NOT HIGHLIGHT AND THEREFORE UPLOAD DATA, CODE, OR TEXT THAT SHOULD
REMAIN PRIVATE**

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

## Privacy Notice

**Privacy note:** these functions work by taking the text or code you
have highlighted/selected with the cursor and send these to OpenAI as
part of a prompt, they fall under their privacy notice/rules/exceptions
you agreed to with OpenAI when making an account. I do not know how
secure these are when sent to OpenAI, I also don’t know what OpenAI does
with them. The code is designed to ONLY share the highlighted/selected
text and no other elements of your R environment (i.e. data) unless you
have highlighted it when running the addin. This may limit usability for
now, but I do not want people to accidentally share sensitive data with
OpenAI.

DO NOT HIGHLIGHT, AND THEREFORE UPLOAD, DATA/CODE/TEXT THAT SHOULD
REMAIN PRIVATE

## Code of Conduct

Please note that the gpttools project is released with a [Contributor
Code of
Conduct](https://jameshwade.github.io/gpttools/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.