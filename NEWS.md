# gpttools (development version)

# gpttools 0.0.8

## Major features and improvements

* Added a GitHub Copilot-like code suggestion adding called "Copilot."
* Added support for more AI services: anthropic, huggingface, google ai studio, and ollama (local models) (in #0042b93, #f3c64c2).
* Implemented a fully local option for AI models (in #231f2c8, #482157f).
* Users can now use local embeddings as an option within the package (in #0042b93).
* Improved site scraping workflow for more efficiency and better data retrieval (in #308c809).
* Enhanced the package documentation index updating functions for smoother maintenance (in #2139bfb, #4aa2344, #0304daf).
* Provided package options for easy switching between different AI models (in #2027dbb).
* Added save options to the chat with retrieval app for better user experience (in #51cc6a3, #4202256).
* Optionally execute code in the chat with retrieval app (in #7546037).

## Bug fixes and other changes

* Reconciled local versus OpenAI API for history tracking, ensuring consistency and reliability in the app's history feature (in #f3c64c2, #6d328b2).
* Improved scraping of packages, exploring potential improvements in scraping efficiency (in #46e233a).
* Added an "all indices" option to the app, providing users with a comprehensive view of indices.
* Updated the README and pkgdown site to reflect recent changes and guide the users through new features (in #15a971b).
* Improved AI services vignettes to provide more detailed and up-to-date documentation on using various AI services (in #0304daf).
* Added a new vignette for package scraping, making the scraping process clearer for other developers.
* Added a vignette for chat with retrieval, offering a tutorial on how to leverage this new feature within the package.
* Removed dedicated azure functions and azure embedding option due to the new focus on fully local options (in #ea30c57, #4259474).

## Developmental changes

* Major cleanup of the codebase to remove old, unused files, and tidy the namespace and global variables (in #7b71ab9, #737a266, #5e83b2e).
* Updated pre-commit configurations to help maintain a clean and consistent coding style (in #39828fb, #466830e, #32e5f1e, #423e107, #4c1713f).
* Removed unnecessary dependencies such as miniUI to streamline the package (in #f337b32).

# early versions of gpttools 0.0.7

-  comment code
-  generate roxygen
-  suggest unit tests
-  convert a script into function(s)
-  document data

* Added a `NEWS.md` file to track changes to the package.
