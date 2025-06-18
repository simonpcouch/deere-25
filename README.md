# Building with LLMs in R and Python

This talk demonstrates how to build a chat-integrated Shiny app. The app is built in R, but each of the tools used to build it has a Python analogue.

![](figures/app.gif)

The app was "vibe-coded" in Claude Code over the course of 30ish minutes. It simulates some product line data and then situates it in a shiny app. The app has a chat feature that allows users to filter the data underlying the app in natural language. The app uses:

* [ellmer](https://ellmer.tidyverse.org): An R interface to LLMs
     - The Python analogue to ellmer is [chatlas](https://posit-dev.github.io/chatlas/)
* [shiny](https://shiny.posit.co/): Interactive web apps
* [querychat](https://github.com/posit-dev/querychat): Chat with your data
* [brand.yml](https://posit-dev.github.io/brand-yml/): Unified branding with a simple YAML file
     - I generated my .yaml file by taking a screenshot of the Deere website and asking Claude to [write one for me](https://posit-dev.github.io/brand-yml/articles/llm-brand-yml-prompt/)

shiny, querychat, and brand.yml are compatible with both R and Python.

While I didn't use it in this case, you may be interested in [Shiny Assistant](https://gallery.shinyapps.io/assistant/). It's a chat app built with ellmer that has a bunch of context and tools built in that make it very good at generating code for Shiny apps.

## Installation

To run this app yourself:

1) Clone the repo
2) Run `pak::pak(".")` to install needed R package dependencies
3) [Configure an `ANTHROPIC_API_KEY`](https://ellmer.tidyverse.org/reference/chat_anthropic.html) so that you can run `ellmer::chat_anthropic()` successfully
4) Run the `app/app.R` file!

## Other Resources <img src="figures/wall.png" align="right" height="240" alt="A hex wall of the packages described below." />

If you're interested in the intersection of R and LLMs, you may find some of the other projects I work on interesting:

- [vitals](https://github.com/tidyverse/vitals): Large language model evaluation in R
- [btw](https://github.com/posit-dev/btw): Describe R stuff to LLMs
- [acquaint](https://github.com/posit-dev/acquaint): Model Context Protocol in R
- [gander](https://github.com/simonpcouch/gander): high-performance, low-friction chat for data science
- [chores](https://github.com/simonpcouch/chores): an extensible collection of LLM assistants
