# XMLAdventure

## CompendiumBuildR
A Shiny App to build xml compendium elements. The xml Outputs follow the Lion's Den style guide so you can easily import it to the app.

### Motivations
Writing xml directly in text edit is hard and error prone, but [GM 5 is so great !](https://donfarland.com/technology/introduction-to-lions-den-game-master-5) .

### Features

#### General Principle
Fill in preformated inputs to generate an xml that you can copy/paste into your compendium files.

#### Create

- Choose a a category of element to add (only items for now)
- Fill the various inputs that are generated depending on the category

![](doc/build.gif)

#### Save (copy/paste)

Use the buttons to copy the text output. You can choose either the whole structure or just the element.

![](doc/copypaste.gif)

#### Reset

Change type or category to reset all the inputs or click on the 'reset' button.

![](doc/reset.gif)

### Technical details

This app is based on the [R](https://www.r-project.org) language and its [Shiny package](https://shiny.rstudio.com), developped by [RStudio](https://github.com/rstudio/shiny).

### TODO
  - [x] Build a POC
  - [x] Get Feedbacks
  - [ ] Publish on shinyapps.io for free and easy access
  - [ ] Support other category of element
  - [ ] add the possibility to build several element as one output
  - [ ] (maybe) adapt inputs depending on types within a category

