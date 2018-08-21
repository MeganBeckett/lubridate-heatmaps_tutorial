# Tutorial: Working with dates and visualising patterns in data over time

This tutorial was developed for our R-Ladies Cape Town meetup on 21 August 2018 to learn how to work with dates in R using the lubridate package, and then I've included a case study on analysing running activities over time and vislausing some patterns using various heatmaps.

To get started, open rladies_tutorial.Rproj file in R.

**Lubridate tutorial:**

- Open the R Markdown file to get going: lubridate_tutorial.Rmd
- Installation of lubridate is recorded in the notebook.
- There are explanations, snippets of code, and some exercises under "You try"
- The html rendered view is also available.
- I used the following links as references:
  - https://rpubs.com/mr148/303800
  - https://www.r-statistics.com/2012/03/do-more-with-dates-and-times-in-r-with-lubridate-1-1-0/
  

**Case study: Visualising activity**

- Open R Markdown file: running_casestudy.Rmd
- Required packages are listed at the top of the file.
- The data is my own, downloaded from Garmin Connect.
- Example images of the heatmpas generated are in /results
- calendarHeat.R is a function from Paul Bleicher (GNU GPL2 license)