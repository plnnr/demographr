# Demographic Shaper
Generate a demographic summary from user-drawn shapes. Currently under development.

The concept is that a user can interact with the shiny app to draw shapes on a map. The shape selects census tracts on the backend, and then aggregates the data (including margins of error). The aggregation is then plotted in a time series graph and dynamic text is rendered to describe the data for the currently selected year.

## Demo
Below is a prototype example of what the app can currently do. Only totals at this time; percentages to come later.
![Screenshot of shiny app](https://github.com/plnnr/demographr/blob/main/static/images/beta_demonstration_02232021_lores.gif?raw=true)