# Run the app ----
library(shiny)
path_to_app<-file.path(
  getwd(),
  "app_bp_med_viz"
)
runApp(path_to_app)
