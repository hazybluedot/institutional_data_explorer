library(shinythemes)


JScode <-
  "$(function() {
    setTimeout(function(){
      $('#termRange').data('ionRangeSlider').update({
      'prettify_enabled': true, 
      'prettify': function(num) { 
          var remainder = (num % 1).toFixed(1);
          var year = Math.floor(num);
          if (remainder == 0) {
              var term = 'Spring';
          } else {
              var term = 'Fall';
          }
          return (term+' '+year); 
      }
})
    }, 5)})"

# expSlider javascript function
JS.termify <-
  "
// function to exponentiate a sliderInput
function termSlider (sliderId, sci = false) {
  $('#'+sliderId).data('ionRangeSlider').update({
  'prettify': function (num) { return ('2<sup>'+num+'</sup>'); }
  })
}"

# call expSlider for each relevant sliderInput
JS.onload <-
  "
// execute upon document loading
$(document).ready(function() {
// wait a few ms to allow other scripts to execute
setTimeout(function() {
// include call for each slider
termSlider('termRange', sci = true)
}, 5)})
"
currentYear <- as.numeric(format(Sys.Date(), "%Y"))

shinyUI(navbarPage("Institutional Records Explorer", 
  tabPanel("Course",
           tags$head(tags$script(HTML(JScode))),
           sidebarLayout(
             sidebarPanel(
               selectInput("subject", "Subject", c(as.character(NA)), width = "8em"),
               selectInput("number", "Number", c(as.character(NA)), width = "8em"),
               sliderInput("termRange", "Date Range", min = currentYear - 1, 
                           max = currentYear, 
                           value = c(currentYear - 1, currentYear), 
                           dragRange = TRUE,
                           sep = "",
                           step = 0.5),
               selectInput("groupBy", "Compare Across", c(as.character(NA)), width = "12em"),
               verbatimTextOutput("status"),
               width = 3),
             mainPanel(
              tabsetPanel(id = "profile",
                tabPanel("Grade Distribution", gradeDistributionUI("GradeDist"), value = "grades"),
                tabPanel("Predictors of Success", successAnalysisUI("SuccessAnalysis"), value = "success")
              ),
              tabsetPanel(
                tabPanel("Taken Before", courseWidgetUI("CoursesBefore", "Taken Before")),
                tabPanel("Taken With", courseWidgetUI("CoursesWith", "Taken With")),
                tabPanel("Taken After", courseWidgetUI("CoursesAfter", "Taken After"))
                ),
      DT::dataTableOutput("DegreesAfter"),
      width = 9))), 
  tabPanel("Degree", h1("Degree"))
  )
)