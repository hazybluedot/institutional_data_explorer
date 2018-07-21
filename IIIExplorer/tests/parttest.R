# There is a bug, likely in ShinySky::select2, that results in
# > app$setInputs(majorFilter = "ME")
# Error in session_makeRequest(self, private, endpoint, data, params, headers) : 
#  undefined is not a function (evaluating 'binding.setValue($el[0], value)')
# so for now our tests do not include using the select2 inputs, i.e. those in the filter block.

app <- ShinyDriver$new("../")
app$snapshotInit("parttest")

app$setInputs(profile_course = "ESM_2204", timeout_=5000)
app$snapshot()
# Input '`CoursesBefore-CourseTable_rows_selected`' was set, but doesn't have an input binding.
# Input '`CoursesBefore-CourseTable_row_last_clicked`' was set, but doesn't have an input binding.
# Input '`CoursesBefore-CourseTable_cell_clicked`' was set, but doesn't have an input binding.
app$snapshot()
app$setInputs(groupBy = "Gender")
app$setInputs(groupBy = "URM")
app$snapshot()
app$setInputs(groupBy = "group")
# Input '`CoursesBefore-CourseTable_rows_selected`' was set, but doesn't have an input binding.
# Input '`CoursesBefore-CourseTable_row_last_clicked`' was set, but doesn't have an input binding.
# Input '`CoursesBefore-CourseTable_cell_clicked`' was set, but doesn't have an input binding.
app$snapshot()
# Input '`CoursesBefore-CourseTable_rows_selected`' was set, but doesn't have an input binding.
# Input '`CoursesBefore-CourseTable_row_last_clicked`' was set, but doesn't have an input binding.
# Input '`CoursesBefore-CourseTable_cell_clicked`' was set, but doesn't have an input binding.
app$setInputs(hasDegree = TRUE)
app$setInputs(applyFilter = "click")
# Input '`CoursesBefore-CourseTable_rows_selected`' was set, but doesn't have an input binding.
# Input '`CoursesBefore-CourseTable_cell_clicked`' was set, but doesn't have an input binding.
app$snapshot()
app$setInputs(resetInputs = "click")
app$snapshot()
app$setInputs(profile_course = "ENGE_1215")
# Input '`CoursesAfter-CourseTable_rows_selected`' was set, but doesn't have an input binding.
# Input '`CoursesAfter-CourseTable_row_last_clicked`' was set, but doesn't have an input binding.
# Input '`CoursesAfter-CourseTable_cell_clicked`' was set, but doesn't have an input binding.
app$setInputs(applyFilter = "click")
# Input '`CoursesAfter-CourseTable_rows_selected`' was set, but doesn't have an input binding.
# Input '`CoursesAfter-CourseTable_cell_clicked`' was set, but doesn't have an input binding.
app$setInputs(groupBy = "Gender")
app$snapshot()
