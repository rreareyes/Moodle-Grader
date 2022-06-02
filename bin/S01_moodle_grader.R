
# Load the functions we'll need --------------------------------------
dir_root    <- dirname(dirname(rstudioapi::getActiveDocumentContext()$path))
dir_scripts <- file.path(dir_root, "bin")
dir_helpers <- file.path(dir_scripts, "helpers")

dir_inputs     <- file.path(dir_root, "inputs")
dir_gradebooks <- file.path(dir_inputs, "gradebooks")
dir_rubrics    <- file.path(dir_inputs, "rubrics")

dir_outputs  <- file.path(dir_root, "outputs")
dir_feedback <- file.path(dir_outputs, "feeback")
dir_grades   <- file.path(dir_outputs, "grades")

source(file.path(dir_helpers, "H01_generate_lab_roster.R"))
source(file.path(dir_helpers, "H02_extract_assignment_ids.R"))
source(file.path(dir_helpers, "H03_subset_rubric.R"))
source(file.path(dir_helpers, "H04_generate_report.R"))

# Create the main course roster, you only need to do this once ------------
generate_roster(reference_file = file.path(dir_inputs, 
                                           "sample_course_totals.csv"))

# Get the ids to start grading with the rubric -----------------------
extract_assignment_ids(roster_file    = file.path(dir_inputs, 
                                                  "sample_roster.csv"),
                       gradebook_file = file.path(dir_gradebooks, 
                                                  "hw1_gradebook_example.csv"), 
                       sections       = c("LQ", "LV", "LW"))


# Generate the report from the rubric --------------------------------
generate_report(n_questions = 13, sections = c("LQ", "LV", "LW"), 
                rubric_file = file.path(dir_rubrics,
                                        "hw1_rubric_example.csv"), 
                gradebook_file = file.path(dir_gradebooks, 
                                           "hw1_gradebook_example.csv"),
                template_file = file.path(dir_helpers, 
                                          "feedback_template.Rmd"),
                
                feedback_file_w = T)

