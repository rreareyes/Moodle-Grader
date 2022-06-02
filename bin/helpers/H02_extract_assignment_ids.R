# Create database with sections and moodle assignment ids -----------------

# This function provides you with a list of all the students and their IDs
# that Moodle attaches to them in each assignment. 
#
# The student roster must contain a column with their full name and the lab
# section as two letters.
#
# The gradebook can be downloaded from Moodle by going to each assignment
# then go to "View/grade all submissions" and on the top of the page from 
# the menu "Grading action" select" "Download grading worksheet".
#
# If this option is not available, go back to the main assignment page and 
# click on the gear icon. From the options go to "Edit settings" and under
# the section "Feedback types" check "Offline grading worksheet" and also
# "Feedback files".
# 
# The function defaults to create a list using all the students in the
# course, but you can select specific lab section by providing them as a 
# character vector in the "sections" argument. The output file defaults to
# a generic "assignment_ids" name, but you can also change it by using the
# corresponding argument.

extract_assignment_ids <- function(
    roster_file    = file.choose(), 
    gradebook_file = file.choose(), 
    sections       = NULL, 
    output_file    = paste(str_extract(gradebook_file, 
                                       "hw\\d?\\d"), 
                           "ids.csv", 
                           sep = "_")) {
  
# Default to all lab sections ---------------------------------------------
require(tidyverse)
  
dir_output = file.path(dirname(dirname(gradebook_file)), "ids")

dir.create(dir_output, showWarnings = FALSE)
  
lab_sections = c("LL", "LM", "LN", "LQ", "LR", "LS", 
                 "LU", "LV", "LW", "LX", "LZ", "ML")
  
  if (is.null(sections)) {
    
    sections = lab_sections
  }


# Standardize headers just to be safe -------------------------------------

  gradebook_headers = c("Identifier", "Full name", "Email address", "Status", 
                        "Grade", "Maximum Grade", "Grade can be changed", 
                        "Last modified (submission)", "Last modified (grade)",
                        "Feedback comments")
  
  lab_names = c("Full name", "Section")
  

# Load file with Moodle IDs and Lab roster --------------------------------

  roster = read.csv(file        = roster_file, 
                    header      = T,
                    col.names   = lab_names,
                    check.names = F)
  
  gradebook = read.csv(file        = gradebook_file,
                       header      = T,
                       col.names   = gradebook_headers,
                       check.names = F)
  
  assignment_ids <- gradebook %>% 
    mutate(Identifier = str_remove(Identifier, "Participant")) %>% 
    full_join(roster) %>% 
    arrange(match(Section, lab_sections)) %>% 
    drop_na(Section) %>% 
    filter(Section %in% sections) %>% 
    select(c("Full name", "Section", "Identifier"))
  
  write.csv(assignment_ids, 
            row.names = F,
            file = file.path(dir_output, output_file))
  
}
