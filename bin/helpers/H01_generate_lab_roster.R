
# Create lab roster from course grades ------------------------------------

# This function creates a list with the student's full names and their corresponding lab section. It uses as reference the group totals database that you can download from your course in Moodle. This is only necessary to use if you have groups enabled for your class, otherwise you can ignore it.

generate_roster <- function(reference_file = file.choose(), 
                            output_file    = "sample_roster.csv") {
  
## Default to all lab sections --------------------------------------------
  require(tidyverse)

  reference_roster = read_csv(reference_file)
  
  student_roster = reference_roster %>% 
    unite(col = "Full name", "First name", "Last name", sep = " ") %>% 
    mutate(Section = str_extract(string = `Lab sec`, "(?<=Lab 01)\\w\\w")) %>% 
    select(c(`Full name`, `Section`))
 
write.csv(student_roster,
            row.names = F,
            file = file.path(dirname(reference_file), output_file))
  
}
