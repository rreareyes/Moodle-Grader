generate_report <- function(rubric_file     = file.choose(), 
                            gradebook_file  = file.choose(), 
                            template_file   = file.choose(),
                            n_questions     = NULL,
                            sections        = NULL,
                            feedback_file_w = F){
  
  require(tidyverse)
  
  lab_sections = c("LL", "LM", "LN", "LQ", "LR", "LS", 
                   "LU", "LV", "LW", "LX", "LZ", "ML")
  
  dir_root = dirname(rstudioapi::getActiveDocumentContext()$path)
  
  dir_output = file.path(dirname(dirname(dirname(rubric_file))),
                         "outputs")
  dir.create(dir_output, showWarnings = FALSE)
  
  if (is.null(sections)) {
    
    stop("Please choose only a subset of lab sections")
    
  }
  
  if (is.null(n_questions)){
    
    stop("Missing number of questions, please indicate the number of items in this assignment")
    
  }
  
  waste_class = c("NULL", "NULL", "NULL", "NULL", "NULL",
                  "NULL", "NULL", "NULL", "NULL", "NULL",
                  "NULL", "NULL", "NULL", "NULL", "NULL",
                  "NULL", "NULL") #initial columns that we don't need
  
  id_class = c("factor",                               #lab
               "factor", "factor", "factor",  #id
               "character", "factor")                  #initials, TA
  
  deduct_class = c("factor", "numeric",              #complete
                   "factor", "numeric",              #late
                   "factor", "character", "numeric") #other
  
  question_class = rep(c("factor", "character", "character", "character", "numeric"), 
                       n_questions) # vector for all the questions, feedback and score
  
  import_class = c(waste_class, id_class, 
                   deduct_class, question_class, "character") #set space for final comments
  
  raw_survey = read.csv(file       = rubric_file,
                        header     = T,
                        skip       = 2,
                        row.names  = NULL,
                        colClasses = import_class)
  
  id_names = c("lab", 
               sections,
               "initials", "grader")
  
  deduct_names = c("status", "status_deduction",
                   "deadline", "deadline_deduction",
                   "other", "other_message", "other_deduction")
  
  question_names = sort(c(sprintf("q%02d", 1:n_questions), 
                          sprintf("q%02d_Acor", 1:n_questions),
                          sprintf("q%02d_Bpar", 1:n_questions),
                          sprintf("q%02d_Cin", 1:n_questions),
                          sprintf("q%02d_points", 1:n_questions)
                          ))
  
  good_comments = c("Great job!", "Everything looks fine, good job!", "Nice work!",
                    "Nothing to correct, good work!", 
                    "Your assignment has no issues, good job!",
                    "All good, no need to correct anything", 
                    "Excellent job!",
                    "Everything runs without issues, good work!", 
                    "Nice job! Everything looks good.",
                    "Great work! Nothing to correct here.", 
                    "Good work! Evertything runs as expected.",
                    "Your homework looks good, nothing to correct here. Good job!")
  
  decent_comments = c("Great job!", "Only minor changes needed, good job!", "Nice work!",
                      "Your assignment has no major issues, good job!",
                      "All good, just small things to fix. Good job!", 
                      "Everything runs without major issues, good work!", 
                      "Nice job! Everything looks good.",
                      "Great work! Nothing mejor to correct here.", 
                      "Good work! Evertything runs as expected, only minor issues in some areas.",
                      "Your homework looks good, just fix the small issues pointed out. Good job!")
  
  survey_names = c(id_names, deduct_names, question_names, "Comments")
  
  q_value_raw = read_csv(file       = rubric_file,
                         col_names  = T, 
                         skip       = 1,
                         n_max      = 0) %>% 
    colnames()
  
  q_reference = q_value_raw[str_detect(q_value_raw, " points\\)")]
  
  q_value = as.numeric(str_extract_all(q_reference, "(?<=\\().*(?= points)"))
  
  colnames(raw_survey) = survey_names
  
  gradebook = read_csv(gradebook_file,
                       col_names = T)
  
  identifiers = gradebook %>% 
    select("id" = 1, "full_name" = 2) %>% 
    mutate(id = str_remove(id, "Participant "))
  
  point_key = tibble("question" = sprintf("q%02d", 1:n_questions),
                      "total"    = q_value)
  
  survey = raw_survey %>% 
    select(-any_of(lab_sections[!(lab_sections %in% sections)]))
  
# Reshape survey data -----------------------------------------------------
  
  deduction_type = survey %>%
    select(lab:other_deduction) %>%
    pivot_longer(cols = c(status, deadline, other), 
                 names_to = "submission", values_to = "status") %>%
    select(c(lab:grader, "submission", "status")) %>%
    unite("id", sections, na.rm = T, sep = "")

  deduction_message = survey %>%
    select(lab:other_deduction) %>%
    pivot_longer(cols = c(other_message), 
                 names_to = c("submission", NA), names_sep = "_", values_to = "message") %>%
    select(c(lab:grader, "submission", "message")) %>%
    unite("id", sections, na.rm = T, sep = "")
 
  deduction_points = survey %>%
    select(lab:other_deduction) %>%
    pivot_longer(cols = c(status_deduction, deadline_deduction, other_deduction), 
                 names_to = c("submission", NA), names_sep = "_", values_to = "Penalty") %>%
    mutate(submission = str_remove_all(submission, "_deduction")) %>% 
    select(c(lab:grader, "submission", "Penalty")) %>%
    unite("id", sections, na.rm = T, sep = "")
  
  deductions = full_join(deduction_type, deduction_message) %>% 
    full_join(deduction_points) %>% 
    mutate(Penalty = replace_na(Penalty, 0)) %>% 
    select(-submission) %>% 
    filter(Penalty != 0 | status == "No Submission") %>% 
    unite("Deduction", status, message, na.rm = T, sep = " ") %>% 
    left_join(identifiers) %>% 
    relocate(full_name) %>% 
    filter(!is.na(full_name)) %>% 
    group_by(full_name, lab, id, initials, grader) %>% 
    summarise(Penalty = sum(Penalty),
              Deduction = paste(Deduction, collapse = '. ')) %>% 
    ungroup()
  
  valence_data = subset_rubric(data = survey, 
                               type = "valence", 
                               sections = sections, 
                               n_questions = n_questions)
  
  correct_feedback_data  = subset_rubric(data = survey, 
                                         type = "correct", 
                                         sections = sections, 
                                         n_questions = n_questions)
  
  partial_feedback_data  = subset_rubric(data = survey, 
                                         type = "partial", 
                                         sections = sections, 
                                         n_questions = n_questions)
  
  incorrect_feedback_data  = subset_rubric(data = survey, 
                                           type = "incorrect", 
                                           sections = sections, 
                                           n_questions = n_questions)
  
  points_data  = subset_rubric(data = survey, 
                               type = "points", 
                               sections = sections, 
                               n_questions = n_questions)
  

  grade_data = full_join(valence_data, correct_feedback_data) %>% 
    full_join(partial_feedback_data) %>% 
    full_join(incorrect_feedback_data) %>% 
    full_join(points_data) %>% 
    left_join(identifiers) %>% 
    full_join(point_key) %>%
    full_join(deductions) %>% 
    unite("feedback", c(valence, correct, partial, incorrect), na.rm = T, sep = " ") %>%
    mutate(question = str_remove(question, "q")) %>%
    select(full_name, initials, id, lab, grader, question, feedback, points, total, Comments) %>%
    filter(!is.na(question)) %>% 
    filter(!is.na(full_name))
  
  export_data <- grade_data %>%
    group_by(full_name, id) %>%
    summarise(result = round(sum(points)*100/sum(total), 2)) %>%
    ungroup() %>% 
    full_join(select(deductions, c(full_name, Penalty))) %>% 
    mutate(Penalty = replace_na(Penalty, 0),
           result = replace_na(result, 0)) %>% 
    mutate(x = "Participant") %>%
    unite("Identifier", c(x, id), sep = " ") %>%
    group_by(Identifier) %>%
    summarise(Grade = result + Penalty)
  
  students_graded <- unique(export_data$Identifier)
  
  grading_export = gradebook %>%
    filter(Identifier %in% students_graded) %>%
    select(-Grade) %>%
    full_join(export_data, by = "Identifier") %>%
    select(Identifier:Status, Grade, everything()) %>%
    mutate(`Feedback comments` = "")
  
  dir_grades = file.path(dir_output, 
                         "grades")
  
  dir.create(dir_grades, showWarnings = FALSE)
  
  write.csv(grading_export,
            file = file.path(dir_grades,
                             paste("final_upload_", 
                                   str_extract(gradebook_file, "hw\\d?\\d"), 
                                   ".csv", 
                                   sep = "")),
            row.names = F)
  
  dir_deductions = file.path(dir_output, 
                             "deductions")
  
  dir.create(dir_deductions, showWarnings = FALSE)

  write.csv(deductions,
            file = file.path(dir_deductions,
                             paste(str_extract(gradebook_file, "hw\\d?\\d"), 
                                   "deductions.csv", sep = "_")),
            row.names = F)

  # Create feedback files ---------------------------------------------------
  student_IDs   <- unique(grade_data$id)
  nStudents     <- length(student_IDs)
  student_names <- unique(grade_data$full_name)
  
  dir_feedback = file.path(dir_output, 
                           "feedback", 
                           str_extract(gradebook_file, "hw\\d?\\d"))
  
  dir.create(dir_feedback, showWarnings = FALSE)
  
  for (iStudent in 1:nStudents) {
    
    scores = filter(grade_data, id == student_IDs[iStudent]) %>%
      select(-c(id, initials, lab, grader))
    
    ind_deductions = filter(deductions, id == student_IDs[iStudent]) %>%
      select(-c(id, initials, lab, grader))
    
    detailed_grade = scores %>% 
      select(-c(full_name, Comments)) %>% 
      rename("Question" = question,
             "Feedback" = feedback,
             "Points Obtained" = points,
             "Points Possible" = total)
    
    additional_comments = scores %>% 
      select("Final comments" = Comments) %>% 
      unique()
    
    if (sum(is.na(scores$points)) == n_questions) {
      
        points_obtained = scores %>% 
          group_by(full_name) %>% 
          summarise("Points Obtained" = 0,
                    "Total Possible" = sum(total)) %>% 
          mutate("Grade" = `Points Obtained`*100/`Total Possible`) %>% 
          rename("Name" = "full_name")
        
        additional_comments$`Final comments`[1] = "No Submission"
      
      } else if(sum(ind_deductions$Penalty) < 0) {
       
        additional_comments$`Final comments`[1] = paste(ind_deductions$Deduction, 
                                                        collapse = " ")
        
        points_obtained = scores %>% 
          group_by(full_name) %>% 
          summarise("Points Obtained" = sum(points),
                    "Total Possible" = sum(total)) %>% 
          mutate("Deductions" = sum(ind_deductions$Penalty)) %>% 
          mutate("Grade" = round((`Points Obtained`*100/`Total Possible`) + Deductions, 2)) %>% 
          rename("Name" = "full_name")
         
      } else {
        
        points_obtained = scores %>% 
          group_by(full_name) %>% 
          summarise("Points Obtained" = sum(points),
                    "Total Possible" = sum(total)) %>% 
          mutate("Grade" = round(`Points Obtained`*100/`Total Possible`, 2)) %>% 
          rename("Name" = "full_name")
        
        if (additional_comments$`Final comments`[1] == "" && 
            points_obtained$Grade == 100) {
          
          additional_comments$`Final comments`[1] = sample(good_comments, 1)
          
        } else if (additional_comments$`Final comments`[1] == "" 
                   && points_obtained$Grade >= 80) {
          
          additional_comments$`Final comments`[1] = sample(decent_comments, 1)
          
        } else if (additional_comments$`Final comments`[1] == "" 
                   && points_obtained$Grade < 80) {
          
          additional_comments$`Final comments`[1] = "Don't hessitate in contacting me if you are struggling with any of the topics from the class or programming in R"
          
        }
        
    }
    
    if (feedback_file_w == T) {
      
      rmarkdown::render(envir         = new.env(),
                        input         = template_file,
                        output_format = "pdf_document",
                        output_file   = paste(student_names[iStudent],
                                              student_IDs[iStudent],
                                              "assignsubmission_file",
                                              student_names[iStudent],
                                              str_extract(gradebook_file, "hw\\d?\\d"),
                                              "Grade_",
                                              sep = "_"),
                        output_dir    = dir_feedback)
      
    }
    

  }
  
  feedback_files = dir(dir_feedback)
  setwd(dir_feedback)
  zip(zipfile = file.path(dirname(dir_feedback),
                          str_extract(gradebook_file, "hw\\d?\\d")),
      files = feedback_files)
  setwd(dir_root)
}
