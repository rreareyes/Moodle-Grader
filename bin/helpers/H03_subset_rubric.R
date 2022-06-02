subset_rubric <- function(data = NULL, type = "", sections = "", n_questions = NULL){
  
  if (type == "correct") {
    
    pattern = sprintf("q%02d_Acor", 1:n_questions)
    
  } else if (type == "partial"){
    
    pattern = sprintf("q%02d_Bpar", 1:n_questions)
    
  } else if (type == "incorrect"){
   
    pattern = sprintf("q%02d_Cin", 1:n_questions)
     
  } else if (type == "valence"){
    
    pattern = sprintf("q%02d", 1:n_questions)
    
  } else if (type == "points"){
    
    pattern = sprintf("q%02d_points", 1:n_questions)
    
  }
  
  rubric_data = data %>% 
    pivot_longer(cols = pattern, 
                 names_to = "question",
                 values_to = type) %>% 
    select(c(lab:grader, question, type, Comments)) %>% 
    mutate(question = str_extract_all(question, sprintf("q%02d", 1:n_questions), simplify = T)) %>% 
    unite("id", sections, na.rm = T, sep = "")
  
  return(rubric_data)
  
}
