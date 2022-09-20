# Moodle-Grader
 Auto grader for moodle assignments

 This autograder works to get the assignment IDs created by Moodle and assigned to each student. These are unique in each assignment, and there is no other way of knowing them. 

 It extracts these IDs from the gradebook file that is created when you enable the option of uploading a gradebook and feedback files.

 The grades are generated from a qualtrics rubric (see the template in the helpers folder). The number of questions can increase/decrease, simply copy the two sections and change the question number. The functions detect the number of points from the question name, so keep the same naming format from the template. Any feedback you add is saved into a table that will be compiled from each student and saved into a pdf if you choose to generate the feedback files. 

 You can add additional lab sections, again, use the same format and make sure you add if necessary any section names in the helper functions, in the current state it covers about 12 sections.

 The final grades to upload are in the "grades" folder inside outputs. You can also upload the feedback files using the zip file created inside the "feedback" folder. It has the format that Moodle can recognize to correctly assign it to each student, so please don't modify the file name inside the feedback template in the helpers.
