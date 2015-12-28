;; adapted from Ol Bini's work http://olabini.com/blog/2006/11/an-emacs-diversion-font-sizes

(defun inc-font-size () 
  (interactive) 
  (let* 
      ((current-font (cdr (assoc 'font (frame-parameters))))
       (splitted (split-string current-font "-"))
       (new-size (+ (string-to-number (nth 7 splitted)) 1))
;;       (new-size (ceiling (* 1.10 (string-to-number (nth 7 splitted)))))
       (new-font (concat (nth 0 splitted) "-"
			 (nth 1 splitted) "-"                          
			 (nth 2 splitted) "-"                          
			 (nth 3 splitted) "-"                          
			 (nth 4 splitted) "-"                         
			 (nth 5 splitted) "-"                         
			 (nth 6 splitted) "-"                   
			 (number-to-string new-size) "-*-" 
                         (nth 9 splitted) "-"                          
			 (nth 10 splitted) "-"                          
			 (nth 11 splitted) "-*-"                         
			 (nth 13 splitted))))  
    (if (> (length splitted) 14)       
	(dotimes (n (- (length splitted) 14)) 
	  (setq new-font (concat new-font "-" (nth (+ n 14) splitted)))))  
    (set-frame-font new-font t)
    (message "Frame font %s size increased to %s" (nth 2 splitted) new-size)
    ))

(defun dec-font-size ()
  (interactive)
  (let* 
      ((current-font (cdr (assoc 'font (frame-parameters))))       
       (splitted (split-string current-font "-"))   
       (new-size (- (string-to-number (nth 7 splitted)) 1))   
;;       (new-size (floor (* 0.9 (string-to-number (nth 7 splitted)))))
       (new-font (concat (nth 0 splitted) "-"                       
			 (nth 1 splitted) "-"                    
			 (nth 2 splitted) "-"                     
			 (nth 3 splitted) "-"                      
			 (nth 4 splitted) "-"                      
			 (nth 5 splitted) "-"                       
			 (nth 6 splitted) "-"                
			 (number-to-string new-size) "-*-"                
			 (nth 9 splitted) "-"                          
			 (nth 10 splitted) "-"    
			 (nth 11 splitted) "-*-"    
			 (nth 13 splitted))))
    (if (> (length splitted) 14)    
	(dotimes (n (- (length splitted) 14)) 
	  (setq new-font (concat new-font "-" (nth (+ n 14) splitted)))))
    (set-frame-font new-font t)
    (message "Frame font %s size decreased to %s" (nth 2 splitted) new-size)
    ))

(defun font-current ()
  (interactive)
  (cdr (assoc 'font (frame-parameters))))

(provide 'fontize)
