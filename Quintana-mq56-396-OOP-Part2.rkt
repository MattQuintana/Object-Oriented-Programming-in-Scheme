;; Matthew Quintana
;;
;; CS 396
;; Project 2: OOP
;; April 12, 2016
;; --------------------------------------------------------------------------------------------------


(define in (open-input-file "Input_File.txt"))

;; load-file is a simple function that loads the definitions from a file
;; It recursives calls itself, reading one line at a time; on the recursive return, it cons'es
;; all of the lines together in a giant list, which it returns to caller.

;; The "port" parameter is a read port that you pass it.  The easiest way to get one is to 
;; use the open-input-file built-in function.  So:  (open-input-file filename)
;; returns you a port to that file.
( define load-file
   ( lambda ( port )
      ( let ( ( nextrec ( read port ) ) )
         ( cond
            ( ( eof-object? nextrec ) '() ) ;; If I've read off the end, return empty list
            ( else
              ( let* ( ( nascent-db ( load-file port ) ) ) ;; Recursive call to finish reading file
                 ;; Now add the line read at this level to growing list
                 ( cons nextrec nascent-db ) ) ) ) ) ) )

;; Load all of the classes from the class file for usse in the program. 
(define load-classes
  (lambda (file-name) 
    (define class-list (load-file file-name))
    (build_classes (make_classes `() class-list))))

;; Creates complete class definitions of all of the classes, including
;; the children classes.
;;
;; Takes in a list of previously defined classes and a classes that are still to be
;; completely defined. 
(define make_classes
  (lambda (previous_classes classes)
    (cond
      ;; If all classes have been searched through,
      ;; return the list that has been searched with all of the
      ;; child classes completely defined. 
      ((null? classes) previous_classes)
      ;; Check if the class is a child of another class
      ((eq? (length (third (first classes))) 2)
       (make_classes
        (append
         ;; If the class being checked is a child, attach
         ;; its completed definition to the previous classes that
         ;; have been checked and move on to check the rest of the classes. 
         previous_classes
         (list (build_child
                ;; Build the child class definition completely
                (first classes)
                (find_parent (second (third (first classes))) previous_classes))))
        (rest classes))
       )
      (else
       ;; If the class being checked does not have a parent, attach it to the
       ;; classes that have already been checked and move on to check the rest
       ;; of the class definitions
       (make_classes (append previous_classes (list (first classes))) (rest classes))
       )
      )
    )
  )

;; Finds the class list that a parent belongs to and returns the list
(define find_parent
  (lambda (parent_name classes_list)
    (cond
      ;; If all of the classes in a list have been checked,
      ;; return back the class list
      ((null? classes_list) classes_list)
      ;; If the parent has been found, return the list that it belongs to
      ((eq? parent_name (second (first classes_list))) (first classes_list))
      (else
       ;; Otherwise search through the rest of the classes
       (find_parent parent_name (rest classes_list))
       )
      )
    )
  )


;; Builds the child class definition based on the parent class definition
(define build_child
  (lambda (child_def parent_def)
    ;; Returns the child class definition with parent ivars and methods shadowed
    ;; and any new ivars and methods added in. 
    (cons (first child_def)
          ;; Get the class name argument
          (cons (second child_def)
                ;; Get the parent argument
                (cons (third child_def)
                      ;; Get the constructor arguments
                      (cons (fourth child_def)
                            ;; Add in the new ivars
                            (cons (cons (first (fifth child_def))
                                        (make_c_methods (rest (fifth child_def))
                                                        (rest (fifth parent_def))))
                                  ;; Add in the new methods
                                  (list (cons (first (sixth child_def))
                                              (make_c_methods (rest (sixth child_def))
                                                              (rest (sixth parent_def))))))))))))



;; Makes child methods based on the methods list of the parent
;; Replaces any parent methods that have the same name as a child method
;; with the code of the child method. Returns a list of methods with the
;; replacements included and the additional methods placed in.
(define make_c_methods
  (lambda (c_methods p_methods)
    (cond
      ;; If there are no more child list to be looked through pass back
      ;; the parent methods with all of the additions included
      ((null? c_methods) p_methods)
      ;; If there is a match for a list to be replaced
      ((name_match? (first c_methods) p_methods)
       ;; Make recursive call to check the rest of the elements in the child list with the
       ;; new list that includes any overwritten elements
       (make_c_methods (rest c_methods) (find_and_replace (first c_methods) p_methods))
       )
      (else
       ;; If there isn't a match, attach the new child list to the list containing
       ;; all replacements and further additions
       (cons (first c_methods) (make_c_methods (rest c_methods) p_methods))
       )
      )
    )
  )

;; Checks if there is a match between a symbol and a list to search
(define name_match?
  (lambda (method_def methods_list)
    (cond
      ;; If the end of the list has been reached, meaning all elements
      ;; have been checked, then return false since the target doesn't exist
      ;; in the list. 
      ((null? methods_list) #f)
      ;; If there is a match, return true
      ((eq? (first method_def) (first (first methods_list))) #t)
      (else
       ;; Otherwise, check the rest of the elements in the list
       (name_match? method_def (rest methods_list))
       )
      )
    )
  )

;; Finds a list that contains a certain symbol and replaces that list
;; with the new list definition that is passed in.
(define find_and_replace
  (lambda (method_def methods_list)
    (cond
      ;; If the end of the list has been reached, return the empty list
      ((null? methods_list) methods_list)
      ;; If there is a match between the target and one of the elements of a list
      ((eq? (first method_def) (first (first methods_list)))
       (cond
         ((and
           (> (length method_def) 2)
           (eq?
            (length (second method_def))
            (length (second (first methods_list)))))
          ;; Replace the list that the element belongs to with the list that the
          ;; target belongs to.
          (cons method_def (rest methods_list)))
         ((> (length method_def) 2)
          (cons (first methods_list) (find_and_replace method_def (rest methods_list))))
         (else
          (cons method_def (rest methods_list)))))
      (else
       ;; Otherwise search through the rest of the list
       (cons (first methods_list) (find_and_replace method_def (rest methods_list)))))))

;; Defines a class name to be used in the 'new' function 
(define build_classes
  (lambda (classes)
    (cond
      ((null? classes))
      (else
       ;; Evaluate the define with the class name set as the class list that it belongs to. 
       (eval `(define ,(second (first classes)) (quote ,(first classes))))
       (display "Defined class: ")
       ;; Dipslay the name of the class
       (begin
         (display (second (first classes)))
         (display " ")
         (display (rest (fourth (first classes)))))
       (newline)
       ;; Call the build on the rest of the classes to be defined. 
       (build_classes (rest classes))))))


;; Returns a list of ivars to be placed in the class maker function
(define place_ivar
  (lambda (ivars_list)
    (cond 
      ((null? ivars_list) ivars_list)
      (else 
       (cons 
        `( ,(first (first ivars_list)) ,(second (first ivars_list))) 
        (place_ivar (rest ivars_list)))))))

#|
;; Returns a list of methods to be placed in the class maker function				
(define place_methods
  (lambda (methods_list)
    (cond
      ((null? methods_list) methods_list)
      (else 
       (cons 
        `(,(first (first methods_list)) 
          (lambda ,(second (first methods_list)) 
            ,(third(first methods_list)))) 
        (place_methods (rest methods_list)))))))|#
  

;; Returns a list of conditional statements to be placed in the class maker function
(define place_conds
  (lambda (methods_list)
    (cond 
      ((null? methods_list) methods_list)
      (else 
       (cons 
        `((and (eq? mname (quote ,(first (first methods_list)))) 
               (eq? (length args) ,(length (second (first methods_list))))) 
          (apply (lambda ,(second (first methods_list)) ,(third (first methods_list))) args))
        (place_conds (rest methods_list)))))))
					
					
;; Returns a list to be evaluated as a "factory" for a certain class of objects.
(define class_maker
  (lambda (classdef_list)
    (append
     ;; Creation of the "factory maker" for any object
     `(lambda ,(rest (fourth classdef_list)))
     `(
       ,(append 
         `(let*)
         (cons 
          (append
           ;; Bring in the ivars of the class
           (place_ivar (rest (fifth classdef_list))) 
           `((*this* null)))                      
          (append 
           `()
           `(
             ;; Message handler that captures the method being
             ;; called and the arguments fit is being called on
             (lambda message
               (let*
                   (
                    [mname (first message)] 
                    [args (rest message)]
                    [num_args (length args)]
                    )                       
                 ,(append 
                   (append 											
                    '(cond)
                    (append
                     ;; Include the conditional statements to check the method to be
                     ;; executed
                     (place_conds (rest (sixth classdef_list)))
                     '(((and (eq? mname 'm_setthis) (null? *this*))
                        (set! *this* (first (rest message))))
                       )
                     )
                    )                                                   
                   `((else (println "Function doesn't exist."))) 
                   )))))))))))

;; Creates a new instance of a class based on a class definition and
;; constructor arguments. 
(define new 
  (lambda new_args
    ;; Create a "factory" to build an object of a certain class
    (define instance_maker (eval (class_maker (first new_args))))
    ;; Create an instance of an object using the "factory" with
    ;; constructor arguments.
    (define return_object (apply instance_maker (rest new_args)))
    ;; Set the *this* argument in the 
    (return_object 'm_setthis return_object)
    ;; Return the new instantiated object for use
    return_object
    )
  )


(load-classes in)
(close-input-port in)

;; Object instantiations go here