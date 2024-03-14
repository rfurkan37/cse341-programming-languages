(defun inputChecker () ; this function is used to check the input from the user
  (loop ; this loop is used to read the input from the user
    (format t "g++> ") 
    (let ((line (read-line))) 
      (if (string= (subseq line 0 2) ";;") ; this if statement is used to check if the line is comment or not
          (format t "COMMENT~%")
          (let ((start 0)) ; this let statement is used to set the start of the line to 0
            (loop
              for end = (or (position #\Space line :start start) (length line)) ; this loop is used to read the line word by word
              for word = (subseq line start end) 
              do (progn ; this progn is used to execute the following code
                   (tokenize word) ; this function is used to tokenize the word
                   (setf start (1+ end))) ; this setf is used to set the start of the line to the end of the previous word
              while (< end (length line)))))))) ; this while is used to check if the end of the line is reached or not

(defun split-at-b (str) ; this function is used to split the string at the first b
  (let ((pos (position #\b str)))
    (if pos
        (list (subseq str 0 pos) (subseq str (1+ pos)))
        (list str))))


(defun match-fraction-value (str) ; this function is used to check if the string is a VALUEF or not
  (let ((parts (split-at-b str)))
    (and (= (length parts) 2)
         (not (string= "" (first parts)))
         (not (string= "" (second parts)))
         (every #'digit-char-p (first parts))
         (every #'digit-char-p (second parts)))))

(defvar *operators* '(("+" . "OP_PLUS") ("-" . "OP_MINUS") ("/" . "OP_DIV") ("*" . "OP_MULT") ("(" . "OP_OP") (")" . "OP_CP") ("," . "OP_COMMA")))
(defvar *keywords* '(("and" . "KW_AND") ("or" . "KW_OR") ("not" . "KW_NOT") ("equal" . "KW_EQUAL") ("less" . "KW_LESS") ("nil" . "KW_NIL") ("list" . "KW_LIST") ("append" . "KW_APPEND") ("concat" . "KW_CONCAT") ("set" . "KW_SET") ("def" . "KW_DEF") ("for" . "KW_FOR") ("if" . "KW_IF") ("exit" . "KW_EXIT") ("load" . "KW_LOAD") ("display" . "KW_DISPLAY") ("true" . "KW_TRUE") ("false" . "KW_FALSE")))

(defun get-token-type (token)  ; this function is used to get the type of the token
  (cond
    ((cdr (assoc token *keywords* :test #'string=))) ; this cond is used to check if the token is a keyword or not
    ((cdr (assoc token *operators* :test #'string=))) ; this cond is used to check if the token is an operator or not
    ((match-fraction-value token) ; this cond is used to check if the token is a fraction or not
     "VALUEF")
    ((and (> (length token) 0) 
          (alpha-char-p (char token 0)) 
          (every #'alphanumericp (coerce token 'list)))
     "IDENTIFIER")
    (t (format nil "SYNTAX_ERROR ~A cannot be tokenized." token))))

(defun tokenize (input) ; this function is used to tokenize the input
  (let ((current_token ""))
    (loop for c across input do
      (cond
        ((char= c #\Space) nil)  ; ignore spaces
        ((or (assoc (string c) *operators* :test #'string=)
             (assoc (string c) *keywords* :test #'string=))
         (when (not (string= current_token ""))
           (format t "~A~%" (get-token-type current_token)) ; print the token type
           (setf current_token ""))
         (format t "~A~%" (get-token-type (string c))))
        (t (setf current_token (concatenate 'string current_token (string c))))))
    (when (not (string= current_token ""))
      (format t "~A~%" (get-token-type current_token)))))
    
    
(defun read-file (filename) ; this function is used to read the file
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line do
            (if (string= (subseq line 0 2) ";;")
                (format t "COMMENT~%")
                (let ((start 0))
                  (loop
                    for end = (or (position #\Space line :start start) (length line))
                    for word = (subseq line start end)
                    do (progn
                         (when (not (string= word ""))
                           (tokenize word))
                         (setf start (1+ end)))
                    while (< end (length line))))))))

(defun gppinterpreter (&optional filename)  ; this function is used to read the file or the input from the user
  (if filename
      (read-file filename)
      (inputChecker)
  )
)

(defun main()
(let ((args ext:*args*)) ;; this let statement is used to get the arguments from the command line
  (if args
      (gppinterpreter (first args))
      (gppinterpreter))))

(main)
