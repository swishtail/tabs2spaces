;;; Compile with Chicken
(import (chicken process-context)) ; for "command-line-arguments"

(define file->list
  (lambda (filename)
    (call-with-input-file filename
      (lambda (port)
        (define read-input-to-list
          (lambda ()
            (let ((current-char (read-char port)))
              (if (eof-object? current-char)
                  '()
                  (cons current-char
                        (read-input-to-list))))))
        (read-input-to-list)))))

(define list->file
  (lambda (l filename)
    (call-with-output-file filename
      (lambda (port)
        (define write-list-to-output
          (lambda (l)
            (if (not (null? l))
                (begin
                  (write-char (car l) port)
                  (write-list-to-output (cdr l))))))
        (write-list-to-output l)))))

(define replace-tabs-with-spaces
  (lambda (l tab-width)
    (if (null? l)
        '()
        (if (char=? (car l) #\tab)
            (let space-loop ((n tab-width))
              (if (= n 0)
                  (replace-tabs-with-spaces (cdr l) tab-width)
                  (cons #\space
                        (space-loop (- n 1)))))
            (cons (car l)
                  (replace-tabs-with-spaces (cdr l) tab-width))))))

(define usage-message
  (list "\nIncorrect number of arguments.\n"
        "Usage: \x1b[1mtabs2spaces\x1b[0m \x1b[4msource-file\x1b[0m \x1b[4mdestination-file\x1b[0m [\x1b[4mtab-width\x1b[0m]\n"
        "The default tab width is 8 spaces.\n\n"))

(define main
  (lambda ()
    (let ((arguments (command-line-arguments)))
      (if (not (>= (length arguments) 2))
          (for-each display usage-message)
          (let ((input-file (car arguments))
                (output-file (cadr arguments))
                (tab-width (if (>= (length arguments) 3) ; ignore superfluous arguments
                               (string->number (caddr arguments))
                               8))) ; default tab-width
            (list->file
             (replace-tabs-with-spaces (file->list input-file) tab-width)
             output-file))))))

(main)
