(in-package #:xiaocc)

(defclass ast-node () ())

(defclass literal-node (ast-node)
  ((value
    :initarg :value
    :reader value)))

(defclass binary-op-node (ast-node)
  ((left
    :initarg :left
    :reader left-child
    :type ast-node)
   (op
     :initarg :op
     :reader operator
     :type symbol)
   (right
    :initarg :right
    :reader right-child
    :type ast-node)))

(defgeneric pretty-print (node &optional stream)
  (:documentation "Print an AST node as an S-expression to STREAM."))

(defmethod pretty-print ((node literal-node) &optional (stream t))
  (format stream "~A" (value node)))

(defmethod pretty-print ((node binary-op-node) &optional (stream t))
  (format stream "(~A ~A ~A)" (operator node) (left-child node) (right-child node)))

(defmethod print-object ((node ast-node) stream)
  (pretty-print node stream))

(defun binary-op-from-token-kind (kind)
  "Map a token kind to a symbolic operator name for binary-op-node."
  (ecase kind
    (:plus     :add)
    (:minus    :subtract)
    (:star     :multiply)
    (:slash    :divide)))

(defun parse-primary! (token-reader)
  "Parse a single integer literal; signal an error on anything else."
  (let ((tok (next-token! token-reader)))
    (cond
      ((null tok)
       (error "unexpected end of input"))
      ((eq (kind tok) :int-literal)
       (make-instance 'literal-node :value (parse-integer (token tok))))
      (t
       (error "expected integer literal, got ~A" (kind tok))))))

(defun parse-binexpr! (reader)
  "Parse a left-associative chain of binary expressions.
   Grammar: expr := primary (op primary)*"
  (iter
    (with left = (parse-primary! reader))
    (for op-tok = (peek-token! reader))
    (while (and op-tok (arithmetic-operatorp (token op-tok))))
    (next-token! reader)                  ; consume the operator
    (let ((right (parse-primary! reader)))
      (setf left (make-instance 'binary-op-node
                                :left  left
                                :op    (binary-op-from-token-kind (kind op-tok))
                                :right right)))
    (finally (return left))))

(defun parse-string (input)
  "Convenience: parse an expression from a plain string."
  (with-input-from-string (s input)
    (parse-binexpr! (make-instance 'token-reader :stream s))))
