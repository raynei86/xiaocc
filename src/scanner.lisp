(in-package #:xiaocc)

(deftype token-kind () '(member :plus :minus :star :slash :int-literal))

(defclass token ()
  ((token
    :initarg :token
    :accessor token
    :type char)
   (kind
    :initarg :kind
    :accessor kind
    :type token-kind)))

(defclass token-reader ()
  ((stream
    :initarg :stream
    :reader stream-of)
   (peeked
    :initform nil
    :accessor peeked-token)))

(defun next-token! (reader)
  "Consume and return the next token (drains peek buffer first)."
  (if (peeked-token reader)
      (prog1 (peeked-token reader)
        (setf (peeked-token reader) nil))
      (scan-token! (stream-of reader))))

(defun peek-token! (reader)
  "Return the next token without consuming it."
  (unless (peeked-token reader)
    (setf (peeked-token reader) (scan-token! (stream-of reader))))
  (peeked-token reader))

(defun whitespacep (char)
  (serapeum:in char #\Space #\Newline #\Tab #\Linefeed #\Page #\Return))

(defun arithmetic-operatorp (char)
  (serapeum:in char #\+ #\- #\* #\/))

(defun skip-whitespace! (stream)
  (peek-char t stream nil nil)) ;; I love builtin utilities

(defun scan-intlit! (stream)
  (coerce (iter
     (for c = (peek-char nil stream nil nil nil))
     (while (and c (digit-char-p c)))
     (collect (read-char stream)))
	  'string))

(defun scan-arithmetic-operator! (stream)
  (read-char stream)) ;; All single characters, so this works for now

(defun scan-token! (stream)
  (let ((ch (skip-whitespace! stream)))
    (cond
      ((null ch) nil)
      ((digit-char-p ch) (make-instance 'token :token (scan-intlit! stream) :kind :int-literal))
      ((arithmetic-operatorp ch)     (trivia:match ch
				       (#\+ (make-instance 'token :token (scan-arithmetic-operator! stream) :kind :plus))
				       (#\- (make-instance 'token :token (scan-arithmetic-operator! stream) :kind :minus))
				       (#\* (make-instance 'token :token (scan-arithmetic-operator! stream) :kind :star))
				       (#\/ (make-instance 'token :token (scan-arithmetic-operator! stream) :kind :slash))))
      (t (error "Unable to scan for token")))))

(defun scan-stream! (stream)
  "Scans all the tokens in the stream"
  (iter
    (for token = (scan-token! stream))
    (while (not (null token)))
    (collect token)))
