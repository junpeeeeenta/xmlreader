;;;-*- Mode: common-lisp; syntax: common-lisp; package: xmlreader; base: 10 -*-
;;;
;;;; XML Reader Module
;;;
;;; Copyright (c) 2014-2016 Seiji Koide <koide@ontolonomy.co.jp>
;;; All rights reserved.
;;; 
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are met: 
;;; 
;;; 1. Redistributions of source code must retain the above copyright notice,
;;;    this list of conditions and the following disclaimer. 
;;; 2. Redistributions in binary form must reproduce the above copyright notice,
;;;    this list of conditions and the following disclaimer in the documentation
;;;    and/or other materials provided with the distribution. 
;;; 
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
;;; ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;; 
;;; The views and conclusions contained in the software and documentation are those
;;; of the authors and should not be interpreted as representing official policies, 
;;; either expressed or implied, of the FreeBSD Project.
;;;
;; History
;; -------
;; 2020/08/29  xml and xsd package in basic-packages.lisp is moved to here.
;; 2016/10/10  File created.
;;

(cl:provide :xmlreader)

(eval-when (:execute :load-toplevel :compile-toplevel)
  (require :line-reader)
  ) ; end of eval-when

(cl:defpackage :xmlreader
  (:use :common-lisp)
  (:shadow #:stream #:with-open-file #:read-line #:read)
  (:export #:stream #:with-open-file #:root-tag
           *entity-decls* NameStartChar-p NameChar-p NCNameStartChar-p NCNameChar-p
           #:read-XMLDecl #:read-Eq #:read-upto-delimiter-string
           make-unique-nodeID
           parse-iri
           get-uri-namedspace uri-namedspace
           comment-p
           #:match-pattern-p #:skip-pattern
           ))

(cl:defpackage :xmlns
  (:use ) ; supressing using common lisp package
  )
(cl:defpackage :xml
  (:use ) ; supressing using common lisp package
  (:export lang ))
(cl:defpackage :_                 ; this package is provided for nodeID symbol.
  (:use ) ; supressing using common lisp package
  )
(cl:defpackage :xsd
  (:nicknames :xs)
  (:use ) ; supressing using common lisp package
  (:export "string" "boolean" "decimal" "float" "double" "dateTime" "time" "date"
           "gYearMonth" "gYear" "gMonthDay" "gDay" "gMonth" "hexBinary" "base64Binary"
           "anyURI" "normallizedString" "token" "language" "NMTOKEN" "Name" "NCName"
           "integer" "nonPositiveInteger" "negativeInteger" "long" "int" "short" "byte"
           "nonNegativeInteger" "unsignedLong" "unsignedInt" "unsignedShort" "unsignedByte"
           "positiveInteger" "simpleType" "anySimpleType" "true" "false"
           "duration" "duration-year" "duration-month" "duration-day" "duration-hour"
           "duration-minute" "duration-second"
           ;; OWL 2 MOF model
           "dateTimeStamp" "length"
           "maxExclusive" "maxInclusive" "maxLength"
           "minExclusive" "minInclusive" "minLength"
           "normalizedString" "pattern"
           )
  (:documentation "http://www.w3.org/2001/XMLSchema#"))

(in-package :xmlreader)

(defclass xmlreader:stream (line:stream)
  ((root-tag :initform 0 :accessor root-tag
             :documentation "placeholder of XML primary tag."))
  (:documentation "This stream has an XML root tag string.")
  )

(defmacro with-open-file (varargs &rest body)
  "calling sequence: `with-open-file (stream filespec {options}*) {declaration}* {form}*'
   this macro adds class option for <rdf:stream>."
  `(cl:with-open-file (,(car varargs) ,(cadr varargs) ,@(cddr varargs)
                       :class 'xmlreader:stream)
     ,@body))

;;;
;;; read utiles
;;;

(defun match-pattern-p (pattern stream &optional (pos (file-position stream)))
  (prog1
      (loop for char1 character across (the simple-string pattern)
          always (char= char1 (read-char stream)))
    (file-position stream pos)))

(defun read-pattern-p (pattern stream &optional (pos (file-position stream)))
  (decf pos)
  (loop for char1 character across (the simple-string pattern)
      always (char= char1 (read-char stream))))

(defun skip-pattern (pattern stream &optional (pos (file-position stream)))
  (file-position stream (+ pos (length pattern))))

(defun assert-pattern (pattern stream &optional (pos (file-position stream)))
  "asserts that input from <stream> matches to <pattern>. 
   <pattern> is a string. This function eats up all characters 
   that equal to pattern. In case of mismatch, an error occurs."
  (declare (ignore pos))
  (or (read-pattern-p pattern stream)
      (error "Assertion error for pattern: ~A" pattern)))

(defun read-upto-delimiter-string (upto stream &optional (pos (file-position stream)))
  "reads input character from <stream> until the occurence of
   <upto> and returns read string. <upto> string is not eaten."
  (declare (ignore pos))
  (coerce
    (loop until (match-pattern-p upto stream)
        for c = (read-char stream)
        do (when (char= c #\Newline) (error "Encountered a newline."))
        collect c)
    'cl:string))

(defun read-quoted-string (quote stream)
  "reads a quoted string from <stream>."
  (assert (or (char= quote #\") (char= quote #\')))
  (coerce 
   (loop for c = (read-char stream)
       until (char= c quote)
       collect c)
   'cl:string))

;;;
;;;
;;;

(declaim (inline ; %read-Name %read-NCName %read-Nmtoken 
                 %read-EncName 
                 NameStartChar-p NCNameStartChar-p NameChar-p NCNameChar-p EncName-p CDStart-p
                 EntityDecl? markupdecl?
                 ))

;;;
;;;; Sharable Functions for XML 1.1 or 1.0 and NameSpace
;;;
;;; In the followings, each specification from XML 1.1 (http://www.w3.org/TR/xml11/)
;;; or 1.0 (http://www.w3.org/TR/xml/) or Namespaces in XML 1.1 (http://www.w3.org/TR/xml-names11/) 
;;; is listed before the definition in Lisp.

;;; ----------------------------------------------------------------------------------
;;; [4]    NameStartChar    ::=
;;;         ":" | [A-Z] | "_" | [a-z] | [#xC0-#xD6] | [#xD8-#xF6] | [#xF8-#x2FF] | 
;;;         [#x370-#x37D] | [#x37F-#x1FFF] | [#x200C-#x200D] | [#x2070-#x218F] | 
;;;         [#x2C00-#x2FEF] | [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] | 
;;;         [#x10000-#xEFFFF] 
;;; ----------------------------------------------------------------------------------
(defun NameStartChar-p (char)
  "returns true if <char> is a NameStartChar."
  (declare (optimize (speed 3) (safety 1)))
  (let ((code (char-code char)))
    (or (= code #x003A)         ; ':'
        (<= #x0041 code #x005A) ; [A-Z]
        (= code #x005F)         ; '_'
        (<= #x0061 code #x007A) ; [a-z]
        (<= #x00C0 code #x00D6) (<= #x00D8 code #x00F6) (<= #x00F8 code #x02FF)
        (<= #x0370 code #x037D) (<= #x037F code #x1FFF) (<= #x200C code #x200D) (<= #x2070 code #x218F)
        (<= #x2C00 code #x2FEF) (<= #x3001 code #xD7FF) (<= #xF900 code #xFDCF) (<= #xFDF0 code #xFFFD)
        (<= #x10000 code #xEFFFF))))
;;;
;;; ----------------------------------------------------------------------------------
;;; Namespaces[6]    NCNameStartChar    ::=    NameStartChar - ':'  
;;; ----------------------------------------------------------------------------------
(defun NCNameStartChar-p (char)
  "returns true if <char> is an NCNameStartChar."
  (declare (optimize (speed 3) (safety 1)))
  (when char
    (let ((code (char-code char)))
      (or (<= #x0041 code #x005A)
          (= code #x005F)
          (<= #x0061 code #x007A)
          (<= #x00C0 code #x00D6) (<= #x00D8 code #x00F6) (<= #x00F8 code #x02FF)
          (<= #x0370 code #x037D) (<= #x037F code #x1FFF) (<= #x200C code #x200D) (<= #x2070 code #x218F)
          (<= #x2C00 code #x2FEF) (<= #x3001 code #xD7FF) (<= #xF900 code #xFDCF) (<= #xFDF0 code #xFFFD)
          (<= #x10000 code #xEFFFF)))))
;;;
;;; ----------------------------------------------------------------------------------
;;; [4a]    NameChar    ::=
;;;          NameStartChar | "-" | "." | [0-9] | #xB7 | [#x0300-#x036F] |
;;;          [#x203F-#x2040] 
;;; ----------------------------------------------------------------------------------
(defun NameChar-p (char)
  "returns true if <char> is a NameChar."
  (declare (optimize (speed 3) (safety 1)))
  (or (NameStartChar-p char)
      (let ((code (char-code char)))
        (or (= #x002D code)
            (= #x002E code)
            (<= #x0030 code #x0039)
            (= code #x00B7)
            (<= #x0300 code #x036F)
            (<= #x203F code #x2040)))))
;;;
;;; ----------------------------------------------------------------------------------
;;; Namespaces[5]    NCNameChar    ::=    NameChar - ':' 
;;; ----------------------------------------------------------------------------------
(defun NCNameChar-p (char)
  "returns true if <char> is an NCNameChar."
  (declare (optimize (speed 3) (safety 1)))
  (or (NCNameStartChar-p char)
      (let ((code (char-code char)))
        (or (= #x002D code)
            (= #x002E code)
            (<= #x0030 code #x0039)
            (= code #x00B7)
            (<= #x0300 code #x036F)
            (<= #x203F code #x2040)))))
;;;
;;; ----------------------------------------------------------------------------------
;;; [81]    EncName    ::=    [A-Za-z] ([A-Za-z0-9._] | '-')* 
;;; ----------------------------------------------------------------------------------
(defun EncName-p (char)
  "returns true if <char> is an EncName."
  (declare (optimize (speed 3) (safety 1)))
  (let ((code (char-code char)))
    (or (<= #x0041 code #x005A)         ;[A-Z]
        (<= #x0061 code #x007A)         ;[a-z]
        (<= #x0030 code #x0039)         ;[0-9]
        (= code #x002D)                 ; -
        (= code #x002E)                 ; .
        (= code #x005F)                 ; _
        )))
;;;
;;; ----------------------------------------------------------------------------------
;;; [5]    Name    ::=    NameStartChar (NameChar)* 
;;; ----------------------------------------------------------------------------------
(defun read-Name (stream)
  "reads a name from <stream> and returns it as string."
;;;  (declare (optimize (speed 3) (safety 1)))
  (coerce (%read-Name stream) 'cl:string))

(defun %read-Name (stream)
  "reads a name from <stream> and returns characters in list."
  (declare (optimize (speed 3) (safety 1)))
  (assert (NameStartChar-p (line:peeknext-char stream)) () "Illegal NameStartChar ~W at line ~S"
          (line:peeknext-char stream) (line:line-count stream))
  (let (c name)
    (setq c (line:getnext-char stream))
    (setq name
          (loop while (NameChar-p c)
              collect c
              do (setq c (line:getnext-char stream))))
    (line:putback-char c stream)
    name))
;;;
;;; ----------------------------------------------------------------------------------
;;; [4]    NCName    ::=    NCNameStartChar NCNameChar* /* An XML Name, minus the ":" */ 
;;; ----------------------------------------------------------------------------------
(defun read-NCName (stream)
  "reads a NCname from <stream> and returns it as string. The token must be NCName."
;;;  (declare (optimize (speed 3) (safety 1)))
  (when (NCNameStartChar-p (peek-char nil stream nil nil))
    (coerce (%read-NCName stream) 'cl:string)))

(defun %read-NCName (stream)
  "reads a NCName from <stream> and returns chars in list."
;;;  (declare (optimize (speed 3) (safety 1)))
  (let ((c (read-char stream)))
    (let ((name 
           (loop while (NCNameChar-p c)
               collect c
               do (setq c (read-char stream)))))
      (unread-char c stream)
      (coerce name 'cl:string))))

(defun read-quoted-NCName (stream)
  "reads a quoted NCName from <stream> and returns the NCName as string."
  (declare (optimize (speed 3) (safety 1)))
  (let ((q (line:getnext-char stream)))
    (assert (or (char= q #\") (char= q #\')))
    (let ((NCName (%read-NCName stream)))
      (assert (char= q (line:getnext-char stream)))
      (coerce NCName 'cl:string))))
;;;
;;; ----------------------------------------------------------------------------------
;;; [7]    Nmtoken    ::=    (NameChar)+ 
;;; ----------------------------------------------------------------------------------
(defun read-Nmtoken (stream)
  "reads a Nmtoken from <stream> and returns it as string."
;;;  (declare (optimize (speed 3) (safety 1)))
  (coerce (%read-Nmtoken stream) 'cl:string))

(defun %read-Nmtoken (stream)
  "reads a Nmtoken from <stream> and returns chars in list."
  (declare (optimize (speed 3) (safety 1)))
  (assert (NameChar-p (line:peeknext-char stream)) () "Illegal NameChar ~W at line ~S"
          (line:peeknext-char stream) (line:line-count stream))
  (let (c name)
    (setq name
          (loop while (NameChar-p (setq c (line:getnext-char stream)))
              collect c))
    (line:putback-char c stream)
    name))
;;;
;;; ----------------------------------------------------------------------------------
;;; [81]    EncName    ::=    [A-Za-z] ([A-Za-z0-9._] | '-')* 
;;; ----------------------------------------------------------------------------------

(defun %read-EncName (stream)
  "reads a EncName from <stream> and returns chars in list."
  (declare (optimize (speed 3) (safety 1)))
  (let (c name)
    (setq c (line:peeknext-char stream))
    (assert (or (<= #x0041 (char-code c) #x005A) (<= #x0061 (char-code c) #x007A))
            () "Illegal EncName ~W at line ~S" c (line:line-count stream))
    (setq c (line:getnext-char stream))
    (setq name
          (loop while (EncName-p c)
              collect c
              do (setq c (line:getnext-char stream))))
    (line:putback-char c stream)
    name))

(defun read-EncName (stream)
  "reads a EncName from <stream> and returns it as string."
;;;  (declare (optimize (speed 3) (safety 1)))
  (coerce (%read-EncName stream) 'cl:string))

;;
;; XML Reader
;;

;;; ----------------------------------------------------------------------------------
;;; [14]    CharData    ::=    [<&]* - ([<&]* ']]>' [<&]*) 
;;; [18]    CDSect    ::=    CDStart CData CDEnd  
;;; [19]    CDStart    ::=    '<![CDATA[' 
;;; [20]    CData    ::=    (Char* - (Char* ']]>' Char*))  
;;; [21]    CDEnd    ::=    ']]>' 
;;; ----------------------------------------------------------------------------------
(defun CDStart-p (stream)
  "Does CDStart characters come from <stream> next?"
  (declare (optimize (speed 3) (safety 1)))
  (when (match-pattern-p "<![CDATA[" stream)
    (skip-pattern "<![CDATA[" stream)
    t))

(defun read-CData-to-CDEnd (stream)
  "reads string from <stream> up to CDEnd, and returns CData."
  (declare (optimize (speed 3) (safety 1)))
  (prog1 (read-upto-delimiter-string "]]>" stream)
    (skip-pattern "]]>" stream)))

;;;
;;;; Read-entity-decls
;;;
;;; <read-entity-decls> tranforms a sequence of characters from stream that are declared as character 
;;; entity to the designated character. For example,
;;;  * &amp;amp; -> '&amp;'
;;;  * &amp;lt;  -> '&lt;'
;;;  * &amp;gt;  -> '&gt;'
;;;  * &amp;apos; -> "'"
;;;  * &amp;quot; -> '&quot;'

(defparameter *entity-decls*
  (acons "quot" #\"
         (acons "apos" #\'
                (acons "gt" #\>
                       (acons "lt" #\<
                              (acons "amp" #\& nil)))))
  "storage for entity-decls")

(defun read-entity-decls (stream)
  "reads entity delcs and returns mapped char. This function should be called just after '&'."
;;;  (declare (optimize (speed 3) (safety 1)))
  (let ((pat (loop for cc = (line:getnext-char stream)
                 until (char= cc #\;)
                 collect cc)))
    (setq pat (coerce pat 'cl:string))
    (let ((found (cdr (assoc pat *entity-decls* :test #'string=))))
      (cond (found)
            (t (error "There is no entity definition for ~A at line ~S." pat (line:line-count stream)))))))
;;;
;;; ----------------------------------------------------------------------------------
;;; [25]    Eq    ::=    S? '=' S? 
;;; ----------------------------------------------------------------------------------
(defun read-Eq (stream)
  "reads Eq and returns character '='"
;;;  (declare (optimize (speed 3) (safety 1)))
  (line:skipbl stream)
  (cond ((char= (read-char stream) #\=)
         (line:skipbl stream)
         #\=)
        ((error "No Eq at linenumber ~D ~S"
           (+ (line:line-count stream) 1) (line:expose-one-line-buf stream)))))
;;;
;;; ----------------------------------------------------------------------------------
;;; [23]    XMLDecl    ::=    '<?xml' VersionInfo EncodingDecl? SDDecl? S? '?>' 
;;; ----------------------------------------------------------------------------------
(defun read-XMLDecl (stream)
  "reads XMLDecl just after '<?xml'."
  (let* ((version (read-VersionInfo stream))
         (encoding (when (EncodingDecl? stream) (read-EncodingDecl stream)))
         (standalone (when (SDDecl? stream) (read-SDDecl stream))))
    (line:skipbl stream)
    (assert-pattern "?>" stream)
    (values version encoding standalone)))
;;;
;;; ----------------------------------------------------------------------------------
;;; [24]    VersionInfo    ::=
;;;                      S 'version' Eq ("'" VersionNum "'" | '"' VersionNum '"') 
;;; ----------------------------------------------------------------------------------
(defun read-VersionInfo (stream)
  (line:skipbl stream)
  (assert (string= "version" (line:next-token stream)))
  (read-Eq stream)
  (read-quoted-string (read-char stream) stream))
;;;
;;; ----------------------------------------------------------------------------------
;;; [80]    EncodingDecl    ::=
;;;                         S 'encoding' Eq ('"' EncName '"' | "'" EncName "'" ) 
;;; ----------------------------------------------------------------------------------
(defun EncodingDecl? (stream)
  "returns true if character sequence 'enconding' is detected from <stream>."
  (line:skipbl stream)
  (match-pattern-p "encoding" stream))
(defun read-EncodingDecl (stream)
  "reads EncodingDecl and returns EncName that follows '='."
  (line:skipbl stream)
  (assert-pattern "encoding" stream)
  (read-Eq stream)
  (let ((q (line:getnext-char stream)))
    (assert (or (char= q #\") (char= q #\')))
    (prog1 (read-EncName stream)
      (assert (char= (line:getnext-char stream) q)))))
;;;
;;; ----------------------------------------------------------------------------------
;;; [32]    SDDecl    ::=    S 'standalone' Eq (("'" ('yes' | 'no') "'") |
;;;                          ('"' ('yes' | 'no') '"'))  
;;; ----------------------------------------------------------------------------------
(defun SDDecl? (stream)
  "returns true if character sequence 'standalone' is detected from <stream>."
  (line:skipbl stream)
  (match-pattern-p "standalone" stream))
(defun read-SDDecl (stream)
  "Not Yet implemented."
  (declare (ignore stream))
  (error "Not Yet!"))

;;;
;;;; Comment
;;; ----------------------------------------------------------------------------------
;;;  [15]    Comment    ::=    '<!--' ((Char - '-') | ('-' (Char - '-')))* '-->' 
;;; ----------------------------------------------------------------------------------
(defun Comment? (stream)
  "returns true if character sequence '<!--' is detected from <stream>."
  (declare (optimize (speed 3) (safety 1)))
  (match-pattern-p "<!--" stream))

(defun read-Comment (stream)
  "reads commented string from <stream> using <read-upto-delimiter-string>."
  (assert-pattern "<!--" stream)
  (prog1 (read-upto-delimiter-string "-->" stream)
    (assert-pattern "-->" stream)))

;;;(defun read-comment (stream)
;;;  (loop for token = (line:next-token stream)
;;;      until (and (string= token "--") (char= #\> (peek-char nil stream nil nil)))
;;;      collect token
;;;      finally (read-char stream)))

;;;
;;; In SWCLOS, comment is a structure that has slot <body>.

(defstruct (comment (:print-function print-comment)) body)
(defun print-comment (r s k)
  "prints out Comment string. This function is not intended to be used by user."
  (declare (ignore k))
  (format s "~&<!--~A-->" (comment-body r)))

(defun parse-Comment (stream)
  "reads Comment from <stream> and returns a comment structure."
  (make-comment :body (read-Comment stream)))
;;;
;;; ----------------------------------------------------------------------------------
;;; [11]    SystemLiteral    ::=    ('"' [^"]* '"') | ("'" [^']* "'") 
;;; ----------------------------------------------------------------------------------
(defun read-SystemLiteral (stream)
  "reads SystemLiteral and returns the string."
  (let ((q (line:getnext-char stream)))
    (assert (or (char= q #\") (char= q #\')))
    (coerce 
     (loop for c = (line:getnext-char stream)
         until (char= c q)
         collect c)
     'cl:string)))
;;;
;;; ----------------------------------------------------------------------------------
;;; [13]    PubidChar    ::=    #x20 | #xD | #xA | [a-zA-Z0-9] | [-'()+,./:=?;!*#@$_%] 
;;; ----------------------------------------------------------------------------------
(defun PubidChar-p (char)
  "returns true if <char> is a PubidChar."
  (declare (optimize (speed 3) (safety 1)))
  (let ((code (char-code char)))
    (or (= code #x20)
        (= code #xD)
        (= code #xA)
        (<= #x0041 code #x005A)
        (<= #x0061 code #x007A)
        (<= #x0030 code #x0039)
        (= code #x002D) ; -
        (= code #x0027) ; '
        (= code #x0028) ; (
        (= code #x0029) ; )
        (= code #x002B) ; +
        (= code #x002C) ; ,
        (= code #x002E) ; .
        (= code #x002F) ; /
        (= code #x003A) ; :
        (= code #x003D) ; =
        (= code #x003F) ; ?
        (= code #x003B) ; ;
        (= code #x0021) ; !
        (= code #x002A) ; *
        (= code #x0023) ; #
        (= code #x0040) ; @
        (= code #x0024) ; $
        (= code #x005F) ; _
        (= code #x0025) ; %
        )))
;;;
;;; ----------------------------------------------------------------------------------
;;; [12]    PubidLiteral    ::=    '"' PubidChar* '"' | "'" (PubidChar - "'")* "'" 
;;; ----------------------------------------------------------------------------------
(defun read-PubidLiteral (stream)
  "reads PubidLiteral from <stream> and returns the string."
  (let ((q (line:getnext-char stream)))
    (assert (or (char= q #\") (char= q #\')))
    (coerce 
     (loop for c = (line:getnext-char stream)
         until (char= c q)
         do (assert (PubidChar-p c))
         collect c)
     'cl:string)))
;;;
;;; ----------------------------------------------------------------------------------
;;; [75]    ExternalID    ::=    'SYSTEM' S SystemLiteral | 'PUBLIC' S PubidLiteral S SystemLiteral 
;;; ----------------------------------------------------------------------------------
(defun ExternalID? (stream)
  "Is the next pattern in <stream> SYSTEM or PUBLIC?"
  (declare (optimize (speed 3) (safety 1)))
  (or (match-pattern-p "SYSTEM" stream)
      (match-pattern-p "PUBLIC" stream)))

(defun parse-ExternalID (stream)
  "If the next pattern is SYSTEM or PUBLIC, then reads and parse the ExternalID from <stream>."
  (cond ((match-pattern-p "SYSTEM" stream)
         (read-pattern-p "SYSTEM" stream)
         (line:skipbl stream)
         (read-SystemLiteral stream))
        ((match-pattern-p "PUBLIC" stream)
         (read-pattern-p "PUBLIC" stream)
         (line:skipbl stream)
         (list (prog1 (read-PubidLiteral stream) (line:skipbl stream))
                  (read-SystemLiteral stream)))))
;;;
;;; ----------------------------------------------------------------------------------
;;; [29]    markupdecl    ::=    elementdecl | AttlistDecl | EntityDecl | 
;;;                              NotationDecl | PI | Comment 
;;; ----------------------------------------------------------------------------------
(defun EntityDecl? (stream)
  "returns true if EntityDecl is detected from <stream>."
  (declare (optimize (speed 3) (safety 1)))
  (match-pattern-p "<!ENTITY" stream))
(defun markupdecl? (stream)
  "this function only detects EntityDecl."
  (declare (optimize (speed 3) (safety 1)))
  (or (EntityDecl? stream)))
(defun parse-markupdecl (stream)
  "this function only parses EntityDecl."
  (declare (optimize (speed 3) (safety 1)))
  (cond ((EntityDecl? stream) (parse-EntityDecl stream))
        ((error "Not Yet!"))))
;;;
;;; ----------------------------------------------------------------------------------
;;; [28b]    intSubset    ::=    (markupdecl | DeclSep)* 
;;; ----------------------------------------------------------------------------------
(defun intSubset? (stream)
  "returns true if intSubset is detected from <stream>."
  (or (markupdecl? stream)
      (DeclSep? stream)))
(defun parse-intSubset (stream)
  "reads markupdecl or Comment and parse it."
  (line:skipbl stream)
  (loop while (or (intSubset? stream) (Comment? stream))
      do (cond ((markupdecl? stream) (parse-markupdecl stream))
               ((Comment? stream) (parse-Comment stream))
               ((DeclSep? stream) (error "Not Yet!"))
               ((error "Cant happen!")))
        (line:skipbl stream)))
;;;
;;; ----------------------------------------------------------------------------------
;;; [28a]    DeclSep    ::=    PEReference | S 
;;; ----------------------------------------------------------------------------------
(defun DeclSep? (stream)
  "returns true if PEReference is detected."
  (char= #\% (line:peeknext-char stream)))
;;;
;;; ----------------------------------------------------------------------------------
;;; [28]    doctypedecl    ::=
;;;             '<!DOCTYPE' S Name (S ExternalID)? S? ('[' intSubset ']' S?)? '>' 
;;; ----------------------------------------------------------------------------------
(defun read-doctypedecl (stream)
  "reads doctypedecl."
  (line:skipbl stream)
  (let* ((Name (read-Name stream)) 
         (eID (when (ExternalID? stream) (parse-ExternalID stream)))
         (values nil))
    (assert (or (string= Name "rdf:RDF") (string= Name "RDF")
                (string= Name "owl")
                (string= Name "uridef"))) ; uridef is for service.owl
    (line:skipbl stream)
    (cond ((char= #\[ (line:peeknext-char stream))
           (line:getnext-char stream)
           (setq values (parse-intSubset stream))
           (assert (char= #\] (line:getnext-char stream)))
           ))
    (line:skipbl stream)
    (assert (char= #\> (line:getnext-char stream)))
    (values Name eID values)))
;;;
;;; ----------------------------------------------------------------------------------
;;; [6]     QName            ::=    PrefixedName 
;;;                                 | UnprefixedName 
;;; [6a]    PrefixedName     ::=    Prefix ':' LocalPart  
;;; [6b]    UnprefixedName   ::=    LocalPart  
;;; [7]     Prefix           ::=    NCName 
;;; [8]     LocalPart        ::=    NCName 
;;; ----------------------------------------------------------------------------------

(defun read-QNameString (stream)
  "reads a Qname from <stream> and returns it as string. The first token in <stream> must be NCName."
  (declare (optimize (speed 3) (safety 1)))
  (let (Prefix LocalPart)
    (setq Prefix (read-NCName stream))
    (cond ((match-pattern-p ":" stream)
           (read-pattern-p ":" stream)
           (setq LocalPart (read-NCName stream))
           (concatenate 'string Prefix ":" LocalPart))
          (t Prefix ; returns a string
             ))))

;;;(defun peep-QNameString (stream)
;;;  "peeps QName in <stream> and returns the string. The first token in <stream> must be NCName."
;;;  (declare (optimize (speed 3) (safety 1)))
;;;  (let ((QName (read-QNameString stream)))
;;;    (putback-pattern QName stream)
;;;    QName))

(defun read-QName (stream)
  "returns a QName string."
  (declare (optimize (speed 3) (safety 1)))
  (let ((prefix (read-NCName stream)))
    (cond ((null prefix) nil)    ; may be an end of attribute list
          ((char= #\: (peek-char nil stream))
           (read-char stream)
           (let ((LocalPart (read-NCName stream)))
             (concatenate 'string prefix ":" LocalPart)))
          (t prefix))))

;;;
;;; ----------------------------------------------------------------------------------
;;; [70]    EntityDecl   ::=    GEDecl | PEDecl 
;;; [71]    GEDecl       ::=    '<!ENTITY' S Name S EntityDef S? '>' 
;;; [72]    PEDecl       ::=    '<!ENTITY' S '%' S Name S PEDef S? '>' 
;;; [73]    EntityDef    ::=    EntityValue | (ExternalID NDataDecl?) 
;;; ----------------------------------------------------------------------------------


(defun parse-EntityDecl (stream)
  "reads EntityDecl from <stream> and registers the declaration."
;;;  (declare (optimize (speed 3) (safety 1)))
  (assert-pattern "<!ENTITY" stream)
  (line:skipbl stream)
  (cond ((match-pattern-p "% " stream)
         (line:skipbl stream)
         (read-Name stream)
         (line:skipbl stream)
         (error "Not Yet!"))
        (t (let ((Name (read-Name stream)) quoted)
             (line:skipbl stream)
             (setq quoted (read-quoted-string (read-char stream) stream))
             (unless (and (cdr (assoc Name *entity-decls* :test #'string=))
                          (string= (cdr (assoc Name *entity-decls* :test #'string=)) quoted))
               (setq *entity-decls* (acons Name quoted *entity-decls*))))))
  (line:skipbl stream)
  (assert (char= #\> (line:getnext-char stream))))

;;;
;;;; Peeping File Coding
;;;
;;; Even though a file includes the character encoding information in XMLDecl part, 
;;; we cannot know it without opening and peeping it.
;;; The following functions allow us to peep a file, looking for character encoding.
;;;

(defun peep-XMLDecl-code-from-file (file)
  "peeps <file> and returns a character code declared in XMLDecl."
  (with-open-file (stream (pathname file))
    (%peep-XMLDecl-code stream)))
;;;
;;;(defun peep-XMLDecl-code-from-string (rdf-string)
;;;  "peeps <rdf-string> and returns a character code declared in XMLDecl."
;;;  (with-input-from-string (stream rdf-string)
;;;    (%peep-XMLDecl-code stream)))

(defun %peep-XMLDecl-code (stream)
  "peeps <stream> and returns a character code declared in XMLDecl."
  (line:skipbl stream)
  (when (string= "<?xml " (line:next-token stream))
    (multiple-value-bind (version encoding standalone) (read-XMLDecl stream)
      (declare (ignore version standalone))
      (cond ((null encoding) nil)
            ((string-equal encoding "") nil)
            ((string-equal encoding "Shift_JIS") :932)
            ((string-equal encoding "ISO-8859-1") :latin1)
            ((string-equal encoding "UTF-8") :utf-8)      ; by smh
            ((error "Please update %peep-XMLDecl-code for ~A" encoding))))))

;;;
;;;; Data Type duration
;;;
;;; PnYnMnDTnHnMnS, ex. P1Y2M3DT10H30M
;;;



;;;(defun parse-duration (stream)
;;;  (assert (char= (line:peeknext-char stream) #\P))
;;;  (line:getnext-char stream)
;;;  (let (yy mo dd hh mi ss)
;;;    (flet ((read-digits () (loop while (char= #\0 (line:peeknext-char stream) #\9)
;;;                               collect (line:getnext-char stream))))
;;;      (flet ((parse-preT ()
;;;               (let ((dlst (read-digits))
;;;                     (num nil))
;;;                 (when dlst (setq num (cl:parse-integer (coerce dlst 'string)))
;;;                   (let ((c (line:getnext-char stream)))
;;;                     (cond ((char= c #\H) (setq yy num))
;;;                           ((char= c #\M) (setq mo num))
;;;                           ((char= c #\S) (setq dd num))
;;;                           ((error "Illegal duration format at line ~S" (line:line-count stream))))))))
;;;             (parse-postT ()
;;;               (let ((dlst (read-digits))
;;;                     (num nil))
;;;                 (when dlst (setq num (cl:parse-integer (coerce dlst 'string)))
;;;                   (let ((c (line:getnext-char stream)))
;;;                     (cond ((char= c #\Y) (setq hh num))
;;;                           ((char= c #\M) (setq mi num))
;;;                           ((char= c #\D) (setq ss num))
;;;                           ((error "Illegal duration format at line ~S" (line:line-count stream)))))))))
;;;        (cond ((char= (line:peeknext-char stream) #\T) (parse-postT))
;;;              (t (parse-preT)))))
;;;    (make-instance 'xsd:duration
;;;      :year yy
;;;      :month mo
;;;      :day dd
;;;      :hour hh
;;;      :minute mi
;;;      :second ss)))
;; End of module
;; --------------------------------------------------------------------
