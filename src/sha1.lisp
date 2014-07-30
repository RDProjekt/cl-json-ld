;;;; Copyright (c) 2014 RD Projekt Sp. z o.o. Sp. k.
;;;; http://www.rdprojekt.pl/
;;;; Use is subject to license terms.
;;;;
;;;; This file is part of CL-JSON-LD.
;;;;
;;;; CL-JSON-LD is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; CL-JSON-LD is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with CL-JSON-LD.  If not, see <http://www.gnu.org/licenses/>.

(in-package :cl-json-ld-sha1)

;;; OpenSSL library calls.

(defparameter *use-open-ssl* nil)

#+allegro 
(progn
  (let ((ssl-lib-filename
       #+os-windows "libeay32.dll"
       #+(or os-unix unix) "/lib/i386-linux-gnu/libssl.so.1.0.0"
       #-(or os-windows os-unix unix) nil))
    (when ssl-lib-filename
      (handler-case 
          (progn 
            (load ssl-lib-filename :foreign t)
            (setf *use-open-ssl* t))
        (error ()
          (format t "Could not load SSL library from: ~a - using ironclad instead." ssl-lib-filename))))))

(defparameter *evp-sha1* nil)

(if *use-open-ssl*
  #+allegro (progn
    (ff:def-foreign-call 
        (evp-md-ctx-create "EVP_MD_CTX_create") 
        (:void) 
      :returning :foreign-address)

    (ff:def-foreign-call
        (evp-sha1 "EVP_sha1") (:void) :returning (:int))

    (ff:def-foreign-call
        (evp-digest-init "EVP_DigestInit")
        ((ctx :foreign-address) (type :int)))
     
    (ff:def-foreign-call
        (evp-digest-update "EVP_DigestUpdate")
        ((ctx :foreign-address) (d (* :char) (simple-array (unsigned-byte 8) (*))) (len :int))
      :strings-convert nil)

    (ff:def-foreign-call
        (evp-digest-final "EVP_DigestFinal_ex")
        ((ctx :foreign-address) (md (* :char) (simple-array (unsigned-byte 8) (*))) 
         (s :int (simple-array '(signed-byte 32) (1))))
      :strings-convert nil)

    (ff:def-foreign-call
        (evp-md-ctx-destroy "EVP_MD_CTX_destroy")
        ((ctx :foreign-address)))

    ;;; Internal class for keeping digest context.

    (setf *evp-sha1* (evp-sha1))

    (defclass sha1-digester ()
      ((ctx)))

    (defmethod initialize-instance :after ((digester sha1-digester) &key)
      (with-slots (ctx) digester
        (setf ctx (evp-md-ctx-create))
        (evp-digest-init ctx *evp-sha1*)))

    ;;; public API

    (defun make-sha1-digester ()
      (make-instance 'sha1-digester))

    (defun sha1-hash-strings (strings)
      "Hash a list of strings and return a string representation of the hash."
      (let ((digester (make-sha1-digester)))
        (loop for string in strings do
              (digester-add-string digester string))
        (digester-hexdigest digester)))

    (defun digester-add-string (digester string)
      "Update digester with a given string."
      (let ((octets (flexi-streams:string-to-octets string)))
        (evp-digest-update (slot-value digester 'ctx) octets (length octets))))

    (defun digester-hexdigest (digester)
      "Return a string representation of the digester's hash. Modifies digester!"
      (with-slots (ctx) digester
        (let ((buffer (make-array 64 :element-type '(unsigned-byte 8)))
              (s (make-array 1 :element-type '(signed-byte 32) :initial-element 64)))
          (evp-digest-final ctx buffer s)
          (prog1
              (crypto:byte-array-to-hex-string (subseq buffer 0 (elt s 0)))
            (evp-md-ctx-destroy ctx))))))
  #-allegro (error "OpenSSL libraries implemented only for Allegro.")
  
  ;; else (not *use-open-ssl*)
  (progn
    (defun make-sha1-digester ()
      (crypto:make-digest :sha1))
    
    (defun sha1-hash-strings (strings)
      "Hash a list of strings and return a string representation of the hash."
      (let ((digester (crypto:make-digest :sha1)))
        (loop for string in strings do
              (digester-add-string digester string))
        (digester-hexdigest digester)))
    
    (defun digester-add-string (digester string)
      "Update digester with a given string."
      (crypto:update-digest digester (flexi-streams:string-to-octets string)))
    
    (defun digester-hexdigest (digester)
      "Return a string representation of the digester's hash. Modifies digester!"
      (crypto:byte-array-to-hex-string (crypto:produce-digest digester)))))
  
  

    
    