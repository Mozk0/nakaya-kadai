(defparameter *tests* nil)

(defmacro define-test (name &body body)
  `(macrolet ((fail (&optional (msg ""))
                `(progn (format t "~A~%" ,msg)
                        (return-from ,',name nil))))
     (push (list ',name (lambda () (block ,name ,@body))) *tests*)))

(defun run-test ()
  (loop for test in *tests*
       for res = (funcall (cadr test))
       if (not res)
       do (format t "FAIL: ~A~%" (car test))))

(defparameter *raw-chrom*
  '(C1 C1 C1 C1 C1 C1 C1 C1 C1 C1 C1 C1 C1 C2 C2 C2 C2 C2 C2 C2 C2 C2 C2 C2 C2 C3 C3 C3 C3 C3 C3 C3 C3 C3 C3 C3 C3 C3))
(defparameter *raw-pos*
  '(0.0 9.3 17.2 29.9 38.7 52.8 57.8 72.4 76.6 93.2 97.0 115.5 116.5 0.0 15.9 16.9 32.7 34.0 47.2 49.6 50.6 60.5 91.0 92.0 97.0 0.0 3.5 33.1 38.6 39.6 52.5 73.9 81.2 93.3 106.1 108.3 125.1 142.0))
(defparameter *raw-traits*
  '((I-001 40.1609 25.0789 11.1917 1 2 2 * 2 2 2 2 2 2 1 1 1 1 2 1 2 2 2 2 2 2 2 2 2 2 1 1 2 2 2 2 2 2 2 2 2 2 2)
    (I-002 42.1206 24.6962 14.2351 2 1 1 2 2 2 2 2 2 2 2 2 2 2 1 1 1 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)
    (I-003 39.8114 22.4848 7.0315 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 1 1 2 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)
    (I-004 nil 23.1155 11.2001 2 2 2 2 1 1 1 1 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 1 1 2 2 2 2 2 2 2 2 2 2 2)
    (I-005 39.5613 21.0934 3.5133 3 2 1 1 1 1 1 1 1 1 1 1 1 1 2 1 2 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 1 1)
    (I-006 35.9214 24.4051 6.5268 1 2 2 2 1 1 2 2 2 2 2 2 2 2 2 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 2)
    (I-007 35.7827 22.6201 6.677 1 * 1 1 1 2 1 1 1 1 1 1 1 1 1 2 1 1 2 1 1 1 1 2 2 2 1 1 1 1 1 1 1 2 2 2 2 2 1)
    (I-008 40.3456 25.6763 13.8114 3 2 2 2 2 2 2 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 2 2 2 2 2 2 2 2 2 2 2)
    (I-009 34.915 18.4293 9.6165 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 1 1 1 1 2 2 1 1 1 1 1 1 1 1 1 1 1)
    (I-010 38.2035 21.4365 12.5332 3 2 2 2 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 2 2 2 2 1 1 1 1)
    (I-011 37.569 21.6936 10.093 2 1 1 1 1 1 2 2 2 2 2 2 2 2 2 1 2 2 1 2 2 2 1 2 2 2 1 1 1 1 1 1 2 2 2 2 2 2 2)
    (I-012 36.2877 19.8964 4.3426 2 1 2 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 2 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 1)
    (I-013 35.1703 24.3329 8.7456 2 2 2 2 1 1 1 1 1 1 1 1 2 2 1 2 1 2 2 2 2 2 2 1 1 1 2 2 1 1 1 1 1 2 2 2 2 2 2)
    (I-014 35.9284 23.1083 9.263 1 1 * 2 2 2 2 2 1 1 1 1 1 1 2 2 2 2 1 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 1)
    (I-015 35.2581 21.6275 2.1578 2 1 1 1 1 1 1 1 2 1 1 1 1 1 2 1 2 1 1 2 2 2 2 1 1 1 1 1 2 2 2 1 1 1 1 1 1 1 1)
    (I-016 35.5318 20.2117 2.0452 1 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1)
    (I-017 32.9361 19.3172 6.7152 1 1 2 2 2 2 2 2 1 1 2 2 2 2 1 2 1 1 2 1 1 1 1 1 1 1 2 2 1 1 1 1 1 1 1 1 1 1 1)
    (I-018 39.2119 21.5842 7.3653 3 2 2 2 1 1 1 1 1 1 1 1 1 1 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1)
    (I-019 37.5822 20.7091 6.3376 3 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 2 2 2 1)
    (I-020 34.3745 21.5412 1.7542 3 1 1 1 1 1 1 1 1 1 2 2 1 1 1 1 1 1 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2)
    (I-021 41.1683 18.755 6.412 1 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2)
    (I-022 38.8503 22.7577 6.5795 1 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 1 1 2 2 2 2 2 2 2 2 2 2 2)
    (I-023 36.2489 22.0595 10.708 2 1 1 1 1 1 2 2 2 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 2 2 2 2 2 2 2)
    (I-024 36.0199 21.8695 7.2566 2 1 1 1 2 2 2 2 2 2 1 1 1 1 2 2 2 2 2 1 1 1 1 2 2 2 1 1 1 1 1 1 1 1 1 2 2 2 2)
    (I-025 40.8797 21.012 12.9149 1 2 2 2 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1)
    (I-026 37.032 22.744 10.0882 2 1 1 1 1 2 1 1 2 2 2 2 1 1 1 1 1 1 2 2 2 2 2 1 1 1 1 1 2 2 2 2 1 1 1 1 1 2 2)
    (I-027 37.4105 19.3142 5.6415 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 2 2 2 2 2 1 1 1 2 1 1 1 1 1 1 1)
    (I-028 35.1721 21.1632 8.2553 1 2 2 2 2 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 1 1 2 2 2 1 1 1 1 1 1 1 1)
    (I-029 42.4946 24.3402 6.895 3 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2)
    (I-030 34.9585 19.3804 8.7403 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1)
    (I-031 36.5364 21.9401 9.7476 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 2 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2)
    (I-032 39.8688 23.8597 12.5529 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 1)
    (I-033 37.9361 22.6061 10.6419 2 1 1 1 1 1 1 1 1 1 2 2 2 2 1 2 2 2 2 1 1 1 2 2 2 2 1 1 2 2 2 2 2 2 2 2 2 2 2)
    (I-034 nil 21.6625 8.7109 3 2 2 1 1 2 2 2 2 2 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 1 1 2 2 2 2 2 2 1 1 1 1 1)
    (I-035 36.4191 21.4859 8.2028 3 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 2 2 2 2 2 2 1 1)
    (I-036 36.9236 21.9065 10.2534 3 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 2 2 2 2 2 2 2 2)
    (I-037 39.2499 20.964 7.4226 3 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 2 2 1 1 1 2 2 2 2 2 2 2 2)
    (I-038 40.068 21.3415 5.663 1 2 2 1 1 1 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2)
    (I-039 37.1344 24.1826 9.9754 2 1 1 2 2 2 2 2 2 2 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 2 2 2 1 1)
    (I-040 34.5096 23.2078 4.1392 1 2 2 2 2 1 1 1 1 1 1 1 1 1 2 1 1 1 1 1 1 1 1 1 1 2 1 1 1 1 1 1 1 1 1 1 1 2 2)
    (I-041 40.679 22.2618 9.5799 2 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2)
    (I-042 35.0991 20.5021 2.8445 1 1 1 1 1 1 1 1 1 1 2 2 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 1 1 1 1 1 1 1 2 2 2 2)
    (I-043 37.0128 22.6108 9.9654 2 1 2 2 2 2 2 2 1 1 2 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1)
    (I-044 36.7059 19.4681 5.3701 1 2 2 2 2 2 2 2 2 2 2 1 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 1 1 1 1)
    (I-045 36.2928 22.4794 10.0887 2 1 2 2 2 2 2 2 2 2 2 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 2 2 2 1 1)
    (I-046 41.6065 22.804 9.8058 2 2 2 2 2 2 2 2 2 2 2 2 1 1 2 2 2 2 2 2 2 2 2 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2)
    (I-047 40.3296 22.1121 7.2232 2 2 2 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)
    (I-048 36.7904 19.6637 3.9335 3 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1)
    (I-049 38.0721 20.2767 2.4387 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 1 1 1 1 1)
    (I-050 36.6653 21.2682 6.8253 2 2 2 2 2 2 1 1 1 1 2 2 2 2 1 1 1 1 1 1 1 1 1 2 2 2 1 1 1 1 1 1 2 2 2 2 2 2 2)))

(defstruct (marker-position (:type list))
  (chromosome nil :type symbol)
  (coordinate 0.0 :type float))

(defstruct (trait (:type list))
  (id nil :type symbol)
  (qt nil :type (or float null))
  (markers (make-array 0 :element-type '(or integer null))
           :type (simple-array (or integer null))))

(defparameter *marker-positions*
  (map 'vector
       #'(lambda (raw-chrom-and-pos)
           (make-marker-position
            :chromosome (first raw-chrom-and-pos)
            :coordinate (second raw-chrom-and-pos)))
       (mapcar #'list *raw-chrom* *raw-pos*)))

(defparameter *traits-qt1*
  (map 'vector
       #'(lambda (raw-trait)
           (make-trait
            :id (first raw-trait)
            :qt (second raw-trait)
            :markers (make-array (- (length raw-trait) 5)
                                 :element-type '(or integer null)
                                 :initial-contents (sublis '((* . nil)) (nthcdr 5 raw-trait)))))
       *raw-traits*))

(defparameter *traits-qt2*
  (map 'vector
       #'(lambda (raw-trait)
           (make-trait
            :id (first raw-trait)
            :qt (third raw-trait)
            :markers (make-array (- (length raw-trait) 5)
                                 :element-type '(or integer null)
                                 :initial-contents (sublis '((* . nil)) (nthcdr 5 raw-trait)))))
       *raw-traits*))

(defun average (seq)
  (let ((na-free-seq (remove nil seq)))
    (/ (reduce #'+ na-free-seq)
       (length na-free-seq))))
(define-test average
  (= (average #(1 2 3 nil 4)) 2.5))

;;;;
;;;; Impute missing values.
;;;;

(defun impute-missing-values (traits)
  ;; missing QT value is imputed to be average.
  (let ((avg (average (map 'list #'trait-qt traits))))
    (map 'nil #'(lambda (trait)
                  (setf #1=(trait-qt trait)
                        (or #1# avg)))
         traits))
  ;; missing marker is imputed to be left marker.
  (map 'nil #'(lambda (trait)
                (setf (trait-markers trait)
                      (map 'vector
                           #'(lambda (marker left-marker)
                               (or marker left-marker))
                           (trait-markers trait)
                           (concatenate 'vector #(1) (trait-markers trait)))))
       traits))

(impute-missing-values *traits-qt1*)
(impute-missing-values *traits-qt2*)

(define-test imputation
  (every #'(lambda (trait)
             (and (every #'identity (trait-markers trait))
                  (trait-qt trait)))
         *traits-qt1*))

;;;;
;;;; EM Algorithm.
;;;;

;;
;; initial solution for EM algorithm.
;;
(defun square (x) (* x x))
(defun sum_of_seq (seq) (reduce #'+ seq))

(defun standard-deviation (seq)
  (let ((avg (average seq)))
    (sqrt (/ (sum_of_seq (map 'list
                              #'(lambda (x)
                                  (square (- x avg)))
                              seq))
             (length seq)))))
(define-test standard-deviation
  (= (standard-deviation #(1 2 3))
     (sqrt (/ 2 3))))

(defun initial-solution (traits)
  (let ((qts (map 'list #'trait-qt traits)))
    (values (average qts)
            (standard-deviation qts))))

;;
;; conditional probability
;;
(defun Haldane (x) ; (1 - exp(-2x)) / 2
  (/ (- 1
        (exp (* -2 x)))
     2)) 
(defun distance (x y) (abs (- x y)))

(defun P-single-forward (marker     left-marker     right-marker
                         marker-pos left-marker-pos right-marker-pos
                         &optional (map-function #'Haldane))
  (declare (ignore right-marker right-marker-pos))
  (let ((key `(,marker ,left-marker))
        (r (funcall map-function (distance left-marker-pos marker-pos))))
    (cond
      ((equal key '(1 1)) (- 1.0 r))
      ((equal key '(2 1)) r)
      ((equal key '(1 2)) r)
      ((equal key '(2 2)) (- 1.0 r)))))

(defun P-single-backward (marker     left-marker     right-marker
                          marker-pos left-marker-pos right-marker-pos
                          &optional (map-function #'Haldane))
  (declare (ignore left-marker left-marker-pos))
  (let ((key `(,marker ,right-marker))
        (r (funcall map-function (distance right-marker-pos marker-pos))))
    (cond
      ((equal key '(1 1)) (- 1.0 r))
      ((equal key '(2 1)) r)
      ((equal key '(1 2)) r)
      ((equal key '(2 2)) (- 1.0 r)))))

(defun P-double (marker     left-marker     right-marker
                 marker-pos left-marker-pos right-marker-pos
                 &optional (map-function #'Haldane))
  (let ((key `(,marker ,left-marker ,right-marker))
        (r-right (funcall map-function (distance right-marker-pos marker-pos)))
        (r-left  (funcall map-function (distance right-marker-pos marker-pos)))
        (r       (funcall map-function (distance right-marker-pos left-marker-pos))))
    (cond
      ((equal key '(1 1 1)) 1.0)
      ((equal key '(1 1 2)) (/ r-left r))
      ((equal key '(1 2 1)) (/ r-right r))
      ((equal key '(1 2 2)) 0.0)
      ((equal key '(2 1 1)) 0.0)
      ((equal key '(2 1 2)) (/ r-right r))
      ((equal key '(2 2 1)) (/ r-left r))
      ((equal key '(2 2 2)) 1.0))))

(defun maximum (seq)
  (reduce #'max seq))

(defun minimum (seq)
  (reduce #'min seq))

(defun neighbor (chr coord marker-positions)
  (let* ((coordinates (map 'list #'marker-position-coordinate
                           (remove-if-not #'(lambda (x)
                                              (eq (marker-position-chromosome x) chr))
                                          marker-positions)))
         (left-neighbor (maximum (remove-if-not #'(lambda (x) (<= x coord))
                                                coordinates)))
         (right-neighbor (minimum (remove-if-not #'(lambda (x) (> x coord))
                                                 coordinates))))
    (list left-neighbor right-neighbor)))
(define-test neighbor
  (equal (neighbor 'C1 22.0 *marker-positions*)
         '(17.2 29.9)))

;;
;; EM-algorithm
;;

(defun EM-algorithm (chr coord traits marker-positions &optional (prob #'P-double))
  (destructuring-bind (left-coord right-coord) (neighbor chr coord marker-positions)
    (labels ((expectation ()