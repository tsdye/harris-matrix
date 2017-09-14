;;; hm-cfg.lisp

;; Copyright (C) Thomas Dye 2017

;; Licensed under the Gnu Public License Version 3 or later

(in-package #:hm)

;; internal, tested
(defun graphviz-color-string (index scheme &optional range)
  "Given a colorscheme, SCHEME, optionally an integer, RANGE, that indicates how
  many colors to select from, and a zero-based INDEX, return the Graphviz color
  string. INDEX can be an integer, in which case SCHEME must be the base of a
  Brewer color name, or a string, in which case SCHEME must be `x11', `svg', or
  `solarized', else `x11' will be used."
  (let ((local-scheme scheme)
        (local-range (if (and range (> 3 range)) 3 range)))
    (etypecase index
      (integer (unless (brewer-colorscheme-distinctions scheme :member t)
                 (error "Error: ~s does not indicate a Brewer colorscheme.~&"
                        scheme))
       (unless range
         (error "Error: No range for `graphviz-color-string'.~&"))
       (let ((brewer-range
               (if (> local-range (brewer-colorscheme-distinctions scheme))
                   (brewer-colorscheme-distinctions scheme)
                   local-range)))
         (format nil
                 "/~a~s/~s"
                 scheme
                 brewer-range
                 (1+ (mod index brewer-range)))))
      (string (unless (fset:contains? (fset:set "x11" "svg" "solarized")
                                      scheme)
                (setf local-scheme "x11"))
       (if (string= "solarized" local-scheme)
           (if (solarized-map index :member t)
               (graphviz-hex-color (solarized-map index))
               (error "Error: ~s is not a solarized color name.~&"
                      index))
           (format nil "/~a/~a" local-scheme index))))))

;; internal
(defun color-name-to-rgb (color)
  "Return the rgb representation of the x11 color name COLOR."
  (eval (symbolicate #\+ (string-upcase color) #\+)))

;; internal, tested
(defun graphviz-hsv-string (color)
  "Given a cl-colors package x11 color name COLOR, return the Graphviz HSV
  string that specifies the color."
  (let ((hsv-color (etypecase color
                     (string (rgb-to-hsv (color-name-to-rgb color)))
                     (rgb (as-hsv color)))))
    (format nil "~,3f ~,3f ~,3f"
            (/ (hsv-hue hsv-color) 360)
            (hsv-saturation hsv-color)
            (hsv-value hsv-color))))

;; internal, tested
(defun graphviz-color-from-ramp (index color1 color2 steps)
  "Given two strings, COLOR1 and COLOR2, each with a valid x11 color
  name and the number of STEPS in the ramp, return a Graphviz hsv
  color string."
  (when (> index steps)
    (error "Error: Index out of bounds."))
  (let ((c1 (color-name-to-rgb color1))
        (c2 (color-name-to-rgb color2))
        (alpha (/ index steps)))
    (graphviz-hsv-string (rgb-combination c1 c2 alpha))))

(defun solarized-map (name &key member)
  "Given a solarized color NAME, return as values the corresponding
hexadecimal representation string and a boolean indicating whether or
not color-scale was found.  If MEMBER is non-nil, then return nil if
NAME is not a map key and non-nil otherwise."
  (let ((map (-> (fset:empty-map)
                 (fset:with "base03" "002b36")
                 (fset:with "base02" "073642")
                 (fset:with "base01" "586e75")
                 (fset:with "base00" "657b83")
                 (fset:with "base0" "839496")
                 (fset:with "base1" "93a1a1")
                 (fset:with "base2" "eee8d5")
                 (fset:with "base3" "fdf6e3")
                 (fset:with "yellow" "b58900")
                 (fset:with "orange" "cb4b16")
                 (fset:with "red" "dc322f")
                 (fset:with "magenta" "d33682")
                 (fset:with "violet" "6c71c4")
                 (fset:with "blue" "268bd2")
                 (fset:with "cyan" "2aa198")
                 (fset:with "green" "859900"))))
    (if member
        (fset:domain-contains? map name)
        (fset:@ map name))))

(defun brewer-colorscheme-distinctions (color-scale &key member)
  "Given a COLOR-SCALE name as a string, return as values the number
of distinctions as an integer and a boolean indicating whether or not
color-scale was found.  If MEMBER is non-nil, then return nil if
COLOR-SCALE is not a map key and non-nil otherwise."
  (let ((map (-> (fset:empty-map)
                 (fset:with "accent" 8)
                 (fset:with "blues" 9)
                 (fset:with "brbg" 11)
                 (fset:with "bugn" 9)
                 (fset:with "bupu" 9)
                 (fset:with "dark2" 8)
                 (fset:with "gnbu" 9)
                 (fset:with "greens" 9)
                 (fset:with "greys" 9)
                 (fset:with "oranges" 9)
                 (fset:with "orrd" 9)
                 (fset:with "paired" 12)
                 (fset:with "pastel1" 9)
                 (fset:with "pastel2" 8)
                 (fset:with "piyg" 11)
                 (fset:with "prgn" 11)
                 (fset:with "pubu" 9)
                 (fset:with "pubugn" 9)
                 (fset:with "puor" 11)
                 (fset:with "purd" 9)
                 (fset:with "purples" 9)
                 (fset:with "rdbu" 11)
                 (fset:with "rdgy" 11)
                 (fset:with "rdpu" 9)
                 (fset:with "rdylbu" 11)
                 (fset:with "rdylgn" 11)
                 (fset:with "reds" 9)
                 (fset:with "set1" 9)
                 (fset:with "set2" 8)
                 (fset:with "set3" 12)
                 (fset:with "spectral" 11)
                 (fset:with "ylgn" 9)
                 (fset:with "ylgnbu" 9)
                 (fset:with "ylorbr" 9)
                 (fset:with "ylorrd" 9))))
    (if member
        (fset:domain-contains? map color-scale)
        (fset:@ map color-scale))))

(defun graphviz-hex-color (hex)
  "A convenience function to bridge a difference in how cl-colors and
Graphviz represent hexadecimal color specifications.  Given a 6
character hexidecimal string, returns a string where the hexadecimal
specification is prefixed with an octothorp."
  (assert (eq (length hex) 6)
          (hex)
          "Error: ~a is not 6 characters."
          hex)
  (format nil "#~a" hex))
