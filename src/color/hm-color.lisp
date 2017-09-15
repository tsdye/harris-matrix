;;; hm-cfg.lisp

;; Copyright (C) Thomas Dye 2017

;; Licensed under the Gnu Public License Version 3 or later

(in-package #:hm)

;; internal, tested
(defun graphviz-color-string (index scheme &optional range)
  "Given a colorscheme, SCHEME, optionally an integer, RANGE, that indicates how
  many colors to select from, and a zero-based INDEX, return the Graphviz color
  string. INDEX can be an integer, in which case SCHEME must be the base of a
  Brewer color name or a CET color name; or a string, in which case SCHEME must
  be `x11', `svg', or `solarized'."
  (let ((named-colors (fset:set "x11" "svg" "solarized")))
    (unless (or (fset:contains? named-colors scheme)
                (brewer-colorscheme-distinctions scheme t)
                (cet-name-p scheme))
      (error "The string \"~a\" is an invalid color scheme.~%" scheme))
    (etypecase index
      (integer
       (progn
         (unless (>= index 0)
           (error "The index ~a is not in the range [0 .. ~a].~%" index range))
         (when (fset:contains? named-colors scheme)
           (error "The ~a colorscheme requires a color name, not ~a.~%"
                  scheme index))
         (if (cet-name-p scheme)
             (let ((map (cet-map scheme)))
               (cet-color map (1+ index) range))
             (let ((b-range (brewer-colorscheme-distinctions scheme)))
               (when (< range b-range) (setf b-range range))
               (format nil "/~a~s/~s" scheme b-range
                       (1+ (mod index b-range)))))))
      (string
       (progn
         (when (and (string= "solarized" scheme) (not (solarized-map index t)))
           (error "Error: ~s is not a solarized color name.~&" index))
         (if (string= scheme "solarized")
             (graphviz-hex-color (solarized-map index))
             (format nil "/~a/~a" scheme index)))))))

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

(defun solarized-map (name &optional (member nil))
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

(defun brewer-colorscheme-distinctions (color-scale &optional (member nil))
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

(defun cet-name-p (name)
  (fset:contains? (fset:set "cet-bgyw" "cet-kbc" "cet-blues" "cet-bmw"
                            "cet-inferno" "cet-kgy" "cet-gray" "cet-dimgray"
                            "cet-fire" "cet-kb" "cet-kg" "cet-kr" "cet-rainbow")
                  name))

(defun cet-pathname (name)
  "Returns a path to the CET csv file NAME."
  (let ((source (asdf:system-source-directory :hm)))
    (uiop:merge-pathnames* name source)))

(defun cet-map (name)
  "Return an fset map of the CET colormap, NAME, where the key is an integer in
[1..256] and the value is a graphviz HSV color string."
  (let ((file-name
          (cond
            ((string= name "cet-bgyw") "src/color/linear_bgyw_15-100_c67_n256.csv")
            ((string= name "cet-kbc") "src/color/linear_blue_5-95_c73_n256.csv")
            ((string= name "cet-blues") "src/color/linear_blue_95-50_c20_n256.csv")
            ((string= name "cet-bmw") "src/color/linear_bmw_5-95_c86_n256.csv")
            ((string= name "cet-inferno") "src/color/linear_bmy_10-95_c71_n256.csv")
            ((string= name "cet-kgy") "src/color/linear_green_5-95_c69_n256.csv")
            ((string= name "cet-gray") "src/color/linear_grey_0-100_c0_n256.csv")
            ((string= name "cet-dimgray") "src/color/linear_grey_10-95_c0_n256.csv")
            ((string= name "cet-fire") "src/color/linear_kryw_0-100_c71_n256.csv")
            ((string= name "cet-kb")
             "src/color/linear_ternary-blue_0-44_c57_n256.csv")
            ((string= name "cet-kg")
             "src/color/linear_ternary-green_0-46_c42_n256.csv")
            ((string= name "cet-kr")
             "src/color/linear_ternary-red_0-50_c52_n256.csv")
            ((string= name "cet-rainbow")
             "src/color/rainbow_bgyr_35-85_c72_n256.csv")
            (t nil)))
        (csv-file)
        (counter 1)
        (divisor 255.0)
        (map (fset:empty-map)))
    (unless file-name
      (error "The string \"~a\" is an invalid CET map alias.~%" name))
    (setf csv-file (read-table (cet-pathname file-name) nil nil))
    (dolist (row csv-file)
      (setf map
            (fset:with map counter
                       (graphviz-hsv-string
                        (rgb (/ (parse-integer (nth 0 row)) divisor)
                             (/ (parse-integer (nth 1 row)) divisor)
                             (/ (parse-integer (nth 2 row)) divisor)))))
      (setf counter (1+ counter)))
    map))

(defun cet-color (map index colors)
  "Return a graphviz HSV color string from MAP simulating a color ramp with
  COLORS colors. INDEX is an index into the color map, an integer in the range
  [1..COLORS]."
  (unless (and (> index 0) (<= index colors))
    (error "Color index ~a out of range [1 .. ~a].~%" index colors))
  (multiple-value-bind (increment base) (floor 256 colors)
    (fset:@ map (+ base (* index increment)))))
