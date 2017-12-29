;;; hm-cfg.lisp

;; Copyright (C) Thomas Dye 2017

;; Licensed under the Gnu Public License Version 3 or later

(in-package #:hm)

;; internal
(defun graphviz-color-string (index scheme &optional range)
  "Given a colorscheme, SCHEME, optionally an integer, RANGE, that indicates how
  many colors to select from, and a zero-based INDEX, return the Graphviz color
  string. INDEX can be an integer, in which case SCHEME must be the base of a
  Brewer color name or a CET color name; or a string, in which case SCHEME must
  be `x11', `svg', or `solarized'."
  (cond
    ((indexed-color-palette-p scheme)
     (unless range (error "Color palette range is missing.~%"))
     (if (cet-name-p scheme)
         (cet-color scheme index range)
         (brewer-color scheme index range)))
    ((named-color-palette-p scheme)
     (when (and (equal "solarized" scheme) (not (solarized-name-p index)))
       (error "Error: \"~s\" is not a solarized color name.~&" index))
     (when (and (equal "x11" scheme) (not (x11-name-p index)))
       (error "Error: the name \"~a\" is not an ~a color.~%" index scheme))
     (when (and (equal "svg" scheme) (not (svg-name-p index)))
       (error "Error: the name \"~a\" is not an ~a color.~%" index scheme))
     (if (equal scheme "solarized")
         (graphviz-hex-color (solarized-map index))
         (format nil "/~a/~a" scheme index)))
    (t (error "The string \"~a\" is an invalid color scheme name.~%" scheme))))

(defun brewer-color (scheme index range)
  "Returns a Graphviz dot string that names a Brewer color, given a valid Brewer
color SCHEME, a string or integer INDEX into the Brewer palette, and the
required RANGE of colors. If RANGE is greater than the number of distinctions
possible for the SCHEME, then the colors are recycled."
  (let ((b-range (brewer-colorscheme-distinctions scheme))
        (ind (typecase index
               (integer index)
               (string (parse-integer index))
               (otherwise (error "Error: ~s is not a valid Brewer color index." index)))))
    (when (< range b-range) (setf b-range (if (< range 3) 3 range)))
    (format nil "/~a~s/~s" scheme b-range (1+ (mod ind b-range)))))

(defun named-color-palette-p (palette)
  "Returns non-nil if the string PALETTE is one of `x11', `svg', or `solarized',
nil otherwise."
  (fset:contains? (fset:set "x11" "svg" "solarized") palette))

(defun indexed-color-palette-p (palette)
  "Returns non-nil if the string PALETTE names a valid Brewer palette as defined
  by Graphviz dot or one of the CET color schemes named by hm, nil otherwise."
  (or (brewer-color-scale-p palette) (cet-name-p palette)))

;; internal
(defun color-name-to-rgb (color)
  "Return the rgb representation of the x11 color name COLOR."
  (let ((sym (symbolicate #\+ (string-upcase color) #\+)))
    (if (boundp sym) (eval sym) nil)))

(defun x11-colors ()
  (fset:set "snow" "ghostwhite" "whitesmoke" "gainsboro" "floralwhite" "oldlace" "linen" "antiquewhite" "papayawhip" "blanchedalmond" "bisque" "peachpuff" "navajowhite" "moccasin" "cornsilk" "ivory" "lemonchiffon" "seashell" "honeydew" "mintcream" "azure" "aliceblue" "lavender" "lavenderblush" "mistyrose" "white" "black" "darkslategray" "darkslategrey" "dimgray" "dimgrey" "slategray" "slategrey" "lightslategray" "lightslategrey" "gray" "grey" "x11gray" "x11grey" "webgray" "webgrey" "lightgrey" "lightgray" "midnightblue" "navy" "navyblue" "cornflowerblue" "darkslateblue" "slateblue" "mediumslateblue" "lightslateblue" "mediumblue" "royalblue" "blue" "dodgerblue" "deepskyblue" "skyblue" "lightskyblue" "steelblue" "lightsteelblue" "lightblue" "powderblue" "paleturquoise" "darkturquoise" "mediumturquoise" "turquoise" "cyan" "aqua" "lightcyan" "cadetblue" "mediumaquamarine" "aquamarine" "darkgreen" "darkolivegreen" "darkseagreen" "seagreen" "mediumseagreen" "lightseagreen" "palegreen" "springgreen" "lawngreen" "green" "lime" "x11green" "webgreen" "chartreuse" "mediumspringgreen" "greenyellow" "limegreen" "yellowgreen" "forestgreen" "olivedrab" "darkkhaki" "khaki" "palegoldenrod" "lightgoldenrodyellow" "lightyellow" "yellow" "gold" "lightgoldenrod" "goldenrod" "darkgoldenrod" "rosybrown" "indianred" "saddlebrown" "sienna" "peru" "burlywood" "beige" "wheat" "sandybrown" "tan" "chocolate" "firebrick" "brown" "darksalmon" "salmon" "lightsalmon" "orange" "darkorange" "coral" "lightcoral" "tomato" "orangered" "red" "hotpink" "deeppink" "pink" "lightpink" "palevioletred" "maroon" "x11maroon" "webmaroon" "mediumvioletred" "violetred" "magenta" "fuchsia" "violet" "plum" "orchid" "mediumorchid" "darkorchid" "darkviolet" "blueviolet" "purple" "x11purple" "webpurple" "mediumpurple" "thistle" "snow1" "snow2" "snow3" "snow4" "seashell1" "seashell2" "seashell3" "seashell4" "antiquewhite1" "antiquewhite2" "antiquewhite3" "antiquewhite4" "bisque1" "bisque2" "bisque3" "bisque4" "peachpuff1" "peachpuff2" "peachpuff3" "peachpuff4" "navajowhite1" "navajowhite2" "navajowhite3" "navajowhite4" "lemonchiffon1" "lemonchiffon2" "lemonchiffon3" "lemonchiffon4" "cornsilk1" "cornsilk2" "cornsilk3" "cornsilk4" "ivory1" "ivory2" "ivory3" "ivory4" "honeydew1" "honeydew2" "honeydew3" "honeydew4" "lavenderblush1" "lavenderblush2" "lavenderblush3" "lavenderblush4" "mistyrose1" "mistyrose2" "mistyrose3" "mistyrose4" "azure1" "azure2" "azure3" "azure4" "slateblue1" "slateblue2" "slateblue3" "slateblue4" "royalblue1" "royalblue2" "royalblue3" "royalblue4" "blue1" "blue2" "blue3" "blue4" "dodgerblue1" "dodgerblue2" "dodgerblue3" "dodgerblue4" "steelblue1" "steelblue2" "steelblue3" "steelblue4" "deepskyblue1" "deepskyblue2" "deepskyblue3" "deepskyblue4" "skyblue1" "skyblue2" "skyblue3" "skyblue4" "lightskyblue1" "lightskyblue2" "lightskyblue3" "lightskyblue4" "slategray1" "slategray2" "slategray3" "slategray4" "lightsteelblue1" "lightsteelblue2" "lightsteelblue3" "lightsteelblue4" "lightblue1" "lightblue2" "lightblue3" "lightblue4" "lightcyan1" "lightcyan2" "lightcyan3" "lightcyan4" "paleturquoise1" "paleturquoise2" "paleturquoise3" "paleturquoise4" "cadetblue1" "cadetblue2" "cadetblue3" "cadetblue4" "turquoise1" "turquoise2" "turquoise3" "turquoise4" "cyan1" "cyan2" "cyan3" "cyan4" "darkslategray1" "darkslategray2" "darkslategray3" "darkslategray4" "aquamarine1" "aquamarine2" "aquamarine3" "aquamarine4" "darkseagreen1" "darkseagreen2" "darkseagreen3" "darkseagreen4" "seagreen1" "seagreen2" "seagreen3" "seagreen4" "palegreen1" "palegreen2" "palegreen3" "palegreen4" "springgreen1" "springgreen2" "springgreen3" "springgreen4" "green1" "green2" "green3" "green4" "chartreuse1" "chartreuse2" "chartreuse3" "chartreuse4" "olivedrab1" "olivedrab2" "olivedrab3" "olivedrab4" "darkolivegreen1" "darkolivegreen2" "darkolivegreen3" "darkolivegreen4" "khaki1" "khaki2" "khaki3" "khaki4" "lightgoldenrod1" "lightgoldenrod2" "lightgoldenrod3" "lightgoldenrod4" "lightyellow1" "lightyellow2" "lightyellow3" "lightyellow4" "yellow1" "yellow2" "yellow3" "yellow4" "gold1" "gold2" "gold3" "gold4" "goldenrod1" "goldenrod2" "goldenrod3" "goldenrod4" "darkgoldenrod1" "darkgoldenrod2" "darkgoldenrod3" "darkgoldenrod4" "rosybrown1" "rosybrown2" "rosybrown3" "rosybrown4" "indianred1" "indianred2" "indianred3" "indianred4" "sienna1" "sienna2" "sienna3" "sienna4" "burlywood1" "burlywood2" "burlywood3" "burlywood4" "wheat1" "wheat2" "wheat3" "wheat4" "tan1" "tan2" "tan3" "tan4" "chocolate1" "chocolate2" "chocolate3" "chocolate4" "firebrick1" "firebrick2" "firebrick3" "firebrick4" "brown1" "brown2" "brown3" "brown4" "salmon1" "salmon2" "salmon3" "salmon4" "lightsalmon1" "lightsalmon2" "lightsalmon3" "lightsalmon4" "orange1" "orange2" "orange3" "orange4" "darkorange1" "darkorange2" "darkorange3" "darkorange4" "coral1" "coral2" "coral3" "coral4" "tomato1" "tomato2" "tomato3" "tomato4" "orangered1" "orangered2" "orangered3" "orangered4" "red1" "red2" "red3" "red4" "deeppink1" "deeppink2" "deeppink3" "deeppink4" "hotpink1" "hotpink2" "hotpink3" "hotpink4" "pink1" "pink2" "pink3" "pink4" "lightpink1" "lightpink2" "lightpink3" "lightpink4" "palevioletred1" "palevioletred2" "palevioletred3" "palevioletred4" "maroon1" "maroon2" "maroon3" "maroon4" "violetred1" "violetred2" "violetred3" "violetred4" "magenta1" "magenta2" "magenta3" "magenta4" "orchid1" "orchid2" "orchid3" "orchid4" "plum1" "plum2" "plum3" "plum4" "mediumorchid1" "mediumorchid2" "mediumorchid3" "mediumorchid4" "darkorchid1" "darkorchid2" "darkorchid3" "darkorchid4" "purple1" "purple2" "purple3" "purple4" "mediumpurple1" "mediumpurple2" "mediumpurple3" "mediumpurple4" "thistle1" "thistle2" "thistle3" "thistle4" "gray0" "grey0" "gray1" "grey1" "gray2" "grey2" "gray3" "grey3" "gray4" "grey4" "gray5" "grey5" "gray6" "grey6" "gray7" "grey7" "gray8" "grey8" "gray9" "grey9" "gray10" "grey10" "gray11" "grey11" "gray12" "grey12" "gray13" "grey13" "gray14" "grey14" "gray15" "grey15" "gray16" "grey16" "gray17" "grey17" "gray18" "grey18" "gray19" "grey19" "gray20" "grey20" "gray21" "grey21" "gray22" "grey22" "gray23" "grey23" "gray24" "grey24" "gray25" "grey25" "gray26" "grey26" "gray27" "grey27" "gray28" "grey28" "gray29" "grey29" "gray30" "grey30" "gray31" "grey31" "gray32" "grey32" "gray33" "grey33" "gray34" "grey34" "gray35" "grey35" "gray36" "grey36" "gray37" "grey37" "gray38" "grey38" "gray39" "grey39" "gray40" "grey40" "gray41" "grey41" "gray42" "grey42" "gray43" "grey43" "gray44" "grey44" "gray45" "grey45" "gray46" "grey46" "gray47" "grey47" "gray48" "grey48" "gray49" "grey49" "gray50" "grey50" "gray51" "grey51" "gray52" "grey52" "gray53" "grey53" "gray54" "grey54" "gray55" "grey55" "gray56" "grey56" "gray57" "grey57" "gray58" "grey58" "gray59" "grey59" "gray60" "grey60" "gray61" "grey61" "gray62" "grey62" "gray63" "grey63" "gray64" "grey64" "gray65" "grey65" "gray66" "grey66" "gray67" "grey67" "gray68" "grey68" "gray69" "grey69" "gray70" "grey70" "gray71" "grey71" "gray72" "grey72" "gray73" "grey73" "gray74" "grey74" "gray75" "grey75" "gray76" "grey76" "gray77" "grey77" "gray78" "grey78" "gray79" "grey79" "gray80" "grey80" "gray81" "grey81" "gray82" "grey82" "gray83" "grey83" "gray84" "grey84" "gray85" "grey85" "gray86" "grey86" "gray87" "grey87" "gray88" "grey88" "gray89" "grey89" "gray90" "grey90" "gray91" "grey91" "gray92" "grey92" "gray93" "grey93" "gray94" "grey94" "gray95" "grey95" "gray96" "grey96" "gray97" "grey97" "gray98" "grey98" "gray99" "grey99" "gray100" "grey100" "darkgrey" "darkgray" "darkblue" "darkcyan" "darkmagenta" "darkred" "lightgreen" "crimson" "indigo" "olive" "rebeccapurple" "silver" "teal" ))

;; internal
(defun x11-name-p (name)
  "Return non-nil if NAME is an x11 color name, nil otherwise."
  (if (fset:contains? (x11-colors) name) t nil))

;; internal
(defun color-name-to-hsv (color)
  "Return the hsv string of the x11 color name COLOR."
  (let ((rgb-color (color-name-to-rgb color)))
    (if rgb-color (rgb-to-hsv rgb-color) nil)))

;; internal, tested
(defun graphviz-hsv-string (color)
  "Given a cl-colors package x11 color name COLOR, return the Graphviz HSV
  string that specifies the color."
  (let ((hsv-color (typecase color
                     (string (rgb-to-hsv (color-name-to-rgb color)))
                     (rgb (as-hsv color))
                     (hsv color)
                     (otherwise (error "Error: ~s is not a valid color." color)))))
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

(defun make-solarized-map ()
  "Return an fset map where the key is a solarized color name and the value is
the hexadecimal representation of the color."
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
    map))

(defun solarized-name-p (name)
  "Return nil if NAME is not a solarized color name, non-nil otherwise."
  (fset:domain-contains? (make-solarized-map) name))

(defun solarized-map (name)
  "Given a solarized color NAME, return as values the corresponding
hexadecimal representation string and a boolean indicating whether or
not color-scale was found."
  (fset:@ (make-solarized-map) name))

(defun make-brewer-map ()
  "Return an fset map where key is a brewer color scheme base name and value is
the number of distinctions in the color scheme."
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
    map))

(defun brewer-colorscheme-distinctions (color-scale)
  "Given a COLOR-SCALE name as a string, return as values the number
of distinctions as an integer and a boolean indicating whether or not
color-scale was found."
  (fset:@ (make-brewer-map) color-scale))

(defun brewer-color-scale-p (color-scale)
  (fset:domain-contains? (make-brewer-map) color-scale))

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



(defun cet-map (name)
  "Return an fset map of the CET colormap, NAME, where the key is an integer in
[0..255] and the value is a graphviz HSV color string."
  (let ((file-name
          (cond
            ((string= name "cet-bgyw") (cet-pathname "linear_bgyw_15-100_c67_n256.csv"))
            ((string= name "cet-kbc") (cet-pathname "linear_blue_5-95_c73_n256.csv"))
            ((string= name "cet-blues") (cet-pathname "linear_blue_95-50_c20_n256.csv"))
            ((string= name "cet-bmw") (cet-pathname "linear_bmw_5-95_c86_n256.csv"))
            ((string= name "cet-inferno") (cet-pathname "linear_bmy_10-95_c71_n256.csv"))
            ((string= name "cet-kgy") (cet-pathname "linear_green_5-95_c69_n256.csv"))
            ((string= name "cet-gray") (cet-pathname "linear_grey_0-100_c0_n256.csv"))
            ((string= name "cet-dimgray") (cet-pathname "linear_grey_10-95_c0_n256.csv"))
            ((string= name "cet-fire") (cet-pathname "linear_kryw_0-100_c71_n256.csv"))
            ((string= name "cet-kb")
             (cet-pathname "linear_ternary-blue_0-44_c57_n256.csv"))
            ((string= name "cet-kg")
             (cet-pathname "linear_ternary-green_0-46_c42_n256.csv"))
            ((string= name "cet-kr")
             (cet-pathname "linear_ternary-red_0-50_c52_n256.csv"))
            ((string= name "cet-rainbow")
             (cet-pathname "rainbow_bgyr_35-85_c72_n256.csv"))
            (t nil)))
        (csv-file)
        (counter 0)
        (divisor 255.0)
        (map (fset:empty-map)))
    (unless file-name
      (error "The string \"~a\" is an invalid CET map alias.~%" name))
    (setf csv-file (read-table file-name nil nil))
    (dolist (row csv-file)
      (setf map
            (fset:with map counter
                       (graphviz-hsv-string
                        (rgb (/ (parse-integer (nth 0 row)) divisor)
                             (/ (parse-integer (nth 1 row)) divisor)
                             (/ (parse-integer (nth 2 row)) divisor)))))
      (setf counter (1+ counter)))
    map))

(defun cet-color (name index colors)
  "Return a graphviz HSV color string from the CET map NAME simulating a color
  ramp with COLORS colors. INDEX is an index into the color map, an integer in
  the range [0..COLORS-1]."
  (let ((map (cet-map name))
        (ind (typecase index
               (integer index)
               (string (parse-integer index))
               (otherwise (error "Error: ~s is not a valid cet color index." index))))
        (ret))
    (unless (and (>= ind 0) (< ind colors))
      (error "Color index ~a out of range [0 .. ~a].~%" ind (1- colors)))
    (multiple-value-bind (increment base) (floor 256 colors)
      (setf ret
            (fset:@ map (+ (floor (/ (+ base increment) 2)) (* ind increment)))))
    ret))

(defun svg-map ()
  "Returns an fset map where the keys are svg color names and the values are
  hexadecimal color strings."
  (let ((csv-file (read-table (svg-pathname) nil nil))
        (map (fset:empty-map)))
    (dolist (row csv-file)
      (setf map (fset:with map (nth 0 row) (nth 1 row))))
    map))

(defun svg-name-p (name)
  "Predicate for a valid svg color name."
  (fset:domain-contains? (svg-map) name))

(defun graphviz-svg-hex-color (name)
  "Return a Graphviz hex color string for a valid svg color name."
  (if (svg-name-p name) (graphviz-hex-color (fset:@ (svg-map) name))
      (error "Error: \"~a\" is not an svg color name.~% " name)))

(defun svg-hex-color (name)
  "Return a hex color string for a valid svg color name."
  (if (svg-name-p name) (fset:@ (svg-map) name)
      (error "Error: \"~a\" is not an svg color name.~% " name)))

(defun graphviz-svg-hsv-color (name)
  "Return a Graphviz hsv color string for a valid svg color name."
  (graphviz-hsv-string (hex-to-rgb (svg-hex-color name))))
