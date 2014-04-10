#lang racket

(provide sites-as-dots
         sites-as-colored-spheres)

(require (prefix-in interface:  "interface.rkt")
         math/array
         racket/generator
         plot
         generic-bind
         )

; Radii and colors for the chemical elements

(define atomic-radii
  (hash "H"   0.053
        "He"  0.031
        "Li"  0.167
        "Be"  0.112
        "B"   0.087
        "C"   0.067
        "N"   0.056
        "O"   0.048
        "F"   0.042
        "Ne"  0.038
        "Na"  0.19 
        "Mg"  0.145
        "Al"  0.118
        "Si"  0.111
        "P"   0.098
        "S"   0.088
        "Cl"  0.079
        "Ar"  0.071
        "K"   0.243
        "Ca"  0.194
        "Sc"  0.184
        "Ti"  0.176
        "V"   0.171
        "Cr"  0.166
        "Mn"  0.161
        "Fe"  0.156
        "Co"  0.152
        "Ni"  0.149
        "Cu"  0.145
        "Zn"  0.142
        "Ga"  0.136
        "Ge"  0.125
        "As"  0.114
        "Se"  0.103
        "Br"  0.094
        "Kr"  0.088
        "Rb"  0.265
        "Sr"  0.219
        "Y"   0.212
        "Zr"  0.206
        "Nb"  0.198
        "Mo"  0.19 
        "Tc"  0.183
        "Ru"  0.178
        "Rh"  0.173
        "Pd"  0.169
        "Ag"  0.165
        "Cd"  0.161
        "In"  0.156
        "Sn"  0.145
        "Sb"  0.133
        "Te"  0.123
        "I"   0.115
        "Xe"  0.108
        "Cs"  0.298
        "Ba"  0.253
        "La"  0.195
        "Ce"  0.185
        "Pr"  0.247
        "Nd"  0.206
        "Pm"  0.205
        "Sm"  0.238
        "Eu"  0.231
        "Gd"  0.233
        "Tb"  0.225
        "Dy"  0.228
        "Ho"  0.226
        "Er"  0.226
        "Tm"  0.222
        "Yb"  0.222
        "Lu"  0.217
        "Hf"  0.208
        "Ta"  0.2  
        "W"   0.193
        "Re"  0.188
        "Os"  0.185
        "Ir"  0.18 
        "Pt"  0.177
        "Au"  0.174
        "Hg"  0.171
        "Tl"  0.156
        "Pb"  0.154
        "Bi"  0.143
        "Po"  0.135
        "At"  0.127
        "Rn"  0.12 
        "Ac"  0.195
        "Th"  0.18 
        "Pa"  0.18 
        "U"   0.175
        "Np"  0.175
        "Pu"  0.175
        "Am"  0.175))

(define (atom-radius element)
  ; Most elements for which there is no value in the table above are
  ; rather big, so we use 0.2 as the default value.
  (hash-ref atomic-radii element 0.2))

(define atom-colors
  (hash "H"  'gray
        "C"  'black
        "N"  'darkblue
        "O"  'red
        "F"  'green
        "Cl" 'green
        "Br" 'darkred
        "I"  'darkviolet
        "He" 'cyan
        "Ne" 'cyan
        "Ar" 'cyan
        "Xe" 'cyan
        "Kr" 'cyan
        "P"  'orange
        "S"  'yellow
        "B"  'salmon
        "Li" 'violet
        "Na" 'violet
        "K"  'violet
        "Rb" 'violet
        "Cs" 'violet
        "Fr" 'violet
        "Be" 'darkgreen
        "Mg" 'darkgreen
        "Ca" 'darkgreen
        "Sr" 'darkgreen
        "Ba" 'darkgreen
        "Ra" 'darkgreen
        "Ti" 'gray
        "Fe" 'orange))

(define (atom-color element)
  (hash-ref atom-colors element 'pink))

; Sort the sites by chemical element, i.e. into groups
; that are drawn with the same radius and color.
(define (sites-by-element universe)
  (~for/fold ([by-element (hash)])
             ([($list a ai si) (interface:in-sites-with-indices universe)])
    (let ([element (interface:atom.name a)])
      (hash-update by-element element (λ (sites) (cons si sites)) (list)))))


; The simplest renderer: each site becomes a dot

(define (sites-as-dots configuration)
  (let ([positions (interface:configuration.positions configuration)])
    (points3d (sequence-map in-array
                            (in-array-axis positions))
              #:sym 'dot)))

; A slightly fancier renderer: each site is drawn as a circle
; whose color and radius reflects the chemical element.

(define (sites-as-colored-spheres configuration)
  (let ([universe (interface:configuration.universe configuration)]
        [positions (interface:configuration.positions configuration)])
    (~for/list ([(⋈ element sites) (sites-by-element universe)])
      (points3d (sequence-map in-array
                              (in-array-axis
                               (array-slice-ref positions
                                                (list  sites (::)))))
                #:sym 'fullcircle
                #:size (* 200. (atom-radius element))
                #:color (atom-color element)))))
