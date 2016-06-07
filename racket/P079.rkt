#lang racket

; Original data set
; (define keylog '(319 680 180 690 129 620 762 689 762 318
;                  368 710 720 710 629 168 160 689 716 731
;                  736 729 316 729 729 710 769 290 719 680
;                  318 389 162 289 162 718 729 319 790 680
;                  890 362 319 760 316 729 380 319 728 716))

; Remove duplicates
; (define keylog '(129 160 162 168 180
;                  289 290
;                  316 318 319 362 368 380 389
;                  620 629 680 689 690
;                  710 716 718 719 720 728 729 731 736 760 762 769 790
;                  890))

; 7 never appears after another digit; it must come first
; (define passcode 7)
; (define keylog '(10 129 16 160 162 168 18 180 19
;                  20 28 289 29 290
;                  31 316 318 319 36 362 368 380 389
;                  60 62 620 629 680 689 69 690
;                  890
;                  90))

; 3 never appears after another digit; it must come next
; (define passcode 73)
; (define keylog '(1 10 129 16 160 162 168 18 180 19
;                  20 28 289 29 290
;                  6 60 62 620 629 68 680 689 69 690
;                  80 890
;                  90))

; 1 never appears after another digit; it must come next
; (define passcode 731)
; (define keylog '(0
;                  20 28 289 29 290
;                  6 60 62 620 629 68 680 689 69 690
;                  8 80 890
;                  9 90))

; 6 never appears after another digit; it must come next
; (define passcode 7316)
; (define keylog '(0
;                  2 20 28 289 29 290
;                  8 80 89 890
;                  9 90))

; 2 never appears after another digit; it must come next
; (define passcode 73162)
; (define keylog '(0
;                  8 80 89 890
;                  9 90))

; 8 never appears after another digit; it must come next
; (define passcode 731628)
; (define keylog '(0
;                  9 90))

; 9 never appears after another digit; it must come next
; 0 is the last digit left
(define passcode 73162890)

passcode
