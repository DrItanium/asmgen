; asmgen
; Copyright (c) 2025, Joshua Scoggins
; All rights reserved.
; 
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions are met:
;     * Redistributions of source code must retain the above copyright
;       notice, this list of conditions and the following disclaimer.
;     * Redistributions in binary form must reproduce the above copyright
;       notice, this list of conditions and the following disclaimer in the
;       documentation and/or other materials provided with the distribution.
; 
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR 
; ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
(defclass MAIN::emittable
  (is-a USER)
  (message-handler emit primary))
(defclass MAIN::statement
  (is-a emittable)
  (slot opcode
        (type LEXEME)
        (storage local)
        (visibility public)
        (default ?NONE))
  (message-handler emit primary))

(defclass MAIN::label
  (is-a statement)
  (message-handler emit primary))

(defmessage-handler MAIN::label emit primary
                    ()
                    (str-cat ?self:opcode
                             :))
(defmessage-handler MULTIFIELD join primary
                    (?separator)
                    (switch (length$ ?self)
                            (case 0 then ?self)
                            (case 1 then ?self)
                            (case 2 then (create$ (nth$ 1 ?self)
                                                  ?separator
                                                  (nth$ 2 ?self)))
                            (default (create$ (nth$ 1 ?self)
                                              ?separator
                                              (send (subseq$ ?self
                                                             2 (length$ ?self))
                                                    join
                                                    ?separator)))))
(defclass MAIN::register
  (is-a emittable)
  (slot index
        (type INTEGER)
        (range 0 35)
        (storage local)
        (visibility public)
        (access initialize-only)
        (default ?NONE))
  (slot is-global
        (type SYMBOL)
        (allowed-symbols FALSE
                         TRUE)
        (storage local)
        (visibility public)
        (access initialize-only)
        (default ?NONE))
  (message-handler emit primary))
(defmessage-handler MAIN::register emit primary
                    ()
                    (instance-name-to-symbol (instance-name ?self)))
(defclass MAIN::instruction
  (is-a statement)
  (multislot arguments
        (type LEXEME
              NUMBER
              INSTANCE)
        (storage local)
        (visibility public))
  (message-handler emit primary))
;(defmessage-handler MAIN::instruction emit primary
;                    ()
;                    (format nil
;                            "%s %s"
;                            
;                    (bind ?result
;                          ?self:opcode)
;                    (if ?self:src1 then
;                      (bind ?result
;                            (format nil
;                                    "%s %s"
;                                    ?result
;                                    ?self:src1))
;                      (if ?self:src2 then
;                        (bind ?result 
;                              (str-cat ?result , ?self:src2)))
;                      else
                      


(definstances MAIN::registers
              (pfp of register
                   (index 0)
                   (is-global FALSE))
              (sp of register
                   (index 1)
                   (is-global FALSE))
              (rip of register
                   (index 2)
                   (is-global FALSE))
              (r3 of register 
                  (index 3)
                  (is-global FALSE))
              (r4 of register 
                  (index 4)
                  (is-global FALSE))
              (r5 of register 
                  (index 5)
                  (is-global FALSE))
              (r6 of register 
                  (index 6)
                  (is-global FALSE))
              (r7 of register 
                  (index 7)
                  (is-global FALSE))
              (r8 of register 
                  (index 8)
                  (is-global FALSE))
              (r9 of register 
                  (index 9)
                  (is-global FALSE))
              (r10 of register 
                  (index 10)
                  (is-global FALSE))
              (r11 of register 
                  (index 11)
                  (is-global FALSE))
              (r12 of register 
                  (index 12)
                  (is-global FALSE))
              (r13 of register 
                  (index 13)
                  (is-global FALSE))
              (r14 of register 
                  (index 14)
                  (is-global FALSE))
              (r15 of register 
                  (index 15)
                  (is-global FALSE))
              (g0 of register 
                  (index 16)
                  (is-global TRUE))
              (g1 of register 
                  (index 17)
                  (is-global TRUE))
              (g2 of register 
                  (index 18)
                  (is-global TRUE))
              (g3 of register 
                  (index 19)
                  (is-global TRUE))
              (g4 of register 
                  (index 20)
                  (is-global TRUE))
              (g5 of register 
                  (index 21)
                  (is-global TRUE))
              (g6 of register 
                  (index 22)
                  (is-global TRUE))
              (g7 of register 
                  (index 23)
                  (is-global TRUE))
              (g8 of register 
                  (index 24)
                  (is-global TRUE))
              (g9 of register 
                  (index 25)
                  (is-global TRUE))
              (g10 of register 
                  (index 26)
                  (is-global TRUE))
              (g11 of register 
                  (index 27)
                  (is-global TRUE))
              (g12 of register 
                  (index 28)
                  (is-global TRUE))
              (g13 of register 
                  (index 29)
                  (is-global TRUE))
              (g14 of register 
                  (index 30)
                  (is-global TRUE))
              (fp of register 
                  (index 31)
                  (is-global TRUE))
              (fp0 of register
                   (index 32)
                   (is-global TRUE))
              (fp1 of register
                   (index 33)
                   (is-global TRUE))
              (fp2 of register
                   (index 34)
                   (is-global TRUE))
              (fp3 of register
                   (index 35)
                   (is-global TRUE))
              )
