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
(defmessage-handler LEXEME emit primary () ?self)
(defmessage-handler LEXEME emit-as-argument primary () ?self)
(defmessage-handler NUMBER emit primary () ?self)
(defmessage-handler NUMBER emit-as-argument primary () ?self)
(defmessage-handler MULTIFIELD emit primary
                    ()
                    (bind ?result
                          (create$))
                    (progn$ (?item ?self)
                            (bind ?result
                                  ?result
                                  (send ?item
                                        emit)))
                    ?result)
(defmessage-handler MULTIFIELD emit-as-argument primary
                    ()
                    (bind ?result
                          (create$))
                    (progn$ (?item ?self)
                            (bind ?result
                                  ?result
                                  (send ?item
                                        emit-as-argument)))
                    ?result)


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
(defclass MAIN::has-parent
  (is-a USER)
  (slot parent
        (type INSTANCE
              SYMBOL)
        (allowed-symbols FALSE)
        (storage local)
        (visibility public)
        (default-dynamic FALSE)))
(defclass MAIN::emittable
  (is-a USER)
  (message-handler emit primary)
  (message-handler emit-as-argument primary))
(defmessage-handler MAIN::emittable emit-as-argument
                    ()
                    ; redirect to emit
                    (send ?self
                          emit))
(defclass MAIN::alias
  (is-a emittable)
  (slot target
        (storage local)
        (visibility public)
        (default ?NONE))
  (message-handler emit primary))
(defmessage-handler MAIN::alias emit primary
                    ()
                    (send ?self:target
                          emit))
(defclass MAIN::statement
  (is-a emittable
        has-parent)
  (slot opcode
        (type LEXEME)
        (storage local)
        (visibility public)
        (default ?NONE))
  (message-handler emit primary))

(defclass MAIN::label
  (is-a statement)
  (message-handler emit-as-argument primary)
  (message-handler emit primary))

(defmessage-handler MAIN::label emit primary
                    ()
                    (str-cat ?self:opcode
                             :))
(defmessage-handler MAIN::label emit-as-argument primary
                    ()
                    ?self:opcode)
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
(defmessage-handler MAIN::instruction emit primary
                    ()
                    (str-cat ?self:opcode " "
                             (expand$ (send (send ?self:arguments
                                                  emit-as-argument)
                                            join
                                            ,))))

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

(deffunction MAIN::definstruction
             (?opcode $?args)
             (make-instance of instruction
                            (opcode ?opcode)
                            (arguments ?args)))
(deffunction MAIN::ldconst
             (?value ?destination)
             (definstruction ldconst
                             ?value
                             ?destination))
; CTRL instructions
(deffunction MAIN::b (?targ) (definstruction b ?targ))
(deffunction MAIN::*call (?targ) (definstruction call ?targ))
(deffunction MAIN::*ret () (definstruction ret))
(deffunction MAIN::bal (?targ) (definstruction bal ?targ))
(deffunction MAIN::bno (?targ) (definstruction bno ?targ))
(deffunction MAIN::bg (?targ) (definstruction bg ?targ))
(deffunction MAIN::be (?targ) (definstruction be ?targ))
(deffunction MAIN::bge (?targ) (definstruction bge ?targ))
(deffunction MAIN::bl (?targ) (definstruction bl ?targ))
(deffunction MAIN::bne (?targ) (definstruction bne ?targ))
(deffunction MAIN::ble (?targ) (definstruction ble ?targ))
(deffunction MAIN::bo (?targ) (definstruction bo ?targ))

(deffunction MAIN::faultno () (definstruction faultno))
(deffunction MAIN::faultg () (definstruction faultg))
(deffunction MAIN::faulte () (definstruction faulte))
(deffunction MAIN::faultge () (definstruction faultge))
(deffunction MAIN::faultl () (definstruction faultl))
(deffunction MAIN::faultne () (definstruction faultne))
(deffunction MAIN::faultle () (definstruction faultle))
(deffunction MAIN::faulto () (definstruction faulto))
; cobr instructions
(deffunction MAIN::testno (?dst) (definstruction testno ?dst))
(deffunction MAIN::testg (?dst) (definstruction testg ?dst))
(deffunction MAIN::teste (?dst) (definstruction teste ?dst))
(deffunction MAIN::testge (?dst) (definstruction testge ?dst))
(deffunction MAIN::testl (?dst) (definstruction testl ?dst))
(deffunction MAIN::testne (?dst) (definstruction testne ?dst))
(deffunction MAIN::testle (?dst) (definstruction testle ?dst))
(deffunction MAIN::testo (?dst) (definstruction testo ?dst))
(deffunction MAIN::bbc (?bitpos ?src ?targ) (definstruction bbc ?bitpos ?src ?targ))
(deffunction MAIN::cmpobg (?src1 ?src2 ?targ) (definstruction cmpobg ?src1 ?src2 ?targ))
(deffunction MAIN::cmpobe (?src1 ?src2 ?targ) (definstruction cmpobe ?src1 ?src2 ?targ))
(deffunction MAIN::cmpobge (?src1 ?src2 ?targ) (definstruction cmpobge ?src1 ?src2 ?targ))
(deffunction MAIN::cmpobl (?src1 ?src2 ?targ) (definstruction cmpobl ?src1 ?src2 ?targ))
(deffunction MAIN::cmpobne (?src1 ?src2 ?targ) (definstruction cmpobne ?src1 ?src2 ?targ))
(deffunction MAIN::cmpoble (?src1 ?src2 ?targ) (definstruction cmpoble ?src1 ?src2 ?targ))
(deffunction MAIN::bbs (?bitpos ?src ?targ) (definstruction bbs ?bitpos ?src ?targ))
(deffunction MAIN::cmpibno (?src1 ?src2 ?targ) (definstruction cmpibno ?src1 ?src2 ?targ))
(deffunction MAIN::cmpibg (?src1 ?src2 ?targ) (definstruction cmpibg ?src1 ?src2 ?targ))
(deffunction MAIN::cmpibe (?src1 ?src2 ?targ) (definstruction cmpibe ?src1 ?src2 ?targ))
(deffunction MAIN::cmpibge (?src1 ?src2 ?targ) (definstruction cmpibge ?src1 ?src2 ?targ))
(deffunction MAIN::cmpibl (?src1 ?src2 ?targ) (definstruction cmpibl ?src1 ?src2 ?targ))
(deffunction MAIN::cmpibne (?src1 ?src2 ?targ) (definstruction cmpibne ?src1 ?src2 ?targ))
(deffunction MAIN::cmpible (?src1 ?src2 ?targ) (definstruction cmpible ?src1 ?src2 ?targ))
(deffunction MAIN::cmpibo (?src1 ?src2 ?targ) (definstruction cmpibo ?src1 ?src2 ?targ))

; mem instructions
(deffunction MAIN::*ldob (?src ?dst) (definstruction ldob ?src ?dst))
(deffunction MAIN::*stob (?src ?dst) (definstruction stob ?src ?dst))
(deffunction MAIN::*bx (?targ) (definstruction bx ?targ))
(deffunction MAIN::*balx (?targ ?dst) (definstruction balx ?targ ?dst))
(deffunction MAIN::*callx (?targ) (definstruction callx ?targ))
(deffunction MAIN::*ldos (?src ?dst) (definstruction ldos ?src ?dst))
(deffunction MAIN::*stos (?src ?dst) (definstruction stos ?src ?dst))
(deffunction MAIN::*lda (?src ?dst) (definstruction lda ?src ?dst))
(deffunction MAIN::*ld (?src ?dst) (definstruction ld ?src ?dst))
(deffunction MAIN::*st (?src ?dst) (definstruction ld ?src ?dst))
(deffunction MAIN::*ldl (?src ?dst) (definstruction ldl ?src ?dst))
(deffunction MAIN::*stl (?src ?dst) (definstruction stl ?src ?dst))
(deffunction MAIN::*ldt (?src ?dst) (definstruction ldt ?src ?dst))
(deffunction MAIN::*stt (?src ?dst) (definstruction stt ?src ?dst))
(deffunction MAIN::*ldq (?src ?dst) (definstruction ldq ?src ?dst))
(deffunction MAIN::*stq (?src ?dst) (definstruction stq ?src ?dst))
(deffunction MAIN::*ldib (?src ?dst) (definstruction ldib ?src ?dst))
(deffunction MAIN::*stib (?src ?dst) (definstruction stib ?src ?dst))
(deffunction MAIN::*ldis (?src ?dst) (definstruction ldis ?src ?dst))
(deffunction MAIN::*stis (?src ?dst) (definstruction stis ?src ?dst))

; reg instructions
(deffunction MAIN::notbit (?bitpos ?src ?dst) (definstruction notbit ?bitpos ?src ?dst))
(deffunction MAIN::*and (?src1 ?src2 ?dst) (definstruction and ?src1 ?src2 ?dst))
(deffunction MAIN::*andnot (?src1 ?src2 ?dst) (definstruction andnot ?src1 ?src2 ?dst))
(deffunction MAIN::setbit (?bitpos ?src ?dst) (definstruction setbit ?bitpos ?src ?dst))
(deffunction MAIN::*notand (?src1 ?src2 ?dst) (definstruction notand ?src1 ?src2 ?dst))
(deffunction MAIN::*xor (?src1 ?src2 ?dst) (definstruction xor ?src1 ?src2 ?dst))
(deffunction MAIN::*or (?src1 ?src2 ?dst) (definstruction or ?src1 ?src2 ?dst))
(deffunction MAIN::*nor (?src1 ?src2 ?dst) (definstruction nor ?src1 ?src2 ?dst))
(deffunction MAIN::*xnor (?src1 ?src2 ?dst) (definstruction xnor ?src1 ?src2 ?dst))
(deffunction MAIN::*not (?src ?dst) (definstruction not ?src ?dst))
(deffunction MAIN::*ornot (?src1 ?src2 ?dst) (definstruction ornot ?src1 ?src2 ?dst))
(deffunction MAIN::clrbit (?bitpos ?src ?dst) (definstruction clrbit ?bitpos ?src ?dst))
(deffunction MAIN::*notor (?src1 ?src2 ?dst) (definstruction notor ?src1 ?src2 ?dst))
(deffunction MAIN::*nand (?src1 ?src2 ?dst) (definstruction nand ?src1 ?src2 ?dst))
(deffunction MAIN::alterbit (?bitpos ?src ?dst) (definstruction alterbit ?bitpos ?src ?dst))
(deffunction MAIN::addo (?src1 ?src2 ?dst) (definstruction addo ?src1 ?src2 ?dst))
(deffunction MAIN::addi (?src1 ?src2 ?dst) (definstruction addi ?src1 ?src2 ?dst))
(deffunction MAIN::subo (?src1 ?src2 ?dst) (definstruction subo ?src1 ?src2 ?dst))
(deffunction MAIN::subi (?src1 ?src2 ?dst) (definstruction subi ?src1 ?src2 ?dst))
(deffunction MAIN::shro (?len ?src ?dst) (definstruction shro ?len ?src ?dst))
(deffunction MAIN::shrdi (?len ?src ?dst) (definstruction shrdi ?len ?src ?dst))
(deffunction MAIN::shlo (?len ?src ?dst) (definstruction shlo ?len ?src ?dst))
(deffunction MAIN::rotate (?len ?src ?dst) (definstruction rotate ?len ?src ?dst))
(deffunction MAIN::shli (?len ?src ?dst) (definstruction shli ?len ?src ?dst))
(deffunction MAIN::cmpo (?src1 ?src2) (definstruction cmpo ?src1 ?src2))
(deffunction MAIN::cmpi (?src1 ?src2) (definstruction cmpi ?src1 ?src2))
(deffunction MAIN::concmpo (?src1 ?src2) (definstruction concmpo ?src1 ?src2))
(deffunction MAIN::concmpi (?src1 ?src2) (definstruction concmpi ?src1 ?src2))
(deffunction MAIN::cmpinco (?src1 ?src2 ?dst) (definstruction cmpinco ?src1 ?src2 ?dst))
(deffunction MAIN::cmpinci (?src1 ?src2 ?dst) (definstruction cmpinci ?src1 ?src2 ?dst))
(deffunction MAIN::cmpdeco (?src1 ?src2 ?dst) (definstruction cmpdeco ?src1 ?src2 ?dst))
(deffunction MAIN::cmpdeci (?src1 ?src2 ?dst) (definstruction cmpdeci ?src1 ?src2 ?dst))
(deffunction MAIN::scanbyte (?src1 ?src2) (definstruction scanbyte ?src1 ?src2))
(deffunction MAIN::chkbit (?bitpos ?src) (definstruction chkbit ?bitpos ?src))
(deffunction MAIN::addc (?src1 ?src2 ?dst) (definstruction addc ?src1 ?src2 ?dst))
(deffunction MAIN::subc (?src1 ?src2 ?dst) (definstruction subc ?src1 ?src2 ?dst))
(deffunction MAIN::mov (?src ?dst) (definstruction mov ?src ?dst))
(deffunction MAIN::movl (?src ?dst) (definstruction movl ?src ?dst))
(deffunction MAIN::movt (?src ?dst) (definstruction movt ?src ?dst))
(deffunction MAIN::movq (?src ?dst) (definstruction movq ?src ?dst))
