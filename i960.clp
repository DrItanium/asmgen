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
  (slot next-register
        (type SYMBOL
              INSTANCE)
        (allowed-symbols FALSE)
        (storage local)
        (visibility public)
        (access initialize-only)
        (default-dynamic FALSE))
  (slot is-global
        (type SYMBOL)
        (allowed-symbols FALSE
                         TRUE)
        (storage local)
        (visibility public)
        (access initialize-only)
        (default ?NONE))
  (message-handler has-next-register primary)
  (message-handler emit primary))
(defmessage-handler MAIN::register has-next-register primary
                    ()
                    (instancep ?self:next-register))
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
                   (next-register [sp])
                   (index 0)
                   (is-global FALSE))
              (sp of register
                  (next-register [rip])
                  (index 1)
                  (is-global FALSE))
              (rip of register
                   (next-register [r3])
                   (index 2)
                   (is-global FALSE))
              (r3 of register 
                  (next-register [r4])
                  (index 3)
                  (is-global FALSE))
              (r4 of register 
                  (next-register [r5])
                  (index 4)
                  (is-global FALSE))
              (r5 of register 
                  (next-register [r6])
                  (index 5)
                  (is-global FALSE))
              (r6 of register 
                  (next-register [r7])
                  (index 6)
                  (is-global FALSE))
              (r7 of register 
                  (next-register [r8])
                  (index 7)
                  (is-global FALSE))
              (r8 of register 
                  (next-register [r9])
                  (index 8)
                  (is-global FALSE))
              (r9 of register 
                  (next-register [r10])
                  (index 9)
                  (is-global FALSE))
              (r10 of register 
                   (next-register [r11])
                   (index 10)
                   (is-global FALSE))
              (r11 of register 
                   (next-register [r12])
                   (index 11)
                   (is-global FALSE))
              (r12 of register 
                   (next-register [r13])
                   (index 12)
                   (is-global FALSE))
              (r13 of register 
                   (next-register [r14])
                   (index 13)
                   (is-global FALSE))
              (r14 of register 
                   (next-register [r15])
                   (index 14)
                   (is-global FALSE))
              (r15 of register 
                   (index 15)
                   (is-global FALSE))
              (g0 of register 
                  (next-register [g1])
                  (index 16)
                  (is-global TRUE))
              (g1 of register 
                  (next-register [g2])
                  (index 17)
                  (is-global TRUE))
              (g2 of register 
                  (next-register [g3])
                  (index 18)
                  (is-global TRUE))
              (g3 of register 
                  (next-register [g4])
                  (index 19)
                  (is-global TRUE))
              (g4 of register 
                  (next-register [g5])
                  (index 20)
                  (is-global TRUE))
              (g5 of register 
                  (next-register [g6])
                  (index 21)
                  (is-global TRUE))
              (g6 of register 
                  (next-register [g7])
                  (index 22)
                  (is-global TRUE))
              (g7 of register 
                  (next-register [g8])
                  (index 23)
                  (is-global TRUE))
              (g8 of register 
                  (next-register [g9])
                  (index 24)
                  (is-global TRUE))
              (g9 of register 
                  (next-register [g10])
                  (index 25)
                  (is-global TRUE))
              (g10 of register 
                   (next-register [g11])
                   (index 26)
                   (is-global TRUE))
              (g11 of register 
                   (next-register [g12])
                   (index 27)
                   (is-global TRUE))
              (g12 of register 
                   (next-register [g13])
                   (index 28)
                   (is-global TRUE))
              (g13 of register 
                   (next-register [g14])
                   (index 29)
                   (is-global TRUE))
              (g14 of register 
                   (next-register [fp])
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
(deffunction MAIN::deflabel
             (?name)
             (make-instance of label
                            (opcode ?name)))
(deffunction MAIN::deflocal-label
             (?name)
             ; take advantage of gnu local label syntax
             (deflabel (if (integerp ?name) then 
                         ?name 
                         else 
                         (sym-cat .L 
                                  ?name))))

(deffunction MAIN::definstruction
             (?opcode $?args)
             (make-instance of instruction
                            (opcode ?opcode)
                            (arguments ?args)))
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
(deffunction MAIN::andnot (?src1 ?src2 ?dst) (definstruction andnot ?src1 ?src2 ?dst))
(deffunction MAIN::setbit (?bitpos ?src ?dst) (definstruction setbit ?bitpos ?src ?dst))
(deffunction MAIN::notand (?src1 ?src2 ?dst) (definstruction notand ?src1 ?src2 ?dst))
(deffunction MAIN::xor (?src1 ?src2 ?dst) (definstruction xor ?src1 ?src2 ?dst))
(deffunction MAIN::*or (?src1 ?src2 ?dst) (definstruction or ?src1 ?src2 ?dst))
(deffunction MAIN::nor (?src1 ?src2 ?dst) (definstruction nor ?src1 ?src2 ?dst))
(deffunction MAIN::xnor (?src1 ?src2 ?dst) (definstruction xnor ?src1 ?src2 ?dst))
(deffunction MAIN::*not (?src ?dst) (definstruction not ?src ?dst))
(deffunction MAIN::ornot (?src1 ?src2 ?dst) (definstruction ornot ?src1 ?src2 ?dst))
(deffunction MAIN::clrbit (?bitpos ?src ?dst) (definstruction clrbit ?bitpos ?src ?dst))
(deffunction MAIN::notor (?src1 ?src2 ?dst) (definstruction notor ?src1 ?src2 ?dst))
(deffunction MAIN::nand (?src1 ?src2 ?dst) (definstruction nand ?src1 ?src2 ?dst))
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
(deffunction MAIN::synmov (?dst ?src) (definstruction synmov ?dst ?src))
(deffunction MAIN::synmovl (?dst ?src) (definstruction synmovl ?dst ?src))
(deffunction MAIN::synmovq (?dst ?src) (definstruction synmovq ?dst ?src))
(deffunction MAIN::cmpstr (?src1 ?src2 ?len) (definstruction cmpstr ?src1 ?src2 ?len))
(deffunction MAIN::movqstr (?dst ?src ?len) (definstruction movqstr ?dst ?src ?len))
(deffunction MAIN::movstr (?dst ?src ?len) (definstruction movstr ?dst ?src ?len))
(deffunction MAIN::atmod (?src ?mask ?src/dst) (definstruction atmod ?src ?mask ?src/dst))
(deffunction MAIN::atadd (?src/dst ?src ?dst) (definstruction atadd ?src/dst ?src ?dst))
(deffunction MAIN::inspacc (?src ?dst) (definstruction inspacc ?src ?dst))
(deffunction MAIN::ldphy (?src ?dst) (definstruction ldphy ?src ?dst))
(deffunction MAIN::synld (?src ?dst) (definstruction synld ?src ?dst))
(deffunction MAIN::fill (?dst ?value ?len) (definstruction fill ?dst ?value ?len))
(deffunction MAIN::spanbit (?src ?dst) (definstruction spanbit ?src ?dst))
(deffunction MAIN::scanbit (?src ?dst) (definstruction scanbit ?src ?dst))
(deffunction MAIN::daddc (?src1 ?src2 ?dst) (definstruction daddc ?src1 ?src2 ?dst))
(deffunction MAIN::dsubc (?src1 ?src2 ?dst) (definstruction dsubc ?src1 ?src2 ?dst))
(deffunction MAIN::dmovt (?src ?dst) (definstruction dmovt ?src ?dst))
(deffunction MAIN::modac (?mask ?src ?dst) (definstruction modac ?mask ?src ?dst))
(deffunction MAIN::condrec (?src ?dst) (definstruction condrec ?src ?dst))
(deffunction MAIN::*modify (?mask ?src ?src/dst) (definstruction modify ?mask ?src ?src/dst))
(deffunction MAIN::extract (?bitpos ?len ?src/dst) (definstruction extract ?bitpos ?len ?src/dst))
(deffunction MAIN::modtc (?mask ?src ?src/dst) (definstruction modtc ?mask ?src ?src/dst))
; for modpc, the manuals state that ?src and ?mask should be the same at all times so that is what we are doing!
(deffunction MAIN::modpc (?mask ?src/dst) (definstruction modpc ?mask ?mask ?src/dst))
(deffunction MAIN::*receive (?src ?dst) (definstruction receive ?src ?dst))
(deffunction MAIN::calls (?targ) (definstruction calls ?targ))
(deffunction MAIN::*send (?dst ?src1 ?src2) (definstruction send ?dst ?src1 ?src2))
(deffunction MAIN::sendserv (?src) (definstruction sendserv ?src))
(deffunction MAIN::resumprcs (?src) (definstruction resumprcs ?src))
(deffunction MAIN::schedprcs (?src) (definstruction schedprcs ?src))
(deffunction MAIN::saveprcs () (definstruction saveprcs))
(deffunction MAIN::condwait (?src) (definstruction condwait ?src))
(deffunction MAIN::wait (?src) (definstruction wait ?src))
(deffunction MAIN::signal (?dst) (definstruction signal ?dst))
(deffunction MAIN::mark () (definstruction mark))
(deffunction MAIN::fmark () (definstruction fmark))
(deffunction MAIN::flushreg () (definstruction flushreg))
(deffunction MAIN::syncf () (definstruction syncf))
(deffunction MAIN::emul (?src1 ?src2 ?dst) (definstruction emul ?src1 ?src2 ?dst))
(deffunction MAIN::ediv (?src1 ?src2 ?dst) (definstruction ediv ?src1 ?src2 ?dst))
(deffunction MAIN::ldtime (?dst) (definstruction ldtime ?dst))
; numerics instructions
(deffunction MAIN::cvtir (?src ?dst) (definstruction cvtir ?src ?dst))
(deffunction MAIN::cvtilr (?src ?dst) (definstruction cvtilr ?src ?dst))

(deffunction MAIN::scalerl (?src1 ?src2 ?dst) (definstruction scalerl ?src1 ?src2 ?dst))
(deffunction MAIN::scaler (?src1 ?src2 ?dst) (definstruction scaler ?src1 ?src2 ?dst))
(deffunction MAIN::atanr (?src1 ?src2 ?dst) (definstruction atanr ?src1 ?src2 ?dst))
(deffunction MAIN::logepr (?src1 ?src2 ?dst) (definstruction logepr ?src1 ?src2 ?dst))
(deffunction MAIN::logr (?src1 ?src2 ?dst) (definstruction logr ?src1 ?src2 ?dst))
(deffunction MAIN::remr (?src1 ?src2 ?dst) (definstruction remr ?src1 ?src2 ?dst))
(deffunction MAIN::cmpor (?src1 ?src2) (definstruction cmpor ?src1 ?src2))
(deffunction MAIN::cmpr (?src1 ?src2) (definstruction cmpr ?src1 ?src2))
(deffunction MAIN::sqrtr (?src ?dst) (definstruction sqrtr ?src ?dst))
(deffunction MAIN::expr (?src ?dst) (definstruction expr ?src ?dst))
(deffunction MAIN::logbnr (?src ?dst) (definstruction logbnr ?src ?dst))
(deffunction MAIN::roundr (?src ?dst) (definstruction roundr ?src ?dst))
(deffunction MAIN::sinr (?src ?dst) (definstruction sinr ?src ?dst))
(deffunction MAIN::cosr (?src ?dst) (definstruction cosr ?src ?dst))
(deffunction MAIN::tanr (?src ?dst) (definstruction tanr ?src ?dst))
(deffunction MAIN::classr (?src) (definstruction classr ?src))
(deffunction MAIN::atanrl (?src1 ?src2 ?dst) (definstruction atanrl ?src1 ?src2 ?dst))
(deffunction MAIN::logeprl (?src1 ?src2 ?dst) (definstruction logeprl ?src1 ?src2 ?dst))
(deffunction MAIN::logrl (?src1 ?src2 ?dst) (definstruction logrl ?src1 ?src2 ?dst))
(deffunction MAIN::remrl (?src1 ?src2 ?dst) (definstruction remrl ?src1 ?src2 ?dst))
(deffunction MAIN::cmporl (?src1 ?src2) (definstruction cmporl ?src1 ?src2))
(deffunction MAIN::cmprl (?src1 ?src2) (definstruction cmprl ?src1 ?src2))
(deffunction MAIN::sqrtrl (?src ?dst) (definstruction sqrtrl ?src ?dst))
(deffunction MAIN::exprl (?src ?dst) (definstruction exprl ?src ?dst))
(deffunction MAIN::logbnrl (?src ?dst) (definstruction logbnrl ?src ?dst))
(deffunction MAIN::roundrl (?src ?dst) (definstruction roundrl ?src ?dst))
(deffunction MAIN::sinrl (?src ?dst) (definstruction sinrl ?src ?dst))
(deffunction MAIN::cosrl (?src ?dst) (definstruction cosrl ?src ?dst))
(deffunction MAIN::tanrl (?src ?dst) (definstruction tanrl ?src ?dst))
(deffunction MAIN::classrl (?src) (definstruction classrl ?src))

(deffunction MAIN::cvtri (?src ?dst) (definstruction cvtri ?src ?dst))
(deffunction MAIN::cvtril (?src ?dst) (definstruction cvtril ?src ?dst))
(deffunction MAIN::cvtzri (?src ?dst) (definstruction cvtzri ?src ?dst))
(deffunction MAIN::cvtzril (?src ?dst) (definstruction cvtzril ?src ?dst))
(deffunction MAIN::movr (?src ?dst) (definstruction movr ?src ?dst))
(deffunction MAIN::movrl (?src ?dst) (definstruction movrl ?src ?dst))
(deffunction MAIN::cpysre (?src1 ?src2 ?dst) (definstruction cpysre ?src1 ?src2 ?dst))
(deffunction MAIN::cpyrsre (?src1 ?src2 ?dst) (definstruction cpyrsre ?src1 ?src2 ?dst))
(deffunction MAIN::movre (?src ?dst) (definstruction movre ?src ?dst))
(deffunction MAIN::mulo (?src1 ?src2 ?dst) (definstruction mulo ?src1 ?src2 ?dst))
(deffunction MAIN::remo (?src1 ?src2 ?dst) (definstruction remo ?src1 ?src2 ?dst))
(deffunction MAIN::divo (?src1 ?src2 ?dst) (definstruction divo ?src1 ?src2 ?dst))
(deffunction MAIN::muli (?src1 ?src2 ?dst) (definstruction muli ?src1 ?src2 ?dst))
(deffunction MAIN::remi (?src1 ?src2 ?dst) (definstruction remi ?src1 ?src2 ?dst))
(deffunction MAIN::modi (?src1 ?src2 ?dst) (definstruction modi ?src1 ?src2 ?dst))
(deffunction MAIN::divi (?src1 ?src2 ?dst) (definstruction divi ?src1 ?src2 ?dst))
(deffunction MAIN::divr (?src1 ?src2 ?dst) (definstruction divr ?src1 ?src2 ?dst))
(deffunction MAIN::mulr (?src1 ?src2 ?dst) (definstruction mulr ?src1 ?src2 ?dst))
(deffunction MAIN::subr (?src1 ?src2 ?dst) (definstruction subr ?src1 ?src2 ?dst))
(deffunction MAIN::addr (?src1 ?src2 ?dst) (definstruction addr ?src1 ?src2 ?dst))
(deffunction MAIN::divrl (?src1 ?src2 ?dst) (definstruction divrl ?src1 ?src2 ?dst))
(deffunction MAIN::mulrl (?src1 ?src2 ?dst) (definstruction mulrl ?src1 ?src2 ?dst))
(deffunction MAIN::subrl (?src1 ?src2 ?dst) (definstruction subrl ?src1 ?src2 ?dst))
(deffunction MAIN::addrl (?src1 ?src2 ?dst) (definstruction addrl ?src1 ?src2 ?dst))

; the only really important pseudo instruction
(defgeneric MAIN::ldconst
            "A psuedo instruction that makes loading constants trivial")
(defmethod MAIN::ldconst
  (?value ?destination)
  (definstruction ldconst 
                  ?value
                  ?destination))
; when we want to load a constant in the range of 0 to 31
(defmethod MAIN::ldconst
  ((?value INTEGER
           (<= 0 ?current-argument
               31))
   ?destination)
  (mov ?value 
       ?destination))
(defmethod MAIN::ldconst
  ((?value INTEGER
           (= ?current-argument 
              -1))
   ?destination)
  (subo 1 0 ?destination))
(defmethod MAIN::ldconst
  ((?value SYMBOL
           (not (neq ?current-argument
                     0xFFFFFFFF
                     0xffffffff)))
   ?destination)
  (*not 0 
        ?destination))
(defmethod MAIN::ldconst
  ((?value INTEGER
           (<= 32 ?current-argument 62))
   ?destination)
  (addo 31 (- ?value 
              31)
        ?destination))

(defclass MAIN::code-body
  (is-a emittable
        has-parent)
  (multislot contents
             (storage local)
             (visibility public)
             (default ?NONE))
  (message-handler emit primary))
(defmessage-handler MAIN::code-body emit primary
                    ()
                    (send ?self:contents
                          emit))

(deffunction MAIN::defbody
             ($?contents)
             (make-instance of code-body 
                            (contents ?contents)))
(defclass MAIN::pseudo-instruction
  (is-a instruction
        code-body)
  (message-handler reconstruct primary)
  (message-handler emit primary))
(defmessage-handler MAIN::pseudo-instruction emit primary
                    ()
                    (send ?self:contents
                          emit))
(defmessage-handler MAIN::pseudo-instruction reconstruct primary
                    ()
                    (str-cat ?self:opcode " "
                             (expand$ (send (send ?self:arguments
                                                  emit-as-argument)
                                            join
                                            ,))))
(deffunction MAIN::defpseudo-instruction
             (?name ?arguments $?body)
             (make-instance of pseudo-instruction
                            (opcode ?name)
                            (arguments ?arguments)
                            (contents ?body)))
; We want to prevent the loss of information so we make pseudo instructions both a code body and instruction
; the instruction refers to the pseudo instruction with the code body referring to the expansion
; pseudo instructions
(deffunction MAIN::save-globals
             (?temporary)
             (defpseudo-instruction save-globals
                                    (create$ ?temporary)
                                    (ldconst 64
                                             ?temporary)
                                    (addo [sp] 
                                          ?temporary
                                          [sp])
                                    (*stq [g0] "-64(sp)")
                                    (*stq [g4] "-48(sp)")
                                    (*stq [g8] "-32(sp)")
                                    (*stt [g12] "-16(sp)")))
(deffunction MAIN::restore-globals
             ()
             (defpseudo-instruction restore-globals
                                    (create$)
                                    (*ldq "-64(sp)" [g0])
                                    (*ldq "-48(sp)" [g4])
                                    (*ldq "-32(sp)" [g8])
                                    (*ldt "-16(sp)" [g12])))
(defgeneric MAIN::long-binary-operation)
(defmethod MAIN::long-binary-operation
  ((?function SYMBOL)
   (?src1 register
          (send ?current-argument
                has-next-register))
   (?src2 register
          (send ?current-argument
                has-next-register))
   (?dst register
         (send ?current-argument
               has-next-register)))
  (defpseudo-instruction (sym-cat ?function l)
                         (create$ ?src1
                                  ?src2
                                  ?dst)
                         (funcall ?function
                                  ?src1
                                  ?src2
                                  ?dst)
                         (funcall ?function
                                  (send ?src1
                                        get-next-register)
                                  (send ?src2
                                        get-next-register)
                                  (send ?dst
                                        get-next-register))))

(defmethod MAIN::nandl
  ((?src1 register
          (send ?current-argument
                has-next-register))
   (?src2 register
          (send ?current-argument
                has-next-register))
   (?dst register
         (send ?current-argument
               has-next-register)))
  (long-binary-operation nand 
                         ?src1 
                         ?src2 
                         ?dst))
(defmethod MAIN::norl
  ((?src1 register
          (send ?current-argument
                has-next-register))
   (?src2 register
          (send ?current-argument
                has-next-register))
   (?dst register
         (send ?current-argument
               has-next-register)))
  (long-binary-operation nor 
                         ?src1 
                         ?src2 
                         ?dst))
(defmethod MAIN::xorl
  ((?src1 register
          (send ?current-argument
                has-next-register))
   (?src2 register
          (send ?current-argument
                has-next-register))
   (?dst register
         (send ?current-argument
               has-next-register)))
  (long-binary-operation xor 
                         ?src1 
                         ?src2 
                         ?dst))
(defmethod MAIN::andnotl
  ((?src1 register
          (send ?current-argument
                has-next-register))
   (?src2 register
          (send ?current-argument
                has-next-register))
   (?dst register
         (send ?current-argument
               has-next-register)))
  (long-binary-operation andnot 
                         ?src1 
                         ?src2 
                         ?dst))
(defmethod MAIN::notandl
  ((?src1 register
          (send ?current-argument
                has-next-register))
   (?src2 register
          (send ?current-argument
                has-next-register))
   (?dst register
         (send ?current-argument
               has-next-register)))
  (long-binary-operation notand 
                         ?src1 
                         ?src2 
                         ?dst))
(defmethod MAIN::ornotl
  ((?src1 register
          (send ?current-argument
                has-next-register))
   (?src2 register
          (send ?current-argument
                has-next-register))
   (?dst register
         (send ?current-argument
               has-next-register)))
  (long-binary-operation ornot 
                         ?src1 
                         ?src2 
                         ?dst))
(defmethod MAIN::notorl
  ((?src1 register
          (send ?current-argument
                has-next-register))
   (?src2 register
          (send ?current-argument
                has-next-register))
   (?dst register
         (send ?current-argument
               has-next-register)))
  (long-binary-operation notor 
                         ?src1 
                         ?src2 
                         ?dst))
; @todo handle and, or, not specially

(deffunction MAIN::zero-register 
             (?dest) 
             (mov 0 
                  ?dest))
(deffunction MAIN::send-iac 
             (?dest ?src)
             (defpseudo-instruction send-iac
                                    (create$ ?dest
                                             ?src)
                                    (ldconst 0xFF000010 
                                             ?dest)
                                    (synmovq ?dest 
                                             ?src)))

; two instruction combination for comparing and saving the result in a register
(deffunction MAIN::cmpx->reg
             (?src1 ?src2 ?dest ?cmp-op ?op)
             (defpseudo-instruction (sym-cat cmp ?cmp-op ?op)
                                    (create$ ?src1
                                             ?src2
                                             ?dest)
                                    (funcall (sym-cat cmp 
                                                      ?cmp-op)
                                             ?src1
                                             ?src2)
                                    (funcall (sym-cat test ?op)
                                             ?dest)))
(deffunction MAIN::cmpi->reg
             (?src1 ?src2 ?dest ?op)
             (cmpx->reg ?src1
                        ?src2
                        ?dest
                        i
                        ?op))
(deffunction MAIN::cmpo->reg
             (?src1 ?src2 ?dest ?op)
             (cmpx->reg ?src1
                        ?src2
                        ?dest
                        o
                        ?op))
(deffunction MAIN::cmpr->reg
             (?src1 ?src2 ?dest ?op)
             (cmpx->reg ?src1
                        ?src2
                        ?dest
                        r
                        ?op))
(deffunction MAIN::cmprl->reg
             (?src1 ?src2 ?dest ?op)
             (cmpx->reg ?src1
                        ?src2
                        ?dest
                        rl
                        ?op))
(deffunction MAIN::cmpie (?src1 ?src2 ?dest) (cmpi->reg ?src1 ?src2 ?dest e))
(deffunction MAIN::cmpine (?src1 ?src2 ?dest) (cmpi->reg ?src1 ?src2 ?dest ne))
(deffunction MAIN::cmpil (?src1 ?src2 ?dest) (cmpi->reg ?src1 ?src2 ?dest l))
(deffunction MAIN::cmpile (?src1 ?src2 ?dest) (cmpi->reg ?src1 ?src2 ?dest le))
(deffunction MAIN::cmpig (?src1 ?src2 ?dest) (cmpi->reg ?src1 ?src2 ?dest g))
(deffunction MAIN::cmpige (?src1 ?src2 ?dest) (cmpi->reg ?src1 ?src2 ?dest ge))
(deffunction MAIN::cmpio (?src1 ?src2 ?dest) (cmpi->reg ?src1 ?src2 ?dest o))
(deffunction MAIN::cmpino (?src1 ?src2 ?dest) (cmpi->reg ?src1 ?src2 ?dest no))
(deffunction MAIN::cmpoe (?src1 ?src2 ?dest) (cmpo->reg ?src1 ?src2 ?dest e))
(deffunction MAIN::cmpone (?src1 ?src2 ?dest) (cmpo->reg ?src1 ?src2 ?dest ne))
(deffunction MAIN::cmpol (?src1 ?src2 ?dest) (cmpo->reg ?src1 ?src2 ?dest l))
(deffunction MAIN::cmpole (?src1 ?src2 ?dest) (cmpo->reg ?src1 ?src2 ?dest le))
(deffunction MAIN::cmpog (?src1 ?src2 ?dest) (cmpo->reg ?src1 ?src2 ?dest g))
(deffunction MAIN::cmpoge (?src1 ?src2 ?dest) (cmpo->reg ?src1 ?src2 ?dest ge))
(deffunction MAIN::cmpoo (?src1 ?src2 ?dest) (cmpo->reg ?src1 ?src2 ?dest o))
(deffunction MAIN::cmpono (?src1 ?src2 ?dest) (cmpo->reg ?src1 ?src2 ?dest no))
(deffunction MAIN::cmpre (?src1 ?src2 ?dest) (cmpr->reg ?src1 ?src2 ?dest e))
(deffunction MAIN::cmprne (?src1 ?src2 ?dest) (cmpr->reg ?src1 ?src2 ?dest ne))
(deffunction MAIN::cmprl (?src1 ?src2 ?dest) (cmpr->reg ?src1 ?src2 ?dest l))
(deffunction MAIN::cmprle (?src1 ?src2 ?dest) (cmpr->reg ?src1 ?src2 ?dest le))
(deffunction MAIN::cmprg (?src1 ?src2 ?dest) (cmpr->reg ?src1 ?src2 ?dest g))
(deffunction MAIN::cmprge (?src1 ?src2 ?dest) (cmpr->reg ?src1 ?src2 ?dest ge))
(deffunction MAIN::cmpro (?src1 ?src2 ?dest) (cmpr->reg ?src1 ?src2 ?dest o))
(deffunction MAIN::cmprno (?src1 ?src2 ?dest) (cmpr->reg ?src1 ?src2 ?dest no))
(deffunction MAIN::cmprle (?src1 ?src2 ?dest) (cmprl->reg ?src1 ?src2 ?dest e))
(deffunction MAIN::cmprlne (?src1 ?src2 ?dest) (cmprl->reg ?src1 ?src2 ?dest ne))
(deffunction MAIN::cmprll (?src1 ?src2 ?dest) (cmprl->reg ?src1 ?src2 ?dest l))
(deffunction MAIN::cmprlle (?src1 ?src2 ?dest) (cmprl->reg ?src1 ?src2 ?dest le))
(deffunction MAIN::cmprlg (?src1 ?src2 ?dest) (cmprl->reg ?src1 ?src2 ?dest g))
(deffunction MAIN::cmprlge (?src1 ?src2 ?dest) (cmprl->reg ?src1 ?src2 ?dest ge))
(deffunction MAIN::cmprlo (?src1 ?src2 ?dest) (cmprl->reg ?src1 ?src2 ?dest o))
(deffunction MAIN::cmprlno (?src1 ?src2 ?dest) (cmprl->reg ?src1 ?src2 ?dest no))
; we can also do this to cmpstr as well
; 0b010 -> identical strings
; 0b100 -> mismatch where src1's byte is less than src2's byte
; 0b001 -> mismatch where src1's byte is greater than src2's byte

(deffunction MAIN::strings-equal
             (?src1 ?src2 ?len ?dest)
             ; okay we want to see if we got 0b010
             (defpseudo-instruction strings-equal
                                    (create$ ?src1
                                             ?src2
                                             ?len
                                             ?dest)

                                    (cmpstr ?src1 ?src2 ?len)
                                    (teste ?dest)))


; branch not equal to zero integer
(deffunction MAIN::bnezi (?src ?dest) (cmpibne 0 ?src ?dest))
(deffunction MAIN::bezi (?src ?dest) (cmpibe 0 ?src ?dest))
(deffunction MAIN::bgzi (?src ?dest) (cmpibg 0 ?src ?dest))
(deffunction MAIN::bgezi (?src ?dest) (cmpibge 0 ?src ?dest))
(deffunction MAIN::blzi (?src ?dest) (cmpibl 0 ?src ?dest))
(deffunction MAIN::blezi (?src ?dest) (cmpible 0 ?src ?dest))

(deffunction MAIN::clear-msb (?src ?dest) (clrbit 31 ?src ?dest))
(deffunction MAIN::clear-lsb (?src ?dest) (clrbit 0 ?src ?dest))
(deffunction MAIN::set-msb (?src ?dest) (setbit 31 ?src ?dest))
(deffunction MAIN::set-lsb (?src ?dest) (setbit 0 ?src ?dest))


(deffunction MAIN::toggle-msb (?src ?dest) (notbit 31 ?src ?dest))
(deffunction MAIN::toggle-lsb (?src ?dest) (notbit 0 ?src ?dest))
(deffunction MAIN::check-msb (?src) (chkbit 31 ?src))
(deffunction MAIN::check-lsb (?src) (chkbit 0 ?src))
(deffunction MAIN::is-even 
             (?src ?dest)
             (defpseudo-instruction is-even
                                    (create$ ?src
                                             ?dest)
                                    (check-lsb ?src)
                                    (testno ?dest)))
(deffunction MAIN::is-odd
             (?src ?dest)
             (defpseudo-instruction is-odd
                                    (create$ ?src
                                             ?dest)
                                    (check-lsb ?src)
                                    (teste ?dest)))

(deffunction MAIN::leaf-return () (*bx "(g14)"))
(defclass MAIN::function-declaration
  (is-a code-body)
  (slot title 
        (type SYMBOL)
        (visibility public)
        (storage local)
        (default ?NONE))
  (message-handler init after))
(defmessage-handler MAIN::function-declaration init after
                    ()
                    (bind ?self:contents
                          (deflabel ?self:title)
                          ?self:contents))

(defclass MAIN::window-function
  (is-a function-declaration)
  (message-handler init after))
(defmessage-handler MAIN::window-function init after
                    ()
                    (bind ?self:contents
                          ?self:contents
                          (*ret)))

(defclass MAIN::leaf-function
  (is-a function-declaration)
  (message-handler init after))
(defmessage-handler MAIN::leaf-function init after
                    ()
                    (bind ?self:contents
                          ?self:contents
                          (leaf-return)))
(deffunction MAIN::defun
             (?kind ?name $?body)
             (make-instance of ?kind
                            (title ?name)
                            (contents ?body)))

(deffunction MAIN::defun-window (?name $?body) (defun window-function ?name ?body))
(deffunction MAIN::defun-leaf (?name $?body) (defun leaf-function ?name ?body))
