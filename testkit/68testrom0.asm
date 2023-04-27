            ORG     $8000
            fdb     $00         ;create empty block $8000-$BFFF

            ORG     $C000

pia0base    EQU     $FF00       ;PIA registers
pia1base    EQU     $FF04
pia2base    EQU     $FF08
piadataa    EQU     0           ;offset to pia data A register
piacontrola EQU     1           ;offset to pia control A register
piadatab    EQU     2           ;offset to pia data B register
piacontrolb EQU     3           ;offset to pia control B register
piasetdata  EQU     %00000100   ;or mask to set data register
piasetdir   EQU     %11111011   ;and mask to set direction register
pia0datab   EQU     $FF02

test        CLRA                    ; direction register 0=output on all bits
            LDX     #pia0datab      ; use pia0 register B
            JSR     setpiadir       ; call set direction
* exercise PIA0 register B for output
* cycle through binary values but delay between values
* target CPU cycles is 3MHz
* using leax to count with nops to delay
*  nop = 2 cycles
*  leax = 4 cycles
*  bne = 3 cycles
*  ldx = 3 cycles
teststart   LDA     #$55
loop        LDB     #$0A
iloop       LDX     #$2710      ; 10000
* 200 loops per millisecond -> 10000 loops = 0.05 seconds
iloop1      NOP                 ; 2 cycles
            NOP                 ; 2 cycles
            NOP                 ; 2 cycles
            NOP                 ; 2 cycles
            LEAX    -1,X        ; 4 cycles
            BNE     iloop1      ; 3 cycles = 15 cycles per loop == 5 uSec
            DECB
            BNE     iloop
            STA     pia0datab   ;write a to PIA register B data
            EORA    #$FF
            BRA     loop

* set data direction on pia register
* retains condition of pia control
* A = data direction bits
* X = pia register base
setpiadir   PSHS    B
            LDB     piacontrola, X
            PSHS    B
            ANDB    piasetdir
            STB     piacontrola, X
            STA     piadataa, X
            PULS    B
            STB     piacontrola, X
            PULS    B,PC

nmi_entry   RTI
swi_entry   RTI
irq_entry   RTI
firq_entry  RTI
swi2_entry  RTI
swi3_entry  RTI
div0_entry  RTI

* Boot and interrupt vectors
            ORG     $FFF0
div0        FDB     div0_entry  ;specific to 6309 native mode
swi3        FDB     swi3_entry
swi2        FDB     swi2_entry
firq        FDB     firq_entry
irq         FDB     irq_entry
swi         FDB     swi_entry
nmi         FDB     nmi_entry
reset       FDB     test