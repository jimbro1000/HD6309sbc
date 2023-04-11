            ORG     $C000

controlreg  EQU     $0076       ;zero page copy of control register 1
mempage     EQU     $0077       ;zero page copy of control register 2 (memory paging)
MDREG       EQU     $00E6       ;zero page copy of MD register
memreg1     EQU     $FF24       ;control register 1
memreg2     EQU     $FF25       ;control register 2
pia0base    EQU     $FF00       ;PIA registers
pia1base    EQU     $FF04
pia2base    EQU     $FF08
aciabase    EQU     $FF0C       ;ACIA registers
intpolltab  EQU     $0100       ;table of mapped registers to poll on interrupt 38 bytes max
                                ;terminates with a $0000 word

* Verify operation of cpu
rst_entry   LDS     #$7FFF      ;initialise system stack
            LDU     #$77FF      ;initialise user stack
            LDA     #$00
            TFR     A,DP        ;set DP to 0
            STA     controlreg  ;initialise control register copy with 0
            STA     memreg1     ;set control register to 0
            STA     mempage     ;set memory page copy to 0
            STA     memreg2     ;set memory page to 0
            STA     intpolltab  ;empty interrupt poll table
            STA     intpolltab+1
            STA     MDREG       ;initialise copy of ME register
            LDA     #$01
            JSR     SETPMD
            JSR     unmaskint   ;enable interrupts
            
test        LDB     #$02
teststart   LDA     #$55
            LDX     #$6000

loop1       STA     $0000, X    ;fill $0000 to $5FFF
            LEAX    -1, X
            BNE     loop1
            LDA     #$AA
            LDX     #$6000

loop2       STA     $0000, X    ;fill $0000 to $5FFF
            LEAX    -1, X
            BNE     loop2
            STB     controlreg
            EORB    #$02
            BRA     teststart   ;repeat

* set paged memory page
* A = page number
setpage     STA     memreg2     ;set memory page
            STA     mempage
            RTS

* Control memory paging mode
* Depending on register A
* A = 0 32K ram, 32K rom
* A <> 0 32K ram, 16K page, 16k shadow rom

* Assumes direct page location control reg 
* contains a copy of memreg1 and direct page
* location mempage contains a copy of memreg2

* Assumes extended memory is available
pagemode    CMPA    #$00        ;enable/disable paging
            BNE     enablepage
disablepage LDA     #$FE
            ANDA    controlreg
            STA     controlreg
            STA     memreg1     ;disable
            RTS
enablepage  PSHS    A           ;enable
            LDA     #$01
            ORA     controlreg
            STA     controlreg
            STA     memreg1
            PULS    A,PC

shadow      PSHS    A,B,X,Y     ;shadow rom into high mem
* disable interrupts
            JSR     maskint
* copy interrupt vectors first as precaution
            LDX     #$FFF0
            LDY     #$4000
            LDW     #$0010
            JSR     blockcopy
            LDA     #$01
            JSR     pagemode
            LDX     #$4000
            LDY     #$FFF0
            LDW     #$0010
            JSR     blockcopy
            LDA     #$00
            JSR     pagemode
* copy rom from C000 to 4000
            LDX     #$C000
            LDY     #$4000
            LDW     #$3F00
            JSR     blockcopy
* enable page mode
            LDA     #$01
            JSR     pagemode
* copy rom from 4000 back to C000
            LDX     #$4000
            LDY     #$C000
            LDW     #$3F00
            JSR     blockcopy
* re-enable interrupts
            JSR     unmaskint
            PULS    A,B,X,Y,PC

* swap memory page
* A = page number
* tests if memory paging is enabled
* nb: this code must not be held
*     in paged memory to avoid
*     unexpected behaviour on return
pageswap    PSHS    B
            LDB     controlreg
            ANDB    #$01
            BEQ     swapret
            STA     mempage
            STA     memreg2
swapret     PULS    B,PC

* copy rom block from X to Y
* X = start address
* Y = destination address
* W = length
blockcopy   TFM     X+,Y+           ; 6309 specific implementation - will fail on a 6809
            PULS    PC

* disable interrupts (ignores existing state)
maskint     ORCC    #%01010000
            RTS

* enable interrupts (ignores prior state)
unmaskint   ANDCC   #%10101111
            RTS

* poll external interrupts
* check interrupt poll table
* each entry is 6 bytes:
*  byte 0/1 is the address to test
*  byte 2 is the test mask
*  byte 3 is control over positive or negative test
*  byte 4/5 is the jump address
* halts on first zero word entry at 0/1
* while flexible and extendable it is also slow
* rapid interrupts will becoming blocking
pollint     LDY     intpolltab  ;point Y at start of table
pollloop    LDX     ,Y++        ;load first register address
            BNE     scanpoll    ;proceed if non-zero value found
            RTI                 ;release from poll without action
scanpoll    LDA     ,Y+         ;grab test mask
            LDB     ,Y+         ;grab test type
            BNE     pollpos     ;select positive of negative test
pollneg     ANDA    ,X          ;mask address at X
            BEQ     vectorpoll  ;if 0 result use vector
pollnext    LEAY    2,Y         ;on negative result skip vector
            BRA     pollloop    ;next entry
pollpos     ANDA    ,X          ;mask address at X
            BNE     vectorpoll  ;if non0 result use vector
            BRA     pollnext    ;next entry
vectorpoll  JMP     ,Y          ;vector to identified handler

* Change processor to Emulation Mode or Native Mode,
* depending on value in Register A
* A=0 Emulation Mode
* A<>0 Native Mode
*
* Assumes direct page location MDREG contains an
* accurate image of the MD register contents (The
* program must initialize MDREG to $00 at start-up).
*
* Since LDMD accepts only an immediate operand, we
* push the appropriate LDMD / RTS sequence onto the
* stack and call it as a subroutine.
* Works for 6309 only.
SETPMD      PSHS    X,D,CC      ;Save registers
            ORCC    #$50        ;Make operation indivisible
            LDB     MDREG       ;Get mode register image
            ANDB    #$FE        ; strip mode selection bit (Emulation)
            TSTA
            BEQ     SETMD2      ;Skip next part if want Emulation
            ORB     #$01        ;Set Native mode bit (INCB lacks clarity)
SETMD2      STB     MDREG       ;B has right value - update register image
            LDA     #$39        ;RTS op-code
            EXG     B,A         ;Now A = LDMD's immed. operand, B = RTS
            LDX     #$103D      ;X has LDMD's 2-byte op-code
            EXG     D,X         ;Now D:X = 10 3D <value> 39
            PSHS    X,D         ;Put subroutine on stack
            JSR     ,S          ;Call subroutine, setting mode
            LEAS    4,S         ; throw away subroutine when done.
            PULS    CC,D,X,PC   ; and return to caller.

nmi_entry   RTI
swi_entry   RTI
irq_entry   JMP     pollint
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
reset       FDB     rst_entry