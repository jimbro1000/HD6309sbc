            ORG     $C000
* Memory map values should be compatible with Dragon and CoCo
controlreg  EQU     $0076       ;zero page copy of control register 1
mempage     EQU     $0077       ;zero page copy of control register 2 (memory paging)
<D.MDREG    EQU     $00E6       ;zero page copy of MD register
memreg1     EQU     $FF24       ;control register 1
memreg2     EQU     $FF25       ;control register 2

* Verify operation of cpu
rst_entry   LDS     #$7FFF      ;initialise system stack
            LDU     #$77FF      ;initialise user stack
            LDA     #$00
            TFR     A,DP        ;set DP to 0
            STA     <D.MDREG    ;initialise copy of ME register
            LDA     #$01
            JSR     SETPMD
            
test        LDA     #$55
            LDX     #$6000

loop1       STA     $0000, X    ;fill $0000 to $5FFF
            LEAX    -1, X
            BNE     loop1
            LDA     #$AA
            LDX     #$6000

loop2       STA     $0000, X    ;fill $0000 to $5FFF
            LEAX    -1, X
            BNE     loop2
            BRA     test        ;repeat

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
            LDA     #$FE
            ANDA    controlreg
            STA     controlreg
            STA     memreg1     ;disable
            RTS
enablepage  PSHU    A           ;enable
            LDA     #$01
            ORA     controlreg
            STA     controlreg
            STA     memreg1
            PULU    A,PC

shadow      PSHU    A,B,X,Y     ;shadow rom into high mem
* copy rom from C000 to 4000
            LDX     #$C000
            LDY     #$4000
            JSR     blockcopy
* enable page mode
            LDA     #$01
            JSR     pagemode
* copy rom from 4000 back to C000
            LDX     #$4000
            LDY     #$C000
            JSR     blockcopy
            PULU    A,B,X,Y,PC

* copy 8K block from X to Y
* X = start address
* Y = destination address
blockcopy   PSHU    W
            LDW     #$2000
            TFM     X+,Y+
            PULU    W,PC

* Change processor to Emulation Mode or Native Mode,
* depending on value in Register A
* A=0 Emulation Mode
* A<>0 Native Mode
*
* Assumes direct page location <D.MDREG contains an
* accurate image of the MD register contents (The
* program must initialize <D.MDREG to $00 at start-up).
*
* Since LDMD accepts only an immediate operand, we
* push the appropriate LDMD / RTS sequence onto the
* stack and call it as a subroutine.
* Works for 6309 only.
SETPMD      PSHS    X,D,CC      ;Save registers
            ORCC    #$50        ;Make operation indivisible
            LDB     <D.MDREG    ;Get mode register image
            ANDB    #$FE        ; strip mode selection bit (Emulation)
            TSTA
            BEQ     SETMD2      ;Skip next part if want Emulation
            ORB     #$01        ;Set Native mode bit (INCB lacks clarity)
SETMD2      STB     <D.MDREG    ;B has right value — update register image
            LDA     #$39        ;RTS op-code
            EXG     B,A         ;Now A = LDMD’s immed. operand, B = RTS
            LDX     #$103D      ;X has LDMD’s 2-byte op-code
            EXG     D,X         ;Now D:X = 10 3D <value> 39
            PSHS    X,D         ;Put subroutine on stack
            JSR     ,S          ;Call subroutine, setting mode
            LEAS    4,S         ; throw away subroutine when done.
            PULS    CC,D,X,PC   ; and return to caller.

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
reset       FDB     rst_entry