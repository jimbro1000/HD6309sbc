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
piadataa    EQU     0           ;offset to pia data A register
piacontrola EQU     1           ;offset to pia control A register
piadatab    EQU     2           ;offset to pia data B register
piacontrolb EQU     3           ;offset to pia control B register
piasetdata  EQU     %00000100   ;or mask to set data register
piasetdir   EQU     %11111011   ;and mask to set direction register

keybdshift  EQU     $0149       ;keyboard shift status 0x00 = off 0xff = on
keybdbase   EQU     $0150       ;keyboard scan table
keybdscan   EQU     $0158       ;keyboard rollover table
keybuffer   EQU     $02dd       ;255 character keyboard buffer
                                ;0-1 = buffer head
                                ;2-3 = buffer tail
                                ;4-255 = data
*keyboard matrix conversion without shift and with shift
keymatrix   FCB     $40, $61, $62, $63, $64, $65, $66, $67
            FCB     $68, $69, $6A, $6B, $6C, $6D, $6E, $6F
            FCB     $70, $71, $72, $73, $74, $75, $76, $77
            FCB     $78, $79, $7A, $0A, $5E, $08, $09, $20
            FCB     $30, $31, $32, $33, $34, $35, $36, $37
            FCB     $38, $39, $3A, $3B, $2C, $2D, $2E, $2F
            FCB     $0D, $0C, $03, $00, $00, $00, $00, $00

keymatrix2  FCB     $13, $41, $42, $43, $44, $45, $46, $47
            FCB     $48, $49, $4A, $4B, $4C, $4D, $4E, $4F
            FCB     $50, $51, $52, $53, $54, $55, $56, $57
            FCB     $58, $59, $5A, $7B, $5B, $15, $5D, $20
            FCB     $30, $21, $22, $23, $24, $25, $26, $27
            FCB     $28, $29, $2A, $2B, $3C, $3D, $3E, $3F
            FCB     $0D, $5C, $03, $00, $00, $00, $00, $00

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

* init keyboard
* sets pia0 for keyboard scanning
* clears rollover table
initkbd     PSHS    A,X
            LDA     #$7f
            LDX     pia0base
            JSR     setpiadir
            LDA     #$00
            LDX     pia0base + 2
            JSR     setpiadir
            LDX     keybdbase
clrkbd      STA     ,X+
            CMPX    keybdbase + 7
            BNE     clrkbd
            PULS    A,X,PC

* scan keyboard
* store scan at rollover scan table
* assumes pia0 PIA0 A0-A6 and PIA B0-B7 are correctly configured
scankbd     PSHS    A,B,X
            LDB     #$01
            LDX     keybdbase
scanloop    STB     pia0base + piadataa
            LDA     pia0base + piadatab
            STA     ,X+
            LSLB
            CMPB    #$80
            BNE     scanloop
            PULS    A,B,X,PC

updatekbd   JSR     scankbd
            PSHS    A,B,X
            LDX     keybdbase
            LDA     7,X
            ANDA    #$40
            BEQ     setshift
            LDA     #$ff
setshift    STA     keybdshift
            LDB     #$00
keyloop     LDA     ,X
            EORA    8,X
* convert new keys to characters in buffer
            LDA     ,X
            STA     8,X
            LEAX    1,X
            INCB
            CMPX    keybdbase + 8
            BNE     keyloop
            PULS    A,B,X,PC

* convert bit position to integer (0-7)
* result in A
bitshift    PSHS    B
            LDA     #$00
shiftloop   CMPB    #$01
            BEQ     bitshiftout
            LSRB
            INCA
            BRA     shiftloop
bitshiftout PULS    B,PC

* translate keypress to character
* A = keys pressed on row
* B = column
kbdconv     PSHS    B,X
            PSHU    B
            LDX     #keymatrix
            TST     keybdshift
            BEQ     kbdconvinit
            LDX     #keymatrix2
            PSHU    A
            JSR     bitshift
            LEAX    A,X
            PULU    A
kbdconvinit LDB     #$01
kbdconvloop PSHU    A
            TFR     B,A
            ANDA    1,U
            BNE     nokey
            LDA     ,X
            JSR     addkey
nokey       LSLB
            CMPB    #$80
            BEQ     endkeyconv
            LEAX    8,X
            BRA     kbdconvloop
endkeyconv  LEAU    2,U
            PULS    B,X,PC

* keyboard buffer handler
* A = character to store
* rejects input if buffer is full
addkey      PSHS    X
            LDX     keybuffer
            CMPX    keybuffer + 2
            BEQ     failaddkey
            LEAX    1,X
            CMPX    #keybuffer + 256
            BNE     storechar
            LDX     #keybuffer + 4
storechar   STA     ,X
            STX     keybuffer
failaddkey  PULS    X,PC

* remove key from buffer
* A = character pulled
* A == 0 on no value found
pullkey     PSHS    X
            LDA     #$00
            LDX     keybuffer + 2
            CMPX    keybuffer
            BEQ     endpullkey
            LDA     ,X+
            CMPX    #keybuffer + 256
            BNE     savetail
            LDX     #keybuffer + 4
savetail    STX     keybuffer + 2
endpullkey  PULS    X,PC

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