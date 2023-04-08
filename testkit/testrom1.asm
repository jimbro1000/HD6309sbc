            org     $C000

* Verify operation of cpu
rst_entry   lds     #$7FFF      ;initialise system stack
            ldu     #$6FFF      ;initialise user stack
            lda     #$00
            tfr     A,DP        ;set DP to 0

test        lda     #$55
            ldx     #$6000

loop1       sta     $0000, X    ;fill $0000 to $5FFF
            leax    -1, X
            bne     loop1
            lda     #$AA
            ldx     #$6000

loop2       sta     $0000, X    ;fill $0000 to $5FFF
            leax    -1, X
            bne     loop2
            bra     test        ;repeat

setpage     sta     $FF25       ;set memory page
            rst_entry

pagemode    cmpa    #$00        ;enable/disable paging
            bne     enablepage
            sta     $FF24       ;disable
            rts
enablepage  pshu    A           ;enable
            lda     #$01
            sta     $FF24
            pulu    A
            rts

shadow      pshu    A,B,X,Y     ;shadow rom into high mem
* copy rom from C000 to 4000
            ldx     #$C000
            ldy     #$4000
            jsr     blockcopy
* enable page mode
            lda     #$01
            jsr     pagemode
* copy rom from 4000 back to C000
            ldx     #$4000
            ldy     #$C000
            jsr     blockcopy
            pulu    A,B,X,Y
            rts
* copy block from X to Y
blockcopy   rts

nmi_entry   rti
swi_entry   rti
irq_entry   rti
firq_entry  rti
swi2_entry  rti
swi3_entry  rti

* Boot and interrupt vectors
            org     $FFF0
reserved    fdb     $0000
swi3        fdb     swi3_entry
swi2        fdb     swi2_entry
firq        fdb     firq_entry
irq         fdb     irq_entry
swi         fdb     swi_entry
nmi         fdb     nmi_entry
reset       fdb     rst_entry