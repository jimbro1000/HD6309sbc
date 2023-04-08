            org     $8000

* Verify operation of cpu
rst_entry   lds     #$7FFF      initialise system stack
            ldu     #$6FFF      initialise user stack
test        lda     #$55
            ldx     #$6000
loop1       sta     $0000, X
            leax    -1, X
            bne     loop1
            lda     #$AA
            ldx     #$6000
loop2       sta     $0000, X
            leax    -1, X
            bne     loop2
            bra     test
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