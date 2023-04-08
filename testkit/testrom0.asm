            org     $8000
* Verify operation of cpu
start       lda     #$55
            ldx     #1000
loop1       sta     $2000, X
            leax    -1, X
            bne     loop1
            lda     #$AA
            ldx     #1000
loop2       sta     $2000, X
            leax    -1, X
            bne     loop2
            jmp     start
* Boot and interrupt vectors
            org     $FFF0
reserved    fdb     $0000
swi3        fdb     start
swi2        fdb     start
firq        fdb     start
irq         fdb     start
swi         fdb     start
nmi         fdb     start
reset       fdb     start