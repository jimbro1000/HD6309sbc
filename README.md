# HD6309 single board computer

This project is a somewhat hopeful and probably overly grandiose
attempt to create an expandable single board computer utilising
the Hitachi HD63C09 processor at its core (the ultimate version
of the ultimate 8-bit processor, allegedly)

## Status

The current version is built and under test. So far the board
is working with a single point of note. The reset line, when
released it not reaching a full high value. It is enough that
the processor treats it as released but I believe this is a
borderline condition which could result in spurious resets

## Architecture

The board is very, very basic and provides only clock, cpu and
memory. All other functions are handled off-board via an
expansion port

The CPU is clocked at 3MHz but each memory access is cycle is
only half of the CPU clock duration allowing the memory to be
shared by two CPUs operating at 3MHz

## Memory

The board operates in two modes, either a 50:50 split on ram
and rom, or a 64KB ram model with up to 4MB of paging (using 16k
pages, yes that's a lot of pages)

## Memory Map

### RAM/ROM model

$0000 - $7FFF - shared ram  
$8000 - $FEFF - rom  
$FF00 - $FFEF - mapped hardware registers  
$FFF0 - $FFFF - interrupt vectors  

### Paged RAM model (requires memory board)

$0000 - $7FFF - shared ram  
$8000 - $BFFF - paged ram  
$C000 - $FFEF - private ram  
$FF00 - $FFEF - mapped hardware registers  
$FFF0 - $FFFF - interrupt vectors  

## Hardware registers

The switching of memory models is handled by writing to bit 0
of address $FF24. Writing a low bit sets the map to RAM/ROM,
writing a high bit sets the map to paged RAM

Bits 1 to 7 are unused by default but are exposed over the
expansion bus to provide switching to other devices

For future use all addresses from $FF00 to $FFEF are reserved
for memory mapped registers. The "MAPPED_MEM" line (pin 27) on
the expansion bus is high for all addresses in this range. This
should help to keep logic on expansion boards down where a new
register is added

For expansion purposes the following addresses are fixed and
should not be reused:

$FF00:$FF03 - PIA 1
$FF04:$FF07 - PIA 2
$FF08:$FF0B - PIA 3
$FF0C:$FF0F - ACIA
$FF10:$FF1F - Video Co-Pro registers
$FF24       - Hardware enable lines (Q0 = memory model, Q1-Q7 unused)
$FF25:$FF27 - reserved for additional enable lines
$FF40:$FF48 - Floppy Disc Controller
$FF50:$FF5F - Hard Drive / MMC Controller

## Expansion port

The processor board exposes all of the CPU and timing control
lines over a 60 pin IO port to allow other devices to interface
with the processor

Intended components are audio output, video co-processor,
general peripheral IO and (slow) serial IO

## Associated designs

* Backplane <https://github.com/jimbro1000/SBCBackplane>
* Memory Board <https://github.com/jimbro1000/SBCMemoryBoard>
* Peripheral Adapter <https://github.com/jimbro1000/PIA-Module>
* VGA Board <https://github.com/jimbro1000/PicoVideoModule>
* 6847Pi <https://github.com/jimbro1000/6847Pi>
* ACIA Board <https://github.com/jimbro1000/ACIA-Module>
