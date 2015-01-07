;-----------------------------------------------------------------------------
; DOS 1.0 Boot Sector (disk image MD5 73c919cecadf002a7124b7e8bfe3b5ba)
;   http://www.pagetable.com/
;-----------------------------------------------------------------------------

                org 0x7C00

                jmp     short start

;-----------------------------------------------------------------------------

os_numsectors   dw 20                   ; how many sectors to read
os_offset       dw 0                    ; offset to load code into
os_segment      dw 0x60                 ; segment to load code into

                db " 7-May-81",0        ; timestamp
                times 31 db 0           ; padding

;-----------------------------------------------------------------------------

start           cli
                mov     ax, cs
                mov     ds, ax          ; DS := CS
                mov     dx, 0
                mov     ss, dx          ; SS := 0000
                mov     sp, 0x7C00      ; stack below code
                sti
                mov     ax, [os_segment]
                mov     ds, ax
                mov     es, ax          ; ES := DS := where to load DOS
                mov     dx, 0
                mov     ax, dx
                int     0x13            ; reset drive 0
                jc      disk_error
again           call    check_sys_files ; check for presence of IBMDOS/IBMBIO
                jc      again           ; not found, try another disk
                mov     cx, [cs:os_numsectors]
                push    cx              ; remaining sectors
                mov     bx, 0
                xor     dx, dx          ; drive 0, head 0
                mov     cx, 8           ; track 0, sector 8
                mov     si, 1           ; read 1 sector in first found
                push    si
                mov     al, 1           ; 1 sector
read_loop       mov     ah, 2
                int     0x13            ; read sector(s)
                jc      disk_error
                pop     si              ; sectors read
                pop     ax              ; remaining sectors
                call    add_si_sectors  ; bx += si*512
                sub     ax, si          ; remaining -= read
                jz      done                ; none left
                inc     ch              ; next track
                mov     cl, 1           ; start at sector 1
                mov     si, 8           ; read up to 8 sectors
                cmp     ax, si          ; how many are left to read?
                jae     at_least_8_left ; at least 8
                mov     si, ax          ; only read remaining amount
                jmp     short skip
at_least_8_left xchg    ax, si          ; read 8 sectors this time
skip            push    si              ; number of remaining sectors
                push    ax              ; number of sectors to read this time
                jmp     read_loop       ; next read
done            jmp     far [cs:os_offset]; jump to IBMBIO.COM

disk_error      mov     si, FAILURE     ; string to print
                mov     ax, rom_basic   ; put return address of "int 18" code
                push    ax              ; onto stack

;-----------------------------------------------------------------------------
; print zero-terminated string pointed to by DS:SI
;-----------------------------------------------------------------------------

print           xor     bh, bh          ; XXX unnecessary
print_loop      lodsb
                and     al, 0x7F        ; clear bit 7 XXX why is it set?
                jz      ret0            ; zero-termination
                push    si
                mov     ah, 0x0E
                mov     bx, 7           ; light grey, text page 0
                int     0x10            ; write character
                pop     si
                jmp     print_loop
ret0            retn

;-----------------------------------------------------------------------------
; test for IBMBIO.COM and IBMDOS.COM in the first two directory entries
;-----------------------------------------------------------------------------

check_sys_files mov     bx, 0           ; read to address 0 in the DOS segment
                mov     cx, 4           ; track 0, sector 4
                mov     ax, 0x0201
                int     0x13            ; read 1 sector
                push    ds
                jc      non_system_disk ; error case
                mov     ax, cs
                mov     ds, ax          ; DS := CS
                mov     di, 0
                mov     cx, 11          ; convert 11 bytes of first two
to_lower        or      byte [es:di], 0x20; directory entries to lowercase
                or      byte [es:di+0x20], 0x20
                nop                     ; XXX original assembler wasted a byte
                inc     di
                loop    to_lower
                mov     di, 0           ; first entry
                mov     si, IBMBIO_COM
                mov     cx, 11
                cld
                rep cmpsb               ; compare first entry with IBMBIO.COM
                jnz     non_system_disk
                mov     di, 0x20        ; second entry
                mov     si, IBMDOS_COM
                mov     cx, 11
                rep cmpsb               ; compare second entry with IBMDOS.COM
                jnz     non_system_disk
                pop     ds
                retn                    ; return with carry clear
non_system_disk mov     si, NON_SYSTEM_DISK
                call    print
                mov     ah, 0
                int     0x16            ; wait for key
                pop     ds
                stc
                retn                    ; return with carry set

;-----------------------------------------------------------------------------

NON_SYSTEM_DISK db 13,10
                db "Non-System disk or disk erro",'r'+0x80
                db 13,10
                db "Replace and strike any key when read",'y'+0x80
                db  13,10,0

;-----------------------------------------------------------------------------

rom_basic       int     0x18                ; ROM BASIC

;-----------------------------------------------------------------------------

FAILURE         db 13,10
                db "Disk Boot failur",'e'+0x80
                db 13,10,0

;-----------------------------------------------------------------------------

add_si_sectors  push    ax              ; bx += si*512
                push    dx
                mov     ax, si
                mov     di, 512
                mul     di
                add     bx, ax
                pop     dx
                pop     ax
                retn

;-----------------------------------------------------------------------------

                db "Robert O'Rear "

IBMBIO_COM      db "ibmbio  com"
                db 0xB0                 ; XXX unused
IBMDOS_COM      db "ibmdos  com"
                db 0xB0, 0xC9           ; XXX unused

;-----------------------------------------------------------------------------

                times 512-($-$$) db 0

;-----------------------------------------------------------------------------
