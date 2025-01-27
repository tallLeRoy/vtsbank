; vtsbank.p8   compile a bankable VERA FX Affine tile set rotation an scaling feature

; %launcher none
%address  $A000
%memtop   $C000
%output   library
;%output   raw
;%option   no_sysinit

;%zeropage dontuse
%encoding iso

%import syslib
%import floats
%import diskio
%import math

main {
    sub start() {
    }
}

vtsbank {
    %option force_output

    const uword SIZE_WORD_TRIG_TABLE_ENTRY = 4 * sys.SIZEOF_WORD

    ; word_trig_table entry offsets
    const ubyte TBL_COSINE  = 0
    const ubyte TBL_SINE    = 2
    const ubyte TBL_64_XPOS = 4
    const ubyte TBL_64_YPOS = 6
    
    const uword SIZE_WORD_TRIG_TABLE = 360 * SIZE_WORD_TRIG_TABLE_ENTRY

    alias temp    = cx16.r12
    alias wangle  = cx16.r13s

    sub affine() {
        const ubyte AFF_CONTEXT_SIZE      = 13  ; 13 bytes for each context
            const ubyte AFF_SCALE             = 0   ; float
            const ubyte AFF_VADDR             = 5
            const ubyte AFF_SPRITE_SIZE       = 6
            const ubyte AFF_SPRITE_DATA_BANK  = 7
            const ubyte AFF_SPRITE_DATA_VADDR = 8   ; uword 
            const ubyte AFF_FX_CTRL           = 10
            const ubyte AFF_4BIT              = 11
            const ubyte AFF_TILE_MAP          = 12  ; ubyte
    
        const ubyte AFF_CREATE_OK                     = 0
        const ubyte AFF_CREATE_AFFINE_TILE_ADDR_ERROR = 1
        const ubyte AFF_CREATE_SPRITE_ADDR_ERROR      = 2
        const ubyte AFF_CREATE_SPRITE_SIZE_ERROR      = 3
        const ubyte AFF_CREATE_TILE_OFFSET_ERROR      = 4

        const float CORNER_ANGLE = (floats.PI * 5.0) / 4.0

        float  theta

        uword this
        float distance_to_corner 
        word  x_position
        word  y_position
        word  x_inc
        word  y_inc
        ubyte position_offset
        bool  is_four_bit
        float scale = 1.0
        bool  use_scale = false
        bool  is_16 = false
        bool  int_source = false

        ubyte row_count
        ubyte byte_count
        ubyte cache_count
        ubyte helper 
        ubyte increment
        ubyte h_increment
        ubyte count
        uword data0_reset

        sub create_context(uword context_addr @ R3,  ; ubyte array[AFF_CONTEXT_SIZE] for context
                ; vram address for the AFFINE_TILES
                ubyte affine_tile_bank @ R4, uword affine_tile_addr @ R5,
                ;vram destination address for transformed sprite 
                ubyte sprite_data_bank @ R6, uword sprite_data_addr @ R7, 
                ; 16 or 64 for 16x16 or 64x64 pixel sprite
                ubyte sprite_size @ R8,
                ; four_bit true for 16 color, false for 256 color sprite
                bool  four_bit @ R9, 
                ; tile offset from affine_tile_bank : affine_tile_addr
                uword  tile_offset @ R10 ) -> ubyte {

            ; calculate the FX Tile Base Address and save it in the context 
            if affine_tile_addr & %0000_0111_1111_1111 != 0 {
                ; a valid affine address cannot use the lowest 10 bits 
                return AFF_CREATE_AFFINE_TILE_ADDR_ERROR
            }
            temp = affine_tile_addr >> 9
            if affine_tile_bank & %0000_0001 == 1 {
                temp |= $0080     ; set the bank
            }
            poke(context_addr + AFF_VADDR, lsb(temp) | %0000_0010 ) ; Affine clip enable

            ; save the sprite data address
            if sprite_data_addr & $000F != 0 {
               return AFF_CREATE_SPRITE_ADDR_ERROR 
            }
            poke(context_addr + AFF_SPRITE_DATA_BANK, sprite_data_bank & %0000_0001 ) 
            pokew(context_addr + AFF_SPRITE_DATA_VADDR, sprite_data_addr)  ; uword value

            ; cache write, cache fill, affine helper mode
            poke(context_addr + AFF_FX_CTRL, (if four_bit %0000_0100 else 0) |  %0110_0011 )

            ; save the sprite size
            if sprite_size in [16,64] {
                poke(context_addr + AFF_SPRITE_SIZE, sprite_size)
            } else {
                return AFF_CREATE_SPRITE_SIZE_ERROR
            }

            ; save four bit flag
            poke(context_addr + AFF_4BIT, four_bit as ubyte)

            ; convert the tile offset into a tile map number
            tile_offset >>= 5                  ; 16 color mode ( 32 bytes per tile )
            if four_bit {
                if tile_offset > 255 - 16 {                
                    return AFF_CREATE_TILE_OFFSET_ERROR
                }
            } else {
                tile_offset >>= 1            ; 256 color mode ( 64 bytes per tile )
                if tile_offset > 255 - 64 {
                    return AFF_CREATE_TILE_OFFSET_ERROR
                }
            }

            ; could be too big for a map number
            if msb(tile_offset) == 0 {
                poke(context_addr + AFF_TILE_MAP, lsb(tile_offset) )
            } else {
                return AFF_CREATE_TILE_OFFSET_ERROR
            }

            ; all contexts start with a scale of 1.0 original size
            pokef(context_addr + AFF_SCALE, 1.0)

            return AFF_CREATE_OK
        }

        const ubyte POSITION_OFFSET_64 = 32
        const ubyte POSITION_OFFSET_16 = 8
        ; DISTANCE_TO_CORNER = SQUARE_ROOT_2 * position_offset
        const float DISTANCE_TO_CORNER_64 = 1.414214 * POSITION_OFFSET_64
        const float DISTANCE_TO_CORNER_16 = 1.414214 * POSITION_OFFSET_16

        sub select(uword context @ R3) {
            this = context    ; save the context for future

            ; set the pre-computed FX Tile Base Address 
            cx16.VERA_CTRL = %0000_0100  ; DCSEL=2 ADDRSEL=0
            cx16.VERA_FX_TILEBASE = peek(context + AFF_VADDR)

            if peek(context + AFF_SPRITE_SIZE) == 16 {
                is_16 = true
            } else {
                is_16 = false
            }

            ; set the distance to the corner, position_offset and row_count
            if is_16 {
                distance_to_corner = DISTANCE_TO_CORNER_16
                position_offset = POSITION_OFFSET_16
                row_count = 16
            } else {
                distance_to_corner = DISTANCE_TO_CORNER_64
                position_offset = POSITION_OFFSET_64
                row_count = 64
            }

            is_four_bit = peek(context + AFF_4BIT) as bool

            ; set up vars for rotation and shear
            byte_count = row_count 
            helper = %0000_0011
            increment = %0001_0000      ; increment 1, 8 bit, DATA0
            if is_four_bit {
                byte_count = row_count >> 1
                helper = %0000_0111
                increment = %0000_0100  ; increment 0, four bit, DATA0
            }
            cache_count = byte_count >> 2
            data0_reset = ((byte_count as uword) * (row_count as uword)) - 1  ; for horizontal shear

            set_map_addr_size()
            set_map(peek(context + AFF_TILE_MAP))

            ; get the scale from the context
            scale = peekf(context + AFF_SCALE)
            if scale == 1.0 {           ; faster version of rotate
                use_scale = false
            } else {
                use_scale = true
            }
        }

        sub set_map_addr_size() {
            ; set the FX Map Base Address
            cx16.VERA_CTRL = %0000_0100  ; DCSEL=2 ADDRSEL=0
            if is_16 {
                cx16.VERA_FX_MAPBASE = $FC ; $1F800 >> 9 size 16 - just above the Charset
            } else {
                cx16.VERA_FX_MAPBASE = $FC | $01 ; $1F800 >> 9 size 64 - just above the Charset
            }
        }

        sub set_map(ubyte origin) {
            ; populate the map
            ubyte map_entry = origin
            cx16.VERA_CTRL = 0
            cx16.VERA_ADDR = $F800
            cx16.VERA_ADDR_H = $0000_0001 | %0001_0000 ; bank 1 , increment DATA0 by 1
            repeat 64 {
                cx16.VERA_DATA0 = map_entry
                map_entry++
            }
        }

        asmsub set_scale(float fscale @ FAC1) {
            %asm{{
                ldx  #<p8b_vtsbank.p8s_affine.p8v_scale
                ldy  #>p8b_vtsbank.p8s_affine.p8v_scale
                jsr  floats.MOVMF
                jmp  p8b_vtsbank.p8s_affine.p8s_set_scale_internal  
            }}
        }

        sub set_scale_internal() {
            pokef(this + AFF_SCALE, scale)
            if scale == 1.0 {
                use_scale = false
            } else {
                use_scale = true
            }
        }

        ; sheer to a given horizontal integer extent
        sub shear_h(word extent) {
            alias context = cx16.r3
            context = this

            when byte_count { ; count of a horizontal line
                8  -> h_increment = increment | %0100_0000  ; increment 8
                16 -> h_increment = increment | %0101_0000  ; increment 16
                32 -> h_increment = increment | %0110_0000  ; increment 32
                64 -> h_increment = increment | %0111_0000  ; increment 64
            }

            ; four bit horizontal shear is quite different, gets its own sub
            if is_four_bit {
                shear_h_4(extent)
                return
            }

            cx16.VERA_CTRL = %0000_0100 ; DCSEL 2, ADDRSEL 0
            cx16.VERA_FX_CTRL = helper  ; affine helper with four bit set or reset

            cx16.VERA_CTRL = %0000_0110 ; DCSEL 3, ADDRSEL 0

            cx16.VERA_FX_X_INCR = extent & $7FFF
            cx16.VERA_FX_Y_INCR = $0200

            cx16.VERA_CTRL = %0000_1000 ; DCSEL 4, ADDRSEL 0

            ; set the destination for the shear
            cx16.VERA_ADDR   = peekw(context + AFF_SPRITE_DATA_VADDR)
            cx16.VERA_ADDR_H = peek(context + AFF_SPRITE_DATA_BANK) | h_increment  ; based on byte_count

            count = 0
            do {
                cx16.VERA_FX_X_POS = count      ; move left to right after each scan
                cx16.VERA_FX_Y_POS = 0          ; move down the tile set for each scan

                sys.pushw(cx16.VERA_ADDR)       ; we want to retore the full ADDR each row
                sys.push(cx16.VERA_ADDR_H)

                repeat row_count {              ; copy the data
                    cx16.VERA_DATA0 = cx16.VERA_DATA1
                }

                ; scan the next column
                cx16.VERA_ADDR_H = sys.pop()
                cx16.VERA_ADDR = sys.popw() + 1
            
                count++                       ; change the position  
            } until (count == row_count)

            ; clean up the regs
            cx16.VERA_CTRL = %0000_0100     ; DCSEL 2, ADDRSEL 0
            cx16.VERA_FX_CTRL = 0
            cx16.VERA_CTRL = 0
        }

        sub shear_h_4(word extent) {
            alias context = cx16.r3
            context = this
            ubyte col_count = row_count >> 1

            ; set the destination buffer for the shear
            cx16.VERA_CTRL   = 0
            cx16.VERA_ADDR   = peekw(context + AFF_SPRITE_DATA_VADDR)
            h_increment &= %1111_1000 ; turn off nibble increment and address
            cx16.VERA_ADDR_H = peek(context + AFF_SPRITE_DATA_BANK) | h_increment ; based on byte_count

            cx16.VERA_CTRL = %0000_0100 ; DCSEL 2, ADDRSEL 0
            cx16.VERA_FX_CTRL = helper  ; affine helper with four bit set or reset

            cx16.VERA_CTRL = %0000_0110 ; DCSEL 3, ADDRSEL 0

            cx16.VERA_FX_X_INCR = extent & $7FFF
            cx16.VERA_FX_Y_INCR = $0200 

            ; begin with the even nibbles
            repeat 2 {
                ; scan the nibble in the column
                count = 0
                do {
                    cx16.VERA_CTRL = %0000_1000 ; DCSEL 4, ADDRSEL 0

                    cx16.VERA_FX_X_POS = (count << 1) as uword  ; move left to right after each scan
                    cx16.VERA_FX_Y_POS = 0               ; move down the tile set for each scan

                    sys.pushw(cx16.VERA_ADDR)       ; we want to retore the full ADDR each row
                    sys.push(cx16.VERA_ADDR_H)

                    ; scan the even nibbles in the column
                    repeat row_count {              ; copy the data
                        cx16.VERA_DATA0 = cx16.VERA_DATA1
                    }
    
                    ; scan the next column
                    cx16.VERA_ADDR_H = sys.pop()
                    cx16.VERA_ADDR = sys.popw() + 1

                    count++                       ; change the position  
                } until count == col_count

                ; now pick up the odd nibbles in the column
                cx16.VERA_ADDR_H |= %0000_0010 ; set the nibble address odd
                cx16.VERA_ADDR -= col_count
            }

            ; clean up the regs
            cx16.VERA_CTRL = %0000_0100     ; DCSEL 2, ADDRSEL 0
            cx16.VERA_FX_CTRL = 0
            cx16.VERA_CTRL = 0
        }

        ; sheer to a given vertical integer extent
        sub shear_v(word extent) { ; use the 32-bit cache
            alias context = cx16.r3
            context = this
 
            ; set the destination for the shear
            cx16.VERA_CTRL   = 0
            cx16.VERA_ADDR   = peekw(context + AFF_SPRITE_DATA_VADDR)
            cx16.VERA_ADDR_H = peek(context + AFF_SPRITE_DATA_BANK) | %0011_0000 ; increment DATA1 1

            cx16.VERA_CTRL = %0000_0100 ; DCSEL 2, ADDRSEL 0
            ; enable cached writes, cache fill
            cx16.VERA_FX_CTRL = helper | %0110_0000 ; affine helper with four bit set or reset

            cx16.VERA_CTRL = %0000_0110 ; DCSEL 3, ADDRSEL 0

            cx16.VERA_FX_X_INCR = $0200
            cx16.VERA_FX_Y_INCR = extent & $7FFF

            cx16.VERA_CTRL = %0000_1001 ; DCSEL 4, ADDRSEL 1

            count = 0
            do {
                cx16.VERA_FX_X_POS = 0      ; start on the left
                cx16.VERA_FX_Y_POS = count  ; move down the tile set

                repeat cache_count {
                    %asm{{
                        lda cx16.VERA_DATA1 
                        lda cx16.VERA_DATA1
                        lda cx16.VERA_DATA1
                        lda cx16.VERA_DATA1
                    }} 
                    if is_four_bit {
                        %asm{{
                            lda cx16.VERA_DATA1 
                            lda cx16.VERA_DATA1
                            lda cx16.VERA_DATA1
                            lda cx16.VERA_DATA1
                        }} 
                    }
                    %asm{{
                        stz cx16.VERA_DATA0
                    }}
                }
                count++                       ; change the position  
            } until count == row_count 
            
            ; clean up the regs
            cx16.VERA_CTRL = %0000_0100     ; DCSEL 2, ADDRSEL 0
            cx16.VERA_FX_CTRL = 0
            cx16.VERA_CTRL = 0
        }

        ; rotate to a given integer degree, faster than the float version
        sub rotate_i(word wdegree) {
            wangle = 360 - wdegree ; reverse the rotation to go clockwise
            int_source = true
            rotate_internal()
        }

        float fdegree = 0.0
        ; rotate to a given degree, does not need to be an integer value
        asmsub rotate_f(float angle @ FAC1) {
            %asm{{
                ldx  #<p8b_vtsbank.p8s_affine.p8v_fdegree
                ldy  #>p8b_vtsbank.p8s_affine.p8v_fdegree
                jsr  floats.MOVMF
                jmp  p8b_vtsbank.p8s_affine.p8s_rotate_f_internal
            }}   
        }

        sub rotate_f_internal() {
            theta = floats.rad(360.0 - fdegree)     ; reverse the angle to ge clockwise
            int_source = false
            rotate_internal()
        }

        ; rotate to a given radian, does not need to be an integer value
        asmsub rotate_r(float angle @ FAC1) {
            %asm{{
                ldx  #<p8b_vtsbank.p8s_affine.p8v_theta
                ldy  #>p8b_vtsbank.p8s_affine.p8v_theta
                jsr  floats.MOVMF
                jmp  p8b_vtsbank.p8s_affine.p8s_rotate_r_internal
            }}   
        }

        sub rotate_r_internal() {
            if cx16.r0 != 0 {
                theta = floats.TWOPI - theta ; reverse theta to go clockwise
            }
            theta -= floats.PI / 2
            int_source = false
            rotate_internal()
        }

        ; rotate to a given radian multiple of PI, does not need to be an integer value
        asmsub rotate_p(float angle @ FAC1) {
            %asm{{
                ldx  #<p8b_vtsbank.p8s_affine.p8v_theta
                ldy  #>p8b_vtsbank.p8s_affine.p8v_theta
                jsr  floats.MOVMF
                jmp  p8b_vtsbank.p8s_affine.p8s_rotate_p_internal
            }}   
        }

        sub rotate_p_internal() {
            theta *= floats.PI
            if cx16.r0 != 0 {
                theta = floats.TWOPI - theta ; reverse theta to go clockwise
            }
            theta -= floats.PI / 2
            int_source = false
            rotate_internal()
        }

        ; rotate to a given angle (float or integer)
        sub rotate_internal() {
            alias context = cx16.r3
            context = this

            ; set the FX Tile Base Address 
            cx16.VERA_CTRL = %0000_0100  ; DCSEL=2 ADDRSEL=0
            cx16.VERA_FX_CTRL = peek(context + AFF_FX_CTRL)

            ; set the destination for the rotated sprite
            cx16.VERA_CTRL = 0
            cx16.VERA_ADDR = peekw(context + AFF_SPRITE_DATA_VADDR)
            cx16.VERA_ADDR_H = peek(context + AFF_SPRITE_DATA_BANK) | %0011_0000 ; increment 4

            ; set the initial position of x and y
            if int_source {
                set_start_int()    ; works of wangle in word degress 
            } else {
                set_start_float()  ; works on theta in float radians
            }    
            
            ; for every line in the sprite
            repeat row_count {
                repeat cache_count {
                    ; set up for the four byte cache transfers for each tile
                    ; four bit modes take 1 cache entry of 8 pixels
                    ; eight bit modes take 2 cache entries of 4 pixels
                    %asm{{
                        lda cx16.VERA_DATA1 
                        lda cx16.VERA_DATA1
                        lda cx16.VERA_DATA1
                        lda cx16.VERA_DATA1
                    }} 
                    if is_four_bit {
                        %asm{{
                            lda cx16.VERA_DATA1 
                            lda cx16.VERA_DATA1
                            lda cx16.VERA_DATA1
                            lda cx16.VERA_DATA1
                        }} 
                    }
                    %asm{{
                        stz cx16.VERA_DATA0
                    }}
                }
                set_next()
            }

            reset_fx()
            return        ; rotate_internal

            sub set_start_int() {  ; uses word wangle in degrees
                alias  tbl    = cx16.r12  ; $1a $1b

                word w_cosine
                word w_sine

                tbl = get_table_entry(wangle)

                cx16.VERA_CTRL = %0000_0110     ; DCSEL = 3, ADDRSEL = 0
                if use_scale { ; slower with floating point for scaling
                    w_cosine = (((peekw(tbl+TBL_COSINE) as word) as float) * scale) as word 
                    w_sine = (((peekw(tbl+TBL_SINE) as word) as float) * scale) as word 
                    cx16.VERA_FX_X_INCR = (w_cosine  & $7FFF)  as uword
                    cx16.VERA_FX_Y_INCR = (w_sine  & $7FFF)  as uword
                } else {
                    ; faster without floats and scaling
                    %asm{{
                        ldy #$00
                        lda ($1a),y             ; peekw(tbl+0)
                        iny
                        sta cx16.VERA_FX_X_INCR
                        lda ($1a),y
                        iny
                        and #$7F
                        sta cx16.VERA_FX_X_INCR+1
                        lda ($1a),y             ; peekw(tbl+2)
                        iny
                        sta cx16.VERA_FX_Y_INCR
                        lda ($1a),y
                        and #$7F
                        sta cx16.VERA_FX_Y_INCR+1
                    }}       
                }

                ; load the starting position for the affine transfer
                x_position = peekw(tbl+TBL_64_XPOS) as word
                y_position = peekw(tbl+TBL_64_YPOS) as word
                if is_16 {
                    x_position /= 4
                    y_position /= 4
                }

                ; set the inc to 0 for the first setting of the position
                x_inc = 0
                y_inc = 0

                set_next()

                ; now set the incs up for the next 63 lines
                if use_scale {
                    x_inc = -(w_sine / 2)          ; efficientcy for cosine(theta + 90) * 256
                    y_inc = (w_cosine / 2)         ; efficientcy for sine(theta + 90) * 256
                } else {
                    x_inc = -(peekw(tbl+TBL_SINE) as word) / 2  ; x_inc efficientcy for cosine(wangle + 90) * 256
                    y_inc = peekw(tbl+TBL_COSINE) as word / 2     ; y_inc efficientcy for sine(wangle + 90) * 256
                }
                
                cx16.VERA_CTRL = 0              ; DCSET = 0, ADDREL = 0
            }

            sub set_start_float() { ; uses theta as float in radians
                word w_cosine
                word w_sine
                if use_scale {  ; a bit faster to save a float multiply if original size
                    w_cosine = ((floats.cos(theta) * scale) * 512.0) as word 
                    w_sine = ((floats.sin(theta) * scale) * 512.0) as word
                } else {
                    w_cosine = (floats.cos(theta) * 512.0) as word 
                    w_sine = (floats.sin(theta) * 512.0) as word
                }
                cx16.VERA_CTRL = %0000_0110     ; DCSEL = 3, ADDRSEL = 0
                cx16.VERA_FX_X_INCR = (w_cosine  & $7FFF)  as uword
                cx16.VERA_FX_Y_INCR = (w_sine  & $7FFF)  as uword

                ; calculate the starting position for the affine transfer
                x_position = (((floats.cos(CORNER_ANGLE + theta) * distance_to_corner) + (position_offset as float)) * 256.0) as word
                y_position = (((floats.sin(CORNER_ANGLE + theta) * distance_to_corner) + (position_offset as float)) * 256.0) as word

                ; set the x and y inc to 0 for the initial setting of the position
                x_inc = 0
                y_inc = 0
                
                set_next()

                ; now set the inc up for the next 63 lines
                x_inc = -(w_sine / 2)          ; efficientcy for cosine(theta + 90) * 256
                y_inc = (w_cosine / 2)         ; efficientcy for sine(theta + 90) * 256

                cx16.VERA_CTRL = 0              ; DCSET = 0, ADDREL = 0
            }

            sub set_next() { ; called once for each row in the tile set
                ; move to new position
                x_position += x_inc
                y_position += y_inc


                ; set the new integer position
                cx16.VERA_CTRL = %0000_1000     ; DCSEL = 4, ADDRSEL = 0
                %asm{{
                    ; set FX_X_POS
                    lda  p8v_x_position+1
                    sta  cx16.VERA_FX_X_POS
                    bpl  +
                    lda  #$07
                    bra  ++
                +   lda  #$00 
                +   sta  cx16.VERA_FX_X_POS+1

                    ; set FX_Y_POS
                    lda  p8v_y_position+1
                    sta  cx16.VERA_FX_Y_POS
                    bpl  +
                    lda  #$07
                    bra  ++
                +   lda  #$00 
                +   sta  cx16.VERA_FX_Y_POS+1
                }}

                ; set the new decimal position
                cx16.VERA_CTRL = %0000_1010     ; DCSEL = 5, ADDRSEL = 0
                cx16.VERA_FX_X_POS_S = lsb(x_position)
                cx16.VERA_FX_Y_POS_S = lsb(y_position)
            }
        }

        sub reset_fx() {
            cx16.VERA_CTRL = %0000_0100  ; DCSEL = 2. ADDRSEL = 0
            cx16.VERA_FX_CTRL = 0
            cx16.VERA_CTRL = 0
        }

        sub get_table_entry(word angle) -> uword {
            ; handle negative angles
            while angle < 0 {
                angle += 360
            }   
            ; handle angles larger than 360
            while angle >= 360 {
                angle -= 360
            }
            ; angle will now range 0 to 359 in integer degrees

            ; set the base of our 6 word values for this angle
            return (&word_trig_table + (angle * SIZE_WORD_TRIG_TABLE_ENTRY)) as uword    
        }

    }

    word_trig_table: 
;        %asmbinary "RESOURCE/affine-trig.bin"
    %asm{{
        ; entries for 0 through 359
        ; TBL_COSINE TBL_SINE TBL_64_XPOS TBL_64_YPOS 
        .word  $01FF, $0000, $0000, $0000
        .word  $01FF, $0008, $0090, $FF72
        .word  $01FF, $0011, $0122, $FEE7
        .word  $01FF, $001A, $01B7, $FE5E
        .word  $01FE, $0023, $024F, $FDD8
        .word  $01FE, $002C, $02E9, $FD55
        .word  $01FD, $0035, $0385, $FCD4
        .word  $01FC, $003E, $0423, $FC56
        .word  $01FB, $0047, $04C3, $FBDB
        .word  $01F9, $0050, $0566, $FB63
        .word  $01F8, $0058, $060A, $FAED
        .word  $01F6, $0061, $06B1, $FA7B
        .word  $01F4, $006A, $075A, $FA0B
        .word  $01F2, $0073, $0804, $F99F
        .word  $01F0, $007B, $08B1, $F935
        .word  $01EE, $0084, $095F, $F8CE
        .word  $01EC, $008D, $0A0F, $F86B
        .word  $01E9, $0095, $0AC1, $F80A
        .word  $01E6, $009E, $0B74, $F7AD
        .word  $01E4, $00A6, $0C29, $F753
        .word  $01E1, $00AF, $0CDF, $F6FC
        .word  $01DD, $00B7, $0D97, $F6A8
        .word  $01DA, $00BF, $0E51, $F657
        .word  $01D7, $00C8, $0F0C, $F60A
        .word  $01D3, $00D0, $0FC8, $F5C0
        .word  $01D0, $00D8, $1085, $F579
        .word  $01CC, $00E0, $1144, $F535
        .word  $01C8, $00E8, $1203, $F4F5
        .word  $01C4, $00F0, $12C4, $F4B8
        .word  $01BF, $00F8, $1386, $F47F
        .word  $01BB, $00FF, $1449, $F449
        .word  $01B6, $0107, $150D, $F416
        .word  $01B2, $010F, $15D1, $F3E7
        .word  $01AD, $0116, $1697, $F3BB
        .word  $01A8, $011E, $175D, $F393
        .word  $01A3, $0125, $1824, $F36E
        .word  $019E, $012C, $18EB, $F34D
        .word  $0198, $0134, $19B3, $F32F
        .word  $0193, $013B, $1A7C, $F315
        .word  $018D, $0142, $1B45, $F2FE
        .word  $0188, $0149, $1C0E, $F2EA
        .word  $0182, $014F, $1CD7, $F2DA
        .word  $017C, $0156, $1DA1, $F2CE
        .word  $0176, $015D, $1E6B, $F2C5
        .word  $0170, $0163, $1F35, $F2C0
        .word  $016A, $016A, $2000, $F2BE
        .word  $0163, $0170, $20CA, $F2C0
        .word  $015D, $0176, $2194, $F2C5
        .word  $0156, $017C, $225E, $F2CE
        .word  $014F, $0182, $2328, $F2DA
        .word  $0149, $0188, $23F1, $F2EA
        .word  $0142, $018D, $24BA, $F2FE
        .word  $013B, $0193, $2583, $F315
        .word  $0134, $0198, $264C, $F32F
        .word  $012C, $019E, $2714, $F34D
        .word  $0125, $01A3, $27DB, $F36E
        .word  $011E, $01A8, $28A2, $F393
        .word  $0116, $01AD, $2968, $F3BB
        .word  $010F, $01B2, $2A2E, $F3E7
        .word  $0107, $01B6, $2AF2, $F416
        .word  $00FF, $01BB, $2BB6, $F449
        .word  $00F8, $01BF, $2C79, $F47F
        .word  $00F0, $01C4, $2D3B, $F4B8
        .word  $00E8, $01C8, $2DFC, $F4F5
        .word  $00E0, $01CC, $2EBB, $F535
        .word  $00D8, $01D0, $2F7A, $F579
        .word  $00D0, $01D3, $3037, $F5C0
        .word  $00C8, $01D7, $30F3, $F60A
        .word  $00BF, $01DA, $31AE, $F657
        .word  $00B7, $01DD, $3268, $F6A8
        .word  $00AF, $01E1, $3320, $F6FC
        .word  $00A6, $01E4, $33D6, $F753
        .word  $009E, $01E6, $348B, $F7AD
        .word  $0095, $01E9, $353E, $F80A
        .word  $008D, $01EC, $35F0, $F86B
        .word  $0084, $01EE, $36A0, $F8CE
        .word  $007B, $01F0, $374E, $F935
        .word  $0073, $01F2, $37FB, $F99F
        .word  $006A, $01F4, $38A5, $FA0B
        .word  $0061, $01F6, $394E, $FA7B
        .word  $0058, $01F8, $39F5, $FAED
        .word  $0050, $01F9, $3A99, $FB63
        .word  $0047, $01FB, $3B3C, $FBDB
        .word  $003E, $01FC, $3BDC, $FC56
        .word  $0035, $01FD, $3C7A, $FCD4
        .word  $002C, $01FE, $3D16, $FD55
        .word  $0023, $01FE, $3DB0, $FDD8
        .word  $001A, $01FF, $3E48, $FE5E
        .word  $0011, $01FF, $3EDD, $FEE7
        .word  $0008, $01FF, $3F6F, $FF72
        .word  $0000, $01FF, $3FFF, $FFFF
        .word  $FFF7, $01FF, $408D, $0090
        .word  $FFEE, $01FF, $4118, $0122
        .word  $FFE5, $01FF, $41A1, $01B7
        .word  $FFDC, $01FE, $4227, $024F
        .word  $FFD3, $01FE, $42AA, $02E9
        .word  $FFCA, $01FD, $432B, $0385
        .word  $FFC1, $01FC, $43A9, $0423
        .word  $FFB8, $01FB, $4424, $04C3
        .word  $FFAF, $01F9, $449C, $0566
        .word  $FFA7, $01F8, $4512, $060A
        .word  $FF9E, $01F6, $4584, $06B1
        .word  $FF95, $01F4, $45F4, $075A
        .word  $FF8C, $01F2, $4660, $0804
        .word  $FF84, $01F0, $46CA, $08B1
        .word  $FF7B, $01EE, $4731, $095F
        .word  $FF72, $01EC, $4794, $0A0F
        .word  $FF6A, $01E9, $47F5, $0AC1
        .word  $FF61, $01E6, $4852, $0B74
        .word  $FF59, $01E4, $48AC, $0C29
        .word  $FF50, $01E1, $4903, $0CDF
        .word  $FF48, $01DD, $4957, $0D97
        .word  $FF40, $01DA, $49A8, $0E51
        .word  $FF37, $01D7, $49F5, $0F0C
        .word  $FF2F, $01D3, $4A3F, $0FC8
        .word  $FF27, $01D0, $4A86, $1085
        .word  $FF1F, $01CC, $4ACA, $1144
        .word  $FF17, $01C8, $4B0A, $1203
        .word  $FF0F, $01C4, $4B47, $12C4
        .word  $FF07, $01BF, $4B80, $1386
        .word  $FF00, $01BB, $4BB6, $1449
        .word  $FEF8, $01B6, $4BE9, $150D
        .word  $FEF0, $01B2, $4C18, $15D1
        .word  $FEE9, $01AD, $4C44, $1697
        .word  $FEE1, $01A8, $4C6C, $175D
        .word  $FEDA, $01A3, $4C91, $1824
        .word  $FED3, $019E, $4CB2, $18EB
        .word  $FECB, $0198, $4CD0, $19B3
        .word  $FEC4, $0193, $4CEA, $1A7C
        .word  $FEBD, $018D, $4D01, $1B45
        .word  $FEB6, $0188, $4D15, $1C0E
        .word  $FEB0, $0182, $4D25, $1CD7
        .word  $FEA9, $017C, $4D31, $1DA1
        .word  $FEA2, $0176, $4D3A, $1E6B
        .word  $FE9C, $0170, $4D3F, $1F35
        .word  $FE95, $016A, $4D41, $2000
        .word  $FE8F, $0163, $4D3F, $20CA
        .word  $FE89, $015D, $4D3A, $2194
        .word  $FE83, $0156, $4D31, $225E
        .word  $FE7D, $014F, $4D25, $2328
        .word  $FE77, $0149, $4D15, $23F1
        .word  $FE72, $0142, $4D01, $24BA
        .word  $FE6C, $013B, $4CEA, $2583
        .word  $FE67, $0134, $4CD0, $264C
        .word  $FE61, $012C, $4CB2, $2714
        .word  $FE5C, $0125, $4C91, $27DB
        .word  $FE57, $011E, $4C6C, $28A2
        .word  $FE52, $0116, $4C44, $2968
        .word  $FE4D, $010F, $4C18, $2A2E
        .word  $FE49, $0107, $4BE9, $2AF2
        .word  $FE44, $0100, $4BB6, $2BB6
        .word  $FE40, $00F8, $4B80, $2C79
        .word  $FE3B, $00F0, $4B47, $2D3B
        .word  $FE37, $00E8, $4B0A, $2DFC
        .word  $FE33, $00E0, $4ACA, $2EBB
        .word  $FE2F, $00D8, $4A86, $2F7A
        .word  $FE2C, $00D0, $4A3F, $3037
        .word  $FE28, $00C8, $49F5, $30F3
        .word  $FE25, $00BF, $49A8, $31AE
        .word  $FE22, $00B7, $4957, $3268
        .word  $FE1E, $00AF, $4903, $3320
        .word  $FE1B, $00A6, $48AC, $33D6
        .word  $FE19, $009E, $4852, $348B
        .word  $FE16, $0095, $47F5, $353E
        .word  $FE13, $008D, $4794, $35F0
        .word  $FE11, $0084, $4731, $36A0
        .word  $FE0F, $007B, $46CA, $374E
        .word  $FE0D, $0073, $4660, $37FB
        .word  $FE0B, $006A, $45F4, $38A5
        .word  $FE09, $0061, $4584, $394E
        .word  $FE07, $0058, $4512, $39F5
        .word  $FE06, $0050, $449C, $3A99
        .word  $FE04, $0047, $4424, $3B3C
        .word  $FE03, $003E, $43A9, $3BDC
        .word  $FE02, $0035, $432B, $3C7A
        .word  $FE01, $002C, $42AA, $3D16
        .word  $FE01, $0023, $4227, $3DB0
        .word  $FE00, $001A, $41A1, $3E48
        .word  $FE00, $0011, $4118, $3EDD
        .word  $FE00, $0008, $408D, $3F6F
        .word  $FE00, $0000, $3FFF, $3FFF
        .word  $FE00, $FFF7, $3F6F, $408D
        .word  $FE00, $FFEE, $3EDD, $4118
        .word  $FE00, $FFE5, $3E48, $41A1
        .word  $FE01, $FFDC, $3DB0, $4227
        .word  $FE01, $FFD3, $3D16, $42AA
        .word  $FE02, $FFCA, $3C7A, $432B
        .word  $FE03, $FFC1, $3BDC, $43A9
        .word  $FE04, $FFB8, $3B3C, $4424
        .word  $FE06, $FFAF, $3A99, $449C
        .word  $FE07, $FFA7, $39F5, $4512
        .word  $FE09, $FF9E, $394E, $4584
        .word  $FE0B, $FF95, $38A5, $45F4
        .word  $FE0D, $FF8C, $37FB, $4660
        .word  $FE0F, $FF84, $374E, $46CA
        .word  $FE11, $FF7B, $36A0, $4731
        .word  $FE13, $FF72, $35F0, $4794
        .word  $FE16, $FF6A, $353E, $47F5
        .word  $FE19, $FF61, $348B, $4852
        .word  $FE1B, $FF59, $33D6, $48AC
        .word  $FE1E, $FF50, $3320, $4903
        .word  $FE22, $FF48, $3268, $4957
        .word  $FE25, $FF40, $31AE, $49A8
        .word  $FE28, $FF37, $30F3, $49F5
        .word  $FE2C, $FF2F, $3037, $4A3F
        .word  $FE2F, $FF27, $2F7A, $4A86
        .word  $FE33, $FF1F, $2EBB, $4ACA
        .word  $FE37, $FF17, $2DFC, $4B0A
        .word  $FE3B, $FF0F, $2D3B, $4B47
        .word  $FE40, $FF07, $2C79, $4B80
        .word  $FE44, $FF00, $2BB6, $4BB6
        .word  $FE49, $FEF8, $2AF2, $4BE9
        .word  $FE4D, $FEF0, $2A2E, $4C18
        .word  $FE52, $FEE9, $2968, $4C44
        .word  $FE57, $FEE1, $28A2, $4C6C
        .word  $FE5C, $FEDA, $27DB, $4C91
        .word  $FE61, $FED3, $2714, $4CB2
        .word  $FE67, $FECB, $264C, $4CD0
        .word  $FE6C, $FEC4, $2583, $4CEA
        .word  $FE72, $FEBD, $24BA, $4D01
        .word  $FE77, $FEB6, $23F1, $4D15
        .word  $FE7D, $FEB0, $2328, $4D25
        .word  $FE83, $FEA9, $225E, $4D31
        .word  $FE89, $FEA2, $2194, $4D3A
        .word  $FE8F, $FE9C, $20CA, $4D3F
        .word  $FE95, $FE95, $2000, $4D41
        .word  $FE9C, $FE8F, $1F35, $4D3F
        .word  $FEA2, $FE89, $1E6B, $4D3A
        .word  $FEA9, $FE83, $1DA1, $4D31
        .word  $FEB0, $FE7D, $1CD7, $4D25
        .word  $FEB6, $FE77, $1C0E, $4D15
        .word  $FEBD, $FE72, $1B45, $4D01
        .word  $FEC4, $FE6C, $1A7C, $4CEA
        .word  $FECB, $FE67, $19B3, $4CD0
        .word  $FED3, $FE61, $18EB, $4CB2
        .word  $FEDA, $FE5C, $1824, $4C91
        .word  $FEE1, $FE57, $175D, $4C6C
        .word  $FEE9, $FE52, $1697, $4C44
        .word  $FEF0, $FE4D, $15D1, $4C18
        .word  $FEF8, $FE49, $150D, $4BE9
        .word  $FF00, $FE44, $1449, $4BB6
        .word  $FF07, $FE40, $1386, $4B80
        .word  $FF0F, $FE3B, $12C4, $4B47
        .word  $FF17, $FE37, $1203, $4B0A
        .word  $FF1F, $FE33, $1144, $4ACA
        .word  $FF27, $FE2F, $1085, $4A86
        .word  $FF2F, $FE2C, $0FC8, $4A3F
        .word  $FF37, $FE28, $0F0C, $49F5
        .word  $FF40, $FE25, $0E51, $49A8
        .word  $FF48, $FE22, $0D97, $4957
        .word  $FF50, $FE1E, $0CDF, $4903
        .word  $FF59, $FE1B, $0C29, $48AC
        .word  $FF61, $FE19, $0B74, $4852
        .word  $FF6A, $FE16, $0AC1, $47F5
        .word  $FF72, $FE13, $0A0F, $4794
        .word  $FF7B, $FE11, $095F, $4731
        .word  $FF84, $FE0F, $08B1, $46CA
        .word  $FF8C, $FE0D, $0804, $4660
        .word  $FF95, $FE0B, $075A, $45F4
        .word  $FF9E, $FE09, $06B1, $4584
        .word  $FFA7, $FE07, $060A, $4512
        .word  $FFAF, $FE06, $0566, $449C
        .word  $FFB8, $FE04, $04C3, $4424
        .word  $FFC1, $FE03, $0423, $43A9
        .word  $FFCA, $FE02, $0385, $432B
        .word  $FFD3, $FE01, $02E9, $42AA
        .word  $FFDC, $FE01, $024F, $4227
        .word  $FFE5, $FE00, $01B7, $41A1
        .word  $FFEE, $FE00, $0122, $4118
        .word  $FFF7, $FE00, $0090, $408D
        .word  $0000, $FE00, $0000, $3FFF
        .word  $0008, $FE00, $FF72, $3F6F
        .word  $0011, $FE00, $FEE7, $3EDD
        .word  $001A, $FE00, $FE5E, $3E48
        .word  $0023, $FE01, $FDD8, $3DB0
        .word  $002C, $FE01, $FD55, $3D16
        .word  $0035, $FE02, $FCD4, $3C7A
        .word  $003E, $FE03, $FC56, $3BDC
        .word  $0047, $FE04, $FBDB, $3B3C
        .word  $0050, $FE06, $FB63, $3A99
        .word  $0058, $FE07, $FAED, $39F5
        .word  $0061, $FE09, $FA7B, $394E
        .word  $006A, $FE0B, $FA0B, $38A5
        .word  $0073, $FE0D, $F99F, $37FB
        .word  $007B, $FE0F, $F935, $374E
        .word  $0084, $FE11, $F8CE, $36A0
        .word  $008D, $FE13, $F86B, $35F0
        .word  $0095, $FE16, $F80A, $353E
        .word  $009E, $FE19, $F7AD, $348B
        .word  $00A6, $FE1B, $F753, $33D6
        .word  $00AF, $FE1E, $F6FC, $3320
        .word  $00B7, $FE22, $F6A8, $3268
        .word  $00BF, $FE25, $F657, $31AE
        .word  $00C8, $FE28, $F60A, $30F3
        .word  $00D0, $FE2C, $F5C0, $3037
        .word  $00D8, $FE2F, $F579, $2F7A
        .word  $00E0, $FE33, $F535, $2EBB
        .word  $00E8, $FE37, $F4F5, $2DFC
        .word  $00F0, $FE3B, $F4B8, $2D3B
        .word  $00F8, $FE40, $F47F, $2C79
        .word  $0100, $FE44, $F449, $2BB6
        .word  $0107, $FE49, $F416, $2AF2
        .word  $010F, $FE4D, $F3E7, $2A2E
        .word  $0116, $FE52, $F3BB, $2968
        .word  $011E, $FE57, $F393, $28A2
        .word  $0125, $FE5C, $F36E, $27DB
        .word  $012C, $FE61, $F34D, $2714
        .word  $0134, $FE67, $F32F, $264C
        .word  $013B, $FE6C, $F315, $2583
        .word  $0142, $FE72, $F2FE, $24BA
        .word  $0149, $FE77, $F2EA, $23F1
        .word  $014F, $FE7D, $F2DA, $2328
        .word  $0156, $FE83, $F2CE, $225E
        .word  $015D, $FE89, $F2C5, $2194
        .word  $0163, $FE8F, $F2C0, $20CA
        .word  $016A, $FE95, $F2BE, $2000
        .word  $0170, $FE9C, $F2C0, $1F35
        .word  $0176, $FEA2, $F2C5, $1E6B
        .word  $017C, $FEA9, $F2CE, $1DA1
        .word  $0182, $FEB0, $F2DA, $1CD7
        .word  $0188, $FEB6, $F2EA, $1C0E
        .word  $018D, $FEBD, $F2FE, $1B45
        .word  $0193, $FEC4, $F315, $1A7C
        .word  $0198, $FECB, $F32F, $19B3
        .word  $019E, $FED3, $F34D, $18EB
        .word  $01A3, $FEDA, $F36E, $1824
        .word  $01A8, $FEE1, $F393, $175D
        .word  $01AD, $FEE9, $F3BB, $1697
        .word  $01B2, $FEF0, $F3E7, $15D1
        .word  $01B6, $FEF8, $F416, $150D
        .word  $01BB, $FEFF, $F449, $1449
        .word  $01BF, $FF07, $F47F, $1386
        .word  $01C4, $FF0F, $F4B8, $12C4
        .word  $01C8, $FF17, $F4F5, $1203
        .word  $01CC, $FF1F, $F535, $1144
        .word  $01D0, $FF27, $F579, $1085
        .word  $01D3, $FF2F, $F5C0, $0FC8
        .word  $01D7, $FF37, $F60A, $0F0C
        .word  $01DA, $FF40, $F657, $0E51
        .word  $01DD, $FF48, $F6A8, $0D97
        .word  $01E1, $FF50, $F6FC, $0CDF
        .word  $01E4, $FF59, $F753, $0C29
        .word  $01E6, $FF61, $F7AD, $0B74
        .word  $01E9, $FF6A, $F80A, $0AC1
        .word  $01EC, $FF72, $F86B, $0A0F
        .word  $01EE, $FF7B, $F8CE, $095F
        .word  $01F0, $FF84, $F935, $08B1
        .word  $01F2, $FF8C, $F99F, $0804
        .word  $01F4, $FF95, $FA0B, $075A
        .word  $01F6, $FF9E, $FA7B, $06B1
        .word  $01F8, $FFA7, $FAED, $060A
        .word  $01F9, $FFAF, $FB63, $0566
        .word  $01FB, $FFB8, $FBDB, $04C3
        .word  $01FC, $FFC1, $FC56, $0423
        .word  $01FD, $FFCA, $FCD4, $0385
        .word  $01FE, $FFD3, $FD55, $02E9
        .word  $01FE, $FFDC, $FDD8, $024F
        .word  $01FF, $FFE5, $FE5E, $01B7
        .word  $01FF, $FFEE, $FEE7, $0122
        .word  $01FF, $FFF7, $FF72, $0090
    }}



    %asm{{
    next_code_addr: = *
        * = $BFD0
        jmp p8b_main.p8s_start
        jmp p8b_vtsbank.p8s_affine.p8s_create_context
        jmp p8b_vtsbank.p8s_affine.p8s_select
        jmp p8b_vtsbank.p8s_affine.p8s_set_scale
        jmp p8b_vtsbank.p8s_affine.p8s_rotate_i
        jmp p8b_vtsbank.p8s_affine.p8s_rotate_f    
        jmp p8b_vtsbank.p8s_affine.p8s_rotate_r    
        jmp p8b_vtsbank.p8s_affine.p8s_rotate_p  
        jmp p8b_vtsbank.p8s_affine.p8s_shear_v  
        jmp p8b_vtsbank.p8s_affine.p8s_shear_h  
        jmp p8b_vtsbank.p8s_affine.p8s_reset_fx
        * = next_code_addr    
    }}
}



