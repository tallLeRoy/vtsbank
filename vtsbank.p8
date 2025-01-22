; vtsbank.p8   compile a bankable VERA FX Affine tile set rotation an scaling feature

%launcher none
%address  $A000
%memtop   $C000
%output   raw
%option   no_sysinit
%zeropage dontuse

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

    const uword SIZE_WORD_TRIG_TABLE_ENTRY = 6 * sys.SIZEOF_WORD

    ; word_trig_table entry offsets
    const ubyte TBL_COSINE  = 0
    const ubyte TBL_SINE    = 2
    const ubyte TBL_64_XPOS = 4
    const ubyte TBL_64_YPOS = 6
    const ubyte TBL_16_XPOS = 8
    const ubyte TBL_16_YPOS = 10
    
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
                if is_16 {
                    x_position = peekw(tbl+TBL_16_XPOS) as word
                    y_position = peekw(tbl+TBL_16_YPOS) as word
                } else {
                    x_position = peekw(tbl+TBL_64_XPOS) as word
                    y_position = peekw(tbl+TBL_64_YPOS) as word
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
        %asmbinary "affine-trig.bin"

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



