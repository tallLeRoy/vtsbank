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
%import sprites

main {
    %jmptable (
        vtsbank.affine.create_context,
        vtsbank.affine.select,
        vtsbank.affine.set_scale,
        vtsbank.affine.rotate_i,
        vtsbank.affine.rotate_f,    
        vtsbank.affine.rotate_r,    
        vtsbank.affine.rotate_p,  
        vtsbank.affine.shear_v,  
        vtsbank.affine.shear_h,  
        vtsbank.affine.reset_fx,
        vtsbank.affine.sprite_to_tile_set,
    )

    sub start() {
    }
}

vtsbank {
    %option force_output

    const uword SIZE_WORD_TRIG_TABLE_ENTRY = 3 * sys.SIZEOF_WORD

    ; word_trig_table entry offsets
    const ubyte TBL_SINE    = 0
    const ubyte TBL_64_XPOS = 2
    const ubyte TBL_64_YPOS = 4

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

        const ubyte POSITION_OFFSET_64 = 31
        const ubyte POSITION_OFFSET_16 = 7
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
                    w_cosine = (get_cosine(wangle) as float) * scale as word
                    w_sine = (((peekw(tbl+TBL_SINE) as word) as float) * scale) as word 
                } else {
                    w_cosine = get_cosine(wangle)
                    w_sine = peekw(tbl+TBL_SINE) as word
                }

                cx16.VERA_FX_X_INCR = (w_cosine  & $7FFF)  as uword
                cx16.VERA_FX_Y_INCR = (w_sine  & $7FFF)  as uword

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
                x_inc = -(w_sine / 2)          ; efficientcy for cosine(theta + 90) * 256
                y_inc = (w_cosine / 2)         ; efficientcy for sine(theta + 90) * 256
                
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

/*
        sub get_64_ypos(word angle @R5) -> word {
            ; modify the angle to look up the YPOS in the XPOS table. 
            ; saves over 500 bytes to do this
            %asm{{
    			cmp  #<0              ; is the angle 0
    			bne  +
    			cpy  #>0
    			beq  set_return_code
    			cmp  #<180            ; is the angle 180
    			bne  +
    			cpy  #>180
    			beq  set_return_code
    			cmp  #<360            ; is the angle 360
    			bne  +
    			cpy  #>360
    			beq  set_return_code

            less_than_46    
                cmp  #<$2e
                tya
                sbc  #>$2e
                bvs  +
                eor  #128
            +   bmi  less_than_90
                lda  p8v_angle
                clc
                adc  #<270
                sta  p8v_angle
                lda  p8v_angle+1
                adc  #>270
                sta  p8v_angle+1
                bra  set_return_code

            less_than_90
                lda  p8v_angle
                cmp  #<$5a
                tya
                sbc  #>$5a
                bvs  +
                eor  #128
            +   bmi  less_than_226
                lda  #0
                sec
                sbc  p8v_angle
                sta  p8v_angle
                lda  #0
                sbc  p8v_angle+1
                sta  p8v_angle+1
                lda  p8v_angle
                clc
                adc  #<360
                sta  p8v_angle
                lda  p8v_angle+1
                adc  #>360
                sta  p8v_angle+1
                bra  set_return_code
                
            less_than_226
                lda  p8v_angle
                cmp  #<$e2
                tya
                sbc  #>$e2
                bvs  +
                eor  #128
            +   bmi  greater_than_225
                lda  p8v_angle
                sec
                sbc  #90
                sta  p8v_angle
                bcs  +
                dec  p8v_angle+1
			+	bra  set_return_code

            greater_than_225
                lda  p8v_angle
                sec
                sbc  #<$e1
                tax
                tya
                sbc  #>$e1
                tay
                txa
                sta  P8ZP_SCRATCH_W1
                sty  P8ZP_SCRATCH_W1+1
                ldy  #>$87
                lda  #<$87
                sec
                sbc  P8ZP_SCRATCH_W1
                tax
                tya
                sbc  P8ZP_SCRATCH_W1+1
                tay
                txa
                sta  p8v_angle
                sty  p8v_angle+1

            set_return_code
            }}

;            repeat 1 {
;                if angle in [0 ,180 as word] { ; angle as is
;                    break
;                }
;                if angle < 46 {                ; angle assending much higher 
;                    angle += 270
;                    break
;                }
;                if angle < 90 {
;                    angle = 360 - angle         ; angle descending
;                    break
;                } 
;                if angle < 226 {
;                    angle -= 90                 ; angle descending
;                    break
;                }
;                angle = 135 - (angle - 225)     ; angle decending from 135
;            }  
            return peekw(vtsbank.affine.get_table_entry(angle) + TBL_64_XPOS) as word   
        }
*/
        sub get_cosine(word angle) -> word {
            ; use sine/cosine phase relationship to get cosine from the sine table
            return peekw(get_table_entry(90 - angle) + TBL_SINE) as word
        }

        ; put the sprite buffer into the tile set buffer as tiles
        ; uses the selected context
        sub sprite_to_tile_set() {
            alias row_next_tile = cx16.r10
            alias row_next_set  = cx16.r11
            alias tile_width    = cx16.r12L
            alias tile_size     = cx16.r12H
            alias tile_rows     = cx16.r13L
            alias tile_columns  = cx16.r13H
            alias tile_addr_reg = cx16.r14L
            alias tile_bank     = cx16.r14H
            alias tile_addr     = cx16.r15

            alias sprite_byte   = cx16.VERA_DATA0
            alias tile_byte     = cx16.VERA_DATA1

            tile_width = if peek(this + AFF_4BIT) != 0 4 else 8
            tile_size = tile_width * 8
            tile_rows = peek(this + AFF_SPRITE_SIZE) / 8
            tile_columns = tile_rows
            row_next_tile = tile_width * tile_columns
            row_next_set = row_next_tile as uword * 8

            tile_addr_reg = peek(this + AFF_VADDR)
            tile_bank = tile_addr_reg >> 7
            tile_addr = tile_addr_reg & %0111_1100
            tile_addr <<= 9
            tile_addr += peek(this + AFF_TILE_MAP) as uword * tile_size

            cx16.VERA_CTRL = 1
            cx16.VERA_ADDR = tile_addr
            cx16.VERA_ADDR_H = tile_bank | $10 ; increment by one

            cx16.VERA_CTRL = 0
            cx16.VERA_ADDR = peekw(this + AFF_SPRITE_DATA_VADDR)
            cx16.VERA_ADDR_H = peek(this + AFF_SPRITE_DATA_BANK) | $10 ; increment by one

            ; move the bytes from sprite to tile set
            repeat tile_rows {
                sys.pushw(cx16.VERA_ADDR + row_next_set)
                repeat tile_columns {
                    sys.pushw(cx16.VERA_ADDR + tile_width)
                    repeat 8 {
                        sys.pushw(cx16.VERA_ADDR + row_next_tile)
                        repeat tile_width {
                            tile_byte = sprite_byte
                        }
                        cx16.VERA_ADDR = sys.popw()
                    }
                    cx16.VERA_ADDR = sys.popw()
                }
                cx16.VERA_ADDR = sys.popw()
            }
        }
    }

    word_trig_table: 
    %asm{{
        ; entries for 0 through 359
        ; the word[0] is the SINE, get_cosine() gets the COSINE fron the SINE
        ; thw word[1] is the 64_XPOS, get
        ; TBL_SINE/COSINE   TBL_64_XPOS/YPOS
        .word  $0000,  $0000,  $0000    ; 0
        .word  $0009,  $008C,  $FF77    ; 1
        .word  $0012,  $011A,  $FEEF    ; 2
        .word  $001A,  $01AA,  $FE6C    ; 3
        .word  $0024,  $023D,  $FDE9    ; 4
        .word  $002C,  $02D1,  $FD6B    ; 5
        .word  $0035,  $036A,  $FCEE    ; 6
        .word  $003F,  $0403,  $FC74    ; 7
        .word  $0048,  $049E,  $FBFC    ; 8
        .word  $0050,  $053B,  $FB89    ; 9
        .word  $0059,  $05DA,  $FB17    ; 10
        .word  $0061,  $067C,  $FAA7    ; 11
        .word  $006B,  $0720,  $FA3C    ; 12
        .word  $0073,  $07C4,  $F9D3    ; 13
        .word  $007C,  $086C,  $F96C    ; 14
        .word  $0085,  $0914,  $F908    ; 15
        .word  $008D,  $09BE,  $F8A8    ; 16
        .word  $0096,  $0A6B,  $F84A    ; 17
        .word  $009E,  $0B18,  $F7F1    ; 18
        .word  $00A7,  $0BC8,  $F798    ; 19
        .word  $00AF,  $0C79,  $F744    ; 20
        .word  $00B7,  $0D2C,  $F6F4    ; 21
        .word  $00C0,  $0DDF,  $F6A5    ; 22
        .word  $00C8,  $0E93,  $F65A    ; 23
        .word  $00D0,  $0F49,  $F613    ; 24
        .word  $00D8,  $1002,  $F5CD    ; 25
        .word  $00E0,  $10BB,  $F58C    ; 26
        .word  $00E8,  $1174,  $F54F    ; 27
        .word  $00F0,  $122E,  $F513    ; 28
        .word  $00F8,  $12EB,  $F4DB    ; 29
        .word  $0100,  $13A8,  $F4A8    ; 30
        .word  $0108,  $1465,  $F477    ; 31
        .word  $0110,  $1524,  $F448    ; 32
        .word  $0116,  $15E3,  $F41F    ; 33
        .word  $011E,  $16A2,  $F3F7    ; 34
        .word  $0126,  $1764,  $F3D4    ; 35
        .word  $012C,  $1824,  $F3B3    ; 36
        .word  $0135,  $18E6,  $F396    ; 37
        .word  $013C,  $19A9,  $F37C    ; 38
        .word  $0142,  $1A6B,  $F366    ; 39
        .word  $014A,  $1B2E,  $F353    ; 40
        .word  $014F,  $1BF2,  $F344    ; 41
        .word  $0156,  $1CB5,  $F339    ; 42
        .word  $015D,  $1D79,  $F32F    ; 43
        .word  $0163,  $1E3C,  $F32A    ; 44
        .word  $016B,  $1F00,  $F329    ; 45
        .word  $0171,  $1FC4,  $F32A    ; 46
        .word  $0177,  $2088,  $F32F    ; 47
        .word  $017D,  $214C,  $F339    ; 48
        .word  $0183,  $220F,  $F344    ; 49
        .word  $0188,  $22D3,  $F353    ; 50
        .word  $018D,  $2396,  $F366    ; 51
        .word  $0193,  $2457,  $F37C    ; 52
        .word  $0198,  $251A,  $F396    ; 53
        .word  $019F,  $25DC,  $F3B3    ; 54
        .word  $01A4,  $269D,  $F3D4    ; 55
        .word  $01A9,  $275E,  $F3F7    ; 56
        .word  $01AD,  $281E,  $F41F    ; 57
        .word  $01B3,  $28DD,  $F448    ; 58
        .word  $01B6,  $299C,  $F477    ; 59
        .word  $01BC,  $2A58,  $F4A8    ; 60
        .word  $01BF,  $2B15,  $F4DB    ; 61
        .word  $01C5,  $2BD1,  $F513    ; 62
        .word  $01C8,  $2C8D,  $F54F    ; 63
        .word  $01CC,  $2D46,  $F58C    ; 64
        .word  $01D1,  $2DFE,  $F5CD    ; 65
        .word  $01D3,  $2EB6,  $F613    ; 66
        .word  $01D7,  $2F6C,  $F65A    ; 67
        .word  $01DB,  $3021,  $F6A5    ; 68
        .word  $01DD,  $30D5,  $F6F4    ; 69
        .word  $01E2,  $3188,  $F744    ; 70
        .word  $01E5,  $3238,  $F798    ; 71
        .word  $01E6,  $32E8,  $F7F1    ; 72
        .word  $01E9,  $3395,  $F84A    ; 73
        .word  $01EC,  $3441,  $F8A8    ; 74
        .word  $01EF,  $34EB,  $F908    ; 75
        .word  $01F0,  $3595,  $F96C    ; 76
        .word  $01F2,  $363C,  $F9D3    ; 77
        .word  $01F5,  $36E0,  $FA3C    ; 78
        .word  $01F7,  $3784,  $FAA7    ; 79
        .word  $01F8,  $3825,  $FB17    ; 80
        .word  $01F9,  $38C4,  $FB89    ; 81
        .word  $01FB,  $3962,  $FBFC    ; 82
        .word  $01FD,  $39FE,  $FC74    ; 83
        .word  $01FE,  $3A97,  $FCEE    ; 84
        .word  $01FF,  $3B2F,  $FD6B    ; 85
        .word  $01FE,  $3BC3,  $FDE9    ; 86
        .word  $01FF,  $3C55,  $FE6C    ; 87
        .word  $01FF,  $3CE7,  $FEEF    ; 88
        .word  $01FF,  $3D74,  $FF77    ; 89
        .word  $0200,  $3E00,  $0000    ; 90
        .word  $01FF,  $3E89,  $008C    ; 91
        .word  $01FF,  $3F11,  $011A    ; 92
        .word  $01FF,  $3F95,  $01AA    ; 93
        .word  $01FE,  $4017,  $023D    ; 94
        .word  $01FF,  $4096,  $02D1    ; 95
        .word  $01FE,  $4112,  $036A    ; 96
        .word  $01FD,  $418C,  $0403    ; 97
        .word  $01FB,  $4203,  $049E    ; 98
        .word  $01F9,  $4278,  $053B    ; 99
        .word  $01F8,  $42E9,  $05DA    ; 100
        .word  $01F7,  $4359,  $067C    ; 101
        .word  $01F5,  $43C4,  $0720    ; 102
        .word  $01F2,  $442E,  $07C4    ; 103
        .word  $01F0,  $4494,  $086C    ; 104
        .word  $01EF,  $44F7,  $0914    ; 105
        .word  $01EC,  $4558,  $09BE    ; 106
        .word  $01E9,  $45B6,  $0A6B    ; 107
        .word  $01E6,  $4610,  $0B18    ; 108
        .word  $01E5,  $4668,  $0BC8    ; 109
        .word  $01E2,  $46BB,  $0C79    ; 110
        .word  $01DD,  $470D,  $0D2C    ; 111
        .word  $01DB,  $475B,  $0DDF    ; 112
        .word  $01D7,  $47A6,  $0E93    ; 113
        .word  $01D3,  $47EE,  $0F49    ; 114
        .word  $01D1,  $4833,  $1002    ; 115
        .word  $01CC,  $4873,  $10BB    ; 116
        .word  $01C8,  $48B2,  $1174    ; 117
        .word  $01C5,  $48ED,  $122E    ; 118
        .word  $01BF,  $4925,  $12EB    ; 119
        .word  $01BC,  $4959,  $13A8    ; 120
        .word  $01B6,  $498A,  $1465    ; 121
        .word  $01B3,  $49B7,  $1524    ; 122
        .word  $01AD,  $49E2,  $15E3    ; 123
        .word  $01A9,  $4A09,  $16A2    ; 124
        .word  $01A4,  $4A2C,  $1764    ; 125
        .word  $019F,  $4A4D,  $1824    ; 126
        .word  $0198,  $4A6A,  $18E6    ; 127
        .word  $0193,  $4A83,  $19A9    ; 128
        .word  $018D,  $4A99,  $1A6B    ; 129
        .word  $0188,  $4AAD,  $1B2E    ; 130
        .word  $0183,  $4ABC,  $1BF2    ; 131
        .word  $017D,  $4AC8,  $1CB5    ; 132
        .word  $0177,  $4AD1,  $1D79    ; 133
        .word  $0171,  $4AD6,  $1E3C    ; 134
        .word  $016B,  $4AD7,  $1F00    ; 135
        .word  $0163,  $4AD6,  $1FC4    ; 136
        .word  $015D,  $4AD1,  $2088    ; 137
        .word  $0156,  $4AC8,  $214C    ; 138
        .word  $014F,  $4ABC,  $220F    ; 139
        .word  $014A,  $4AAD,  $22D3    ; 140
        .word  $0142,  $4A99,  $2396    ; 141
        .word  $013C,  $4A83,  $2457    ; 142
        .word  $0135,  $4A6A,  $251A    ; 143
        .word  $012C,  $4A4D,  $25DC    ; 144
        .word  $0126,  $4A2C,  $269D    ; 145
        .word  $011E,  $4A09,  $275E    ; 146
        .word  $0116,  $49E2,  $281E    ; 147
        .word  $0110,  $49B7,  $28DD    ; 148
        .word  $0108,  $498A,  $299C    ; 149
        .word  $0100,  $4959,  $2A58    ; 150
        .word  $00F8,  $4925,  $2B15    ; 151
        .word  $00F0,  $48ED,  $2BD1    ; 152
        .word  $00E8,  $48B2,  $2C8D    ; 153
        .word  $00E0,  $4873,  $2D46    ; 154
        .word  $00D8,  $4833,  $2DFE    ; 155
        .word  $00D0,  $47EE,  $2EB6    ; 156
        .word  $00C8,  $47A6,  $2F6C    ; 157
        .word  $00C0,  $475B,  $3021    ; 158
        .word  $00B7,  $470D,  $30D5    ; 159
        .word  $00AF,  $46BB,  $3188    ; 160
        .word  $00A7,  $4668,  $3238    ; 161
        .word  $009E,  $4610,  $32E8    ; 162
        .word  $0096,  $45B6,  $3395    ; 163
        .word  $008D,  $4558,  $3441    ; 164
        .word  $0085,  $44F7,  $34EB    ; 165
        .word  $007C,  $4494,  $3595    ; 166
        .word  $0073,  $442E,  $363C    ; 167
        .word  $006B,  $43C4,  $36E0    ; 168
        .word  $0061,  $4359,  $3784    ; 169
        .word  $0059,  $42E9,  $3825    ; 170
        .word  $0050,  $4278,  $38C4    ; 171
        .word  $0048,  $4203,  $3962    ; 172
        .word  $003F,  $418C,  $39FE    ; 173
        .word  $0035,  $4112,  $3A97    ; 174
        .word  $002C,  $4096,  $3B2F    ; 175
        .word  $0024,  $4017,  $3BC3    ; 176
        .word  $001A,  $3F95,  $3C55    ; 177
        .word  $0012,  $3F11,  $3CE7    ; 178
        .word  $0009,  $3E89,  $3D74    ; 179
        .word  $0000,  $3E00,  $3E00    ; 180
        .word  $FFF7,  $3D74,  $3E89    ; 181
        .word  $FFEE,  $3CE7,  $3F11    ; 182
        .word  $FFE6,  $3C55,  $3F95    ; 183
        .word  $FFDC,  $3BC3,  $4017    ; 184
        .word  $FFD4,  $3B2F,  $4096    ; 185
        .word  $FFCB,  $3A97,  $4112    ; 186
        .word  $FFC1,  $39FE,  $418C    ; 187
        .word  $FFB8,  $3962,  $4203    ; 188
        .word  $FFB0,  $38C4,  $4278    ; 189
        .word  $FFA7,  $3825,  $42E9    ; 190
        .word  $FF9F,  $3784,  $4359    ; 191
        .word  $FF95,  $36E0,  $43C4    ; 192
        .word  $FF8D,  $363C,  $442E    ; 193
        .word  $FF84,  $3595,  $4494    ; 194
        .word  $FF7B,  $34EB,  $44F7    ; 195
        .word  $FF73,  $3441,  $4558    ; 196
        .word  $FF6A,  $3395,  $45B6    ; 197
        .word  $FF62,  $32E8,  $4610    ; 198
        .word  $FF59,  $3238,  $4668    ; 199
        .word  $FF51,  $3188,  $46BB    ; 200
        .word  $FF49,  $30D5,  $470D    ; 201
        .word  $FF40,  $3021,  $475B    ; 202
        .word  $FF38,  $2F6C,  $47A6    ; 203
        .word  $FF30,  $2EB6,  $47EE    ; 204
        .word  $FF28,  $2DFE,  $4833    ; 205
        .word  $FF20,  $2D46,  $4873    ; 206
        .word  $FF18,  $2C8D,  $48B2    ; 207
        .word  $FF10,  $2BD1,  $48ED    ; 208
        .word  $FF08,  $2B15,  $4925    ; 209
        .word  $FF00,  $2A58,  $4959    ; 210
        .word  $FEF8,  $299C,  $498A    ; 211
        .word  $FEF0,  $28DD,  $49B7    ; 212
        .word  $FEEA,  $281E,  $49E2    ; 213
        .word  $FEE2,  $275E,  $4A09    ; 214
        .word  $FEDA,  $269D,  $4A2C    ; 215
        .word  $FED4,  $25DC,  $4A4D    ; 216
        .word  $FECB,  $251A,  $4A6A    ; 217
        .word  $FEC4,  $2457,  $4A83    ; 218
        .word  $FEBE,  $2396,  $4A99    ; 219
        .word  $FEB6,  $22D3,  $4AAD    ; 220
        .word  $FEB1,  $220F,  $4ABC    ; 221
        .word  $FEAA,  $214C,  $4AC8    ; 222
        .word  $FEA3,  $2088,  $4AD1    ; 223
        .word  $FE9D,  $1FC4,  $4AD6    ; 224
        .word  $FE95,  $1F00,  $4AD7    ; 225
        .word  $FE8F,  $1E3C,  $4AD6    ; 226
        .word  $FE89,  $1D79,  $4AD1    ; 227
        .word  $FE83,  $1CB5,  $4AC8    ; 228
        .word  $FE7D,  $1BF2,  $4ABC    ; 229
        .word  $FE78,  $1B2E,  $4AAD    ; 230
        .word  $FE73,  $1A6B,  $4A99    ; 231
        .word  $FE6D,  $19A9,  $4A83    ; 232
        .word  $FE68,  $18E6,  $4A6A    ; 233
        .word  $FE61,  $1824,  $4A4D    ; 234
        .word  $FE5C,  $1764,  $4A2C    ; 235
        .word  $FE57,  $16A2,  $4A09    ; 236
        .word  $FE53,  $15E3,  $49E2    ; 237
        .word  $FE4D,  $1524,  $49B7    ; 238
        .word  $FE4A,  $1465,  $498A    ; 239
        .word  $FE44,  $13A8,  $4959    ; 240
        .word  $FE41,  $12EB,  $4925    ; 241
        .word  $FE3B,  $122E,  $48ED    ; 242
        .word  $FE38,  $1174,  $48B2    ; 243
        .word  $FE34,  $10BB,  $4873    ; 244
        .word  $FE2F,  $1002,  $4833    ; 245
        .word  $FE2D,  $0F49,  $47EE    ; 246
        .word  $FE29,  $0E93,  $47A6    ; 247
        .word  $FE25,  $0DDF,  $475B    ; 248
        .word  $FE23,  $0D2C,  $470D    ; 249
        .word  $FE1E,  $0C79,  $46BB    ; 250
        .word  $FE1B,  $0BC8,  $4668    ; 251
        .word  $FE1A,  $0B18,  $4610    ; 252
        .word  $FE17,  $0A6B,  $45B6    ; 253
        .word  $FE14,  $09BE,  $4558    ; 254
        .word  $FE11,  $0914,  $44F7    ; 255
        .word  $FE10,  $086C,  $4494    ; 256
        .word  $FE0E,  $07C4,  $442E    ; 257
        .word  $FE0B,  $0720,  $43C4    ; 258
        .word  $FE09,  $067C,  $4359    ; 259
        .word  $FE08,  $05DA,  $42E9    ; 260
        .word  $FE07,  $053B,  $4278    ; 261
        .word  $FE05,  $049E,  $4203    ; 262
        .word  $FE03,  $0403,  $418C    ; 263
        .word  $FE02,  $036A,  $4112    ; 264
        .word  $FE01,  $02D1,  $4096    ; 265
        .word  $FE02,  $023D,  $4017    ; 266
        .word  $FE01,  $01AA,  $3F95    ; 267
        .word  $FE01,  $011A,  $3F11    ; 268
        .word  $FE01,  $008C,  $3E89    ; 269
        .word  $FE00,  $0000,  $3E00    ; 270
        .word  $FE01,  $FF77,  $3D74    ; 271
        .word  $FE01,  $FEEF,  $3CE7    ; 272
        .word  $FE01,  $FE6C,  $3C55    ; 273
        .word  $FE02,  $FDE9,  $3BC3    ; 274
        .word  $FE01,  $FD6B,  $3B2F    ; 275
        .word  $FE02,  $FCEE,  $3A97    ; 276
        .word  $FE03,  $FC74,  $39FE    ; 277
        .word  $FE05,  $FBFC,  $3962    ; 278
        .word  $FE07,  $FB89,  $38C4    ; 279
        .word  $FE08,  $FB17,  $3825    ; 280
        .word  $FE09,  $FAA7,  $3784    ; 281
        .word  $FE0B,  $FA3C,  $36E0    ; 282
        .word  $FE0E,  $F9D3,  $363C    ; 283
        .word  $FE10,  $F96C,  $3595    ; 284
        .word  $FE11,  $F908,  $34EB    ; 285
        .word  $FE14,  $F8A8,  $3441    ; 286
        .word  $FE17,  $F84A,  $3395    ; 287
        .word  $FE1A,  $F7F1,  $32E8    ; 288
        .word  $FE1B,  $F798,  $3238    ; 289
        .word  $FE1E,  $F744,  $3188    ; 290
        .word  $FE23,  $F6F4,  $30D5    ; 291
        .word  $FE25,  $F6A5,  $3021    ; 292
        .word  $FE29,  $F65A,  $2F6C    ; 293
        .word  $FE2D,  $F613,  $2EB6    ; 294
        .word  $FE2F,  $F5CD,  $2DFE    ; 295
        .word  $FE34,  $F58C,  $2D46    ; 296
        .word  $FE38,  $F54F,  $2C8D    ; 297
        .word  $FE3B,  $F513,  $2BD1    ; 298
        .word  $FE41,  $F4DB,  $2B15    ; 299
        .word  $FE44,  $F4A8,  $2A58    ; 300
        .word  $FE4A,  $F477,  $299C    ; 301
        .word  $FE4D,  $F448,  $28DD    ; 302
        .word  $FE53,  $F41F,  $281E    ; 303
        .word  $FE57,  $F3F7,  $275E    ; 304
        .word  $FE5C,  $F3D4,  $269D    ; 305
        .word  $FE61,  $F3B3,  $25DC    ; 306
        .word  $FE68,  $F396,  $251A    ; 307
        .word  $FE6D,  $F37C,  $2457    ; 308
        .word  $FE73,  $F366,  $2396    ; 309
        .word  $FE78,  $F353,  $22D3    ; 310
        .word  $FE7D,  $F344,  $220F    ; 311
        .word  $FE83,  $F339,  $214C    ; 312
        .word  $FE89,  $F32F,  $2088    ; 313
        .word  $FE8F,  $F32A,  $1FC4    ; 314
        .word  $FE95,  $F329,  $1F00    ; 315
        .word  $FE9D,  $F32A,  $1E3C    ; 316
        .word  $FEA3,  $F32F,  $1D79    ; 317
        .word  $FEAA,  $F339,  $1CB5    ; 318
        .word  $FEB1,  $F344,  $1BF2    ; 319
        .word  $FEB6,  $F353,  $1B2E    ; 320
        .word  $FEBE,  $F366,  $1A6B    ; 321
        .word  $FEC4,  $F37C,  $19A9    ; 322
        .word  $FECB,  $F396,  $18E6    ; 323
        .word  $FED4,  $F3B3,  $1824    ; 324
        .word  $FEDA,  $F3D4,  $1764    ; 325
        .word  $FEE2,  $F3F7,  $16A2    ; 326
        .word  $FEEA,  $F41F,  $15E3    ; 327
        .word  $FEF0,  $F448,  $1524    ; 328
        .word  $FEF8,  $F477,  $1465    ; 329
        .word  $FF00,  $F4A8,  $13A8    ; 330
        .word  $FF08,  $F4DB,  $12EB    ; 331
        .word  $FF10,  $F513,  $122E    ; 332
        .word  $FF18,  $F54F,  $1174    ; 333
        .word  $FF20,  $F58C,  $10BB    ; 334
        .word  $FF28,  $F5CD,  $1002    ; 335
        .word  $FF30,  $F613,  $0F49    ; 336
        .word  $FF38,  $F65A,  $0E93    ; 337
        .word  $FF40,  $F6A5,  $0DDF    ; 338
        .word  $FF49,  $F6F4,  $0D2C    ; 339
        .word  $FF51,  $F744,  $0C79    ; 340
        .word  $FF59,  $F798,  $0BC8    ; 341
        .word  $FF62,  $F7F1,  $0B18    ; 342
        .word  $FF6A,  $F84A,  $0A6B    ; 343
        .word  $FF73,  $F8A8,  $09BE    ; 344
        .word  $FF7B,  $F908,  $0914    ; 345
        .word  $FF84,  $F96C,  $086C    ; 346
        .word  $FF8D,  $F9D3,  $07C4    ; 347
        .word  $FF95,  $FA3C,  $0720    ; 348
        .word  $FF9F,  $FAA7,  $067C    ; 349
        .word  $FFA7,  $FB17,  $05DA    ; 350
        .word  $FFB0,  $FB89,  $053B    ; 351
        .word  $FFB8,  $FBFC,  $049E    ; 352
        .word  $FFC1,  $FC74,  $0403    ; 353
        .word  $FFCB,  $FCEE,  $036A    ; 354
        .word  $FFD4,  $FD6B,  $02D1    ; 355
        .word  $FFDC,  $FDE9,  $023D    ; 356
        .word  $FFE6,  $FE6C,  $01AA    ; 357
        .word  $FFEE,  $FEEF,  $011A    ; 358
        .word  $FFF7,  $FF77,  $008C    ; 359
    }}
/*
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
        jmp p8b_vtsbank.p8s_affine.p8s_sprite_to_tile_set
        * = next_code_addr    
    }}
*/
}



