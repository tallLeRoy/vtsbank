; sprt2tile.p8
; transform a sprite into a tile format for VERA FX Affine transforms

%import textio
%import floats
%import strings
%import diskio
%import verafx

;%import lk_dbg

%encoding iso
%zeropage basicsafe
%option no_sysinit

main {
    ; this set of addresses are set up for the maximum sprite size of 64x64 256 colors
    ; the tile size will be identical to the sprite size. 
    const ubyte SPRITE_VRAM_ADDR16        = $01
    const uword SPRITE_VRAM_ADDR          = $4000
    const ubyte TILE_VRAM_ADDR16          = $01
    const uword TILE_VRAM_ADDR            = $5000
    const ubyte TILE_FX_VRAM_ADDR         = $15000 >> 9
    const ubyte AFFINE_SPRITE_VRAM_ADDR16 = $01
    const uword AFFINE_SPRITE_VRAM_ADDR   = $6040
    const ubyte MAP_VRAM_ADDR16           = $01
    const uword MAP_VRAM_ADDR             = $F800 ; small area for our 64 bytes of map
    const ubyte MAP_SIZE_16x16            = $00
    const ubyte MAP_SIZE_64x64            = $01

    const ubyte SPRITE_WIDTH_8            = $00
    const ubyte SPRITE_WIDTH_16           = $10
    const ubyte SPRITE_WIDTH_32           = $20
    const ubyte SPRITE_WIDTH_64           = $30

    const ubyte SPRITE_HEIGHT_8           = $00
    const ubyte SPRITE_HEIGHT_16          = $40
    const ubyte SPRITE_HEIGHT_32          = $80
    const ubyte SPRITE_HEIGHT_64          = $C0

    ; bits per pixel    
    const ubyte SPRITE_BPP_4              = $00
    const ubyte SPRITE_BPP_8              = $80 

    const ubyte SPRITE_FRONT              = $0C

    const ubyte SPRITE_PALETTE_GRAY       = $01   
    const ubyte SPRITE_PALETTE_256        = $00

    const ubyte SPRITE_FLIP_X             = $01

    ubyte[] kb_buffer = [0] * 81  
    ubyte[] kb_buffer2 = [0] * 81  
    uword size
    ubyte map_size                   
    uword vera_ptr
    uword sprite_filename
    ubyte bpp                           ; bits per pixel
    ubyte sprite_width               
    ubyte sprite_height
    ubyte palette

    bool  cell_fourbit
    bool  cell_pad
    ubyte cell_rows
    ubyte cell_columns
    ubyte cell_bytes
    ubyte cell_width

    bool is_natural = true
    bool save_0_to_63 = false

    sub start() {
        txt.clear_screen()
        txt.iso()

        defer verafx.affine_f.reset_fx()  ; always exit cleanly

        bool dowork
        dowork = true
        while dowork {
            save_0_to_63 = false
            work()        

            repeat 3 txt.nl()
            txt.print("  Convert another sprite now? (Y/N) ")

            cx16.r0L = txt.waitkey()
            cx16.VERA_DC_VIDEO &= %1011_1111 ; turn off sprites
            when cx16.r0L {
                'Y','y' -> {
                    txt.cls() ; clear screen
                }
                else ->  {
                    dowork = false 
                    repeat 3 txt.nl()
                }
            }
        }
    }

    sub work() {
        
        repeat 4 txt.nl()
        txt.print("  Program to convert a headerless sprite binary file on disk into a tile")
        repeat 2 txt.nl()
        txt.print("  binary file on disk that may be used with VERA FX Affine transforms.")
        repeat 3 txt.nl()
        txt.print("  Enter the name of the sprite file ")
        if txt.input_chars(kb_buffer) == 0 {
            sprite_filename = "turtle_0.bin"
        } else {
            sprite_filename = kb_buffer
        }
        size = diskio.getfilesize(sprite_filename)

        cell_fourbit = false
        cell_pad = false
        when size {
            32 -> { ; 8x8 16 color
                map_size = MAP_SIZE_16x16
                sprite_width = SPRITE_WIDTH_8
                sprite_height = SPRITE_HEIGHT_8
                bpp = SPRITE_BPP_4
                palette = 0

                cell_fourbit = true
                cell_pad = true
                cell_rows = 1
                cell_columns = 1
                cell_bytes = 4
                cell_width = 8
            }
            64 -> { ; 8x8 256 color
                map_size = MAP_SIZE_16x16
                sprite_width = SPRITE_WIDTH_8
                sprite_height = SPRITE_HEIGHT_8
                bpp = SPRITE_BPP_8
                palette = SPRITE_PALETTE_256

                cell_pad = true
                cell_rows = 1
                cell_columns = 1
                cell_bytes = 8
                cell_width = 8
            }
            128 -> { ; 16x16 16 color
                map_size = MAP_SIZE_16x16
                sprite_width = SPRITE_WIDTH_16
                sprite_height = SPRITE_HEIGHT_16
                bpp = SPRITE_BPP_4
;                palette = SPRITE_PALETTE_GRAY
                palette = 0 

                cell_fourbit = true
                cell_rows = 2
                cell_columns = 2
                cell_bytes = 4
                cell_width = 16
            }   
            256 -> { ; 16x16 256 color
                map_size = MAP_SIZE_16x16
                sprite_width = SPRITE_WIDTH_16
                sprite_height = SPRITE_HEIGHT_16
                bpp = SPRITE_BPP_8
                palette = SPRITE_PALETTE_256

                cell_rows = 2
                cell_columns = 2 
                cell_bytes = 8
                cell_width = 16
            }
            512 -> { ; 32x32 16 color
                map_size = MAP_SIZE_64x64
                sprite_width = SPRITE_WIDTH_32
                sprite_height = SPRITE_HEIGHT_32
                bpp = SPRITE_BPP_4
;                palette = SPRITE_PALETTE_GRAY
                palette = 0
;                palette = 3

                cell_fourbit = true
                cell_pad = true
                cell_rows = 4
                cell_columns = 4
                cell_bytes = 4
                cell_width = 32
            }
            1024 -> { ; 32x32 256 color
                map_size = MAP_SIZE_64x64
                sprite_width = SPRITE_WIDTH_32
                sprite_height = SPRITE_HEIGHT_32
                bpp = SPRITE_BPP_8
                palette = SPRITE_PALETTE_256

                cell_pad = true
                cell_rows = 4
                cell_columns = 4
                cell_bytes = 8
                cell_width = 32
            }
            2048 -> { ; 64x64 16 color
                map_size = MAP_SIZE_64x64
                sprite_width = SPRITE_WIDTH_64
                sprite_height = SPRITE_HEIGHT_64
                bpp = SPRITE_BPP_4
;                palette = SPRITE_PALETTE_GRAY
                palette = 8

                cell_fourbit = true
                cell_rows = 8
                cell_columns = 8
                cell_bytes = 4
                cell_width = 64
            }
            4096 -> { ; 64x64 356 color
                map_size = MAP_SIZE_64x64
                sprite_width = SPRITE_WIDTH_64
                sprite_height = SPRITE_HEIGHT_64
                bpp = SPRITE_BPP_8
                palette = SPRITE_PALETTE_256

                cell_rows = 8
                cell_columns = 8
                cell_bytes = 8
                cell_width = 64
            }
            else -> {
                txt.print("did not find a binary sprite file without header.")
            }
        }

        init_VRAM()

        if cell_width == 8 {
            create_affine_tiles8()
        } else {
            create_affine_tiles()
        }

        ubyte[verafx.affine_f.AFF_CONTEXT_SIZE] context

        ubyte rc = verafx.affine_f.create_context(context, 
                                     TILE_VRAM_ADDR16, TILE_VRAM_ADDR,
                                     MAP_VRAM_ADDR16, MAP_VRAM_ADDR,
                                     AFFINE_SPRITE_VRAM_ADDR16, AFFINE_SPRITE_VRAM_ADDR,
                                     if cell_pad cell_width * 2 else cell_width,
                                     cell_fourbit)

        if rc != verafx.affine_f.AFF_CREATE_OK {
            txt.print("Problem with affine_f.create_context return code " )
            txt.print_ub(rc)
            txt.nl()
            return
        }    

        alias af_select = verafx.affine_f.select
        alias af_scale = verafx.affine_f.scale
        alias af_rotate = verafx.affine_f.rotate_d

;        verafx.affine_f.select(context)    
        af_select(context)
 
        float angle 
        float f_inc
        float scale


        f_inc = 3
        scale = 3.0
        repeat 3 {
            angle = 0
            do {
;                verafx.affine_f.scale = scale
;                verafx.affine_f.rotate_d(angle)  ; rotate degrees
                af_scale = scale
                af_rotate(angle)  ; rotate degrees
                angle += f_inc
                scale -= 0.0101
            } until angle == 360
            f_inc += 3
        }

        repeat 34 {
            verafx.affine_f.scale = scale
            verafx.affine_f.rotate_d(0.0)
            scale += 0.101
        }

        repeat 34 {
            verafx.affine_f.scale = scale
            verafx.affine_f.rotate_d(0.0)
            scale -= 0.101
            if scale < 1.0 break
        }

        ; test for blank natural upper left tile for 8 or 4 bit in the sprite
;        uword offset_natural = TILE_VRAM_ADDR + 8 * cell_bytes
        uword offset_natural = TILE_VRAM_ADDR + 8 * 8
        is_natural = true
        repeat 8 * cell_bytes {
            if cx16.vpeek(1,offset_natural) != 0 {
                is_natural = false
                break
            }
            offset_natural++
        }

        if is_natural {
            repeat 3 txt.nl()
            txt.print("  This tile set is naturally blank in the upper left tile.  ")
            repeat 3 txt.nl()
            txt.print("  The tile set is suitable for the 0 position in a multi-tile buffer. ")
;            when txt.waitkey() {
;                'Y','y' -> {
;                            txt.print("Y")
;                            save_0_to_63 = true
;                        }
;                else -> {
;                            txt.print("N")
;                        } 
;            }      
        }

        repeat 3 txt.nl()
        txt.print("  Save as ")
        txt.print(sprite_filename)
;        if save_0_to_63 {
        if is_natural {
            txt.print("-0vts")
        } else {
            txt.print("-vts")
        }

        bool retry = true
        while retry {
            txt.column(10)
            void txt.input_chars(kb_buffer2)

            if diskio.exists(kb_buffer2) { ; test for existing file
                repeat 3 txt.nl()
                txt.print("  The file exists, do you want to overwrite it? Y/N ")
                when txt.waitkey() {
                    'Y','y' -> {
                                txt.print("Y")
                                diskio.delete(kb_buffer2)
                                retry = false
                            }
                    else -> {
                                txt.print("N")
                                ; clear the prompt
                                txt.column(0)
                                repeat 64 cbm.CHROUT(' ')
                                ; set cursor back to the file name entry
                                txt.row(15)
                            } 
                }      
            } else {
                retry = false
            }
        }

        ; calculate the size of the sprite buffer and the Affine tile buffer
        if cell_width < 32 {
            ; sprite buffer size
            cx16.r10 = cell_bytes as uword * 32 ; a 16x16 sprite or 2x2 tiles
            ; Affine tile buffer size
;            cx16.r9 = cx16.r10 + (cell_bytes * 8)  ; 5 tiles of 8 lines
        } else {
            ; sprite buffer size
            cx16.r10 = cell_bytes as uword * 512 ; a 64x64 sprite or 8x8 tiles
            ; Affine tile buffer size
;            cx16.r9 = cx16.r10 + (cell_bytes * 8)  ; 65 tiles of 8 lines
        }
        cx16.r9 = cx16.r10

;        if save_0_to_63 {   
;            void diskio.vsave_raw(kb_buffer2, TILE_VRAM_ADDR16, TILE_VRAM_ADDR + 8 * cell_bytes, cx16.r10 >> 1) 
;        } else {    
;            void diskio.vsave_raw(kb_buffer2, TILE_VRAM_ADDR16, TILE_VRAM_ADDR, cx16.r9 >> 1) 
;        }
        ; save just the tile set, leave off the blank 0 tile
        void diskio.vsave_raw(kb_buffer2, TILE_VRAM_ADDR16, TILE_VRAM_ADDR + 8 * cell_bytes, cx16.r10 >> 1) 

        repeat 21 txt.nl()
        txt.print("  Affine tile buffer size \x9e")
        if save_0_to_63 {   
            txt.print_uw(cx16.r10)
        } else {            
            txt.print_uw(cx16.r9)
        }
        txt.print("\x05 Must begin on multiples of 512 bytes in VRAM")
        repeat 3 txt.nl()
;        txt.print("  Affine tile map of \x9e1 though ")
;        txt.print_ub(if cell_width < 32 4 else 64)
;        txt.print("\x05 Must begin on multiples of 512 bytes in VRAM")
        txt.print("  Affine tile map of \x9e")
        if save_0_to_63 {
            txt.print("0 through 63")
        } else {
            txt.print("1 through 64")
        }
        txt.print("\x05 Must begin on multiples of 512 bytes in VRAM")
        repeat 3 txt.nl()
        if cell_fourbit {
            txt.print("  These Affine tiles contain \x9e4\x05 bits per pixel")
        } else {
            txt.print("  These Affine tiles contain \x9e8\x05 bits per pixel")
        }
        repeat 3 txt.nl()
        txt.print("  Sprite buffer size \x9e")
        txt.print_uw(cx16.r10)
        txt.print("\x05 Must begin on multiples of 32 bytes in VRAM")

    }

    ; set up VRAM for the sprites and the Affine tiles and map
    sub init_VRAM() {
        uword col
        uword row
        uword addr

        ; load the sprite into vram
        void diskio.vload_raw(sprite_filename, SPRITE_VRAM_ADDR16, SPRITE_VRAM_ADDR)
        if sprite_filename == "greenship.spr"
        void diskio.vload_raw("greenship.pal", 1, $fa00 + (3 * 32))

        ; set the VERA FX Affine map 1 - 64 , Affine tile 0 is not in the map
        ; all Affine tile sets produced by this progam use this Affine map
        vera.setdata0(MAP_VRAM_ADDR16| vera.STRIDE_1, MAP_VRAM_ADDR)
        ubyte map_entry = 1
        repeat 64 {
            cx16.VERA_DATA0 = map_entry
            map_entry++
        }
        cx16.VERA_CTRL = %0000_0100  ; DCSEL=2 ADDRSEL=0
        cx16.VERA_FX_MAPBASE = $FC | map_size ; $1f800 >> 9 | map size 
        cx16.VERA_CTRL = 0
    
        ; clear the vram where VERA FX Affine sprite will be generated
        ; size of 1024 is actually 4096 bytes, verafx.clear() operates on 4 bytes long words
        ; 16 is actually 64 bytes used for the blank tile 0
        ; the size is for the largest generated Affine tile set
        verafx.clear(TILE_VRAM_ADDR16,TILE_VRAM_ADDR,$00,1024 + 16)
        verafx.clear(AFFINE_SPRITE_VRAM_ADDR16, AFFINE_SPRITE_VRAM_ADDR,$00,1024)

        ; set up the sprite registers for the pattern sprite
        col = 480
        row = 70
        addr = SPRITE_VRAM_ADDR
        addr >>= 5
        addr |= mkword(SPRITE_VRAM_ADDR16 << 3, 0) ; $0800 for upper bank
        vera_ptr = vera.get_sprite_address(10)
        vera.setdata0(vera.STRIDE_INC_1|vera.BANK_1,vera_ptr)
        cx16.VERA_DATA0 = lsb(addr)
        cx16.VERA_DATA0 = (msb(addr) | bpp) & $8F
        cx16.VERA_DATA0 = lsb(col)
        cx16.VERA_DATA0 = msb(col)   & $03
        cx16.VERA_DATA0 = lsb(row)
        cx16.VERA_DATA0 = msb(row)   & $03
        cx16.VERA_DATA0 = SPRITE_FRONT 
        cx16.VERA_DATA0 = sprite_height | sprite_width | palette

        ; increase the Affine sprite size for 8x8 and 32x32 pixel sprites
        if cell_pad {
            if sprite_width == SPRITE_WIDTH_8 {
                sprite_height = SPRITE_HEIGHT_16
                sprite_width = SPRITE_WIDTH_16
            } else {
                sprite_height = SPRITE_HEIGHT_64
                sprite_width = SPRITE_WIDTH_64
            }
        }

        ; set up the sprite registers for the affine generated sprite
        ; this sprite will turn clockwise
        col = 480
        row = 200
        addr = AFFINE_SPRITE_VRAM_ADDR
        addr >>= 5
        addr |= mkword(AFFINE_SPRITE_VRAM_ADDR16 << 3, 0) ; $0800 for upper bank
        vera_ptr = vera.get_sprite_address(11)
        vera.setdata0(vera.STRIDE_INC_1|vera.BANK_1,vera_ptr)
        cx16.VERA_DATA0 = lsb(addr)
        cx16.VERA_DATA0 = (msb(addr) | bpp) & $8F
        cx16.VERA_DATA0 = lsb(col)
        cx16.VERA_DATA0 = msb(col)   & $03
        cx16.VERA_DATA0 = lsb(row)
        cx16.VERA_DATA0 = msb(row)   & $03
        cx16.VERA_DATA0 = SPRITE_FRONT 
        cx16.VERA_DATA0 = sprite_height | sprite_width | palette

        ; set up the sprite registers for the affine generated sprite
        ; this sprite will turn counter-clockwise because of SPRITE_FLIP_X
        col = 400
        row = 200
        addr = AFFINE_SPRITE_VRAM_ADDR
        addr >>= 5
        addr |= mkword(AFFINE_SPRITE_VRAM_ADDR16 << 3, 0) ; $0800 for upper bank
        vera_ptr = vera.get_sprite_address(12)
        vera.setdata0(vera.STRIDE_INC_1|vera.BANK_1,vera_ptr)
        cx16.VERA_DATA0 = lsb(addr)
        cx16.VERA_DATA0 = (msb(addr) | bpp) & $8F
        cx16.VERA_DATA0 = lsb(col)
        cx16.VERA_DATA0 = msb(col)   & $03
        cx16.VERA_DATA0 = lsb(row)
        cx16.VERA_DATA0 = msb(row)   & $03
        cx16.VERA_DATA0 = SPRITE_FRONT | SPRITE_FLIP_X
        cx16.VERA_DATA0 = sprite_height | sprite_width | palette

        ; now tell VERA to display sprites
        cx16.VERA_CTRL = 0 
        cx16.VERA_DC_VIDEO |= %0100_0000 ; enable any sprites

    }


    ; does the conversion from the displayed pattern sprite intp Affine tiles
    ; there will always be a blank tile 0 in addition to the tiles 1 - n 
    ; for each conversion. So five tiles for 8x8 and 16x16 pixel sprites, 
    ; 65 tiles for 32x32 and 64x64 pixel sprites
    ;
    ; Affine only deals in sizes of 16x16 and 64x64 pixels, so 8x8 and 32x32 
    ; pixel sprites are centered into larger affine tile sets.
    ; all tile sets created by this routine can use a tile map of 1-64
    sub create_affine_tiles() {

        repeat 3 txt.nl()
        txt.print("  Create affine tiles")

        vera.setdata0(SPRITE_VRAM_ADDR16 | vera.STRIDE_1, SPRITE_VRAM_ADDR)
        vera.setdata1(TILE_VRAM_ADDR16 | vera.STRIDE_1, TILE_VRAM_ADDR)

        uword col
        uword line
        uword row
        uword pad
        uword tile_size 

        row = 0
        pad = 0
        tile_size = cell_bytes * 8
        if cell_pad {
            cx16.VERA_ADDR = TILE_VRAM_ADDR + (tile_size * 19)
            repeat 4 {
                repeat 8 {   ; number of lines in a cell
                    col = 0
                    repeat cell_columns { ; number of cells in a row
                        repeat cell_bytes {  ; number of bytes in a cell line
                            cx16.VERA_DATA1 = cx16.VERA_DATA0
                        }
                        cx16.VERA_ADDR += tile_size - cell_bytes
                    }
                    cx16.VERA_ADDR -= tile_size * 4 
                    cx16.VERA_ADDR += cell_bytes
                }
                cx16.VERA_ADDR += tile_size * 7
            }
            return
        }
        if cell_fourbit {
            repeat cell_rows {       ; number of cell rows
                line = 0
                repeat 8 {   ; number of lines in a cell
                    col = 0
                    repeat cell_columns { ; number of cells in a row
                        cx16.VERA_ADDR = TILE_VRAM_ADDR + (cell_bytes * 8) + col + line + row ; for DATA1
                        repeat cell_bytes {  ; number of bytes in a cell line
                            cx16.VERA_DATA1 = cx16.VERA_DATA0
                        }
                        col += cell_bytes * 8
                    }
                    line += cell_bytes
                }
                row += (cell_bytes as uword * cell_width)
            }
        } else {
            repeat cell_rows {       ; number of cell rows
                line = 0
                repeat 8 {   ; number of lines in a cell
                    col = 0
                    repeat cell_columns { ; number of cells in a row
                        cx16.VERA_ADDR = TILE_VRAM_ADDR + (cell_bytes * 8) + col + line + row ; for DATA1
                        repeat 2 {
                            repeat 4 {  ; number of bytes in a cell line
                                cx16.VERA_DATA1 = cx16.VERA_DATA0
                            }
                        }
                        col += cell_bytes * 8
                    }
                    line += cell_bytes
                }
                row += (cell_bytes as uword * cell_width as uword)
            }
        }

        cx16.VERA_CTRL = 0 

        txt.print(" completed")
    } 

    sub create_affine_tiles8() {

        repeat 3 txt.nl()
        txt.print("  Create affine tiles")

        vera.setdata0(SPRITE_VRAM_ADDR16 | vera.STRIDE_1, SPRITE_VRAM_ADDR)
        vera.setdata1(TILE_VRAM_ADDR16 | vera.STRIDE_1, TILE_VRAM_ADDR)

        uword tile_size 

        tile_size = cell_bytes * 8

        ; index in the size of tile 0 + 4 lines of 0
        cx16.VERA_ADDR = TILE_VRAM_ADDR + tile_size + (cell_bytes * 4) 
        repeat 4 {
            repeat cell_bytes / 2 {
                cx16.VERA_DATA1 = 0
            }
            repeat cell_bytes / 2 {
                cx16.VERA_DATA1 = cx16.VERA_DATA0
            }
            cx16.VERA_ADDR += tile_size - cell_bytes
            repeat cell_bytes / 2 {
                cx16.VERA_DATA1 = cx16.VERA_DATA0
            }
            repeat cell_bytes / 2 {
                cx16.VERA_DATA1 = 0
            }
            cx16.VERA_ADDR -= tile_size
        }   

        ; index in the size of tile 0 + 8 lines of 
        cx16.VERA_ADDR = TILE_VRAM_ADDR + tile_size + (cell_bytes * 16)
        repeat 4 {
            repeat cell_bytes / 2 {
                cx16.VERA_DATA1 = 0
            }
            repeat cell_bytes / 2 {
                cx16.VERA_DATA1 = cx16.VERA_DATA0
            }
            cx16.VERA_ADDR += tile_size - cell_bytes
            repeat cell_bytes / 2 {
                cx16.VERA_DATA1 = cx16.VERA_DATA0
            }
            repeat cell_bytes / 2 {
                cx16.VERA_DATA1 = 0
            }
            cx16.VERA_ADDR -= tile_size
        }    

        cx16.VERA_CTRL = 0 

        txt.print(" completed")
    } 

}


diskio {
    %option merge

    sub getfilesize(uword filename) -> uword { ; in r2 and r3 lsb order
        cx16.r5 = 0
        if f_open(filename) {
            void, void, void, void = f_tell()
            f_close()
        }
        return cx16.r2
    }

;    sub exists(str filename) -> bool {
;        defer f_close()
;        return f_open(filename)
;    }

    ; save vram to a file 
    ; specify the save size in words
    sub vsave_raw(uword filenameptr, ubyte startbank, uword startaddress, uword savewords) -> bool {
        
        if not f_open_w(filenameptr) {
            return false
        }
        defer f_close_w()

        cbm.CHKOUT(WRITE_IO_CHANNEL)   ; MCIOUT requires this

        ; set VERA_DATA0 to point to the startbank and address
        cx16.VERA_CTRL = 0
        cx16.VERA_ADDR_H = startbank | %0001_0000  ; INC 1
        cx16.VERA_ADDR   = startaddress

        repeat 2 {
            cx16.r7 = savewords
            while cx16.r7 > 0 {
                void, cx16.r8 = cx16.MCIOUT(cx16.r7L, &cx16.VERA_DATA0, true)
                if_cs return internal_fallback() ; MCIOUT is not in all ROMS
                cx16.r7 -= cx16.r8
            }
        }
        return true

        sub internal_fallback() -> bool {
            repeat savewords {
                cbm.CHROUT(cx16.VERA_DATA0)
                cbm.CHROUT(cx16.VERA_DATA0)
            }
            return true
        }    
    }

}

verafx {
    %option merge

    sub affine_f() {
        const ubyte AFF_CONTEXT_SIZE      = 8   ; 8 bytes for each context
            const ubyte AFF_VADDR             = 0
            const ubyte AFF_MAP_VADDR         = 1
            const ubyte AFF_SPRITE_SIZE       = 2
            const ubyte AFF_SPRITE_DATA_BANK  = 3
            const ubyte AFF_SPRITE_DATA_VADDR = 4   ; uword 
            const ubyte AFF_FX_CTRL           = 6
            const ubyte AFF_4BIT              = 7
    
        const ubyte AFF_CREATE_OK                     = 0
        const ubyte AFF_CREATE_AFFINE_TILE_ADDR_ERROR = 1
        const ubyte AFF_CREATE_AFFINE_MAP_ADDR_ERROR  = 2
        const ubyte AFF_CREATE_SPRITE_SIZE_ERROR      = 3

        const float CORNER_ANGLE = (floats.PI * 5.0) / 4.0

        uword this
        float distance_to_corner 
        word  x_position
        word  y_position
        word  x_inc
        word  y_inc
        ubyte position_offset
        bool  bool_four_bit
        float scale = 1.0

        sub create_context(uword context_addr,  ; ubyte array[AFF_CONTEXT_SIZE] for context
                ubyte affine_tile_bank, uword affine_tile_addr, ; vram address for the AFFINE_TILES
                ubyte affine_map_bank, uword affine_map_addr, ; vram address for the AFFILE_TIME_MAP
                ubyte sprite_data_bank, uword sprite_data_addr, ;vram destination address for rotated sprite 
                ubyte sprite_size,    ; 16, 32 or 64 square pixel sprite
                bool four_bit ) -> ubyte { ; four_bit true for 16 color, false for 256 color sprite

            ; calculate the FX Tile Base Address and save it in the context 
            if affine_tile_addr & %0000_0011_1111_1111 != 0 {
                ; a valid affine address cannot use the lowest 10 bits 
                return AFF_CREATE_AFFINE_TILE_ADDR_ERROR
            }
            uword temp = affine_tile_addr >> 9
            if affine_tile_bank == 1 {
                temp |= $0080     ; set the bank
            }
            poke(context_addr + AFF_VADDR, lsb(temp) | %0000_0010 ) ; Affine clip enable

            ; calculate the FX Map Base Address and save it in the context
            if affine_map_addr & %0000_0011_1111_1111 != 0 {
                ; a valid affine address cannot use the lowest 10 bits 
                return AFF_CREATE_AFFINE_MAP_ADDR_ERROR
            }
            temp = affine_map_addr >> 9
            if affine_map_bank == 1 {
                temp |= $0080     ; set the bank
            }
            poke(context_addr + AFF_MAP_VADDR, lsb(temp) | if sprite_size == 16 0 else 1 ) ; set map_size 2x2 or 8x0

            ; save the sprite size
            if not sprite_size in [16,32,64] {
                return AFF_CREATE_SPRITE_SIZE_ERROR
            }
            poke(context_addr + AFF_SPRITE_SIZE, sprite_size)

            ; save the sprite data address
            poke(context_addr + AFF_SPRITE_DATA_BANK, sprite_data_bank | %0011_0000 ) ; increment 4
            pokew(context_addr + AFF_SPRITE_DATA_VADDR, sprite_data_addr)  ; uword value

            ; cache write, cache fill, affine helper mode
            poke(context_addr + AFF_FX_CTRL, (if four_bit %0000_0100 else 0) |  %0110_0011 )

            poke(context_addr + AFF_4BIT, four_bit as ubyte)

            return AFF_CREATE_OK
        }

        sub select(uword context) {
            this = context 

            ; set the FX Tile Base Address 
            cx16.VERA_CTRL = %0000_0100  ; DCSEL=2 ADDRSEL=0
            cx16.VERA_FX_TILEBASE = peek(this + AFF_VADDR)
            ; set the FX Map Base Address
            cx16.VERA_FX_MAPBASE = peek(this + AFF_MAP_VADDR)

            ; set the distance to the corner once here
            ; distance_to_corner = SQUARE_ROOT_2 * 32.0
            distance_to_corner = 45.254834   ; for sprite size 32 and 64
            position_offset = 32
            if peek(this + AFF_SPRITE_SIZE) == 16 {
                distance_to_corner /= 4.0
                position_offset /= 4
            }

            bool_four_bit = peek(this + AFF_4BIT) as bool
        }

        ; rotate to a given degree, does not need to be an integer value
        sub rotate_d(float degree) {
            rotate_internal(floats.rad(360 - degree))
        }

        sub rotate_r(float radian) {
            rotate_internal(radian)
        }

        ; rotate to a given radian
        sub rotate_internal(float radian) {
            ; set the FX Tile Base Address 
            cx16.VERA_CTRL = %0000_0100  ; DCSEL=2 ADDRSEL=0
            cx16.VERA_FX_CTRL = peek(this + AFF_FX_CTRL)

            ; set the destination for the rotated sprite
            cx16.VERA_CTRL = 0
            cx16.VERA_ADDR = peekw(this + AFF_SPRITE_DATA_VADDR)
            cx16.VERA_ADDR_H = peek(this + AFF_SPRITE_DATA_BANK) | %0011_0000 ; increment 4

            ; set the initial position of x and y
            set_start(radian) 

            ; for every line in the sprite
            repeat position_offset * 2 {       ; for each line              
                repeat position_offset / 4 {   ; each tile - 2 for 16 bit, 8 for 32 and 64 bit
                    ; set up for the four byte cache transfers for each tile
                    ; four bit modes take 1 cache entry of 8 pixels
                    ; eight bit modes take 2 cache entries of 4 pixels
                    repeat if bool_four_bit 1 else 2 {
                        unroll 4 {
                            cx16.r0L = cx16.VERA_DATA1
                        }
                        if bool_four_bit {
                            unroll 4 {
                                cx16.r0L = cx16.VERA_DATA1
                            }
                        }
                        cx16.VERA_DATA0 = 0
                    }
                }
                set_next()
            }

            reset_fx()

            return

            sub set_start(float theta) {
                word w_cosine = ((floats.cos(theta) * scale) * 512.0) as word 
                word w_sine = ((floats.sin(theta) * scale) * 512.0) as word

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

            sub set_next() {
                ; move to new position
                x_position += x_inc
                y_position += y_inc

                ; set the new integer position
                cx16.VERA_CTRL = %0000_1000     ; DCSEL = 4, ADDRSEL = 0
                %asm{{
                    ; set FX_X_POS
;                    lda  p8v_x_position+1
                    lda  verafx.affine_f.x_position+1
                    sta  cx16.VERA_FX_X_POS
                    bpl  +
                    lda  #$07
                    bra  ++
                +   lda  #$00 
                +   sta  cx16.VERA_FX_X_POS+1

                    ; set FX_Y_POS
;                    lda  p8v_y_position+1
                    lda  verafx.affine_f.y_position+1
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
    }

}

vera {
    %option ignore_unused

    ; the stride and bank make up ADDR_H
    const ubyte STRIDE_0   = %0000_0000
    const ubyte STRIDE_1   = %0001_0000
    const ubyte STRIDE_2   = %0010_0000
    const ubyte STRIDE_4   = %0011_0000
    const ubyte STRIDE_8   = %0100_0000
    const ubyte STRIDE_16  = %0101_0000
    const ubyte STRIDE_32  = %0110_0000
    const ubyte STRIDE_64  = %0111_0000
    const ubyte STRIDE_128 = %1000_0000
    const ubyte STRIDE_256 = %1001_0000
    const ubyte STRIDE_512 = %1010_0000
    const ubyte STRIDE_40  = %1011_0000
    const ubyte STRIDE_80  = %1100_0000
    const ubyte STRIDE_160 = %1101_0000
    const ubyte STRIDE_320 = %1110_0000
    const ubyte STRIDE_640 = %1111_0000

    const ubyte STRIDE_INC = %0000_0000
    const ubyte STRIDE_DEC = %0000_1000
    
    const ubyte STRIDE_INC_1    = STRIDE_1   | STRIDE_INC
    const ubyte STRIDE_INC_2    = STRIDE_2   | STRIDE_INC
    const ubyte STRIDE_INC_4    = STRIDE_4   | STRIDE_INC
    const ubyte STRIDE_INC_8    = STRIDE_8   | STRIDE_INC
    const ubyte STRIDE_INC_16   = STRIDE_16  | STRIDE_INC
    const ubyte STRIDE_INC_32   = STRIDE_32  | STRIDE_INC
    const ubyte STRIDE_INC_64   = STRIDE_64  | STRIDE_INC
    const ubyte STRIDE_INC_128  = STRIDE_128 | STRIDE_INC
    const ubyte STRIDE_INC_256  = STRIDE_256 | STRIDE_INC
    const ubyte STRIDE_INC_512  = STRIDE_512 | STRIDE_INC
    const ubyte STRIDE_INC_40   = STRIDE_40  | STRIDE_INC
    const ubyte STRIDE_INC_80   = STRIDE_80  | STRIDE_INC
    const ubyte STRIDE_INC_160  = STRIDE_160 | STRIDE_INC
    const ubyte STRIDE_INC_320  = STRIDE_320 | STRIDE_INC
    const ubyte STRIDE_INC_640  = STRIDE_640 | STRIDE_INC

    const ubyte NIBBLE_INC      = %0000_0100

    const ubyte STRIDE_DEC_1    = STRIDE_1   | STRIDE_DEC
    const ubyte STRIDE_DEC_2    = STRIDE_2   | STRIDE_DEC
    const ubyte STRIDE_DEC_4    = STRIDE_4   | STRIDE_DEC
    const ubyte STRIDE_DEC_8    = STRIDE_8   | STRIDE_DEC
    const ubyte STRIDE_DEC_16   = STRIDE_16  | STRIDE_DEC
    const ubyte STRIDE_DEC_32   = STRIDE_32  | STRIDE_DEC
    const ubyte STRIDE_DEC_64   = STRIDE_64  | STRIDE_DEC
    const ubyte STRIDE_DEC_128  = STRIDE_128 | STRIDE_DEC
    const ubyte STRIDE_DEC_256  = STRIDE_256 | STRIDE_DEC
    const ubyte STRIDE_DEC_512  = STRIDE_512 | STRIDE_DEC
    const ubyte STRIDE_DEC_40   = STRIDE_40  | STRIDE_DEC
    const ubyte STRIDE_DEC_80   = STRIDE_80  | STRIDE_DEC
    const ubyte STRIDE_DEC_160  = STRIDE_160 | STRIDE_DEC
    const ubyte STRIDE_DEC_320  = STRIDE_320 | STRIDE_DEC
    const ubyte STRIDE_DEC_640  = STRIDE_640 | STRIDE_DEC


    const ubyte BANK_0     = %0000_0000
    const ubyte BANK_1     = %0000_0001

    const ubyte SPRITES_ENABLE = %0100_0000
    const uword SPRITE_REGS = $FC00    ; actually $1FC00

    const uword SCREEN_CODE_BASE = $F000 ; actually $1F000

    &word  VERA_FX_X_POS  = $9F29
    &word  VERA_FX_Y_POS  = $9F2B
    

    asmsub setaddr(ubyte stride_bank @R0, uword addr @R1) {
        %asm{{
            lda cx16.r0L     ; delta forward or back in top 4 bits
            sta cx16.VERA_ADDR_H
            lda cx16.r1H
            sta cx16.VERA_ADDR_M
            lda cx16.r1L
            sta cx16.VERA_ADDR_L
            rts
        }}
    }

    asmsub setdata0(ubyte stride_bank @R0, uword addr @R1) {
        %asm{{
            stz cx16.VERA_CTRL
            jmp p8s_setaddr
        }}
    }

    asmsub setdata1(ubyte stride_bank @R0, uword addr @R1) {
        %asm{{
            lda #$01
            sta cx16.VERA_CTRL
            jmp p8s_setaddr
        }}
    }

    sub get_sprite_address(uword sprite_index) -> uword {
        return SPRITE_REGS + sprite_index * 8
    }

}

