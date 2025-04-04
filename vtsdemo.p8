; vtsdemo.p8   VERA FX Tile Set demo by tallLeRoy

%import syslib
%import textio
%import floats
%import diskio
%import verafx
%import vtsimport    ; for rotate and scale and shear of VERA tile sets
%import gfx_hires
%import sprites

%encoding iso
%zeropage basicsafe
%option no_sysinit

main {
    ; the VRAM space for the tile set is one tile larger than the sprite
    ; to insure tile zero is always blank
    const ubyte TILE_VRAM_BANK            = $01     ; $15000 - $1603F    
    const uword TILE_VRAM_ADDR            = $5000   ; must start on 2048 byte multiples
    const uword TILE_VRAM_SIZE            = $1040   ; 4160

    ; the VRAM space for a 64x64 256 color sprite.
    ; this demo will use the same location for any smaller sprite
    const ubyte SPRITE_VRAM_BANK          = $01     ; $14000 - $14FFF
    const uword SPRITE_VRAM_ADDR          = $4000   ; must start on 32 byte multiples
    const uword SPRITE_VRAM_SIZE          = $1000   ; 4096

    ubyte figure

    ubyte[vts.VTS_CONTEXT_SIZE] context

    ubyte sprite_size
    ubyte sprite_colors
    ubyte sprite_palette
    uword vts_name
    uword palette_name
    uword tile_offset
    bool  pad_tile_set

    sub start() {

        cbm.CINT()
        txt.clear_screen()
        txt.iso()

        ; load VERA Tile Set feature for rotate, scale and shear into a RAM bank
        if not vts.load_vts_into_bank(5) {
            txt.print("*** Failed to load VERA Tile Set feature into High RAM ***")
            sys.exit(255)   ; failed to load required feature
        }

        ; save off the full default palette
        uword default_palette = memory("dp",32 * 16,1)
        uword src = $FA00
        uword dest = default_palette
        repeat 32 * 16 {
            @(dest) = cx16.vpeek(1,src)
            dest++
            src++
        }

        ; reset the sprite and tile set buffers to zero
        verafx.clear(TILE_VRAM_BANK, TILE_VRAM_ADDR, $00, TILE_VRAM_SIZE / 4)    
        verafx.clear(SPRITE_VRAM_BANK, SPRITE_VRAM_ADDR, $00, SPRITE_VRAM_SIZE / 4 )    

        alias b_rc = cx16.r14L  ; byte return code
        alias w_rc = cx16.r15   ; word return code

/*
        ; code to create the trig_table
        const float CORNER_ANGLE = (floats.PI * 5.0) / 4.0
        const float POSITION_OFFSET_64 = 31.0
        const float DISTANCE_TO_CORNER_64 = 1.414214 * POSITION_OFFSET_64
        diskio.delete("trig_table.txt")
        if diskio.f_open_w("trig_table.txt") {
            word  angle
            float theta
            word[4] @nosplit table_entry
            for angle in 0.0 to 359.0 {
                theta = floats.rad(angle as float)
;                table_entry[0] = (floats.cos(theta) * 512.0) as word 
                table_entry[1] = (floats.sin(theta) * 512.0) as word
                table_entry[2] = (((floats.cos(CORNER_ANGLE + theta as float) * DISTANCE_TO_CORNER_64) + (POSITION_OFFSET_64 as float)) * 256.0) as word
                table_entry[3] = (((floats.sin(CORNER_ANGLE + theta as float) * DISTANCE_TO_CORNER_64) + (POSITION_OFFSET_64 as float)) * 256.0) as word
                diskio.f_write("\n        .word  $", 17)
                conv.str_uwhex(table_entry[1] as uword)
                diskio.f_write(conv.string_out, 4)
                diskio.f_write(",  $", 4)
                conv.str_uwhex(table_entry[2] as uword)
                diskio.f_write(conv.string_out, 4)
                diskio.f_write(",  $", 4)
                conv.str_uwhex(table_entry[3] as uword)
                diskio.f_write(conv.string_out, 4)
                diskio.f_write("    ; ", 6)
                conv.str_w(angle as word)
                diskio.f_write(conv.string_out, 4)
            }
            diskio.f_close_w()
        }
*/
        do {
            ; restore the default palette 
            dest = $FA00
            src = default_palette
            repeat 32 * 16 {
                cx16.vpoke(1,dest,@(src))
                dest++
                src++
            }            

            ; clear out the sprite registers we used
            sprites.reset(1,12)        

            ; display the choices for the user
            repeat 3 txt.nl()
            txt.print("  Select the tile set for demonstration.")
            repeat 3 txt.nl()
            txt.print("  1 - 64x64 256 color sprite ")
            repeat 3 txt.nl()
            txt.print("  2 - 64x64 16 color sprite ")
            repeat 3 txt.nl()
            txt.print("  3 - 32x32 256 color sprite - most capable ")
            repeat 3 txt.nl()
            txt.print("  4 - 32x32 16 color sprite - smaller with most capabilities")
            repeat 3 txt.nl()
            txt.print("  5 - 16x16 256 color sprite ")
            repeat 3 txt.nl()
            txt.print("  6 - 16x16 16 color sprite ")
            repeat 3 txt.nl()
            txt.print("  7 - 8x8 256 color sprite ")
            repeat 3 txt.nl()
            txt.print("  8 - 8x8 16 color sprite ")
            repeat 3 txt.nl()
            txt.print("  9 - Rotate a dozen figures at once ")
            repeat 3 txt.nl()
            txt.print("  C - Convert Sprite to Tile Set ")
            repeat 3 txt.nl()
            txt.print("  F - The tale of the frog and the fly ")
            repeat 3 txt.nl()
            txt.print("  G - Ghosting")
            repeat 3 txt.nl()
            txt.print("  0 - Exit the demonstration ")
            repeat 3 txt.nl()
            txt.print("  Enter the number of your choice [0-6] ")
            ubyte key = txt.waitkey() - $30
            txt.print_ub(key)
            when key {
                1,2,3,4,5,6,7,8,9,19,22,23,51,54,55 -> {
                            figure = key
                        } 
                else -> {
                    figure = 0
                }
            }      

            if figure != 0 {
                pad_tile_set = false
                palette_name = 0

                when figure {
                    1 -> {
                        ; the largest figure, an X16 logo in 256 colors
                        sprite_size   = sprites.SIZE_64
                        sprite_colors = sprites.COLORS_256
                        sprite_palette = 0
                        vts_name = "resource/logo64.bin-vts"
                        palette_name = 1
                        tile_offset = 64
                        pad_tile_set = true
                    }
                    2 -> {
                        ; the largest turtle 16 colors
                        sprite_size   = sprites.SIZE_64
                        sprite_colors = sprites.COLORS_16
                        sprite_palette = 8
                        vts_name = "resource/turtle-0.bin-0vts"
                        tile_offset = 0
                    }
                    3 -> { 
                        ; the large pink ghost in 256 colors
                        sprite_size   = sprites.SIZE_64
                        sprite_colors = sprites.COLORS_256
                        sprite_palette = 0
                        vts_name = "resource/lghost8.bin-0vts"
                        palette_name = 0
                        tile_offset = 0
                    }
                    4 -> {
                        ; the green space ship in 16 colors
                        sprite_size = sprites.SIZE_64
                        sprite_colors = sprites.COLORS_16
                        sprite_palette = 3
                        vts_name = "resource/greenship.spr-0vts"
                        palette_name = "resource/greenship.pal"
                        tile_offset = 0
                    }
                    5 -> {
                        ; the small pink ghost in 256 colors
                        sprite_size   = sprites.SIZE_16
                        sprite_colors = sprites.COLORS_256
                        sprite_palette = 0
                        vts_name = "resource/t9.bin-vts"
                        palette_name = 0
                        tile_offset = 64
                        pad_tile_set = true
                    }
                    6 -> {
                        ; the small red ghost in 16 colors
                        sprite_size   = sprites.SIZE_16
                        sprite_colors = sprites.COLORS_16
                        sprite_palette = 0
                        vts_name = "resource/ghost.bin-vts"
                        palette_name = 0
                        tile_offset = 32
                        pad_tile_set = true
                    }
                    7 -> {
                        ; the small mouse hand in 256 colors
                        sprite_size   = sprites.SIZE_16
                        sprite_colors = sprites.COLORS_256
                        sprite_palette = 0
                        vts_name = "resource/pound8.bin-vts"
                        palette_name = 0
                        tile_offset = 64
                        pad_tile_set = true
                    }
                    8 -> {
                        ; the small x16 butterfly in 16 colors
                        sprite_size   = sprites.SIZE_16
                        sprite_colors = sprites.COLORS_16
                        sprite_palette = 0
                        vts_name = "resource/sbfly.bin-vts"
                        palette_name = 0
                        tile_offset = 32
                        pad_tile_set = true
                    }
                    9 -> {
                        txt.cls()
                        dozen.all_at_once()      ; 64x64 + 16x16
                        void txt.waitkey()
                        continue
                    }   
                    19,51 -> {    ; c or C 
                        txt.cls()
                        conversions.user_sprite() 
                        void txt.waitkey()
                        txt.cls()
                        continue
                    }                 
                    22,54 -> {     ; f or F
                        txt.cls()
                        frogfly.frog_n_fly()
                        void txt.waitkey()
                        txt.cls()
                        continue
                    }    
                    23,55 -> {     ; g or G
                        txt.cls()
                        ghosting.ghost()
                        void txt.waitkey()
                        txt.cls()
                        continue
                    }                
                }

                ; create a sprite to show our rotations and scaling
                sprites.init(1,
                            SPRITE_VRAM_BANK, SPRITE_VRAM_ADDR,
                            sprite_size, sprite_size,
                            sprite_colors, sprite_palette)

                if pad_tile_set {
                    verafx.clear(TILE_VRAM_BANK, TILE_VRAM_ADDR, $00, tile_offset / 4)  
                }               

                ; load the VERA tile set into VRAM
                if not diskio.vload_raw(vts_name, TILE_VRAM_BANK, TILE_VRAM_ADDR + tile_offset) {
                    txt.print("*** Failed to load Tile Set ")
                    txt.print(vts_name)
                    txt.print(" into VERA RAM. ***")
                    txt.nl()
                    continue
                }

                ; load a palette if needed for the figure
                if not palette_name in [0 as uword,1 as uword] {
                    if not diskio.vload_raw(palette_name, 1, $FA00 + (3 * 32)) {
                        txt.print("*** Failed to load Sprite Palette ")
                        txt.print(palette_name) 
                        txt.print(" into VERA RAM. Colors may be wrong. ***")
                    }
                }

                ; set up size and color depth to create a context for this figure
                ubyte ssize = 16  ; sprite size
                if sprite_size == sprites.SIZE_64 {
                    ssize = 64
                }

                bool fourbit = true ; 1 = four bit
                if sprite_colors == sprites.COLORS_256 {
                    fourbit = false
                }

                ; create a context for our tile set
                ; use a different context for each figure
                ubyte ccrc = vts.create_context( context,
                                    TILE_VRAM_BANK, TILE_VRAM_ADDR,
                                    SPRITE_VRAM_BANK, SPRITE_VRAM_ADDR,
                                    ssize, fourbit, tile_offset)

                void check_ccrc(1, ccrc)

                ; focus vts on this tile set
                vts.select(context)

                ; generate the sprite from the tile set with no transforms
                vts.rotate(0)

                ; position the sprite on screen
                sprites.pos(1, 320,240)                       
    
                txt.cls()

                ; perform some transforms
                show_capabilities()

                ; get ready to redisplay the selection screen
                txt.cls()

                ; now tell VERA to hide sprites
                sprites.disablesprites()

                ; put the VERA back to a 'normal' condition
                vts.reset_fx()

                ; reset the sprite and tile set buffers to zero
                verafx.clear(TILE_VRAM_BANK, TILE_VRAM_ADDR, $00, TILE_VRAM_SIZE / 4)    
                verafx.clear(SPRITE_VRAM_BANK, SPRITE_VRAM_ADDR, $00, SPRITE_VRAM_SIZE / 4)    

            }

        } until figure == 0  ; exit request

        txt.iso_off()
        txt.cls()
        
    }

    float scale
    word  degree

    sub show_capabilities() {

        txt.cls() 

        repeat 3 txt.nl()
        txt.print("  Here is the VERA FX Affine tile set shown without scaling or rotation.")
        repeat 3 txt.nl()
        txt.print("  Press any key to animate a horizontal shear on the tile set.")

        void txt.waitkey()

        word value = 0
        repeat 4 {
            repeat 28 {
                vts.shear_h(value)
                value += 6
            }
            repeat 56 {
                vts.shear_h(value)
                value -= 6
            }
            repeat 28 {
                vts.shear_h(value)
                value += 6
            }
            vts.shear_h(value)
        }

        txt.cls() 
        repeat 3 txt.nl()
        txt.print("  Here is the VERA FX Affine tile set shown without scaling or rotation.")
        repeat 3 txt.nl()
        txt.print("  Press any key to animate a vertical shear on the tile set.")

        void txt.waitkey()

        repeat 4 {
            repeat 28 {
                vts.shear_v(value)
                value += 4
            }
            repeat 56 {
                vts.shear_v(value)
                value -= 4
            }
            repeat 28 {
                vts.shear_v(value)
                value += 4
            }
            vts.shear_v(value)
        }

        txt.cls() 
        repeat 3 txt.nl()
        txt.print("  Here is the VERA FX Affine tile set shown without scaling or rotation.")
        repeat 3 txt.nl()
        txt.print("  Press any key to scale down the tile set.")
      
        void txt.waitkey()

        scale = 1.0
        vts.scale(scale)
        ; resize the sprite down 
        repeat 25 {
            scale += 0.101
            vts.scale(scale)
            vts.rotate(0)
        }

        txt.cls() 
        repeat 3 txt.nl()
        txt.print("  Here is the tile set at the smallest practical limit, Scale of about 3.0.")
        repeat 3 txt.nl()
        txt.print("  Press any key to resize the tile set back to original size.")

        void txt.waitkey()
    
        ; resize the sprite up again
        repeat 25 {
            scale -= 0.101
            vts.scale(scale)
            vts.rotate(0)
        }

        txt.cls() 
        repeat 3 txt.nl()
        txt.print("  Here is the tile set at the original size with a Scale of 1.0.")
        repeat 3 txt.nl()
        txt.print("  Press any key to resize the tile set larger than original.")

        void txt.waitkey()

        ; increase the sprite size over original
        repeat 5 {
            scale -= 0.101 / 2
            vts.scale(scale)
            vts.rotate(0)
        }

        txt.cls() 
        repeat 3 txt.nl()
        txt.print("  Here is the tile set at the largest practical limit, Scale of about 0.75.")
        repeat 3 txt.nl()
        txt.print("  Press any key to resize the tile set back to the original size.")

        void txt.waitkey()

        ; decrease the sprite size back to original
        repeat 5 {
            scale += 0.101 / 2
            vts.scale(scale)
            vts.rotate(0)
        }

        txt.cls() 
        repeat 3 txt.nl()
        txt.print("  Back to original size with no rotation.")
        repeat 3 txt.nl()
        txt.print("  The sprite may be rotated to any floating point or integer degree.") 
        repeat 3 txt.nl()
        txt.print("  Press any key to rotate directly to 24.5 degrees.")

        void txt.waitkey()

        scale = 1.0
        vts.scale(scale)
        vts.rotate_f(24.5)

        txt.cls() 
        repeat 3 txt.nl()
        txt.print("  Here is the tile set with a 24.5 degree rotation.")
        repeat 3 txt.nl()
        txt.print("  Press any key to animate clockwise rotation back to 24.5 degrees.") 

        void txt.waitkey()

        float fdegree = 24.5 + 8.0
        repeat 360 / 8 {
            vts.rotate_f(fdegree)
            fdegree += 8.0
        }
        
        txt.cls() 
        repeat 3 txt.nl()
        txt.print("  Here is the tile set back to a 24.5 degree rotation.")
        repeat 3 txt.nl()
        txt.print("  Press any key to animate counter-clockwise rotation back to 0 degrees.") 

        void txt.waitkey()

        degree = 24 - 8
        repeat 3 + 360 / 8 {
            vts.rotate(degree)
            degree -= 8                
        }

        txt.cls() 
        repeat 3 txt.nl()
        txt.print("  Here is the tile set back to no scale or rotation.")
        repeat 3 txt.nl()
        txt.print("  Press any key to animate rotation with scaling.") 

        void txt.waitkey()

        scale = 1.0
        vts.scale(scale)
        degree = 0 + 8
        repeat 360 / 8 {
            vts.rotate(degree)
            scale += 0.060
            vts.scale(scale)
            degree += 8
        }
        repeat 360 / 8 {
            vts.rotate(degree)
            scale -= 0.060
            vts.scale(scale)
            degree -= 8
        }
        scale = 1.0
        vts.scale(scale)
        vts.rotate(0)

        txt.cls() 
        repeat 3 txt.nl()
        txt.print("  Here is the tile set is back to no scale or rotation.")
        repeat 3 txt.nl()
        txt.print("  Press any key show four sprites using the same tile set.") 

        void txt.waitkey()

        ; Create more sprites that use our tile set generated buffer
        ; but change the palette and flip things around to create 
        ; another look. You could also create a number of sprites that 
        ; use the same tile set generated buffer, but are at different
        ; positions on the screen. All would rotate and scale at the 
        ; same time.
        sprites.init(2,
                    SPRITE_VRAM_BANK, SPRITE_VRAM_ADDR,
                    sprite_size, sprite_size,
                    sprite_colors, 
                    if palette_name == 0 1 else sprite_palette)
        sprites.flipx(2, true)                        
        sprites.pos(2, 260, 240)

        sprites.init(3,
                    SPRITE_VRAM_BANK, SPRITE_VRAM_ADDR,
                    sprite_size, sprite_size,
                    sprite_colors, 
                    if palette_name == 0 5 else sprite_palette)
        sprites.flipx(3, true)            
        sprites.flipy(3, true)
        sprites.pos(3, 380, 240)

        sprites.init(4,
                    SPRITE_VRAM_BANK, SPRITE_VRAM_ADDR,
                    sprite_size, sprite_size,
                    sprite_colors, 
                    if palette_name == 0 7 else sprite_palette)
        sprites.flipy(4, true)
        sprites.pos(4, 320, 320)


        txt.cls() 
        repeat 3 txt.nl()
        txt.print("  Here is the tile set back to no scale or rotation with four sprites.")
        repeat 3 txt.nl()
        txt.print("  The palette altered left sprite has sprite H_FLIP set,") 
        repeat 2 txt.nl()
        txt.print("  it will rotate counter-clockwise.")
        repeat 3 txt.nl()
        txt.print("  The center sprite is without sprite transforms.")
        repeat 3 txt.nl()
        txt.print("  The palette altered right sprite has sprite H_FLIP and V_FLIP set,")
        repeat 2 txt.nl()
        txt.print("  it will rotate clockwise.")
        repeat 3 txt.nl()
        txt.print("  The palette altered lower sprite has spite V_FLIP set,")
        repeat 2 txt.nl()
        txt.print("  it will rotate counter-clockwise.")
        repeat 3 txt.nl()
        txt.print("  Press any key show four sprites rotating using the same tile set.") 

        void txt.waitkey()

        degree = 0 + 8
        repeat 360 / 4 {
            vts.rotate(degree)
            degree += 8
        }

        txt.cls() 
        repeat 3 txt.nl()
        txt.print("  Here is the tile set is back to no scale or rotation.")
        repeat 3 txt.nl()
        txt.print("  Press any key to animate rotation with scaling.") 

        void txt.waitkey()

        scale = 1.0
        vts.scale(scale)
        degree = 0.0 + 8
        repeat 360 / 8 {
            vts.rotate(degree)
            scale += 0.060
            vts.scale(scale)
            degree += 8
        }
        repeat 360 / 8 {
            vts.rotate(degree)
            scale -= 0.060
            vts.scale(scale)
            degree -= 8
        }
        scale = 1.0
        vts.scale(scale)
        vts.rotate(0)

        txt.cls()
        repeat 3 txt.nl()
        txt.print("  Press any key to return to the menu.")

        void txt.waitkey()

    }

    ; check the vts.create_context return value
    sub check_ccrc(ubyte id, ubyte ccrc) -> bool {
        bool rc = false
        uword message = 0
        when ccrc {
            vts.VTS_CREATE_OK -> rc = true  
            vts.VTS_CREATE_TILE_ADDR_ERROR ->  message = "Tile buffer address must align with 2048 byte boundry."
            vts.VTS_CREATE_SPRITE_ADDR_ERROR -> message = "Sprite buffer address must align with 32 byte boundry."
            vts.VTS_CREATE_SPRITE_SIZE_ERROR -> message = "Sprite size must be 16 or 64."  
            vts.VTS_CREATE_TILE_OFFSET_ERROR -> message = "Tile offset error, offest is too large."
        }
        if message != 0 {
            txt.print("-- Error found with vts.create_context() return code, id ")
            txt.print_ub(id)
            txt.print(". ")
            txt.print(message)
        }
        return rc
    }
}

; additions to the Prog8 sprites library, may not be needed in future Prog8 releases
sprites {
    %option merge

    ; disable all sprite rendering (including a mouse pointer if present)
    sub disablesprites() {
        cx16.VERA_DC_VIDEO &= ~%0100_0000 ; disable any and all sprites     
    }
}

; special purpose demonstration of 4 64x64 figures at once 
dozen {

    ; buffers for four 64 x 64 sprites
    const ubyte SPRITE_VRAM_BANK1          = $01                ; turtle_0
    const uword SPRITE_VRAM_ADDR1          = $3000  
    const uword SPRITE_VRAM_SIZE1          = 32 * 64

    const ubyte SPRITE_VRAM_BANK2          = SPRITE_VRAM_BANK1  ; enemy_type_d
    const uword SPRITE_VRAM_ADDR2          = SPRITE_VRAM_ADDR1 + SPRITE_VRAM_SIZE1   
    const uword SPRITE_VRAM_SIZE2          = 64 * 64

    const ubyte SPRITE_VRAM_BANK3          = SPRITE_VRAM_BANK2  ; profile
    const uword SPRITE_VRAM_ADDR3          = SPRITE_VRAM_ADDR2 + SPRITE_VRAM_SIZE2  
    const uword SPRITE_VRAM_SIZE3          = 64 * 64

    const ubyte SPRITE_VRAM_BANK4          = SPRITE_VRAM_BANK3  ; kermit 
    const uword SPRITE_VRAM_ADDR4          = SPRITE_VRAM_ADDR3 + SPRITE_VRAM_SIZE3  
    const uword SPRITE_VRAM_SIZE4          = 64 * 64

    ; buffers for 8 16 x 16 sprites
    const ubyte SPRITE_VRAM_BANK5          = SPRITE_VRAM_BANK4               
    const uword SPRITE_VRAM_ADDR5          = SPRITE_VRAM_ADDR4 + SPRITE_VRAM_SIZE4    
    const uword SPRITE_VRAM_SIZE5          = 16 * 16

    const ubyte SPRITE_VRAM_BANK6          = SPRITE_VRAM_BANK5  
    const uword SPRITE_VRAM_ADDR6          = SPRITE_VRAM_ADDR5 + SPRITE_VRAM_SIZE5   
    const uword SPRITE_VRAM_SIZE6          = 16 * 16

    const ubyte SPRITE_VRAM_BANK7          = SPRITE_VRAM_BANK6  
    const uword SPRITE_VRAM_ADDR7          = SPRITE_VRAM_ADDR6 + SPRITE_VRAM_SIZE6   
    const uword SPRITE_VRAM_SIZE7          = 16 * 16

    const ubyte SPRITE_VRAM_BANK8          = SPRITE_VRAM_BANK7  
    const uword SPRITE_VRAM_ADDR8          = SPRITE_VRAM_ADDR7 + SPRITE_VRAM_SIZE7   
    const uword SPRITE_VRAM_SIZE8          = 16 * 16

    const ubyte SPRITE_VRAM_BANK9          = SPRITE_VRAM_BANK8  
    const uword SPRITE_VRAM_ADDR9          = SPRITE_VRAM_ADDR8 + SPRITE_VRAM_SIZE8
    const uword SPRITE_VRAM_SIZE9          = 16 * 16

    const ubyte SPRITE_VRAM_BANK10         = SPRITE_VRAM_BANK9
    const uword SPRITE_VRAM_ADDR10         = SPRITE_VRAM_ADDR9 + SPRITE_VRAM_SIZE9
    const uword SPRITE_VRAM_SIZE10         = 16 * 16

    const ubyte SPRITE_VRAM_BANK11         = SPRITE_VRAM_BANK10
    const uword SPRITE_VRAM_ADDR11         = SPRITE_VRAM_ADDR10 + SPRITE_VRAM_SIZE10
    const uword SPRITE_VRAM_SIZE11         = 16 * 16

    const ubyte SPRITE_VRAM_BANK12         = SPRITE_VRAM_BANK11
    const uword SPRITE_VRAM_ADDR12         = SPRITE_VRAM_ADDR11 + SPRITE_VRAM_SIZE11
    const uword SPRITE_VRAM_SIZE12         = 16 * 16

    ; buffers for 4  64 x 64 tile sets
    const ubyte TILE_VRAM_BANK1            = SPRITE_VRAM_BANK12      ; turtle_0
    const uword TILE_VRAM_ADDR1            = SPRITE_VRAM_ADDR12 + SPRITE_VRAM_SIZE12  
    const uword TILE_VRAM_SIZE1            = 64 * 32 
    const uword TILE_VRAM_OFFSET1          = 0

    const ubyte TILE_VRAM_BANK2            = TILE_VRAM_BANK1        ; enemy_type_d
    const uword TILE_VRAM_ADDR2            = TILE_VRAM_ADDR1 + TILE_VRAM_SIZE1
    const uword TILE_VRAM_SIZE2            = 64 * 64 
    const uword TILE_VRAM_OFFSET2          = TILE_VRAM_ADDR2 - TILE_VRAM_ADDR1

    const ubyte TILE_VRAM_BANK3            = TILE_VRAM_BANK2        ; profie
    const uword TILE_VRAM_ADDR3            = TILE_VRAM_ADDR2 + TILE_VRAM_SIZE2
    const uword TILE_VRAM_SIZE3            = 64 * 64 
    const uword TILE_VRAM_OFFSET3          = TILE_VRAM_ADDR3 - TILE_VRAM_ADDR1

    ; make the 0 tile of the next nine tile sets a natural blank
    const ubyte TILE_VRAM_BANK4            = TILE_VRAM_BANK3        ; kermit
    const uword TILE_VRAM_ADDR4            = TILE_VRAM_ADDR3 + TILE_VRAM_SIZE3
    const uword TILE_VRAM_SIZE4            = 64 * 64 
    const uword TILE_VRAM_OFFSET4          = 0 

    ; buffers for 8 16 x 16 tile sets
    const ubyte TILE_VRAM_BANK5            = TILE_VRAM_BANK4    
    const uword TILE_VRAM_ADDR5            = TILE_VRAM_ADDR4 + TILE_VRAM_SIZE4
    const uword TILE_VRAM_SIZE5            = 16 * 16 
    const uword TILE_VRAM_OFFSET5          = TILE_VRAM_ADDR5 - TILE_VRAM_ADDR4

    const ubyte TILE_VRAM_BANK6            = TILE_VRAM_BANK5
    const uword TILE_VRAM_ADDR6            = TILE_VRAM_ADDR5 + TILE_VRAM_SIZE5
    const uword TILE_VRAM_SIZE6            = 16 * 16 
    const uword TILE_VRAM_OFFSET6          = TILE_VRAM_ADDR6 - TILE_VRAM_ADDR4

    const ubyte TILE_VRAM_BANK7            = TILE_VRAM_BANK6
    const uword TILE_VRAM_ADDR7            = TILE_VRAM_ADDR6 + TILE_VRAM_SIZE6
    const uword TILE_VRAM_SIZE7            = 16 * 16 
    const uword TILE_VRAM_OFFSET7          = TILE_VRAM_ADDR7 - TILE_VRAM_ADDR4

    const ubyte TILE_VRAM_BANK8            = TILE_VRAM_BANK7
    const uword TILE_VRAM_ADDR8            = TILE_VRAM_ADDR7 + TILE_VRAM_SIZE7
    const uword TILE_VRAM_SIZE8            = 16 * 16 
    const uword TILE_VRAM_OFFSET8          = TILE_VRAM_ADDR8 - TILE_VRAM_ADDR4

    const ubyte TILE_VRAM_BANK9            = TILE_VRAM_BANK8
    const uword TILE_VRAM_ADDR9            = TILE_VRAM_ADDR8 + TILE_VRAM_SIZE8
    const uword TILE_VRAM_SIZE9            = 16 * 16 
    const uword TILE_VRAM_OFFSET9          = TILE_VRAM_ADDR9 - TILE_VRAM_ADDR4

    const ubyte TILE_VRAM_BANK10           = TILE_VRAM_BANK9
    const uword TILE_VRAM_ADDR10           = TILE_VRAM_ADDR9 + TILE_VRAM_SIZE9
    const uword TILE_VRAM_SIZE10           = 16 * 16 
    const uword TILE_VRAM_OFFSET10         = TILE_VRAM_ADDR10 - TILE_VRAM_ADDR4

    const ubyte TILE_VRAM_BANK11           = TILE_VRAM_BANK10
    const uword TILE_VRAM_ADDR11           = TILE_VRAM_ADDR10 + TILE_VRAM_SIZE10
    const uword TILE_VRAM_SIZE11           = 16 * 16 
    const uword TILE_VRAM_OFFSET11         = TILE_VRAM_ADDR11 - TILE_VRAM_ADDR4

    const ubyte TILE_VRAM_BANK12           = TILE_VRAM_BANK11
    const uword TILE_VRAM_ADDR12           = TILE_VRAM_ADDR11 + TILE_VRAM_SIZE11
    const uword TILE_VRAM_SIZE12           = 16 * 16 
    const uword TILE_VRAM_OFFSET12         = TILE_VRAM_ADDR12 - TILE_VRAM_ADDR4

    ; shorten the names
    alias sprite_size = sprites.SIZE_64
    alias sprite_size16 = sprites.SIZE_16
    alias colors256 = sprites.COLORS_256
    alias colors16 = sprites.COLORS_16

    ; a vts context for each tile set / sprite
    ubyte[vts.VTS_CONTEXT_SIZE] context1    ; turtle_0
    ubyte[vts.VTS_CONTEXT_SIZE] context2    ; enemy_type_d
    ubyte[vts.VTS_CONTEXT_SIZE] context3    ; profile
    ubyte[vts.VTS_CONTEXT_SIZE] context4    ; kermit
    ubyte[vts.VTS_CONTEXT_SIZE] context5    ; t1
    ubyte[vts.VTS_CONTEXT_SIZE] context6    ; t2
    ubyte[vts.VTS_CONTEXT_SIZE] context7    ; t3
    ubyte[vts.VTS_CONTEXT_SIZE] context8    ; t4
    ubyte[vts.VTS_CONTEXT_SIZE] context9    ; t5
    ubyte[vts.VTS_CONTEXT_SIZE] context10   ; t9
    ubyte[vts.VTS_CONTEXT_SIZE] context11   ; t7
    ubyte[vts.VTS_CONTEXT_SIZE] context12   ; t8

    uword[] @nosplit contexts = [ 0, context1, context2, context3, context4, context5, context6,
                         context7, context8, context9, context10, context11, context12 ]

    sub all_at_once() {
        ; clear the sprite buffers
        verafx.clear(SPRITE_VRAM_BANK1, SPRITE_VRAM_ADDR1, $00, $1000) ; clear r buffers

        ; load the tile sets
;        void diskio.vload_raw("resource/turtle-0.bin-0vts", TILE_VRAM_BANK1, TILE_VRAM_ADDR1)      ; $0800 long
;        void diskio.vload_raw("resource/enemy-type-d.bin-0vts", TILE_VRAM_BANK2, TILE_VRAM_ADDR2)  ; $1000 long
;        void diskio.vload_raw("resource/profile.bin-0vts", TILE_VRAM_BANK3, TILE_VRAM_ADDR3)       ; $1000 long
;        void diskio.vload_raw("resource/kermit.bin-0vts", TILE_VRAM_BANK4, TILE_VRAM_ADDR4)        ; $1000 long 
;        void diskio.vload_raw("resource/t1.bin-vts", TILE_VRAM_BANK5, TILE_VRAM_ADDR5)            ; $0100 long 
;        void diskio.vload_raw("resource/t2.bin-vts", TILE_VRAM_BANK6, TILE_VRAM_ADDR6)            ; $0100 long 
;        void diskio.vload_raw("resource/t3.bin-vts", TILE_VRAM_BANK7, TILE_VRAM_ADDR7)            ; $0100 long 
;        void diskio.vload_raw("resource/t4.bin-vts", TILE_VRAM_BANK8, TILE_VRAM_ADDR8)            ; $0100 long 
;        void diskio.vload_raw("resource/t5.bin-vts", TILE_VRAM_BANK9, TILE_VRAM_ADDR9)            ; $0100 long 
;        void diskio.vload_raw("resource/t9.bin-vts", TILE_VRAM_BANK10, TILE_VRAM_ADDR10)          ; $0100 long 
;        void diskio.vload_raw("resource/t7.bin-vts", TILE_VRAM_BANK11, TILE_VRAM_ADDR11)          ; $0100 long 
;        void diskio.vload_raw("resource/t8.bin-vts", TILE_VRAM_BANK12, TILE_VRAM_ADDR12)          ; $0100 long 

        ; concatinate all tweleve of the VERA tile sets into one 16K file then load it here
        void diskio.vload_raw("resource/twelvecat.bin-0vts", TILE_VRAM_BANK1, TILE_VRAM_ADDR1)        ; $4000 long

        ; set up the sprites
        sprites.init(1,  ; turtle_0 16
                     SPRITE_VRAM_BANK1, SPRITE_VRAM_ADDR1,
                     sprite_size, sprite_size,
                     colors16, 8)
        sprites.init(2,  ; enemy_type_d 256
                     SPRITE_VRAM_BANK2, SPRITE_VRAM_ADDR2,
                     sprite_size, sprite_size,
                     colors256, 0)
        sprites.init(3,  ; profile 256
                     SPRITE_VRAM_BANK3, SPRITE_VRAM_ADDR3,
                     sprite_size, sprite_size,
                     colors256, 0)
        sprites.init(4,  ; kermit 256
                     SPRITE_VRAM_BANK4, SPRITE_VRAM_ADDR4,
                     sprite_size, sprite_size,
                     colors256, 0)
        sprites.init(5,  ; t1 256
                     SPRITE_VRAM_BANK5, SPRITE_VRAM_ADDR5,
                     sprite_size16, sprite_size16,
                     colors256, 0)
        sprites.init(6,  ; t2 256
                     SPRITE_VRAM_BANK6, SPRITE_VRAM_ADDR6,
                     sprite_size16, sprite_size16,
                     colors256, 0)
        sprites.init(7,  ; t3 256
                     SPRITE_VRAM_BANK7, SPRITE_VRAM_ADDR7,
                     sprite_size16, sprite_size16,
                     colors256, 0)
        sprites.init(8,  ; t4 256
                     SPRITE_VRAM_BANK8, SPRITE_VRAM_ADDR8,
                     sprite_size16, sprite_size16,
                     colors256, 0)
        sprites.init(9,  ; t5 256
                     SPRITE_VRAM_BANK9, SPRITE_VRAM_ADDR9,
                     sprite_size16, sprite_size16,
                     colors256, 0)
        sprites.init(10,  ; t9 256
                     SPRITE_VRAM_BANK10, SPRITE_VRAM_ADDR10,
                     sprite_size16, sprite_size16,
                     colors256, 0)
        sprites.init(11,  ; t7 256
                     SPRITE_VRAM_BANK11, SPRITE_VRAM_ADDR11,
                     sprite_size16, sprite_size16,
                     colors256, 0)
        sprites.init(12,  ; t8 256
                     SPRITE_VRAM_BANK12, SPRITE_VRAM_ADDR12,
                     sprite_size16, sprite_size16,
                     colors256, 0)

        ubyte ccrc

        ; create a context for each tile set          
        ccrc = vts.create_context( context1, ; turtle_0
                            TILE_VRAM_BANK1, TILE_VRAM_ADDR1,
                            SPRITE_VRAM_BANK1, SPRITE_VRAM_ADDR1,
                            64, true, TILE_VRAM_OFFSET1)      

        void main.check_ccrc(1, ccrc)                                                                   

        ccrc = vts.create_context( context2, ; enemy_type_d
                            TILE_VRAM_BANK1, TILE_VRAM_ADDR1,
                            SPRITE_VRAM_BANK2, SPRITE_VRAM_ADDR2,
                            64, false, TILE_VRAM_OFFSET2)
    
        void main.check_ccrc(2, ccrc)                                                                   

        ccrc = vts.create_context( context3, ; profile
                            TILE_VRAM_BANK1, TILE_VRAM_ADDR1,
                            SPRITE_VRAM_BANK3, SPRITE_VRAM_ADDR3,
                            64, false, TILE_VRAM_OFFSET3)
                          
        void main.check_ccrc(3, ccrc)                                                                   

        ccrc = vts.create_context( context4, ; kermit
                            TILE_VRAM_BANK4, TILE_VRAM_ADDR4,
                            SPRITE_VRAM_BANK4, SPRITE_VRAM_ADDR4,
                            64, false, TILE_VRAM_OFFSET4)

        void main.check_ccrc(4, ccrc)                                                                   

        ccrc = vts.create_context( context5, ; t1
                            TILE_VRAM_BANK4, TILE_VRAM_ADDR4,
                            SPRITE_VRAM_BANK5, SPRITE_VRAM_ADDR5,
                            16, false, TILE_VRAM_OFFSET5)

        void main.check_ccrc(5, ccrc)                                                                   

        ccrc = vts.create_context( context6, ; t2
                            TILE_VRAM_BANK4, TILE_VRAM_ADDR4,
                            SPRITE_VRAM_BANK6, SPRITE_VRAM_ADDR6,
                            16, false, TILE_VRAM_OFFSET6)

        void main.check_ccrc(6, ccrc)                                                                   

        ccrc = vts.create_context( context7, ; t3
                            TILE_VRAM_BANK4, TILE_VRAM_ADDR4,
                            SPRITE_VRAM_BANK7, SPRITE_VRAM_ADDR7,
                            16, false, TILE_VRAM_OFFSET7)

        void main.check_ccrc(7, ccrc)                                                                   

        ccrc = vts.create_context( context8, ; t4
                            TILE_VRAM_BANK4, TILE_VRAM_ADDR4,
                            SPRITE_VRAM_BANK8, SPRITE_VRAM_ADDR8,
                            16, false, TILE_VRAM_OFFSET8)

        void main.check_ccrc(8, ccrc)                                                                   

        ccrc = vts.create_context( context9, ; t5
                            TILE_VRAM_BANK4, TILE_VRAM_ADDR4,
                            SPRITE_VRAM_BANK9, SPRITE_VRAM_ADDR9,
                            16, false, TILE_VRAM_OFFSET9)

        void main.check_ccrc(9, ccrc)                                                                   

        ccrc = vts.create_context( context10, ; t9
                            TILE_VRAM_BANK4, TILE_VRAM_ADDR4,
                            SPRITE_VRAM_BANK10, SPRITE_VRAM_ADDR10,
                            16, false, TILE_VRAM_OFFSET10)

        void main.check_ccrc(10, ccrc)                                                                   

        ccrc = vts.create_context( context11, ; t7
                            TILE_VRAM_BANK4, TILE_VRAM_ADDR4,
                            SPRITE_VRAM_BANK11, SPRITE_VRAM_ADDR11,
                            16, false, TILE_VRAM_OFFSET11)

        void main.check_ccrc(11, ccrc)                                                                   

        ccrc = vts.create_context( context12, ; t8
                            TILE_VRAM_BANK4, TILE_VRAM_ADDR4,
                            SPRITE_VRAM_BANK12, SPRITE_VRAM_ADDR12,
                            16, false, TILE_VRAM_OFFSET12)

        void main.check_ccrc(12, ccrc)     

        txt.plot(6,52)
        txt.print(" Press any key to continue ")                                                              

        ; transform the tile set into the sprite, full size, no rotation or shear
        word degree = 0
        word degree2 = 0
        ubyte i
        vts.select(contexts[1])
        vts.scale(0.91)
        vts.rotate(degree)

        for i in 2 to 12 {
            vts.select(contexts[i])
            vts.rotate(degree)
        }

        ; show all four large sprites
        sprites.pos(1, 320, 240) ; turtle_0
        sprites.pos(2, 250, 240) ; enemy_type_d
        sprites.pos(3, 390, 240) ; profile
        sprites.pos(4, 320, 320) ; kermit

        ; show the eight small sprites
        word ypos = 24
        word xpos = 200
        for i in 5 to 12 {
            sprites.pos(i, xpos, ypos)
            xpos += 40
        }

        ; time the following 2,164 rotations

        void txt.waitkey()

        txt.cls()

        ; rotate all four
        repeat 270 {
            for i in [1,3, 5, 7, 9, 11] {
                vts.select(contexts[i]) 
                vts.rotate(degree)                
            }
            for i in [2,4, 6, 8, 10, 12] {
                vts.select(contexts[i]) 
                vts.rotate(degree2)                
            }
            degree -= 8
            degree2 = degree * -1

            ; move the small sprites down 
            ypos++
            for i in 5 to 12 {
                sprites.sety(i, ypos)
            }    
        }

        vts.select(context1) ; turtle_0
        vts.scale(1.25)

        for i in 2 to 4 {
            vts.select(contexts[i])
            vts.scale(1.5)
        }

        repeat 271 {        ; one extra to call straighten them all to vertical
            for i in 1 to 11 step 2 {
                vts.select(contexts[i]) 
                vts.rotate(degree2)
            }   
            for i in 2 to 12 step 2 {
                vts.select(contexts[i]) 
                vts.rotate(degree)
            }   
            degree -= 8
            degree2 = degree * -1
        }

        txt.plot(6,52)
        txt.print(" Press any key to return to the menu ")                                                              

        void txt.waitkey()
        txt.cls()
        cx16.kbdbuf_put(' ')

    }

}


frogfly {

    ; buffer for the kermit 64 x 64 sprite
    const ubyte SPRITE_VRAM_BANK1          = $01                ; kermit
    const uword SPRITE_VRAM_ADDR1          = $3000  
    const uword SPRITE_VRAM_SIZE1          = 64 * 64

    ; buffers for 2 16 x 16 fly  sprites
    const ubyte SPRITE_VRAM_BANK2          = SPRITE_VRAM_BANK1  ; t7 / t7-in         
    const uword SPRITE_VRAM_ADDR2          = SPRITE_VRAM_ADDR1 + SPRITE_VRAM_SIZE1    
    const uword SPRITE_VRAM_SIZE2          = 16 * 16

    const ubyte SPRITE_VRAM_BANK3          = SPRITE_VRAM_BANK2  ; foreground kermit
    const uword SPRITE_VRAM_ADDR3          = SPRITE_VRAM_ADDR2 + SPRITE_VRAM_SIZE2
    const uword SPRITE_VRAM_SIZE3          = 64 * 64

    ; buffer for the kermit  64 x 64 tile set
    const ubyte TILE_VRAM_BANK1            = SPRITE_VRAM_BANK3  ; kermit
    const uword TILE_VRAM_ADDR1            = SPRITE_VRAM_ADDR3 + SPRITE_VRAM_SIZE3 + (256 * 7) 
    const uword TILE_VRAM_SIZE1            = 64 * 64 
    const uword TILE_VRAM_OFFSET1          = 0

    ; buffers for 2 16 x 16 fly tile sets
    const ubyte TILE_VRAM_BANK2            = TILE_VRAM_BANK1    ; t7
    const uword TILE_VRAM_ADDR2            = TILE_VRAM_ADDR1 + TILE_VRAM_SIZE1
    const uword TILE_VRAM_SIZE2            = 16 * 16 
    const uword TILE_VRAM_OFFSET2          = TILE_VRAM_ADDR2 - TILE_VRAM_ADDR1

    const ubyte TILE_VRAM_BANK3            = TILE_VRAM_BANK2    ; t7-in
    const uword TILE_VRAM_ADDR3            = TILE_VRAM_ADDR2 + TILE_VRAM_SIZE2
    const uword TILE_VRAM_SIZE3            = 16 * 16 
    const uword TILE_VRAM_OFFSET3          = TILE_VRAM_ADDR3 - TILE_VRAM_ADDR1

    const ubyte TILE_VRAM_BANK4            = TILE_VRAM_BANK3    ; t7-in
    const uword TILE_VRAM_ADDR4            = TILE_VRAM_ADDR3 + TILE_VRAM_SIZE3
    const uword TILE_VRAM_SIZE4            = 64 * 64
    const uword TILE_VRAM_OFFSET4          = TILE_VRAM_ADDR4 - TILE_VRAM_ADDR1

    ; shorten the names
    alias sprite_size = sprites.SIZE_64
    alias sprite_size16 = sprites.SIZE_16
    alias colors256 = sprites.COLORS_256
    alias colors16 = sprites.COLORS_16

    ; a vts context for each tile set / sprite
    ubyte[vts.VTS_CONTEXT_SIZE] context1    ; foreground kermit
    ubyte[vts.VTS_CONTEXT_SIZE] context2    ; t7
    ubyte[vts.VTS_CONTEXT_SIZE] context3    ; t7-in
    ubyte[vts.VTS_CONTEXT_SIZE] context4    ; background kermit
    ubyte[vts.VTS_CONTEXT_SIZE] context5    ; closed mouth kermit
    ubyte[vts.VTS_CONTEXT_SIZE] context6    ; closed mouth kermit

    uword[] @nosplit contexts = [ 0, context1, context2, context3, context4, context5, context6 ]

    ubyte ccrc = 0

    word flyX 
    word flyY 
    const byte wingwait = 2
    uword pathentries = 0
    byte[6] p_entry = [0] * 6
    word degree = 0
    byte entrytype = 0
    bool forefrog = false
    float scale
    bool reverseX

    ubyte key

    sub frog_n_fly() {

        ; clear the sprite buffers
        verafx.clear(SPRITE_VRAM_BANK1, SPRITE_VRAM_ADDR1, $00, $1000) ; clear r buffers

        if not diskio.f_open("resource/path-360.bin") return
        void diskio.f_read(&pathentries, 2)

        ; load the tile sets
        void diskio.vload_raw("resource/kermit-open.bin-0vts", TILE_VRAM_BANK1, TILE_VRAM_ADDR1)        ; $1000 long 
        void diskio.vload_raw("resource/t7.bin-vts", TILE_VRAM_BANK2, TILE_VRAM_ADDR2)             ; $0100 long 
        void diskio.vload_raw("resource/t7-in.bin-vts", TILE_VRAM_BANK3, TILE_VRAM_ADDR3)          ; $0100 long 
        void diskio.vload_raw("resource/kermit-closed.bin-0vts", TILE_VRAM_BANK4, TILE_VRAM_ADDR4)        ; $1000 long 

        ; set up the sprites
        sprites.init(1,  ; foreground kermit 256
                     SPRITE_VRAM_BANK1, SPRITE_VRAM_ADDR1,
                     sprite_size, sprite_size,
                     colors256, 0)
        sprites.init(2,  ; t7 / t7-in 256
                     SPRITE_VRAM_BANK2, SPRITE_VRAM_ADDR2,
                     sprite_size16, sprite_size16,
                     colors256, 0)
        sprites.init(3,  ; background kermit 256
                     SPRITE_VRAM_BANK3, SPRITE_VRAM_ADDR3,
                     sprite_size, sprite_size,
                     colors256, 0)

        ; create a context for each tile set          
        ccrc = vts.create_context( context1, ; foreground kermit
                            TILE_VRAM_BANK1, TILE_VRAM_ADDR1,
                            SPRITE_VRAM_BANK1, SPRITE_VRAM_ADDR1,
                            64, false, TILE_VRAM_OFFSET1)      

        void main.check_ccrc(1, ccrc)                                                                   

        ccrc = vts.create_context( context2, ; t7 
                            TILE_VRAM_BANK1, TILE_VRAM_ADDR1,
                            SPRITE_VRAM_BANK2, SPRITE_VRAM_ADDR2,
                            16, false, TILE_VRAM_OFFSET2)
    
        void main.check_ccrc(2, ccrc)                                                                   

        ccrc = vts.create_context( context3, ; t7-in
                            TILE_VRAM_BANK1, TILE_VRAM_ADDR1,
                            SPRITE_VRAM_BANK2, SPRITE_VRAM_ADDR2,
                            16, false, TILE_VRAM_OFFSET3)
                          
        void main.check_ccrc(3, ccrc)                                                                   

        ccrc = vts.create_context( context4, ; background kermit
                            TILE_VRAM_BANK1, TILE_VRAM_ADDR1,
                            SPRITE_VRAM_BANK3, SPRITE_VRAM_ADDR3,
                            64, false, TILE_VRAM_OFFSET1)      

        void main.check_ccrc(4, ccrc)                                                                   

        ccrc = vts.create_context( context5, ; background kermit
                            TILE_VRAM_BANK1, TILE_VRAM_ADDR1,
                            SPRITE_VRAM_BANK3, SPRITE_VRAM_ADDR3,
                            64, false, TILE_VRAM_OFFSET4)      

        void main.check_ccrc(5, ccrc)                                                                   

        ccrc = vts.create_context( context6, ; background kermit
                            TILE_VRAM_BANK1, TILE_VRAM_ADDR1,
                            SPRITE_VRAM_BANK1, SPRITE_VRAM_ADDR1,
                            64, false, TILE_VRAM_OFFSET4)      

        void main.check_ccrc(5, ccrc)                                                                   

        ; transform the tile set into the sprite, full size, no rotation or shear
        ubyte i
        for i in 1 to 4 {
            vts.select(contexts[i])
            vts.rotate(0)
        }

        flyX = 380
        flyY = 286
        scale = 1.0

        txt.nl()
        txt.nl()
        txt.print("            Press ESC to quit - Press Spacebar to Chomp")


        ; show all three sprites
        sprites.pos(1, 320, 240)    ; foreground kermit
        sprites.pos(2, flyX, flyY)  ; fly
        sprites.pos(3, 320, 240)    ; background kermit

        reverseX = false

        for i in 0 to 253 {
            void diskio.f_read(&p_entry, 6)
            entrytype = p_entry[0]
            if entrytype == 1 {
            } else if entrytype == 2 {
                diskio.f_seek(0,(6 * p_entry[1]) + 2 as uword)
                continue
            } else if entrytype == 0 {   
                flyX = 380
                flyY = 286
                scale = 1.0
                diskio.f_close()
                void diskio.f_open("resource/path-360.bin") 
                diskio.f_seek(0,2)
                i = 0 
                reverseX = not reverseX
                continue
            }
            degree = mkword(p_entry[5] as ubyte, p_entry[4] as ubyte) as word
            repeat p_entry[1] {
                vts.select(context2)
                vts.scale(scale)
                vts.rotate(degree)
                sys.wait(wingwait)
                vts.select(context3)
                vts.scale(scale)
                vts.rotate(degree)
                sys.wait(wingwait)
                if reverseX {
                    flyX -= p_entry[2]
                } else {
                    flyX += p_entry[2]
                }
                flyY -= p_entry[3]
                sprites.pos(2,flyX, flyY)
                scale += p_entry[3] as float * 0.0032
            }
            if i % 16 == 0 {
                if 360 > flyX or flyX < 280 {
                    ; toggle foreground kermit 
                    if forefrog {
                        sprites.hide(1)
                        forefrog = false
                    } else {
                        sprites.show(1)
                        forefrog = true
                    }                       
                }
            }
            when handlekey() {
                1 -> {  ; caught fly
                    break
                }
                2 -> {  ; quit
                    return
                }
            }
        }  
        sys.wait(8)
        vts.select(context5)  ; closed mouth kermit
        vts.rotate(0)

        sprites.hide(2) 
        txt.plot(42,28)
        txt.print("YUM!")
    }

    sub handlekey() -> ubyte {
        ubyte count = 0
        void, key = cbm.GETIN()
        if_ne {
            when key {
                $20 -> { ; spacebar
                    vts.select(context5)  ; closed mouth kermit
                    vts.rotate(0)
                    vts.select(context6)  ; closed mouth kermit
                    vts.rotate(0)
                    count = 12
                    if not forefrog {
                        if flyX in 325 to 365 {
                            if flyY in 252 to 272 {
                                return 1    ; caught fly
                            }
                        }
                    }
                }
                $1B -> { ; esc
                    cx16.kbdbuf_put($20)
                    return 2  ; quit
                }
            }
        } else {
            if count == 0 {
                vts.select(context1)  ; closed mouth kermit
                vts.rotate(0)
                vts.select(context4)  ; closed mouth kermit
                vts.rotate(0)
            }
        }
        if count > 0 count--
        return 0 ; continue the looping
    }
    
}

ghosting {
    ; tile sets
    const ubyte TILE_VRAM_BANK1            = $01             ; upright ghost
    const uword TILE_VRAM_ADDR1            = $3000  
    const uword TILE_VRAM_SIZE1            = 32 * 64 
    const uword TILE_VRAM_OFFSET1          = 0

    const ubyte TILE_VRAM_BANK2            = TILE_VRAM_BANK1  ; sheared ghost
    const uword TILE_VRAM_ADDR2            = TILE_VRAM_ADDR1 + TILE_VRAM_SIZE1
    const uword TILE_VRAM_SIZE2            = 32 * 64
    const uword TILE_VRAM_OFFSET2          = TILE_VRAM_ADDR2 - TILE_VRAM_ADDR1

    ; sprite
    const ubyte SPRITE_VRAM_BANK1          = TILE_VRAM_BANK2   ; upright/sheared ghost
    const uword SPRITE_VRAM_ADDR1          = TILE_VRAM_ADDR2 + TILE_VRAM_SIZE2
    const uword SPRITE_VRAM_SIZE1          = 64 * 64

    alias colors256 = sprites.COLORS_256
    alias colors16 = sprites.COLORS_16

    ubyte[vts.VTS_CONTEXT_SIZE] context1    ; upright ghost
    ubyte[vts.VTS_CONTEXT_SIZE] context2    ; sheared ghost

    ubyte ccrc = 0

    bool is4bit

    sub ghost() {

        ; clear the sprite buffers
        verafx.clear(TILE_VRAM_BANK1, TILE_VRAM_ADDR1, $00, $1000) ; clear our buffers

        ; load the tile sets
        void diskio.vload_raw("resource/lghost.bin-0vts", TILE_VRAM_BANK1, TILE_VRAM_ADDR1)   

        is4bit = true
    
        ; set up the sprite
        sprites.init(1,  ; upright/sheared ghost
                    SPRITE_VRAM_BANK1, SPRITE_VRAM_ADDR1,
                    sprites.SIZE_64, sprites.SIZE_64,
                    if is4bit colors16 else colors256, 0)

        ; create a context for each tile set          
        ccrc = vts.create_context( context1, ; upright ghost
                            TILE_VRAM_BANK1, TILE_VRAM_ADDR1,
                            SPRITE_VRAM_BANK1, SPRITE_VRAM_ADDR1,
                            64, is4bit, TILE_VRAM_OFFSET1)      

        void main.check_ccrc(1, ccrc)                                                                   

        ccrc = vts.create_context( context2, ; sheared ghost
                            TILE_VRAM_BANK1, TILE_VRAM_ADDR1,
                            SPRITE_VRAM_BANK1, SPRITE_VRAM_ADDR1,
                            64, is4bit, TILE_VRAM_OFFSET2)      

        void main.check_ccrc(2, ccrc)                                                                   

        word xpos = 220
        word ypos = 240
        word angle = 0

        ; show the upright ghost tile set
        vts.select(context1)
        vts.rotate(angle)

        sprites.pos(1, xpos, ypos)    ; ghost

        txt.plot(6,52)
        txt.print(" Press any key to continue ")                                                              
        void txt.waitkey()  
        
        txt.cls()

        vts.shear_h(120)    ; give the ghost sprite a horizontal shear transform

        ; make a new tile set from the sheared ghost 
        vts.select(context2)
        void vts.sprite_to_tile_set(TILE_VRAM_SIZE2)

        ; show the new sheared ghost tile set
        vts.rotate(angle)

        sprites.pos(1, xpos, ypos)    ; ghost 

        const ubyte steps = 200
        repeat steps {
            sprites.setx(1,xpos)
            xpos++
            delay()
        }
        repeat 15 {
            angle -= 6
            vts.rotate(angle)
            delay()
        }
        repeat steps {
            sprites.sety(1,ypos)
            ypos--
            delay()
        }
        sprites.flipx(1,true)
        repeat 15 {
            angle += 6
            vts.rotate(angle)
            delay()
        }
        repeat steps * 2 {
            sprites.setx(1,xpos)
            xpos--
            delay()
        }
        repeat 15 {
            angle += 6
            vts.rotate(angle)
            delay()
        }
        repeat steps {
            sprites.sety(1,ypos)
            ypos++
            delay()
        }
        sprites.flipx(1,false)
        repeat 15 {
            angle -= 6
            vts.rotate(angle)
            delay()
        }
        repeat steps {
            sprites.setx(1,xpos)
            xpos++
            delay()
        }

        vts.select(context1)
        vts.rotate(angle)

        txt.plot(6,52)
        txt.print(" Press any key to return to the menu ")                                                              

        void txt.waitkey()
        txt.cls()
        cx16.kbdbuf_put(' ')


    }

    sub delay() {
        float f = floats.PI
        repeat 400 {
            f /= .003
            f = floats.PI
        }
    }
}

conversions {
    ; tile sets
    ; $3000
    const ubyte TILE_VRAM_BANK1            = $01             ; tile set 64
    const uword TILE_VRAM_ADDR1            = $3000  
    const uword TILE_VRAM_SIZE1            = 64 * 64 
    const uword TILE_VRAM_OFFSET1          = 0

    ; $4000
    const ubyte TILE_VRAM_BANK2            = TILE_VRAM_BANK1  ; tile set 16
    const uword TILE_VRAM_ADDR2            = TILE_VRAM_ADDR1 + TILE_VRAM_SIZE1
    const uword TILE_VRAM_SIZE2            = 16 * 16
    const uword TILE_VRAM_OFFSET2          = TILE_VRAM_ADDR2 - TILE_VRAM_ADDR1

    ; sprites
    ; $4100
    const ubyte SPRITE_VRAM_BANK1          = TILE_VRAM_BANK2   ; sprite 64
    const uword SPRITE_VRAM_ADDR1          = TILE_VRAM_ADDR2 + TILE_VRAM_SIZE2
    const uword SPRITE_VRAM_SIZE1          = 64 * 64

    ; $5100
    const ubyte SPRITE_VRAM_BANK2          = SPRITE_VRAM_BANK1  ; sprite 16
    const uword SPRITE_VRAM_ADDR2          = SPRITE_VRAM_ADDR1 + SPRITE_VRAM_SIZE1
    const uword SPRITE_VRAM_SIZE2          = 16 * 16

    ; $5200
    const ubyte SPRITE_VRAM_BANK3          = SPRITE_VRAM_BANK2  ; grid 64
    const uword SPRITE_VRAM_ADDR3          = SPRITE_VRAM_ADDR2 + SPRITE_VRAM_SIZE2
    const uword SPRITE_VRAM_SIZE3          = 64 * 64

    ; $6200
    const ubyte SPRITE_VRAM_BANK4          = SPRITE_VRAM_BANK3  ; grid 16
    const uword SPRITE_VRAM_ADDR4          = SPRITE_VRAM_ADDR3 + SPRITE_VRAM_SIZE3
    const uword SPRITE_VRAM_SIZE4          = 16 * 16

    ; $6300
    const ubyte SPRITE_VRAM_BANK5          = SPRITE_VRAM_BANK4  ; original sprite
    const uword SPRITE_VRAM_ADDR5          = SPRITE_VRAM_ADDR4 + SPRITE_VRAM_SIZE4
    const uword SPRITE_VRAM_SIZE5          = 64 * 64            ; for largest original

    const ubyte SPRITE_64                  = 1
    const ubyte SPRITE_16                  = 2
    const ubyte SPRITE_GRID_64             = 3
    const ubyte SPRITE_GRID_16             = 4
    const ubyte SPRITE_ORIGINAL            = 5

    const word POS_ORIGINAL_X              = 100
    const word POS_ORIGINAL_Y              = 200
    const word POS_64_X                    = 180
    const word POS_64_Y                    = 170
    const word POS_16_X                    = 260   
    const word POS_16_Y                    = 196 

    ubyte[vts.VTS_CONTEXT_SIZE] context1    ; upright ghost
    ubyte[vts.VTS_CONTEXT_SIZE] context2    ; sheared ghost

    ubyte[81] spritefilename
    ubyte[81] defaultspritefilename
    ubyte[81] tsfilename

    ubyte sprite_width
    ubyte sprite_height
    ubyte sprite_bit_depth
    bool is4bit = false
    ubyte key
    bool has2x2 = false

    sub user_sprite() {
        linespace()
        txt.print(" How wide is the sprite?")
                linespace()
        txt.print(" 1. 8 pixels wide")
        linespace()
        txt.print(" 2. 16 pixels wide")
        linespace()
        txt.print(" 3. 32 pixels wide")
        linespace()
        txt.print(" 4. 64 pixels wide")
        linespace()
        txt.print(" ESC to exit")
        linespace()
        txt.print(" Select the sprite width  ")
        ubyte group 
        ubyte item
        do {
            key = txt.waitkey()
            if key == $1B {
                cx16.kbdbuf_put(' ')
                return ; ESC key
            }
            if key in "1234" {
                group = key
            } else {
                txt.bell()
                group = 0
            }
        } until group != 0

        txt.cls()
        spritefilename[0] = 0
        defaultspritefilename[0] = 0
        when group {
            '1' -> {
                void strings.append(defaultspritefilename, "resource/sp8x")
                linespace()
                txt.print(" Sprites that are 8 pixels wide")
                linespace()
                txt.print(" 1. 8x8   16 color")
                linespace()
                txt.print(" 2. 8x8  256 color")
                linespace()
                txt.print(" 3. 8x16  16 color")
                linespace()
                txt.print(" 4. 8x16 256 color")
                linespace()
                txt.print(" 5. 8x32  16 color")
                linespace()
                txt.print(" 6. 8x32 256 color")
                linespace()
                txt.print(" 7. 8x64  16 color")
                linespace()
                txt.print(" 8. 8x64 256 color")
                linespace()
                txt.print(" ESC to exit")
                linespace()
                txt.print(" Select the sprite size and colors  ")
                do {
                    key = txt.waitkey()
                    if key == $1B {
                        cx16.kbdbuf_put(' ')
                        return ; ESC key
                    }
                    if key in "12345678" {
                        item = key
                    } else {
                        txt.bell()
                        item = 0
                    }
                } until item != 0

                sprite_width = sprites.SIZE_8
                sprite_bit_depth = sprites.COLORS_256
                is4bit = false
                if item & $01 == $01 {
                    is4bit = true
                    sprite_bit_depth = sprites.COLORS_16
                }                
                when item {
                    '1','2' -> {
                        sprite_height = sprites.SIZE_8
                    }
                    '3','4' -> {
                        sprite_height = sprites.SIZE_16
                    }
                    '5','6' -> {
                        sprite_height = sprites.SIZE_32
                    }
                    '7','8' -> {
                        sprite_height = sprites.SIZE_64
                    }
                }    
            }
            '2' -> {
                void strings.append(defaultspritefilename, "resource/sp16x")
                linespace()
                txt.print(" Sprites that are 16 pixels wide")
                linespace()
                txt.print(" 1. 16x8   16 color")
                linespace()
                txt.print(" 2. 16x8  256 color")
                linespace()
                txt.print(" 3. 16x16  16 color")
                linespace()
                txt.print(" 4. 16x16 256 color")
                linespace()
                txt.print(" 5. 16x32  16 color")
                linespace()
                txt.print(" 6. 16x32 256 color")
                linespace()
                txt.print(" 7. 16x64  16 color")
                linespace()
                txt.print(" 8. 16x64 256 color")
                linespace()
                txt.print(" ESC to exit")
                linespace()
                txt.print(" Select the sprite size and colors  ")
                do {
                    key = txt.waitkey()
                    if key == $1B {
                        cx16.kbdbuf_put(' ')
                        return ; ESC key
                    }
                    if key in "12345678" {
                        item = key
                    } else {
                        txt.bell()
                        item = 0
                    }
                } until item != 0

                sprite_width = sprites.SIZE_16
                sprite_bit_depth = sprites.COLORS_256
                is4bit = false
                if item & $01 == $01 {
                    is4bit = true
                    sprite_bit_depth = sprites.COLORS_16
                }                
                when item {
                    '1','2' -> {
                        sprite_height = sprites.SIZE_8
                    }
                    '3','4' -> {
                        sprite_height = sprites.SIZE_16
                    }
                    '5','6' -> {
                        sprite_height = sprites.SIZE_32
                    }
                    '7','8' -> {
                        sprite_height = sprites.SIZE_64
                    }
                }    
            }
            '3' -> {
                void strings.append(defaultspritefilename, "resource/sp32x")
                linespace()
                txt.print(" Sprites that are 32 pixels wide")
                linespace()
                txt.print(" 1. 32x8   16 color")
                linespace()
                txt.print(" 2. 32x8  256 color")
                linespace()
                txt.print(" 3. 32x16  16 color")
                linespace()
                txt.print(" 4. 32x16 256 color")
                linespace()
                txt.print(" 5. 32x32  16 color")
                linespace()
                txt.print(" 6. 32x32 256 color")
                linespace()
                txt.print(" 7. 32x64  16 color")
                linespace()
                txt.print(" 8. 32x64 256 color")
                linespace()
                txt.print(" ESC to exit")
                linespace()
                txt.print(" Select the sprite size and colors  ")
                do {
                    key = txt.waitkey()
                    if key == $1B {
                        cx16.kbdbuf_put(' ')
                        return ; ESC key
                    }
                    if key in "12345678" {
                        item = key
                    } else {
                        txt.bell()
                        item = 0
                    }
                } until item != 0

                sprite_width = sprites.SIZE_32
                sprite_bit_depth = sprites.COLORS_256
                is4bit = false
                if item & $01 == $01 {
                    is4bit = true
                    sprite_bit_depth = sprites.COLORS_16
                }                
                when item {
                    '1','2' -> {
                        sprite_height = sprites.SIZE_8
                    }
                    '3','4' -> {
                        sprite_height = sprites.SIZE_16
                    }
                    '5','6' -> {
                        sprite_height = sprites.SIZE_32
                    }
                    '7','8' -> {
                        sprite_height = sprites.SIZE_64
                    }
                }  
            }  
            '4' -> {
                void strings.append(defaultspritefilename, "resource/sp64x")
                linespace()
                txt.print(" Sprites that are 64 pixels wide")
                linespace()
                txt.print(" 1. 64x8   16 color")
                linespace()
                txt.print(" 2. 64x8  256 color")
                linespace()
                txt.print(" 3. 64x16  16 color")
                linespace()
                txt.print(" 4. 64x16 256 color")
                linespace()
                txt.print(" 5. 64x32  16 color")
                linespace()
                txt.print(" 6. 64x32 256 color")
                linespace()
                txt.print(" 7. 64x64  16 color")
                linespace()
                txt.print(" 8. 64x64 256 color")
                linespace()
                txt.print(" ESC to exit")
                linespace()
                txt.print(" Select the sprite size and colors  ")
                do {
                    key = txt.waitkey()
                    if key == $1B {
                        cx16.kbdbuf_put(' ')
                        return ; ESC key
                    }
                    if key in "12345678" {
                        item = key
                    } else {
                        txt.bell()
                        item = 0
                    }
                } until item != 0

                sprite_width = sprites.SIZE_64
                sprite_bit_depth = sprites.COLORS_256
                is4bit = false
                if item & $01 == $01 {
                    is4bit = true
                    sprite_bit_depth = sprites.COLORS_16
                }                
                when item {
                    '1','2' -> {
                        sprite_height = sprites.SIZE_8
                    }
                    '3','4' -> {
                        sprite_height = sprites.SIZE_16
                    }
                    '5','6' -> {
                        sprite_height = sprites.SIZE_32
                    }
                    '7','8' -> {
                        sprite_height = sprites.SIZE_64
                    }
                }    
            }
        }

        when item {
            '1','2' -> void strings.append(defaultspritefilename, "8x")
            '3','4' -> void strings.append(defaultspritefilename, "16x")
            '5','6' -> void strings.append(defaultspritefilename, "32x")
            '7','8' -> void strings.append(defaultspritefilename, "64x")
        }

        if is4bit {
            void strings.append(defaultspritefilename, "16.bin")
        } else {
            void strings.append(defaultspritefilename, "256.bin")
        }

        txt.cls()
        linespace() 
        txt.print(" Enter the name of the sprite to convert, empty name for a test sprite. ")
        linespace()
  
        txt.column(1)
        void txt.input_chars(spritefilename)

        if spritefilename[0] == 0 {
            void strings.append(spritefilename, defaultspritefilename)
        }

        process_sprite()

        if has2x2 {
            txt.plot(2,1)
            txt.print(" For the 2x2 Tile Set")
            linespace()
            cx16.r0L = 0
            if zero_tile_blank(TILE_VRAM_BANK2, TILE_VRAM_ADDR2, 64) {
                cx16.r0L |= $08
            } else if is4bit and zero_tile_blank(TILE_VRAM_BANK2, TILE_VRAM_ADDR2, 32) {
                cx16.r0L |= $04
            }
            when cx16.r0L {
                $00 -> {
                    txt.print(" This Tile Set \x1c\x01\x05is not suitable\x1f\x01\x05 for the first tile in any tile bank.")
                }
                $04 -> {
                    txt.print(" This Tile Set \x9e\x01is suitable only\x01\x05 for the first tile in a 4-bit tile bank.")
                }
                $08 -> {
                    txt.print(" This Tile Set \x1e\x01\x05is suitable\x1f\x01\x05 for the first tile in any tile bank.")    
                }
            }            

            tsfilename[0] = 0
            linespace()
            txt.print(" Enter a name for the Tile Set, press enter alone to skip saving the Tile Set.")
            linespace()
            txt.column(1)
            void txt.input_chars(tsfilename)

            if tsfilename[0] != 0 {
                ; save the tile set to a file
                void vsave_raw(tsfilename, 
                          TILE_VRAM_BANK2, 
                          TILE_VRAM_ADDR2, 
                          if is4bit TILE_VRAM_SIZE2 / 2 
                          else TILE_VRAM_SIZE2)
            }

            linespace()
            txt.print(" Press any key to continue ")
            void txt.waitkey()
            txt.cls()
        }

        txt.plot(2,1)
        txt.print(" For the 8x8 Tile Set")
        linespace()
        if zero_tile_blank(TILE_VRAM_BANK1, TILE_VRAM_ADDR1, 64) {
            cx16.r0L |= $08
        } else if is4bit and zero_tile_blank(TILE_VRAM_BANK1, TILE_VRAM_ADDR1, 32) {
            cx16.r0L |= $04
        }
        when cx16.r0L {
            $00 -> {
                txt.print(" This Tile Set \x1c\x01\x05is not suitable\x1f\x01\x05 for the first tile in any tile bank.")
            }
            $04 -> {
                txt.print(" This Tile Set \x9e\x01is suitable only\x01\x05 for the first tile in a 4-bit tile bank.")
            }
            $08 -> {
                txt.print(" This Tile Set \x1e\x01\x05is suitable\x1f\x01\x05 for the first tile in any tile bank.")    
            }
        }            
        
        tsfilename[0] = 0
        linespace()
        txt.print(" Enter a name for the Tile Set, press enter alone to skip saving the Tile Set.")
        linespace()
        txt.column(1)
        void txt.input_chars(tsfilename)

        if tsfilename[0] != 0 {
            ; save the tile set to a file
            void vsave_raw(tsfilename, 
                        TILE_VRAM_BANK1, 
                        TILE_VRAM_ADDR1, 
                        if is4bit TILE_VRAM_SIZE1 / 2 
                        else TILE_VRAM_SIZE1)
        }

        linespace()
        txt.print(" Press any key to continue ")
        void txt.waitkey()

        cx16.kbdbuf_put(' ')
        sprites.reset(1,5)         
    }   

    sub linespace() {
        repeat 2 txt.nl()
    }

    sub process_sprite() {
        word angle = 0

        ; sprite 64
        sprites.init(SPRITE_64, SPRITE_VRAM_BANK1, SPRITE_VRAM_ADDR1, 
                        sprites.SIZE_64, sprites.SIZE_64, sprite_bit_depth, 0)

        ; sprite 16  
        sprites.init(SPRITE_16, SPRITE_VRAM_BANK2, SPRITE_VRAM_ADDR2, 
                        sprites.SIZE_16, sprites.SIZE_16, sprite_bit_depth, 0)
                        
        ; grid 64
        sprites.init(SPRITE_GRID_64, SPRITE_VRAM_BANK3, SPRITE_VRAM_ADDR3, 
                        sprites.SIZE_64, sprites.SIZE_64, sprites.COLORS_256, 0)
                        
        ; grid 16                         
        sprites.init(SPRITE_GRID_16, SPRITE_VRAM_BANK4, SPRITE_VRAM_ADDR4, 
                        sprites.SIZE_16, sprites.SIZE_16, sprites.COLORS_256, 0)
                        
        ; original sprite                         
        sprites.init(SPRITE_ORIGINAL, SPRITE_VRAM_BANK5, SPRITE_VRAM_ADDR5, 
                        sprite_width, sprite_height, sprite_bit_depth, 0)

        ubyte ccrc
        ; create a context for each tile set          
        ccrc = vts.create_context( context1, ; sprite 64
                            TILE_VRAM_BANK1, TILE_VRAM_ADDR1,
                            SPRITE_VRAM_BANK1, SPRITE_VRAM_ADDR1,
                            64, is4bit, TILE_VRAM_OFFSET1)      

        void main.check_ccrc(1, ccrc)                                                                   

        ccrc = vts.create_context( context2, ; sprite 16
                            TILE_VRAM_BANK1, TILE_VRAM_ADDR1 ,
                            SPRITE_VRAM_BANK2, SPRITE_VRAM_ADDR2,
                            16, is4bit, TILE_VRAM_OFFSET2)      

        void main.check_ccrc(2, ccrc)                                                                   

        ; clear all the buffers 
        verafx.clear(TILE_VRAM_BANK1, TILE_VRAM_ADDR1, $00, $10C0)    

        txt.cls()        

        ; fill the known sprites
        check_vload("resource/spgrid64.bin", SPRITE_VRAM_BANK3, SPRITE_VRAM_ADDR3)
        check_vload("resource/spgrid16.bin", SPRITE_VRAM_BANK4, SPRITE_VRAM_ADDR4)
        check_vload(spritefilename, SPRITE_VRAM_BANK5, SPRITE_VRAM_ADDR5)

        ; show the original sprite            
        sprites.pos(SPRITE_ORIGINAL, POS_ORIGINAL_X, POS_ORIGINAL_Y)   

        ; show grid 64
        sprites.pos(SPRITE_GRID_64, POS_64_X, POS_64_Y)
        ; show the orignal within grid 64
        sprites.pos(SPRITE_64, POS_64_X, POS_64_Y)

        ; convert the original sprite to a 8x8 tile set 
        grow_sprite_to_ts(SPRITE_ORIGINAL,
                            TILE_VRAM_BANK1, 
                            TILE_VRAM_ADDR1, 
                            if is4bit TILE_VRAM_SIZE1/2 else TILE_VRAM_SIZE1)

        vts.select(context1)
        vts.rotate(angle)

        ; show the orignal within grid 64
        sprites.pos(SPRITE_64, POS_64_X, POS_64_Y)

        ; show grid 16 when needed
        sprites.hide(SPRITE_GRID_16)
        has2x2 = false
        if sprite_width | sprite_height <= sprites.SIZE_16 {
            has2x2 = true
            sprites.show(SPRITE_GRID_16)
            sprites.pos(SPRITE_GRID_16, POS_16_X, POS_16_Y)

            ; convert the original sprite to a 2x2 tile set 
            grow_sprite_to_ts(SPRITE_ORIGINAL,
                                TILE_VRAM_BANK2, 
                                TILE_VRAM_ADDR2, 
                                if is4bit TILE_VRAM_SIZE2/2 else TILE_VRAM_SIZE2)

            vts.select(context2)
            vts.rotate(angle)

            sprites.pos(SPRITE_16, POS_16_X, POS_16_Y)

            sys.wait(20)

            for angle in 0 to 360 * 3 step 6 {
                vts.select(context2)
                vts.rotate(angle)
                repeat 1200 %asm{{nop}}  ; goes too fast without a delay
            }
        }    

        for angle in 0 to 360 * 3 step 6 {
            vts.select(context1)
            vts.rotate(angle)
        }

    }

    sub grow_sprite_to_ts(ubyte source_sprite, ubyte bank, uword addr, uword size) {
        ubyte rc = vts.grow_sprite_to_tile_set(source_sprite, bank, addr, size)
        if rc != vts.SPRITE_TO_TILE_OK {
            txt.print("  Sprite to tile set error ")
            if rc == vts.SPRITE_TO_TILE_SIZE_ERROR {
                txt.print("SPRITE_TO_TILE_SIZE_ERROR")
            } else {
                txt.print("SPRITE_TO_TILE_BIT_SIZE_ERROR")                    
            }
            txt.nl()
        }
    }

    sub check_vload(str filename, ubyte bank, uword address) {
        if not diskio.vload_raw(filename, bank, address) {
            txt.print("vload_raw returned false for filename ")
            txt.print(filename)
            txt.nl()
        }
    }
    
    ; check if the zero tile in the tile set is blank
    sub zero_tile_blank(ubyte bank, uword addr, ubyte reps) -> bool {
        bool blank = true
        cx16.vaddr(bank,addr,0,1)
        repeat reps {
            if cx16.VERA_DATA0 != 0 {
                blank = false
                break
            }
        }
        return blank
    }

    ; save vram to a file 
    sub vsave_raw(uword filenameptr, ubyte startbank, uword startaddress, uword savesize) -> bool {

        if diskio.exists(filenameptr) {
            linespace()
            txt.bell()
            txt.print(" This Tile Set exists, do you want to overwrite? (Y/N)  ")
            repeat {
                key = txt.waitkey()
                if key in "ynYN" {
                    if key in "yY" {
                        diskio.delete(filenameptr)
                        linespace()
                        txt.print(" The Tile Set will be overwritten ")
                        break
                    } else {
                        linespace()
                        txt.print(" The Tile Set will not be overwritten ")
                        return false
                    }
                }
            } 
        }

        if not diskio.f_open_w(filenameptr) {
            return false
        }
;        defer diskio.f_close_w()
        cbm.CHKOUT(diskio.WRITE_IO_CHANNEL)   ; MCIOUT requires this

        ; set VERA_DATA0 to point to the startbank and address
        cx16.VERA_CTRL = 0
        cx16.VERA_ADDR_H = startbank | %0001_0000  ; INC 1
        cx16.VERA_ADDR   = startaddress

        while savesize > 0 {
            void, cx16.r8 = cx16.MCIOUT(lsb(savesize), &cx16.VERA_DATA0, true)
            if_cs return internal_fallback() ; MCIOUT is not in all ROMS
            savesize -= cx16.r8
        }
        diskio.f_close_w()
        linespace()
        txt.print(" Tile Set saved")

        return true

        sub internal_fallback() -> bool {
            repeat savesize {
                cbm.CHROUT(cx16.VERA_DATA0)
            }
            diskio.f_close_w()
            linespace()
            txt.print(" Tile Set saved")
            return true
        }    
    }

}
