; vtsimport.p8   import

%import syslib
%import diskio

%encoding iso

vts {

    ubyte vtsbank
    
    const uword VTS_TABLE = $BFD0

    ; called by load_vts_into_bank for bank initialization
    extsub @bank vtsbank VTS_TABLE + 0 = init()

    ; use this routine to load the code into a ram bank
    ; returns the end load address, must be higher than $BF0B
    sub load_vts_into_bank(ubyte bank_number) -> bool {
        sys.push(cx16.getrambank())
        vtsbank = bank_number
        cx16.rambank(bank_number)
        cx16.r2 = diskio.load_raw("vtsbank.bin",$A000)
        cx16.rambank(sys.pop())
        vts.init()   ; initialize the bank
        return cx16.r2 > VTS_TABLE
    }

    ; calling program must allocate a ubyte array of the following size 
    ; that will hold the context for rotation and scaling of tile sets
    const ubyte VTS_CONTEXT_SIZE      = 13   ; 13 bytes for each context

    ; call once to create context for VERA FX Affine rotation and scaling
    ; ubyte array[AFF_CONTEXT_SIZE] for context
    extsub @bank vtsbank VTS_TABLE + 3 = create_context(uword context @ R3, 
                                    ; vram address for the affine tile set
                                    ubyte tile_bank @ R4, uword tile_addr @ R5,
                                    ; vram destination address for transformed sprite 
                                    ubyte sprite_bank @ R6, uword sprite_addr @ R7,
                                    ; 16 or 64 for 16x16 or 64x64 pixel sprite
                                    ubyte sprite_size @ R8, 
                                    ; four_bit = 1 for 16 color, 0 for 256 color sprite
                                    bool  four_bit @ R9,
                                    ; tile set offset from tile_bank : tile_addr
                                    uword  tile_offest @R10 ) -> ubyte @ A

        ; return codes from create_context
        const ubyte VTS_CREATE_OK                     = 0
        const ubyte VTS_CREATE_TILE_ADDR_ERROR        = 1 ; must align to 2048 byte boundries
        const ubyte VTS_CREATE_SPRITE_ADDR_ERROR      = 2 ; must align to 32 byte boundries
        const ubyte VTS_CREATE_SPRITE_SIZE_ERROR      = 3 ; must be 16 or 64
        const ubyte VTS_CREATE_TILE_OFFSET_ERROR      = 4 ; must not exceed these limits
                                                          ; 16x16 16 color  8064
                                                          ; 16x16 256 color 16128
                                                          ; 64x64 16 color  6144
                                                          ; 64x64 256 color 12288
                                                          ; to allow a valid tile map

    extsub @bank vtsbank VTS_TABLE + 6 = select(uword context @ R3)

    ; scale of 1.0 keeps the original size. 1.0 is the default
    ; scale less than 1.0 increases the size, the practical limit is around 0.75 
    ; scale more than 1.0 decreases the size, the practical limit is around 3.0
    extsub @bank vtsbank VTS_TABLE + 9 = scale(float fscale @ FAC1) ; default is 1.0

    ; degree may be positive or negative
    extsub @bank vtsbank VTS_TABLE + 12 = rotate(word degree @ AY)
    extsub @bank vtsbank VTS_TABLE + 15 = rotate_f(float degree @ FAC1) ; address of the degrees float
    extsub @bank vtsbank VTS_TABLE + 18 = rotate_r(float radians @ FAC1, bool clockwise @ R0)
    extsub @bank vtsbank VTS_TABLE + 21 = rotate_p(float angle @ FAC1, bool clockwise @ R0) ; multiple of PI
    extsub @bank vtsbank VTS_TABLE + 24 = shear_v(word extent @ AY)
    extsub @bank vtsbank VTS_TABLE + 27 = shear_h(word extent @ AY)

    ; reset the VERA tile map transforms
    extsub @bank vtsbank VTS_TABLE + 30 = reset_fx() 
}