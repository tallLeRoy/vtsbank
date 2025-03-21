; vtsimport.p8   import

%import syslib
%import diskio

%encoding iso

vts {

    ubyte vtsbank
    
    const uword VTS_TABLE = $A000

    ; the functions in this feature make frequent use of R0-R15
    ; although not part of the declaration, they may clobber R0-R15

    ; called by load_vts_into_bank for bank initialization
    extsub @bank vtsbank VTS_TABLE + 0 = init() clobbers(A,X,Y) 

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
                                    uword  tile_offest @R10 ) clobbers(X,Y) -> ubyte @ A

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

    extsub @bank vtsbank VTS_TABLE + 6 = select(uword context @ R3) clobbers(A,X,Y)

    ; scale of 1.0 keeps the original size. 1.0 is the default
    ; scale less than 1.0 increases the size, the practical limit is around 0.75 
    ; scale more than 1.0 decreases the size, the practical limit is around 3.0
    extsub @bank vtsbank VTS_TABLE + 9 = scale(float fscale @ FAC1) clobbers(A,X,Y) ; default is 1.0

    ; degree may be positive or negative
    extsub @bank vtsbank VTS_TABLE + 12 = rotate(word degree @ AY) clobbers(X) 
    extsub @bank vtsbank VTS_TABLE + 15 = rotate_f(float degree @ FAC1) clobbers(A,X,Y) ; address of the degrees float
    extsub @bank vtsbank VTS_TABLE + 18 = rotate_r(float radians @ FAC1, bool clockwise @ R0) clobbers(A,X,Y)
    extsub @bank vtsbank VTS_TABLE + 21 = rotate_p(float angle @ FAC1, bool clockwise @ R0) clobbers(A,X,Y) ; multiple of PI
    extsub @bank vtsbank VTS_TABLE + 24 = shear_v(word extent @ AY) clobbers(X)
    extsub @bank vtsbank VTS_TABLE + 27 = shear_h(word extent @ AY) clobbers(X)

    ; reset the VERA tile map transforms
    extsub @bank vtsbank VTS_TABLE + 30 = reset_fx() clobbers(A,X,Y)

    ; create a tile set from a sprite when the context is set
    ; 4-bit $80 or $800; 8-bit $100 or $1000
    extsub @bank vtsbank VTS_TABLE + 33 = sprite_to_tile_set(uword ts_size @ AY) clobbers(X) -> ubyte @ A 
        const ubyte SPRITE_TO_TILE_OK            = 0
        const ubyte SPRITE_TO_TILE_SIZE_ERROR     = 1
        const ubyte SPRITE_TO_TILE_BIT_SIZE_ERROR = 2

    ; when the sprite and tile set dimensions are mismatched
    ; same return codes as sprite_to_tile_set() shown above    
    extsub @bank vtsbank VTS_TABLE + 36 = grow_sprite_to_tile_set(ubyte sprite_number @ R0,
                                                                 ubyte tileset_bank   @ R1,
                                                                 uword tileset_addr   @ R2,
                                                                 uword tileset_size   @ R3 ) clobbers(X,Y) -> ubyte @ A 
                                                                 
}