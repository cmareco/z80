const std = @import("std");

pub const z80 = struct {
    // special purpose registers
    pc: u16 = 0x0000, // program counter
    sp: u16 = 0x0000, // stack pointer
    ix: u16 = 0x0000, // index register
    iy: u16 = 0x0000, // index register
    i: u8 = 0x00, // interrupt control vector register
    r: u8 = 0x00, // refresh register
    //general purpose registers
    b: u8 = 0, // register b
    c: u8 = 0, // register c
    d: u8 = 0, // register d
    e: u8 = 0, // register e
    h: u8 = 0, // register h
    l: u8 = 0, // register l
    // accumulator and flags
    a: u8 = 0, // accumulator
    f: flags_struct = .{ .byte = 0x02 }, // initial flags, set carry and zero
    // alternate registers
    b_: u8 = 0,
    c_: u8 = 0,
    d_: u8 = 0,
    e_: u8 = 0,
    h_: u8 = 0,
    l_: u8 = 0,
    a_: u8 = 0,
    f_: u8 = 0x02,

    running_state: run_state = .running,

    // interrupts
    inte: u1 = 0, // interrupt enable bit
    interrupt_req: u1 = 0, // interrupt request signal
    interrupt_delay: u4 = 0, // ??
    interrupt_vector: u8 = 0, // opcode provided by interrupt req

    iff1: u1 = 0,
    iff2: u1 = 0,
    interrupt_mode: u2 = 0, // 0 = IM0, 1 = IM1, 2 = IM2

    ports_in: []u8,
    ports_out: []u8,

    cycles: u32 = 0,

    pub const register_pairs = enum(u2) {
        BC = 0,
        DE = 1,
        HL = 2,
        AF = 3,
    };

    pub const flag_bits = packed struct { c: u1, n: u1, pv: u1, x1: u1, h: u1, x2: u1, z: u1, s: u1 };

    const flags_struct = packed union {
        byte: u8,
        bits: flag_bits,
    };

    const run_state = enum {
        running,
        halted,
        stopped,
    };

    // zig fmt: off
    pub const opcodes = enum(u8) {
        NOP,        LD_BC_NN,    LD__BC__A,    INC_BC,      INC_B,      DEC_B,      LD_B_N,     RLCA,        EX_AF_AF_, ADD_HL_BC,   LD_A__BC_,    DEC_BC,      INC_C,      DEC_C,      LD_C_N,     RRCA,
        DJNZ_D,     LD_DE_NN,    LD__DE__A,    INC_DE,      INC_D,      DEC_D,      LD_D_N,     RLA,         JR_D,       ADD_HL_DE,   LD_A__DE_,    DEC_DE,      INC_E,      DEC_E,      LD_E_N,     RRA,
        JR_NZ_D,    LD_HL_NN,    LD__NN__HL,   INC_HL,      INC_H,      DEC_H,      LD_H_N,     DAA,         JR_Z_D,     ADD_HL_HL,   LD_HL__NN_,   DEC_HL,      INC_L,      DEC_L,      LD_L_N,     CPL,
        JR_NC_D,    LD_SP_NN,    LD__NN__A,    INC_SP,      INC__HL_,   DEC__HL_,   LD__HL__N,  SCF,         JR_C_D,     ADD_HL_SP,   LD_A__NN_,    DEC_SP,      INC_A,      DEC_A,      LD_A_N,     CCF,
        LD_B_B,     LD_B_C,      LD_B_D,      LD_B_E,      LD_B_H,      LD_B_L,     LD_B__HL_,  LD_B_A,      LD_C_B,     LD_C_C,      LD_C_D,      LD_C_E,      LD_C_H,      LD_C_L,     LD_C__HL_,  LD_C_A,
        LD_D_B,     LD_D_C,      LD_D_D,      LD_D_E,      LD_D_H,      LD_D_L,     LD_D__HL_,  LD_D_A,      LD_E_B,     LD_E_C,      LD_E_D,      LD_E_E,      LD_E_H,      LD_E_L,     LD_E__HL_,  LD_E_A,
        LD_H_B,     LD_H_C,      LD_H_D,      LD_H_E,      LD_H_H,      LD_H_L,     LD_H__HL_,  LD_H_A,      LD_L_B,     LD_L_C,      LD_L_D,      LD_L_E,      LD_L_H,      LD_L_L,     LD_L__HL_,  LD_L_A,
        LD__HL__B,  LD__HL__C,   LD__HL__D,   LD__HL__E,   LD__HL__H,   LD__HL__L,  HALT,       LD__HL__A,   LD_A_B,     LD_A_C,      LD_A_D,      LD_A_E,      LD_A_H,      LD_A_L,     LD_A__HL_,  LD_A_A,
        ADD_A_B,    ADD_A_C,     ADD_A_D,     ADD_A_E,     ADD_A_H,     ADD_A_L,    ADD_A__HL_,ADD_A_A,     ADC_A_B,    ADC_A_C,     ADC_A_D,     ADC_A_E,     ADC_A_H,     ADC_A_L,    ADC_A__HL_, ADC_A_A,
        SUB_B,      SUB_C,       SUB_D,       SUB_E,       SUB_H,       SUB_L,      SUB__HL_,   SUB_A,       SBC_A_B,    SBC_A_C,     SBC_A_D,     SBC_A_E,     SBC_A_H,     SBC_A_L,    SBC_A__HL_, SBC_A_A,
        AND_B,      AND_C,       AND_D,       AND_E,       AND_H,       AND_L,      AND__HL_,   AND_A,       XOR_B,      XOR_C,       XOR_D,       XOR_E,       XOR_H,       XOR_L,      XOR__HL_,   XOR_A,
        OR_B,       OR_C,        OR_D,        OR_E,        OR_H,        OR_L,       OR__HL_,    OR_A,        CP_B,       CP_C,        CP_D,        CP_E,        CP_H,        CP_L,       CP__HL_,    CP_A,
        RET_NZ,     POP_BC,      JP_NZ_NN,    JP_NN,       CALL_NZ_NN,  PUSH_BC,     ADD_A_N,    RST_00H,     RET_Z,      RET,         JP_Z_NN,     BIT,         CALL_Z_NN,   CALL_NN,     ADC_A_N,    RST_08H,
        RET_NC,     POP_DE,      JP_NC_NN,    OUT__N__A,   CALL_NC_NN,  PUSH_DE,     SUB_N,      RST_10H,     RET_C,      EXX,         JP_C_NN,     IN_A__N_,    CALL_C_NN,   IX,          SBC_A_N,    RST_18H,
        RET_PO,     POP_HL,      JP_PO_NN,    EX__SP__HL,  CALL_PO_NN,  PUSH_HL,     AND_N,      RST_20H,     RET_PE,     JP__HL_,     JP_PE_NN,    EX_DE_HL,    CALL_PE_NN,  MISC,        XOR_N,      RST_28H,
        RET_P,      POP_AF,      JP_P_NN,     DI,          CALL_P_NN,   PUSH_AF,     OR_N,       RST_30H,     RET_M,      LD_SP_HL,    JP_M_NN,     EI,          CALL_M_NN,   IY,          CP_N,       RST_38H,
    };    

    pub const opcode_names = [_][]const u8{
        "nop",        "ld bc,nn",    "ld (bc),a",    "inc bc",    "inc b",    "dec b",    "ld b,n",    "rlca",       "ex af,af'", "add hl,bc", "ld a,(bc)", "dec bc",    "inc c",    "dec c",    "ld c,n",    "rrca",
        "djnz d",     "ld de,nn",    "ld (de),a",    "inc de",    "inc d",    "dec d",    "ld d,n",    "rla",        "jr d",      "add hl,de", "ld a,(de)", "dec de",    "inc e",    "dec e",    "ld e,n",    "rra",
        "jr nz,d",    "ld hl,nn",    "ld (nn),hl",   "inc hl",    "inc h",    "dec h",    "ld h,n",    "daa",        "jr z,d",    "add hl,hl", "ld hl,(nn)","dec hl",    "inc l",    "dec l",    "ld l,n",    "cpl",
        "jr nc,d",    "ld sp,nn",    "ld (nn),a",    "inc sp",    "inc (hl)", "dec (hl)", "ld (hl),n", "scf",        "jr c,d",    "add hl,sp", "ld a,(nn)", "dec sp",    "inc a",    "dec a",    "ld a,n",    "ccf",
        "ld b,b",     "ld b,c",      "ld b,d",       "ld b,e",    "ld b,h",   "ld b,l",   "ld b,(hl)", "ld b,a",     "ld c,b",    "ld c,c",    "ld c,d",    "ld c,e",    "ld c,h",   "ld c,l",   "ld c,(hl)", "ld c,a",
        "ld d,b",     "ld d,c",      "ld d,d",       "ld d,e",    "ld d,h",   "ld d,l",   "ld d,(hl)", "ld d,a",     "ld e,b",    "ld e,c",    "ld e,d",    "ld e,e",    "ld e,h",   "ld e,l",   "ld e,(hl)", "ld e,a",
        "ld h,b",     "ld h,c",      "ld h,d",       "ld h,e",    "ld h,h",   "ld h,l",   "ld h,(hl)", "ld h,a",     "ld l,b",    "ld l,c",    "ld l,d",    "ld l,e",    "ld l,h",   "ld l,l",   "ld l,(hl)", "ld l,a",
        "ld (hl),b",  "ld (hl),c",   "ld (hl),d",    "ld (hl),e", "ld (hl),h","ld (hl),l","halt",      "ld (hl),a",  "ld a,b",    "ld a,c",    "ld a,d",    "ld a,e",    "ld a,h",   "ld a,l",   "ld a,(hl)", "ld a,a",
        "add a,b",    "add a,c",     "add a,d",      "add a,e",   "add a,h", "add a,l", "add a,(hl)","add a,a",    "adc a,b",   "adc a,c",   "adc a,d",   "adc a,e",   "adc a,h",  "adc a,l",  "adc a,(hl)","adc a,a",
        "sub b",      "sub c",       "sub d",        "sub e",     "sub h",   "sub l",   "sub (hl)",  "sub a",      "sbc a,b",   "sbc a,c",   "sbc a,d",   "sbc a,e",   "sbc a,h",  "sbc a,l",  "sbc a,(hl)","sbc a,a",
        "and b",      "and c",       "and d",        "and e",     "and h",   "and l",   "and (hl)",  "and a",      "xor b",     "xor c",     "xor d",     "xor e",     "xor h",    "xor l",    "xor (hl)",  "xor a",
        "or b",       "or c",        "or d",         "or e",      "or h",    "or l",    "or (hl)",   "or a",       "cp b",      "cp c",      "cp d",      "cp e",      "cp h",     "cp l",     "cp (hl)",   "cp a",
        "ret nz",     "pop bc",      "jp nz,nn",     "jp nn",     "call nz,nn","push bc","add a,n",  "rst 00h",    "ret z",     "ret",       "jp z,nn",   "Bit",       "call z,nn","call nn", "adc a,n",   "rst 08h",
        "ret nc",     "pop de",      "jp nc,nn",     "out (n),a", "call nc,nn","push de","sub n",    "rst 10h",    "ret c",     "exx",       "jp c,nn",   "in a,(n)",  "call c,nn","IX",      "sbc a,n",   "rst 18h",
        "ret po",     "pop hl",      "jp po,nn",     "ex (sp),hl","call po,nn","push hl","and n",    "rst 20h",    "ret pe",    "jp (hl)",   "jp pe,nn",  "ex de,hl",  "call pe,nn","Misc.",   "xor n",     "rst 28h",
        "ret p",      "pop af",      "jp p,nn",      "di",        "call p,nn","push af","or n",     "rst 30h",    "ret m",     "ld sp,hl",  "jp m,nn",   "ei",        "call m,nn","IY",      "cp n",      "rst 38h",
    };

    pub const bit_opcodes = enum(u8) {
        RLC_B,    RLC_C,    RLC_D,    RLC_E,    RLC_H,    RLC_L,    RLC__HL_,    RLC_A,    RRC_B,    RRC_C,    RRC_D,    RRC_E,    RRC_H,    RRC_L,    RRC__HL_,    RRC_A,
        RL_B,    RL_C,    RL_D,    RL_E,    RL_H,    RL_L,    RL__HL_,    RL_A,    RR_B,    RR_C,    RR_D,    RR_E,    RR_H,    RR_L,    RR__HL_,    RR_A,
        SLA_B,    SLA_C,    SLA_D,    SLA_E,    SLA_H,    SLA_L,    SLA__HL_,    SLA_A,    SRA_B,    SRA_C,    SRA_D,    SRA_E,    SRA_H,    SRA_L,    SRA__HL_,    SRA_A,
        SLL_B,    SLL_C,    SLL_D,    SLL_E,    SLL_H,    SLL_L,    SLL__HL_,    SLL_A,    SRL_B,    SRL_C,    SRL_D,    SRL_E,    SRL_H,    SRL_L,    SRL__HL_,    SRL_A,
        BIT_0_B,    BIT_0_C,    BIT_0_D,    BIT_0_E,    BIT_0_H,    BIT_0_L,    BIT_0__HL_,    BIT_0_A,    BIT_1_B,    BIT_1_C,    BIT_1_D,    BIT_1_E,    BIT_1_H,    BIT_1_L,    BIT_1__HL_,    BIT_1_A,
        BIT_2_B,    BIT_2_C,    BIT_2_D,    BIT_2_E,    BIT_2_H,    BIT_2_L,    BIT_2__HL_,    BIT_2_A,    BIT_3_B,    BIT_3_C,    BIT_3_D,    BIT_3_E,    BIT_3_H,    BIT_3_L,    BIT_3__HL_,    BIT_3_A,
        BIT_4_B,    BIT_4_C,    BIT_4_D,    BIT_4_E,    BIT_4_H,    BIT_4_L,    BIT_4__HL_,    BIT_4_A,    BIT_5_B,    BIT_5_C,    BIT_5_D,    BIT_5_E,    BIT_5_H,    BIT_5_L,    BIT_5__HL_,    BIT_5_A,
        BIT_6_B,    BIT_6_C,    BIT_6_D,    BIT_6_E,    BIT_6_H,    BIT_6_L,    BIT_6__HL_,    BIT_6_A,    BIT_7_B,    BIT_7_C,    BIT_7_D,    BIT_7_E,    BIT_7_H,    BIT_7_L,    BIT_7__HL_,    BIT_7_A,
        RES_0_B,    RES_0_C,    RES_0_D,    RES_0_E,    RES_0_H,    RES_0_L,    RES_0__HL_,    RES_0_A,    RES_1_B,    RES_1_C,    RES_1_D,    RES_1_E,    RES_1_H,    RES_1_L,    RES_1__HL_,    RES_1_A,
        RES_2_B,    RES_2_C,    RES_2_D,    RES_2_E,    RES_2_H,    RES_2_L,    RES_2__HL_,    RES_2_A,    RES_3_B,    RES_3_C,    RES_3_D,    RES_3_E,    RES_3_H,    RES_3_L,    RES_3__HL_,    RES_3_A,
        RES_4_B,    RES_4_C,    RES_4_D,    RES_4_E,    RES_4_H,    RES_4_L,    RES_4__HL_,    RES_4_A,    RES_5_B,    RES_5_C,    RES_5_D,    RES_5_E,    RES_5_H,    RES_5_L,    RES_5__HL_,    RES_5_A,
        RES_6_B,    RES_6_C,    RES_6_D,    RES_6_E,    RES_6_H,    RES_6_L,    RES_6__HL_,    RES_6_A,    RES_7_B,    RES_7_C,    RES_7_D,    RES_7_E,    RES_7_H,    RES_7_L,    RES_7__HL_,    RES_7_A,
        SET_0_B,    SET_0_C,    SET_0_D,    SET_0_E,    SET_0_H,    SET_0_L,    SET_0__HL_,    SET_0_A,    SET_1_B,    SET_1_C,    SET_1_D,    SET_1_E,    SET_1_H,    SET_1_L,    SET_1__HL_,    SET_1_A,
        SET_2_B,    SET_2_C,    SET_2_D,    SET_2_E,    SET_2_H,    SET_2_L,    SET_2__HL_,    SET_2_A,    SET_3_B,    SET_3_C,    SET_3_D,    SET_3_E,    SET_3_H,    SET_3_L,    SET_3__HL_,    SET_3_A,
        SET_4_B,    SET_4_C,    SET_4_D,    SET_4_E,    SET_4_H,    SET_4_L,    SET_4__HL_,    SET_4_A,    SET_5_B,    SET_5_C,    SET_5_D,    SET_5_E,    SET_5_H,    SET_5_L,    SET_5__HL_,    SET_5_A,
        SET_6_B,    SET_6_C,    SET_6_D,    SET_6_E,    SET_6_H,    SET_6_L,    SET_6__HL_,    SET_6_A,    SET_7_B,    SET_7_C,    SET_7_D,    SET_7_E,    SET_7_H,    SET_7_L,    SET_7__HL_,    SET_7_A,       
    };

    pub const bit_opcode_names = [_][]const u8{
        "rlc b",    "rlc c",    "rlc d",    "rlc e",    "rlc h",    "rlc l",    "rlc (hl)",    "rlc a",    "rrc b",    "rrc c",    "rrc d",    "rrc e",    "rrc h",    "rrc l",    "rrc (hl)",    "rrc a",
        "rl b",     "rl c",     "rl d",     "rl e",     "rl h",     "rl l",     "rl (hl)",     "rl a",     "rr b",     "rr c",     "rr d",     "rr e",     "rr h",     "rr l",     "rr (hl)",     "rr a",
        "sla b",    "sla c",    "sla d",    "sla e",    "sla h",    "sla l",    "sla (hl)",    "sla a",    "sra b",    "sra c",    "sra d",    "sra e",    "sra h",    "sra l",    "sra (hl)",    "sra a",
        "sll b",    "sll c",    "sll d",    "sll e",    "sll h",    "sll l",    "sll (hl)",    "sll a",    "srl b",    "srl c",    "srl d",    "srl e",    "srl h",    "srl l",    "srl (hl)",    "srl a",
        "bit 0,b",  "bit 0,c",  "bit 0,d",  "bit 0,e",  "bit 0,h",  "bit 0,l",  "bit 0,(hl)",  "bit 0,a",  "bit 1,b",  "bit 1,c",  "bit 1,d",  "bit 1,e",  "bit 1,h",  "bit 1,l",  "bit 1,(hl)",  "bit 1,a",
        "bit 2,b",  "bit 2,c",  "bit 2,d",  "bit 2,e",  "bit 2,h",  "bit 2,l",  "bit 2,(hl)",  "bit 2,a",  "bit 3,b",  "bit 3,c",  "bit 3,d",  "bit 3,e",  "bit 3,h",  "bit 3,l",  "bit 3,(hl)",  "bit 3,a",
        "bit 4,b",  "bit 4,c",  "bit 4,d",  "bit 4,e",  "bit 4,h",  "bit 4,l",  "bit 4,(hl)",  "bit 4,a",  "bit 5,b",  "bit 5,c",  "bit 5,d",  "bit 5,e",  "bit 5   ,h",  "bit 5,l",  "bit 5,(hl)",  "bit 5,a",
        "bit 6,b",  "bit 6,c",  "bit 6,d",  "bit 6,e",  "bit 6,h",  "bit 6,l",  "bit 6,(hl)",  "bit 6,a",  "bit 7,b",  "bit 7,c",  "bit 7,d ",  "bit 7,e",  "bit 7,h",  "bit 7,l",  "bit 7,(hl)",  "bit 7,a",
        "res 0,b",  "res 0,c",  "res 0,d",  "res 0,e",  "res 0,h",  "res 0,l",  "res 0,(hl)",  "res 0,a",  "res 1,b",  "res 1,c",  "res 1,d",  "res 1,e",  "res 1,h",  "res 1,l",  "res 1,(hl)",  "res 1,a",
        "res 2,b",  "res 2,c",  "res 2,d",  "res 2,e",  "res 2,h",  "res 2,l",  "res 2,(hl)",  "res 2,a",  "res 3,b",  "res 3,c",  "res 3,d",  "res 3,e",  "res 3,h",  "res 3,l",  "res 3,(hl)",  "res 3,a",
        "res 4,b",  "res 4,c",  "res 4,d",  "res 4,e",  "res 4,h",  "res 4,l",  "res 4,(hl)",  "res 4,a",  "res 5,b",  "res 5,c",  "res 5,d",  "res 5,e",  "res 5,h",  "res 5,l",  "res 5,(hl)",  "res 5,a",
        "res 6,b",  "res 6,c",  "res 6,d",  "res 6,e",  "res 6,h",  "res 6,l",  "res 6,(hl)",  "res 6,a",  "res 7,b",  "res 7,c",  "res 7,d",  "res 7,e",  "res 7,h",  "res 7,l",  "res 7,(hl)",  "res 7,a",
        "set 0,b",  "set 0,c",  "set 0,d",  "set 0,e",  "set 0,h",  "set 0,l",  "set 0,(hl)",  "set 0,a",  "set 1,b",  "set 1,c",  "set 1,d",  "set 1,e",  "set 1,h",  "set 1,l",  "set 1,(hl)",  "set 1,a",
        "set 2,b",  "set 2,c",  "set 2,d",  "set 2,e",  "set 2,h",  "set 2,l",  "set 2,(hl)",  "set 2,a",  "set 3,b",  "set 3,c",  "set 3,d",  "set 3,e",  "set 3,h",  "set 3,l",  "set 3,(hl)",  "set 3,a",
        "set 4,b",  "set 4,c",  "set 4,d",  "set 4,e",  "set 4,h",  "set 4,l",  "set 4,(hl)",  "set 4,a",  "set 5,b",  "set 5,c",  "set 5,d",  "set 5,e",  "set 5,h",  "set 5,l",  "set 5,(hl)",  "set 5,a",
        "set 6,b",  "set 6,c",  "set 6,d",  "set 6,e",  "set 6,h",  "set 6,l",  "set 6,(hl)",  "set 6,a",  "set 7,b",  "set 7,c",  "set 7,d",  "set 7,e",  "set 7,h",  "set 7,l",  "set 7,(hl)",  "set 7,a",       
    };


    // includes undocumented opcodes but no Z180 opcodes
    pub const misc_opcodes = enum(u8) {
        IN_B__C_=0x40, OUT__C__B, SBC_HL_BC, LD__NN__BC, NEG, RETN, IM0, LD_I_A, IN_C__C_, OUT__C__C, ADC_HL_BC, LD_BC__NN_,MLT_BC, RETI, NOP0, LD_R_A,
        IN_D__C_, OUT__C__D, SBC_HL_DE, LD__NN__DE, NOP1, NOP2, IM1, LD_A_I, IN_E__C_, OUT__C__E, ADC_HL_DE, LD_DE__NN_, MLT_DE, NOP3, IM2, LD_A_R,
        IN_H__C_, OUT__C__H, SBC_HL_HL, LD__NN__HL, NOP4, NOP5, NOP6, RRD, IN_L__C_, OUT__C__L, ADC_HL_HL, LD_HL__NN_, MLT_HL, NOP7, NOP8, RLD,
        IN__C_, OUT__C__0, SBC_HL_SP, LD__NN__SP, NOP9, NOPA, NOPB, NOPC, IN_A__C_, OUT__C__A, ADC_HL_SP, LD_SP__NN_, MLT_SP, NOPD, NOPE, NOPF,  
        LDI=0xA0, CPI, INI, OUTI, LDD=0xA8, CPD, IND, OUTD, 
        LDIR=0xB0, CPIR, INIR, OTIR, LDDR=0xB8, CPDR, INDR, OTDR
    };

    pub const misc_opcode_names = [_][]const u8{
        "in b,(c)",   "out (c),b",   "sbc hl,bc",   "ld (nn),bc", "neg",      "retn",      "im 0",      "ld i,a",
        "in c,(c)",   "out (c),c",   "adc hl,bc",   "ld bc,(nn)", "mlt bc",   "reti",      "nop0",      "ld r,a",
        "in d,(c)",   "out (c),d",   "sbc hl,de",   "ld (nn),de", "nop1",     "nop2",      "im 1",      "ld a,i",
        "in e,(c)",   "out (c),e",   "adc hl,de",   "ld de,(nn)", "mlt de",   "nop3",      "im 2",      "ld a,r",
        "in h,(c)",   "out (c),h",   "sbc hl,hl",   "ld (nn),hl", "nop4",     "nop5",      "nop6",      "rrd",
        "in l,(c)",   "out (c),l",   "adc hl,hl",   "ld hl,(nn)", "mlt hl",   "nop7",      "nop8",      "rld",
        "in (c)",     "out (c),0",   "sbc hl,sp",   "ld (nn),sp", "nop9",     "nopa",      "nopb",      "nopc",
        "in a,(c)",   "out (c),a",   "adc hl,sp",   "ld sp,(nn)", "mlt sp",   "nopd",      "nope",      "nopf",
        "ldi",        "cpi",         "ini",         "outi",       "ldd",      "cpd",       "ind",       "outd",
        "ldir",       "cpir",        "inir",        "otir",       "lddr",     "cpdr",      "indr",      "otdr",
    };

    // includes undocumented opcodes
    pub const ix_opcodes = enum(u8) {
        INC_B = 0x04, DEC_B = 0x05, LD_B_N = 0x06, ADD_IX_BC = 0x09, INC_C = 0x0C, DEC_C = 0x0D, LD_C_N = 0x0E,
        INC_D = 0x14, DEC_D = 0x15, LD_D_N = 0x16, ADD_IX_DE = 0x19, INC_E = 0x1C, DEC_E = 0x1D, LD_E_N = 0x1E,
        LD_IX_NN = 0x21, LD__NN__IX, INC_IX, INC_IXH, DEC_IXH, LD_IXH_N, ADD_IX_IX = 0x29, LD_IX__NN_, DEC_IX, INC_IXL, DEC_IXL, LD_IXL_N,
        INC__IXD_ = 0x34, DEC__IXD_ = 0x35, LD__IXD__N = 0x36, ADD_IX_SP = 0x39, INC_A = 0x3C, DEC_A = 0x3D, LD_A_N = 0x3E,
        LD_B_B = 0x40, LD_B_C, LD_B_D, LD_B_E, LD_B_IXH, LD_B_IXL, LD_B__IXD_, LD_B_A, LD_C_B, LD_C_C, LD_C_D, LD_C_E, LD_C_IXH, LD_C_IXL, LD_C__IXD_, LD_C_A,
        LD_D_B, LD_D_C, LD_D_D, LD_D_E, LD_D_IXH, LD_D_IXL, LD_D__IXD_, LD_D_A, LD_E_B, LD_E_C, LD_E_D, LD_E_E, LD_E_IXH, LD_E_IXL, LD_E__IXD_, LD_E_A,
        LD_IXH_B, LD_IXH_C, LD_IXH_D, LD_IXH_E, LD_IXH_IXH, LD_IXH_IXL, LD_H__IXD_, LD_IXH_A, LD_IXL_B, LD_IXL_C, LD_IXL_D, LD_IXL_E, LD_IXL_IXH, LD_IXL_IXL, LD_L__IXD_, LD_IXL_A,
        LD__IXD__B, LD__IXD__C, LD__IXD__D, LD__IXD__E, LD__IXD__H, LD__IXD__L, LD__IXD__A = 0x77, LD_A_B, LD_A_C, LD_A_D, LD_A_E, LD_A_IXH, LD_A_IXL, LD_A__IXD_, LD_A_A,
        ADD_A_B = 0x80, ADD_A_C, ADD_A_D, ADD_A_E, ADD_A_IXH, ADD_A_IXL, ADD_A__IXD_, ADD_A_A, ADC_A_B, ADC_A_C, ADC_A_D, ADC_A_E, ADC_A_IXH, ADC_A_IXL, ADC_A__IXD_, ADC_A_A,
        SUB_B = 0x90, SUB_C, SUB_D, SUB_E, SUB_IXH, SUB_IXL, SUB__IXD_, SUB_A, SBC_A_B, SBC_A_C, SBC_A_D, SBC_A_E, SBC_A_IXH, SBC_A_IXL, SBC_A__IXD_, SBC_A_A,
        AND_B = 0xA0, AND_C, AND_D, AND_E, AND_IXH, AND_IXL, AND__IXD_, AND_A, XOR_B, XOR_C, XOR_D, XOR_E, XOR_IXH, XOR_IXL, XOR__IXD_, XOR_A,
        OR_B = 0xB0, OR_C, OR_D, OR_E, OR_IXH, OR_IXL, OR__IXD_, OR_A, CP_B, CP_C, CP_D, CP_E, CP_IXH, CP_IXL, CP__IXD_, CP_A,
        IX_BIT = 0xCB,
        POP_IX = 0xE1, EX__SP_IX = 0xE3, JP_IX = 0xE9, PUSH_IX = 0xE5, 
        LD_SP_IX = 0xF9
    };

    pub const ix_opcode_names = [_][]const u8{
        "inc b", "dec b", "ld b,n", "add ix,bc", "inc c", "dec c", "ld c,n",
        "inc d", "dec d", "ld d,n", "add ix,de", "inc e", "dec e", "ld e,n",
        "ld ix,nn", "ld (nn),ix", "inc ix", "dec ix", "ld ixh,n", "add ix,ix", "ld ixl,n", "dec ix", "inc ixl", "dec ixl", "ld ixl,n",
        "inc (ix+d)", "dec (ix+d)", "ld (ix+d),n", "add ix,sp", "inc a", "dec a", "ld a,n",
        "ld b,b", "ld b,c", "ld b,d", "ld b,e", "ld b,ixh", "ld b,ixl", "ld b,(ix+d)", "ld b,a",
        "ld c,b", "ld c,c", "ld c,d", "ld c,e", "ld c,ixh", "ld c,ixl", "ld c,(ix+d)", "ld c,a",
        "ld d,b", "ld d,c", "ld d,d", "ld d,e", "ld d,ixh", "ld d,ixl", "ld d,(ix+d)", "ld d,a",
        "ld e,b", "ld e,c", "ld e,d", "ld e,e", "ld e,ixh", "ld e,ixl", "ld e,(ix+d)", "ld e,a",
        "ld ixh,b", "ld ixh,c", "ld ixh,d", "ld ixh,e", "ld ixh,ixh", "ld ixh,ixl", "ld ixh,(ix+d)", "ld ixh,a",
        "ld ixl,b", "ld ixl,c", "ld ixl,d", "ld ixl,e", "ld ixl,ixh", "ld ixl,ixl", "ld ixl,(ix+d)", "ld ixl,a",
        "ld (ix+d),b", "ld (ix+d),c", "ld (ix+d),d", "ld (ix+d),e", "ld (ix+d),h", "ld (ix+d),l", "ld (ix+d),a",
        "ld a,b", "ld a,c", "ld a,d", "ld a,e", "ld a,ixh", "ld a,ixl", "ld a,(ix+d)", "ld a,a",
        "add a,b", "add a,c", "add a,d", "add a,e", "add a,ixh", "add a,ixl", "add a,(ix+d)", "add a,a",
        "adc a,b", "adc a,c", "adc a,d", "adc a,e", "adc a,ixh", "adc a,ixl", "adc a,(ix+d)", "adc a,a",
        "sub b", "sub c", "sub d", "sub e", "sub ixh", "sub ixl", "sub (ix+d)", "sub a",
        "sbc a,b", "sbc a,c", "sbc a,d", "sbc a,e", "sbc a,ixh", "sbc a,ixl", "sbc a,(ix+d)", "sbc a,a",
        "and b", "and c", "and d", "and e", "and ixh", "and ixl", "and (ix+d)", "and a",
        "xor b", "xor c", "xor d", "xor e", "xor ixh", "xor ixl", "xor (ix+d)", "xor a",
        "or b", "or c", "or d", "or e", "or ixh", "or ixl", "or (ix+d)", "or a",
        "cp b", "cp c", "cp d", "cp e", "cp ixh", "cp ixl", "cp (ix+d)", "cp a",
        "pop ix", "ex de,ix", "jp (ix)", "push ix", "ld sp,ix"
    };
    
    // includes undocumented opcodes
    pub const iy_opcodes = enum(u8) {
        INC_B = 0x04, DEC_B = 0x05, LD_B_N = 0x06, ADD_IY_BC = 0x09, INC_C = 0x0C, DEC_C = 0x0D, LD_C_N = 0x0E,
        INC_D = 0x14, DEC_D = 0x15, LD_D_N = 0x16, ADD_IY_DE = 0x19, INC_E = 0x1C, DEC_E = 0x1D, LD_E_N = 0x1E,
        LD_IY_NN = 0x21, LD__NN__IY, INC_IY, INC_IYH, DEC_IYH, LD_IYH_N, ADD_IY_IY = 0x29, LD_IY__NN_, DEC_IY, INC_IYL, DEC_IYL, LD_IYL_N,
        INC__IYD_ = 0x34, DEC__IYD_ = 0x35, LD__IYD__N = 0x36, ADD_IY_SP = 0x39, INC_A = 0x3C, DEC_A = 0x3D, LD_A_N = 0x3E,
        LD_B_B = 0x40, LD_B_C, LD_B_D, LD_B_E, LD_B_IYH, LD_B_IYL, LD_B__IYD_, LD_B_A, LD_C_B, LD_C_C, LD_C_D, LD_C_E, LD_C_IYH, LD_C_IYL, LD_C__IYD_, LD_C_A,
        LD_D_B, LD_D_C, LD_D_D, LD_D_E, LD_D_IYH, LD_D_IYL, LD_D__IYD_, LD_D_A, LD_E_B, LD_E_C, LD_E_D, LD_E_E, LD_E_IYH, LD_E_IYL, LD_E__IYD_, LD_E_A,
        LD_IYH_B, LD_IYH_C, LD_IYH_D, LD_IYH_E, LD_IYH_IYH, LD_IYH_IYL, LD_H__IYD_, LD_IYH_A, LD_IYL_B, LD_IYL_C, LD_IYL_D, LD_IYL_E, LD_IYL_IYH, LD_IYL_IYL, LD_L__IYD_, LD_IYL_A,
        LD__IYD__B, LD__IYD__C, LD__IYD__D, LD__IYD__E, LD__IYD__H, LD__IYD__L, LD__IYD__A = 0x77, LD_A_B, LD_A_C, LD_A_D, LD_A_E, LD_A_IYH, LD_A_IYL, LD_A__IYD_, LD_A_A,
        ADD_A_B = 0x80, ADD_A_C, ADD_A_D, ADD_A_E, ADD_A_IYH, ADD_A_IYL, ADD_A__IYD_, ADD_A_A, ADC_A_B, ADC_A_C, ADC_A_D, ADC_A_E, ADC_A_IYH, ADC_A_IYL, ADC_A__IYD_, ADC_A_A,
        SUB_B = 0x90, SUB_C, SUB_D, SUB_E, SUB_IYH, SUB_IYL, SUB__IYD_, SUB_A, SBC_A_B, SBC_A_C, SBC_A_D, SBC_A_E, SBC_A_IYH, SBC_A_IYL, SBC_A__IYD_, SBC_A_A,
        AND_B = 0xA0, AND_C, AND_D, AND_E, AND_IYH, AND_IYL, AND__IYD_, AND_A, XOR_B, XOR_C, XOR_D, XOR_E, XOR_IYH, XOR_IYL, XOR__IYD_, XOR_A,
        OR_B = 0xB0, OR_C, OR_D, OR_E, OR_IYH, OR_IYL, OR__IYD_, OR_A, CP_B, CP_C, CP_D, CP_E, CP_IYH, CP_IYL, CP__IYD_, CP_A,
        IY_BIT = 0xCB,

        POP_IY = 0xE1, EX__SP_IY = 0xE3, JP_IY = 0xE9, PUSH_IY = 0xE5, 
        LD_SP_IY = 0xF9
    };

    // includes undocumented opcodes
    pub const ix_bit_opcodes = enum(u8) {
        RLC__IXD_B,    RLC__IXD_C,    RLC__IXD_D,    RLC__IXD_E,    RLC__IXD_H,    RLC__IXD_L,    RLC__IXD_,    RLC__IXD_A,    RRC__IXD_B,    RRC__IXD_C,    RRC__IXD_D,    RRC__IXD_E,    RRC__IXD_H,    RRC__IXD_L,    RRC__IXD_,    RRC__IXD_A,
        RL__IXD_B,    RL__IXD_C,    RL__IXD_D,    RL__IXD_E,    RL__IXD_H,    RL__IXD_L,    RL__IXD_,    RL__IXD_A,    RR__IXD_B,    RR__IXD_C,    RR__IXD_D,    RR__IXD_E,    RR__IXD_H,    RR__IXD_L,    RR__IXD_,    RR__IXD_A,
        SLA__IXD_B,    SLA__IXD_C,    SLA__IXD_D,    SLA__IXD_E,    SLA__IXD_H,    SLA__IXD_L,    SLA__IXD_,    SLA__IXD_A,    SRA__IXD_B,    SRA__IXD_C,    SRA__IXD_D,    SRA__IXD_E,    SRA__IXD_H,    SRA__IXD_L,    SRA__IXD_,    SRA__IXD_A,
        SLL__IXD_B,    SLL__IXD_C,    SLL__IXD_D,    SLL__IXD_E,    SLL__IXD_H,    SLL__IXD_L,    SLL__IXD_,    SLL__IXD_A,    SRL__IXD_B,    SRL__IXD_C,    SRL__IXD_D,    SRL__IXD_E,    SRL__IXD_H,    SRL__IXD_L,    SRL__IXD_,    SRL__IXD_A,
        BIT0__IXD0,    BIT0__IXD1,    BIT0__IXD2,    BIT0__IXD3,    BIT0__IXD4,    BIT0__IXD5,    BIT0__IXD6,    BIT0__IXD7,    BIT1__IXD8,    BIT1__IXD9,    BIT1__IXDA,    BIT1__IXDB,    BIT1__IXDC,    BIT1__IXDD,    BIT1__IXDE,    BIT1__IXDF,
        BIT2__IXD0,    BIT2__IXD1,    BIT2__IXD2,    BIT2__IXD3,    BIT2__IXD4,    BIT2__IXD5,    BIT2__IXD6,    BIT2__IXD7,    BIT3__IXD8,    BIT3__IXD9,    BIT3__IXDA,    BIT3__IXDB,    BIT3__IXDC,    BIT3__IXDD,    BIT3__IXDE,    BIT3__IXDF,
        BIT4__IXD0,    BIT4__IXD1,    BIT4__IXD2,    BIT4__IXD3,    BIT4__IXD4,    BIT4__IXD5,    BIT4__IXD6,    BIT4__IXD7,    BIT5__IXD8,    BIT5__IXD9,    BIT5__IXDA,    BIT5__IXDB,    BIT5__IXDC,    BIT5__IXDD,    BIT5__IXDE,    BIT5__IXDF,
        BIT6__IXD0,    BIT6__IXD1,    BIT6__IXD2,    BIT6__IXD3,    BIT6__IXD4,    BIT6__IXD5,    BIT6__IXD6,    BIT6__IXD7,    BIT7__IXD8,    BIT7__IXD9,    BIT7__IXDA,    BIT7__IXDB,    BIT7__IXDC,    BIT7__IXDD,    BIT7__IXDE,    BIT7__IXDF,
        RES0__IXD_B,    RES0__IXD_C,    RES0__IXD_D,    RES0__IXD_E,    RES0__IXD_H,    RES0__IXD_L,    RES0__IXD,    RES0__IXD_A,    RES1__IXD_B,    RES1__IXD_C,    RES1__IXD_D,    RES1__IXD_E,    RES1__IXD_H,    RES1__IXD_L,    RES1__IXD,    RES1__IXD_A,
        RES2__IXD_B,    RES2__IXD_C,    RES2__IXD_D,    RES2__IXD_E,    RES2__IXD_H,    RES2__IXD_L,    RES2__IXD,    RES2__IXD_A,    RES3__IXD_B,    RES3__IXD_C,    RES3__IXD_D,    RES3__IXD_E,    RES3__IXD_H,    RES3__IXD_L,    RES3__IXD,    RES3__IXD_A,
        RES4__IXD_B,    RES4__IXD_C,    RES4__IXD_D,    RES4__IXD_E,    RES4__IXD_H,    RES4__IXD_L,    RES4__IXD,    RES4__IXD_A,    RES5__IXD_B,    RES5__IXD_C,    RES5__IXD_D,    RES5__IXD_E,    RES5__IXD_H,    RES5__IXD_L,    RES5__IXD,    RES5__IXD_A,
        RES6__IXD_B,    RES6__IXD_C,    RES6__IXD_D,    RES6__IXD_E,    RES6__IXD_H,    RES6__IXD_L,    RES6__IXD,    RES6__IXD_A,    RES7__IXD_B,    RES7__IXD_C,    RES7__IXD_D,    RES7__IXD_E,    RES7__IXD_H,    RES7__IXD_L,    RES7__IXD,    RES7__IXD_A,
        SET0__IXD_B,    SET0__IXD_C,    SET0__IXD_D,    SET0__IXD_E,    SET0__IXD_H,    SET0__IXD_L,    SET0__IXD,    SET0__IXD_A,    SET1__IXD_B,    SET1__IXD_C,    SET1__IXD_D,    SET1__IXD_E,    SET1__IXD_H,    SET1__IXD_L,    SET1__IXD,    SET1__IXD_A,
        SET2__IXD_B,    SET2__IXD_C,    SET2__IXD_D,    SET2__IXD_E,    SET2__IXD_H,    SET2__IXD_L,    SET2__IXD,    SET2__IXD_A,    SET3__IXD_B,    SET3__IXD_C,    SET3__IXD_D,    SET3__IXD_E,    SET3__IXD_H,    SET3__IXD_L,    SET3__IXD,    SET3__IXD_A,
        SET4__IXD_B,    SET4__IXD_C,    SET4__IXD_D,    SET4__IXD_E,    SET4__IXD_H,    SET4__IXD_L,    SET4__IXD,    SET4__IXD_A,    SET5__IXD_B,    SET5__IXD_C,    SET5__IXD_D,    SET5__IXD_E,    SET5__IXD_H,    SET5__IXD_L,    SET5__IXD,    SET5__IXD_A,
        SET6__IXD_B,    SET6__IXD_C,    SET6__IXD_D,    SET6__IXD_E,    SET6__IXD_H,    SET6__IXD_L,    SET6__IXD,    SET6__IXD_A,    SET7__IXD_B,    SET7__IXD_C,    SET7__IXD_D,    SET7__IXD_E,    SET7__IXD_H,    SET7__IXD_L,    SET7__IXD,    SET7__IXD_A,    
    };

    // zig fmt: on

    //
    //    AAA    UU   UU  XX    XX
    //   AA AA   UU   UU   XX  XX
    //  AA   AA  UU   UU    XXXX
    //  AAAAAAA  UU   UU     XX
    //  AA   AA  UU   UU    XXXX
    //  AA   AA  UU   UU   XX  XX
    //  AA   AA  UUUUUUU  XX    XX
    //

    // prints the contents of the registers
    fn print_registers(self: *z80) void {
        std.debug.print("A: {X:02} B: {X:02} C: {X:02} D: {X:02} E: {X:02} H: {X:02} L: {X:02} SP: {X:04} PC: {X:04} FLAGS: {X:02}\n", .{ self.a, self.b, self.c, self.d, self.e, self.h, self.l, self.sp, self.pc, self.f.byte });
    }

    // gets the contents of a single register (0-7)
    fn get_register(self: *z80, reg: u8) u8 {
        switch (reg) {
            7 => return self.a,
            0 => return self.b,
            1 => return self.c,
            2 => return self.d,
            3 => return self.e,
            4 => return self.h,
            5 => return self.l,
            else => return 0, // Invalid register
        }
    }

    // sets the contents of a single register (0-7)
    fn set_register(self: *z80, reg: u8, value: u8) void {
        switch (reg) {
            7 => self.a = value,
            0 => self.b = value,
            1 => self.c = value,
            2 => self.d = value,
            3 => self.e = value,
            4 => self.h = value,
            5 => self.l = value,
            else => {}, // Invalid register, do nothing
        }
    }

    // gets a word from 2 bytes (hi, lo)
    fn get_word_from_bytes(hi: u8, lo: u8) u16 {
        return @as(u16, hi) << 8 | @as(u16, lo);
    }

    // gets the contents of a register pair (0-3)
    pub fn get_register_pair(self: *z80, reg_pair: z80.register_pairs) u16 {
        return switch (reg_pair) {
            .BC => get_word_from_bytes(self.b, self.c),
            .DE => get_word_from_bytes(self.d, self.e),
            .HL => get_word_from_bytes(self.h, self.l),
            .AF => get_word_from_bytes(self.a, self.f.byte),
        };
    }

    // sets the contents of a register pair (0-3)
    fn set_register_pair(self: *z80, reg_pair: z80.register_pairs, value: u16) void {
        //const temp = value >> 8;
        const byte1: u8 = @truncate(value >> 8);
        const byte2: u8 = @truncate(value & 0x00FF);
        switch (reg_pair) {
            .BC => {
                self.b = byte1;
                self.c = byte2;
            },
            .DE => {
                self.d = byte1;
                self.e = byte2;
            },
            .HL => {
                self.h = byte1;
                self.l = byte2;
            },
            .AF => {
                self.a = byte1;
                self.f.byte = byte2;
            },
        }
    }

    // ask for an interrupt
    pub fn interrupt(self: *z80, instruction: u8) void {
        self.interrupt_pending = true;
        self.interrupt_vector = instruction;
    }

    // Stack Ops

    // push word onto the stack
    fn push(self: *z80, word: u16, ram: []u8) void {
        // Pushes a word onto the stack
        ram[self.sp - 1] = @truncate(word >> 8);
        ram[self.sp - 2] = @truncate(word & 0x00FF);
        self.sp -= 2; // Decrement stack pointer
    }

    // pops word from the stack
    fn pop(self: *z80, ram: []u8) u16 {
        // Pops a word from the stack
        const byte1 = ram[self.sp + 1];
        const byte2 = ram[self.sp];
        self.sp += 2;
        return get_word_from_bytes(byte1, byte2);
    }

    // Flag-related functions

    // sets the zero, sign and parity flags based on a value
    fn set_zsp(self: *z80, value: u8) void {
        self.f.bits.z = if (value == 0) 1 else 0;
        self.f.bits.s = if (value & 0x80 != 0) 1 else 0;
        self.f.bits.pv = if (@popCount(value) & 1 == 0) 1 else 0; // Even parity
    }

    // calc carry between bit_no and bit_no - 1
    // when doinf a + b + cy
    fn calc_carry(bit_no: u4, a: u8, b: u8, cy: u8) u1 {
        const result = @as(u16, a) + @as(u16, b) + @as(u16, cy);
        const carry = (result ^ a ^ b) &
            (@as(u16, 0x01) << bit_no); // when there's a <<, the LHS must be large
        // enough to accomodate the RHS
        return if (carry != 0) 1 else 0;
    }

    //------------------------------------------------------------
    // Opcode functions
    //

    fn add_offset(ix: u16, offset: u8) u16 {
        const signed: i9 = @as(i9, @as(i8, @bitCast(offset)));
        const unsigned: u8 = @truncate(@abs(signed));

        if (signed < 0)
            return ix -% @as(u16, unsigned)
        else
            return ix +% @as(u16, unsigned);
    }

    fn move_8bit(self: *z80, instruction: u8, ram: []u8) void {
        const dst: u4 = @truncate((instruction & 0b00111000) >> 3);
        const src: u4 = @truncate(instruction & 0b00000111);

        if (dst == src)
            return;

        if (src == 0b110 or dst == 0b110) { // Load using memory
            // get address from H reg pair
            const H = self.get_register_pair(.HL);
            if (src == 0b110) {
                // LD r, (HL))
                self.set_register(dst, ram[H]);
            } else if (dst == 0b110) {
                // LD (HL), r
                ram[H] = self.get_register(src);
            }
        } else // LD r, r
        self.set_register(dst, self.get_register(src));
    }

    // LD_RP_NN
    fn load_immediate_16bit(self: *z80, opcode: z80.opcodes, ram: []u8) void {
        const lo = ram[self.pc];
        const hi = ram[self.pc + 1];
        const word = get_word_from_bytes(hi, lo);

        switch (opcode) {
            .LD_BC_NN => self.set_register_pair(.BC, word),
            .LD_DE_NN => self.set_register_pair(.DE, word),
            .LD_HL_NN => self.set_register_pair(.HL, word),
            .LD_SP_NN => self.sp = word,
            else => {
                unreachable;
            }, // Invalid opcode
        }
        self.pc = self.pc +% 2;
        // Increment PC by 2 to account for the immediate data
    }

    // INC R
    fn inc_8bit(self: *z80, instruction: u8, ram: []u8) void {
        const reg: u4 = @truncate((instruction & 0b00111000) >> 3);
        var value: u8 = undefined;

        if (reg == 0x06) { // increment memory
            const addr = self.get_register_pair(.HL);
            value = ram[addr];
            value = value +% 1;
            ram[addr] = value;
        } else { // increment register
            value = self.get_register(reg);
            value = value +% 1;
            self.set_register(reg, value);
        }
        // Set flags
        self.set_zsp(value);
        self.f.bits.n = 0; // INC clears N flag
        self.f.bits.pv = if (value == 0x80) 1 else 0; // Overflow sets parity/overflow flag
        self.f.bits.h = if ((value & 0x0F) == 0x00) 1 else 0; // Check for half-carry
    }

    // DEC R
    fn dec_8bit(self: *z80, instruction: u8, ram: []u8) void {
        const reg: u4 = @truncate((instruction & 0b00111000) >> 3);
        var value: u8 = undefined;

        if (reg == 0x06) { // decrement memory
            const addr = self.get_register_pair(.HL);
            value = ram[addr];
            value = value -% 1;
            ram[addr] = value;
        } else { // decrement register
            value = self.get_register(reg);
            value = value -% 1;
            self.set_register(reg, value);
        }
        // Set flags
        self.set_zsp(value);
        self.f.bits.n = 1; // DEC sets N flag
        self.f.bits.pv = if (value == 0x7F) 1 else 0; // Overflow sets parity/overflow flag
        self.f.bits.h = if ((value & 0x0F) == 0x0F) 1 else 0;
    }

    // Load immediate value into register or memory
    fn load_immediate_8bit(self: *z80, instruction: u8, ram: []u8) void {
        const reg: u4 = @truncate((instruction & 0b00111000) >> 3);
        const value: u8 = ram[self.pc];

        if (reg == 0x06) { // LD to memory
            const addr = self.get_register_pair(.HL);
            ram[addr] = value;
        } else { // LD to register
            self.set_register(reg, value);
        }
        self.pc = self.pc +% 1;
        // Increment PC by 1 to account for the operand data
    }

    // Double add
    fn add_16(self: *z80, opcode: z80.opcodes) void {
        const value = switch (opcode) {
            .ADD_HL_BC => self.get_register_pair(.BC),
            .ADD_HL_DE => self.get_register_pair(.DE),
            .ADD_HL_HL => self.get_register_pair(.HL),
            .ADD_HL_SP => self.sp,
            else => unreachable,
        };
        const HL = self.get_register_pair(.HL);
        const result = HL +% value;
        self.set_register_pair(.HL, result);
        self.f.bits.c = if (result < HL) 1 else 0; // Set carry if overflow
    }

    // ADD and ADC
    fn add_adc(self: *z80, instruction: u8, ram: []u8, with_carry: bool) void {
        const reg: u4 = @truncate(instruction & 0b00000111);
        var value: u8 = undefined;

        if (reg == 0x06) { // ADD/ADC memory
            const addr = self.get_register_pair(.HL);
            value = ram[addr];
        } else { // ADD/ADC register
            value = self.get_register(reg);
        }

        const carry_in: u8 = if (with_carry) self.f.bits.c else 0;
        const result = @as(u16, self.a) + @as(u16, value) + @as(u16, carry_in);
        const result_u8: u8 = @truncate(result);

        // Set flags
        self.set_zsp(result_u8);
        self.f.bits.c = z80.calc_carry(8, self.a, value, carry_in);
        self.f.bits.h = z80.calc_carry(4, self.a, value, carry_in);

        self.a = result_u8; // Store result in accumulator
    }

    // SUB and SBB
    fn sub_sbb(self: *z80, instruction: u8, ram: []u8, with_borrow: bool) void {
        const reg: u4 = @truncate(instruction & 0b00000111);
        var value: u8 = undefined;

        if (reg == 0x06) { // SUB/SBB memory
            const addr = self.get_register_pair(.HL);
            value = ram[addr];
        } else { // SUB/SBB register
            value = self.get_register(reg);
        }

        const borrow_in: u8 = if (with_borrow) self.f.bits.c else 0;
        const result = @as(u16, self.a) -% (@as(u16, value) +% @as(u16, borrow_in));
        const result_u8: u8 = @truncate(result);

        // Set flags
        self.set_zsp(result_u8);
        self.f.bits.c = if (result > 0xFF) 1 else 0; // Set carry if underflow
        self.f.bits.h = if (((self.a & 0x0F) < ((value & 0x0F) + borrow_in))) 1 else 0; // Correct half-borrow calculation
        self.a = result_u8; // Store result in accumulator
    }

    // ANA, XRA, ORA
    fn and_xor_or(self: *z80, instruction: u8, ram: []u8) void {
        const reg: u4 = @truncate(instruction & 0b00000111);
        var value: u8 = undefined;

        if (reg == 0x06) { // Memory operation
            const addr = self.get_register_pair(.HL);
            value = ram[addr];
        } else { // Register operation
            value = self.get_register(reg);
        }

        const opcode = @as(z80.opcodes, @enumFromInt(instruction));
        switch (opcode) {
            .AND_B, .AND_C, .AND_D, .AND_E, .AND_H, .AND_L, .AND__HL_, .AND_A => {
                self.a = self.a & value; // AND operation
                self.f.bits.h = if (((self.a | value) & 0x08) != 0) 1 else 0;
                //self.f.bits.h = if ((self.a & 0x0F) < (value & 0x0F)) 1 else 0; // Check for half-carry
            },
            .XOR_B, .XOR_C, .XOR_D, .XOR_E, .XOR_H, .XOR_L, .XOR__HL_, .XOR_A => {
                // XOR operation
                self.a = self.a ^ value;
                self.f.bits.h = 0; // No half-carry for XOR
            },
            .OR_B, .OR_C, .OR_D, .OR_E, .OR_H, .OR_L, .OR__HL_, .OR_A => {
                self.a = self.a | value; // OR operation
                self.f.bits.h = 0; // No half-carry for OR
            },
            else => unreachable,
        }
        // Set flags
        self.set_zsp(self.a);
        self.f.bits.c = 0; // No carry for logical operations
    }

    // DAA
    fn daa(self: *z80) void {
        const lsb = self.a & 0x0F;
        const a_orig = self.a;
        var correction: u8 = 0;

        if (lsb > 9 or self.f.bits.h == 1) {
            self.a += 6;
            correction += 6;
        }

        const msb = (self.a & 0xF0) >> 4;
        if (msb > 9 or self.f.bits.c == 1) {
            self.a = (self.a +% 0x60);
            correction += 0x60;
        }

        // set flags
        self.set_zsp(self.a);
        self.f.bits.c = calc_carry(8, a_orig, correction, 0);
        self.f.bits.h = calc_carry(4, a_orig, correction, 0);
    }

    //*************************************************************************
    //
    //  ZZZZZZZZZZZZ       8888888888     0000000000
    //          ZZZ       88        88   00        00
    //        ZZZ         88        88   00        00
    //      ZZZ     ===     88888888     00        00
    //    ZZZ             88        88   00        00
    //  ZZZ               88        88   00        00
    //  ZZZZZZZZZZZZ       8888888888     0000000000

    //  BBBBBB    AAAAAAA    SSSSS   EEEEEE
    //  BB   BB   AA   AA   SS   SS  EE
    //  BBBBB     AA   AA    SSS     EEEE
    //  BB   BB   AAAAAAA      SSS   EE
    //  BB   BB   AA   AA   SS   SS  EE
    //  BBBBBB    AA   AA    SSSSS   EEEEEE

    // rotate bit instructions: left, right, with carry or not
    fn rotate(self: *z80, instruction: u8, ram: []u8, direction: u8, thru_carry: bool) void {
        // get register or memory location
        const reg: u8 = instruction & 0b00000111;
        var value = if (reg == 0b110) ram[self.get_register_pair(.HL)] else self.get_register(reg);
        const carry: u8 = self.f.bits.c; // Save carry flag

        switch (direction) {
            'L' => {
                self.f.bits.c = if (value & 0x80 != 0) 1 else 0; // Set carry flag with msb
                value = (value << 1) | (if (thru_carry) carry else self.f.bits.c); // Rotate left
            },
            'R' => {
                self.f.bits.c = if (value & 0x01 != 0) 1 else 0; // Set carry flag with lsb
                value = (value >> 1) | (if (thru_carry) carry << 7 else @as(u8, self.f.bits.c) << 7); // Rotate right
            },
            else => unreachable, // Invalid direction
        }
        // set shifted value back to register or memory
        if (reg == 0b110)
            ram[self.get_register_pair(.HL)] = value
        else
            self.set_register(reg, value);
        // set flags
        self.set_zsp(value);
        self.f.bits.h = 0; // No half-carry for rotate operations
        self.f.bits.n = 0; // No subtraction for rotate operations
    }

    // rotate bit instructions for ix, iy : left, right, with carry or not
    fn rotate_ix_iy(self: *z80, instruction: u8, ram: []u8, addr: u16, direction: u8, thru_carry: bool) void {
        // get register or memory location
        const reg: u8 = instruction & 0b00000111;
        var value = ram[addr];
        const carry: u8 = self.f.bits.c; // Save carry flag

        switch (direction) {
            'L' => {
                self.f.bits.c = if (value & 0x80 != 0) 1 else 0; // Set carry flag with msb
                value = (value << 1) | (if (thru_carry) carry else self.f.bits.c); // Rotate left
            },
            'R' => {
                self.f.bits.c = if (value & 0x01 != 0) 1 else 0; // Set carry flag with lsb
                value = (value >> 1) | (if (thru_carry) carry << 7 else @as(u8, self.f.bits.c) << 7); // Rotate right
            },
            else => unreachable, // Invalid direction
        }
        // set shifted value back to memory always
        ram[addr] = value;
        if (reg != 0b110)
            self.set_register(reg, value);
        // set flags
        self.set_zsp(value);
        self.f.bits.h = 0; // No half-carry for rotate operations
        self.f.bits.n = 0; // No subtraction for rotate operations
    }

    // rotate bit instructions: left, right, with carry or not
    fn shift(self: *z80, instruction: u8, ram: []u8, shift_type: []const u8) void {
        // get register or memory location
        const reg: u8 = instruction & 0b00000111;
        var value: u8 = if (reg == 0b110) ram[self.get_register_pair(.HL)] else self.get_register(reg);

        if (std.mem.eql(u8, shift_type, "SLA")) {
            self.f.bits.c = if (value & 0x80 != 0) 1 else 0; // Set carry flag with msb
            value = (value << 1); // Rotate left, bit 0 <- 0
        } else if (std.mem.eql(u8, shift_type, "SLL")) {
            self.f.bits.c = if (value & 0x80 != 0) 1 else 0; // Set carry flag with msb
            value = (value << 1) | 0x01; // Rotate left, bit 0 <- 1
        } else if (std.mem.eql(u8, shift_type, "SRA")) {
            self.f.bits.c = if (value & 0x01 != 0) 1 else 0; // Set carry flag with lsb
            value = (value >> 1) | (@as(u8, if (value & 0x80 != 0) 0x80 else 0x00)); // Rotate right, bit 7 <- msb
        } else if (std.mem.eql(u8, shift_type, "SRL")) {
            self.f.bits.c = if (value & 0x01 != 0) 0x01 else 0x00; // Set carry flag with lsb
            value = (value >> 1); // Rotate right, bit 7 <- 0
        } else {
            unreachable; // Invalid type
        }
        // set shifted value back to memory always
        if (reg != 0b110) // if a register was specified, set it too
            ram[self.get_register_pair(.HL)] = value
        else
            self.set_register(reg, value);
        // set flags
        self.set_zsp(value);
        self.f.bits.h = 0; // No half-carry for rotate operations
        self.f.bits.n = 0; // No subtraction for rotate operations
    }

    // shift bit instructions for ix, iy : left, right, with carry or not
    fn shift_ix_iy(self: *z80, instruction: u8, ram: []u8, addr: u16, shift_type: []const u8) void {
        // get register or memory location
        const reg: u8 = instruction & 0b00000111;
        var value: u8 = ram[addr];

        if (std.mem.eql(u8, shift_type, "SLA")) {
            self.f.bits.c = if (value & 0x80 != 0) 1 else 0; // Set carry flag with msb
            value = (value << 1); // Rotate left, bit 0 <- 0
        } else if (std.mem.eql(u8, shift_type, "SLL")) {
            self.f.bits.c = if (value & 0x80 != 0) 1 else 0; // Set carry flag with msb
            value = (value << 1) | 0x01; // Rotate left, bit 0 <- 1
        } else if (std.mem.eql(u8, shift_type, "SRA")) {
            self.f.bits.c = if (value & 0x01 != 0) 1 else 0; // Set carry flag with lsb
            value = (value >> 1) | (@as(u8, if (value & 0x80 != 0) 0x80 else 0x00)); // Rotate right, bit 7 <- msb
        } else if (std.mem.eql(u8, shift_type, "SRL")) {
            self.f.bits.c = if (value & 0x01 != 0) 0x01 else 0x00; // Set carry flag with lsb
            value = (value >> 1); // Rotate right, bit 7 <- 0
        } else {
            unreachable; // Invalid type
        }
        // set shifted value back to memory always
        ram[addr] = value;
        if (reg != 0b110)
            self.set_register(reg, value);
        // set flags
        self.set_zsp(value);
        self.f.bits.h = 0; // No half-carry for rotate operations
        self.f.bits.n = 0; // No subtraction for rotate operations
    }

    // bit test functions
    fn bit_ops_ix_iy(self: *z80, instruction: u8, ram: []u8, addr: u16) void {
        const bit_no: u3 = @truncate((instruction & 0b00111000) >> 3);
        const bit = @as(u8, 1) << bit_no;
        const reg = instruction & 0b00000111;

        switch (instruction) {
            0x40...0x7F => // BIT
            {
                self.f.bits.z = if (ram[addr] & bit == 0) 1 else 0;
                self.f.bits.h = 1;
                self.f.bits.n = 0;
            },
            0x80...0xBF => // RES
            {
                ram[addr] = ram[addr] & ~bit;
                if (reg != 0b110)
                    self.set_register(reg, ram[addr]);
            },
            0xC0...0xFF => // SET
            {
                ram[addr] = ram[addr] | bit;
                if (reg != 0b110)
                    self.set_register(reg, ram[addr]);
            },
            else => unreachable,
        }
    }

    //  AAAAAAA  XX    XX  XX   XX
    //  AA   AA  XX    XX   XX XX
    //  AAAAAAA  XX    XX    XXX
    //  AA   AA  XX    XX   XX XX
    //  AA   AA  XXXXXXXX  XX   XX

    // RLC, RRC, RL, RR, SLA, SRA, SLL, SRL
    fn bit_instructions(self: *z80, ram: []u8) void {
        const instruction = ram[self.pc];
        const opcode = @as(z80.bit_opcodes, @enumFromInt(instruction));
        self.pc = self.pc +% 1;

        switch (opcode) {
            // Rotate instructions
            .RLC_B, .RLC_C, .RLC_D, .RLC_E, .RLC_H, .RLC_L, .RLC__HL_, .RLC_A => {
                self.rotate(instruction, ram[0..], 'L', false);
            },
            .RL_B, .RL_C, .RL_D, .RL_E, .RL_H, .RL_L, .RL__HL_, .RL_A => {
                self.rotate(instruction, ram[0..], 'L', true);
            },
            .RRC_B, .RRC_C, .RRC_D, .RRC_E, .RRC_H, .RRC_L, .RRC__HL_, .RRC_A => {
                self.rotate(instruction, ram[0..], 'R', false);
            },
            .RR_B, .RR_C, .RR_D, .RR_E, .RR_H, .RR_L, .RR__HL_, .RR_A => {
                self.rotate(instruction, ram[0..], 'R', true);
            },
            // Shift instructions
            .SLA_B, .SLA_C, .SLA_D, .SLA_E, .SLA_H, .SLA_L, .SLA__HL_, .SLA_A => {
                self.shift(instruction, ram[0..], "SLA");
            },
            .SRA_B, .SRA_C, .SRA_D, .SRA_E, .SRA_H, .SRA_L, .SRA__HL_, .SRA_A => {
                self.shift(instruction, ram[0..], "SRA");
            },
            .SLL_B, .SLL_C, .SLL_D, .SLL_E, .SLL_H, .SLL_L, .SLL__HL_, .SLL_A => {
                self.shift(instruction, ram[0..], "SLL");
            },
            .SRL_B, .SRL_C, .SRL_D, .SRL_E, .SRL_H, .SRL_L, .SRL__HL_, .SRL_A => {
                self.shift(instruction, ram[0..], "SRL");
            },
            // Bit test instructions
            .BIT_0_B, .BIT_0_C, .BIT_0_D, .BIT_0_E, .BIT_0_H, .BIT_0_L, .BIT_0__HL_, .BIT_0_A, .BIT_1_B, .BIT_1_C, .BIT_1_D, .BIT_1_E, .BIT_1_H, .BIT_1_L, .BIT_1__HL_, .BIT_1_A, .BIT_2_B, .BIT_2_C, .BIT_2_D, .BIT_2_E, .BIT_2_H, .BIT_2_L, .BIT_2__HL_, .BIT_2_A, .BIT_3_B, .BIT_3_C, .BIT_3_D, .BIT_3_E, .BIT_3_H, .BIT_3_L, .BIT_3__HL_, .BIT_3_A, .BIT_4_B, .BIT_4_C, .BIT_4_D, .BIT_4_E, .BIT_4_H, .BIT_4_L, .BIT_4__HL_, .BIT_4_A, .BIT_5_B, .BIT_5_C, .BIT_5_D, .BIT_5_E, .BIT_5_H, .BIT_5_L, .BIT_5__HL_, .BIT_5_A, .BIT_6_B, .BIT_6_C, .BIT_6_D, .BIT_6_E, .BIT_6_H, .BIT_6_L, .BIT_6__HL_, .BIT_6_A, .BIT_7_B, .BIT_7_C, .BIT_7_D, .BIT_7_E, .BIT_7_H, .BIT_7_L, .BIT_7__HL_, .BIT_7_A => {
                const bit_no: u3 = @truncate((instruction & 0b00111000) >> 3);
                const reg_no: u4 = @truncate(instruction & 0b00000111);
                const value = if (reg_no == 0b110)
                    ram[self.get_register_pair(.HL)]
                else
                    self.get_register(reg_no);
                const bit_mask: u8 = @as(u8, 1) << bit_no;
                self.f.bits.z = if ((value & bit_mask) == 0) 1 else 0; // Set zero flag if bit 0 is 0
                self.f.bits.h = 1; // Set half-carry flag
                self.f.bits.n = 0; // No subtraction for bit test
            },
            // Reset bit instructions
            .RES_0_B, .RES_0_C, .RES_0_D, .RES_0_E, .RES_0_H, .RES_0_L, .RES_0__HL_, .RES_0_A, .RES_1_B, .RES_1_C, .RES_1_D, .RES_1_E, .RES_1_H, .RES_1_L, .RES_1__HL_, .RES_1_A, .RES_2_B, .RES_2_C, .RES_2_D, .RES_2_E, .RES_2_H, .RES_2_L, .RES_2__HL_, .RES_2_A, .RES_3_B, .RES_3_C, .RES_3_D, .RES_3_E, .RES_3_H, .RES_3_L, .RES_3__HL_, .RES_3_A, .RES_4_B, .RES_4_C, .RES_4_D, .RES_4_E, .RES_4_H, .RES_4_L, .RES_4__HL_, .RES_4_A, .RES_5_B, .RES_5_C, .RES_5_D, .RES_5_E, .RES_5_H, .RES_5_L, .RES_5__HL_, .RES_5_A, .RES_6_B, .RES_6_C, .RES_6_D, .RES_6_E, .RES_6_H, .RES_6_L, .RES_6__HL_, .RES_6_A, .RES_7_B, .RES_7_C, .RES_7_D, .RES_7_E, .RES_7_H, .RES_7_L, .RES_7__HL_, .RES_7_A => {
                const bit_no: u3 = @truncate((instruction & 0b00111000) >> 3);
                const reg_no: u4 = @truncate(instruction & 0b00000111);
                var value = if (reg_no == 0b110)
                    ram[self.get_register_pair(.HL)]
                else
                    self.get_register(reg_no);
                const bit_mask: u8 = @as(u8, 1) << bit_no;
                value &= ~bit_mask; // Clear the specified bit
                if (reg_no == 0b110) {
                    ram[self.get_register_pair(.HL)] = value; // Store back to memory
                } else {
                    self.set_register(reg_no, value); // Store back to register
                }
            },
            // Set bit instructions
            .SET_0_B, .SET_0_C, .SET_0_D, .SET_0_E, .SET_0_H, .SET_0_L, .SET_0__HL_, .SET_0_A, .SET_1_B, .SET_1_C, .SET_1_D, .SET_1_E, .SET_1_H, .SET_1_L, .SET_1__HL_, .SET_1_A, .SET_2_B, .SET_2_C, .SET_2_D, .SET_2_E, .SET_2_H, .SET_2_L, .SET_2__HL_, .SET_2_A, .SET_3_B, .SET_3_C, .SET_3_D, .SET_3_E, .SET_3_H, .SET_3_L, .SET_3__HL_, .SET_3_A, .SET_4_B, .SET_4_C, .SET_4_D, .SET_4_E, .SET_4_H, .SET_4_L, .SET_4__HL_, .SET_4_A, .SET_5_B, .SET_5_C, .SET_5_D, .SET_5_E, .SET_5_H, .SET_5_L, .SET_5__HL_, .SET_5_A, .SET_6_B, .SET_6_C, .SET_6_D, .SET_6_E, .SET_6_H, .SET_6_L, .SET_6__HL_, .SET_6_A, .SET_7_B, .SET_7_C, .SET_7_D, .SET_7_E, .SET_7_H, .SET_7_L, .SET_7__HL_, .SET_7_A => {
                const bit_no: u3 = @truncate((instruction & 0b00111000) >> 3);
                const reg_no: u4 = @truncate(instruction & 0b00000111);
                var value = if (reg_no == 0b110)
                    ram[self.get_register_pair(.HL)]
                else
                    self.get_register(reg_no);
                const bit_mask: u8 = @as(u8, 1) << bit_no;
                value |= bit_mask; // Set the specified bit
                if (reg_no == 0b110) {
                    ram[self.get_register_pair(.HL)] = value; // Store back to memory
                } else {
                    self.set_register(reg_no, value); // Store back to register
                }
            },
        }
    }

    //
    // Misc instructions
    //
    fn misc_instructions(self: *z80, ram: []u8) void {
        const instruction = ram[self.pc];
        const opcode = @as(z80.misc_opcodes, @enumFromInt(instruction));
        self.pc = self.pc +% 1;

        switch (opcode) {
            .IN_B__C_, .IN_C__C_, .IN_D__C_, .IN_E__C_, .IN_H__C_, .IN_L__C_, .IN_A__C_ => {
                // Input from port C
                const port_nbr = self.c;
                const value = self.ports_in[port_nbr];
                self.set_register(@truncate((instruction & 0b00111000) >> 3), value);
                self.set_zsp(value);
                self.f.bits.h = 0; // No half-carry for input
                self.f.bits.n = 0; // No subtraction for input
            },

            .OUT__C__B, .OUT__C__D, .OUT__C__E, .OUT__C__H, .OUT__C__L, .OUT__C__A => {
                // Output to port C
                const port_nbr = self.c;
                const value = self.get_register(@truncate((instruction & 0b00111000) >> 3));
                self.ports_out[port_nbr] = value;
            },

            .SBC_HL_BC, .SBC_HL_DE, .SBC_HL_HL, .SBC_HL_SP => {
                const value = switch (opcode) {
                    .SBC_HL_BC => self.get_register_pair(.BC),
                    .SBC_HL_DE => self.get_register_pair(.DE),
                    .SBC_HL_HL => self.get_register_pair(.HL),
                    .SBC_HL_SP => self.sp,
                    else => unreachable,
                };
                const HL = self.get_register_pair(.HL);
                const borrow_in: u16 = @as(u16, self.f.bits.c);
                const result = HL -% (value +% borrow_in);
                self.set_register_pair(.HL, result);
                //self.f.bits.c = if (result < HL or result < (value +% borrow_in)) 1 else 0; // Set carry if underflow

                const borrow_result = @as(u32, HL) -% @as(u32, value) -% @as(u32, borrow_in);
                self.f.bits.c = if (borrow_result > 0xFFFF) 1 else 0; // Set carry if underflow

                self.f.bits.h = if (((HL & 0x0FFF) < ((value & 0x0FFF) + borrow_in))) 1 else 0; // Check for half-borrow
                self.f.bits.n = 1; // Set subtraction flag
                const high: u8 = @truncate(result >> 8);
                self.f.bits.s = if ((high & 0x80) != 0) 1 else 0;
                self.f.bits.z = if (high == 0) 1 else 0; // Overflow (P/V): set if signed overflow
                //self.f.bits.pv = if (result > std.math.maxInt(i16) or result < std.math.minInt(i16)) 1 else 0;
                const HLs = @as(i32, (HL));
                const vals = @as(i32, (value));
                const ress = @as(i32, (result));
                self.f.bits.pv = if (((HLs ^ vals) & (HLs ^ ress) & 0x8000) != 0) 1 else 0;
            },

            .ADC_HL_BC, .ADC_HL_DE, .ADC_HL_HL, .ADC_HL_SP => {
                const value = switch (opcode) {
                    .ADC_HL_BC => self.get_register_pair(.BC),
                    .ADC_HL_DE => self.get_register_pair(.DE),
                    .ADC_HL_HL => self.get_register_pair(.HL),
                    .ADC_HL_SP => self.sp,
                    else => unreachable,
                };
                const HL = self.get_register_pair(.HL);
                const carry_in: u16 = @as(u16, self.f.bits.c);
                const result = HL +% (value +% carry_in);

                const carry_result = @as(u32, HL) + @as(u32, value) + @as(u32, carry_in);

                self.set_register_pair(.HL, result);
                self.f.bits.c = if (carry_result > 0xFFFF) 1 else 0; // Set carry if underflow
                self.f.bits.h = if (((HL & 0x0FFF) + (value & 0x0FFF) + carry_in) > 0x0FFF) 1 else 0; // Check for half-carry
                self.f.bits.n = 0; // Clear subtraction flag
                const high: u8 = @truncate(result >> 8);
                self.f.bits.s = if ((high & 0x80) != 0) 1 else 0;
                self.f.bits.z = if (high == 0) 1 else 0;
                // Overflow (P/V): set if signed overflow
                const HLs = @as(i32, (HL));
                const vals = @as(i32, (value));
                const ress = @as(i32, (result));
                self.f.bits.pv = if (((HLs ^ vals) & (HLs ^ ress) & 0x8000) != 0) 1 else 0;
            },

            .LD_BC__NN_, .LD_DE__NN_, .LD_HL__NN_, .LD_SP__NN_ => {
                // Load immediate value into register pairs
                const addr = get_word_from_bytes(ram[self.pc], ram[self.pc + 1]);
                self.pc = self.pc +% 2;
                // Increment PC by 1 to account for the low byte of the address
                const value = get_word_from_bytes(ram[addr + 1], ram[addr]);

                switch (opcode) {
                    .LD_BC__NN_ => self.set_register_pair(.BC, value),
                    .LD_DE__NN_ => self.set_register_pair(.DE, value),
                    .LD_HL__NN_ => self.set_register_pair(.HL, value),
                    .LD_SP__NN_ => self.sp = value,
                    else => unreachable,
                }
            },

            .LD__NN__BC, .LD__NN__DE, .LD__NN__HL, .LD__NN__SP => {
                const addr = get_word_from_bytes(ram[self.pc], ram[self.pc + 1]);
                self.pc = self.pc +% 2;
                // Increment PC by 1 to account for the low byte of the address
                const value = switch (opcode) {
                    .LD__NN__BC => self.get_register_pair(.BC),
                    .LD__NN__DE => self.get_register_pair(.DE),
                    .LD__NN__HL => self.get_register_pair(.HL),
                    .LD__NN__SP => self.sp,
                    else => unreachable,
                };
                ram[addr] = @truncate(value & 0x00FF); // Low byte
                ram[addr + 1] = @truncate((value & 0xFF00) >> 8); // High byte
            },

            .NEG => {
                // Negate the accumulator
                self.a = ~self.a;
                self.set_zsp(self.a);
                self.f.bits.c = if (self.a == 0) 0 else 1; // Set carry if result is non-zero
                self.f.bits.h = 1; // Set half-carry
                self.f.bits.h = if (((self.a & 0x0F) < (0 & 0x0F))) 1 else 0; // Correct half-borrow calculation
                self.f.bits.n = 1; // Set subtraction flag
            },

            .RETN => {
                // Return from interrupt
                const return_address = self.pop(ram[0..]);
                self.pc = return_address;
                self.iff2 = self.iff1; // Restore interrupt flags
            },

            .RETI => {
                // Return from interrupt, same behavior as RET
                const return_address = self.pop(ram[0..]);
                self.pc = return_address;
            },

            .IM0, .IM1, .IM2 => {
                // Set interrupt mode
                switch (opcode) {
                    .IM0 => self.interrupt_mode = 0,
                    .IM1 => self.interrupt_mode = 1,
                    .IM2 => self.interrupt_mode = 2,
                    else => unreachable, // Invalid opcode
                }
            },

            .LD_I_A => {
                self.i = self.a;
            },
            .LD_A_I => {
                self.a = self.i;
                self.set_zsp(self.a);
                self.f.bits.n = 0;
                self.f.bits.h = 0;
                self.f.bits.pv = if (self.iff2 == 1) 1 else 0;
            },
            .LD_R_A => self.r = self.a,
            .LD_A_R => {
                self.a = self.r;
                self.set_zsp(self.a);
                self.f.bits.n = 0;
                self.f.bits.h = 0;
                self.f.bits.pv = if (self.iff2 == 1) 1 else 0;
            },

            .RLD => {
                const addr = self.get_register_pair(.HL);
                const value = ram[addr];

                const a_lns = self.a & 0x0F; // Lower nibble of A
                const hl_lns = value & 0x0F; // Upper nibble of (HL)
                const hl_mns = (value & 0xF0) >> 4; // Lower nibble of (HL)

                self.a = (self.a & 0xF0) | hl_mns; // Set lower nibble of A to upper nibble of (HL)
                ram[addr] = (hl_lns << 4) | a_lns; // Set (HL) to lower nibble of A and upper nibble of (HL)
                self.set_zsp(self.a); // Update ZSP flags based on A
                self.f.bits.n = 0;
                self.f.bits.h = 0;
            },

            .RRD => {
                const addr = self.get_register_pair(.HL);
                const value = ram[addr];

                const a_lns = self.a & 0x0F; // Lower nibble of A
                const hl_lns = value & 0x0F; // Lower nibble of (HL)
                const hl_mns = (value & 0xF0) >> 4; // Upper nibble of (HL)

                self.a = (self.a & 0xF0) | hl_lns; // Set lower nibble of A to lower nibble of (HL)
                ram[addr] = (a_lns << 4) | hl_mns; // Set (HL) to upper nibble of A and lower nibble of (HL)
                self.set_zsp(self.a); // Update ZSP flags based on A
                self.f.bits.n = 0;
                self.f.bits.h = 0;
            },

            .LDI, .LDIR => {
                // Load and increment
                const addr_hl = self.get_register_pair(.HL);
                const addr_de = self.get_register_pair(.DE);
                ram[addr_de] = ram[addr_hl]; // Store (HL) into (DE)

                self.set_register_pair(.HL, addr_hl + 1); // Increment HL
                self.set_register_pair(.DE, addr_de + 1); // Increment DE
                self.set_register_pair(.BC, self.get_register_pair(.BC) - 1); // Increment DE
                self.f.bits.n = 0;
                self.f.bits.h = 0;
                self.f.bits.pv = if (self.get_register_pair(.BC) -% 1 != 0) 1 else 0; // Set parity flag if BC != 0

                if (opcode == .LDIR and self.get_register_pair(.BC) != 0) {
                    self.pc -= 2; // repeat until BC is zero
                }
            },

            .LDD, .LDDR => {
                // Load and increment
                const addr_hl = self.get_register_pair(.HL);
                const addr_de = self.get_register_pair(.DE);
                ram[addr_de] = ram[addr_hl]; // Store (HL) into (DE)

                self.set_register_pair(.HL, addr_hl - 1); // Increment HL
                self.set_register_pair(.DE, addr_de - 1); // Increment DE
                self.set_register_pair(.BC, self.get_register_pair(.BC) + 1); // Increment DE
                self.f.bits.n = 0;
                self.f.bits.h = 0;
                self.f.bits.pv = if (self.get_register_pair(.BC) - 1 != 0) 1 else 0; // Set parity flag if BC != 0

                if (opcode == .LDDR and self.get_register_pair(.BC) != 0) {
                    self.pc -= 2; // repeat until BC is zero
                }
            },

            .CPI, .CPIR => {
                const addr_hl = self.get_register_pair(.HL);
                const value = ram[addr_hl];
                self.set_register_pair(.HL, addr_hl + 1); // Increment HL
                self.set_register_pair(.BC, self.get_register_pair(.BC) - 1); // Increment DE
                self.set_zsp(self.a);
                self.f.bits.n = 1;
                self.f.bits.h = if ((self.a & 0x0F) < (value & 0x0F)) 1 else 0; // Correct half-borrow calculation
                self.f.bits.pv = if (self.get_register_pair(.BC) - 1 != 0) 1 else 0; // Set parity flag if BC != 0

                if (opcode == .CPIR and self.get_register_pair(.BC) != 0 and self.a != value) {
                    self.pc -= 2; // repeat until BC is zero
                }
            },

            // A – (HL), HL ← HL – 1, BC ← BC – 1
            .CPD, .CPDR => {
                const addr_hl = self.get_register_pair(.HL);
                const value = ram[addr_hl];
                self.set_register_pair(.HL, addr_hl - 1); // Increment HL
                self.set_register_pair(.BC, self.get_register_pair(.BC) - 1); // Increment DE
                self.set_zsp(self.a - value);
                self.f.bits.n = 1;
                self.f.bits.h = if ((self.a & 0x0F) < (value & 0x0F)) 1 else 0; // Correct half-borrow calculation
                self.f.bits.pv = if (self.get_register_pair(.BC) - 1 != 0) 1 else 0; // Set parity flag if BC != 0

                if (opcode == .CPDR and self.get_register_pair(.BC) != 0 and self.a != value) {
                    self.pc -= 2; // repeat until BC is zero
                }
            },

            // (HL) ← (C), B ← B – 1, HL ← HL + 1
            .INI, .INIR => {
                const addr_hl = self.get_register_pair(.HL);
                const port_nbr = self.c; // Store C into (HL)
                ram[addr_hl] = self.ports_in[port_nbr];
                self.set_register_pair(.HL, addr_hl + 1); // Increment HL
                self.b = self.b -% 1; // Decrement B
                self.f.bits.n = 1;
                self.f.bits.h = 0;
                self.f.bits.z = if (self.b - 1 == 0) 1 else 0; // Set parity flag if B != 0

                if (opcode == .INIR and self.b != 0) {
                    self.pc -= 2; // repeat until B is zero
                }
            },
            // (HL) ← (C), B ← B – 1, HL ← HL – 1
            .IND, .INDR => {
                const addr_hl = self.get_register_pair(.HL);
                const port_nbr = self.c; // Store C into (HL)
                ram[addr_hl] = self.ports_in[port_nbr];
                self.set_register_pair(.HL, addr_hl - 1); // Increment HL
                self.b = self.b -% 1; // Decrement B
                self.f.bits.n = 1;
                self.f.bits.h = 0;
                self.f.bits.z = if (self.b - 1 != 0) 1 else 0; // Set parity flag if B != 0

                if (opcode == .INDR and self.b != 0) {
                    self.pc -= 2; // repeat until B is zero
                }
            },

            // OUTI
            .OUTI, .OTIR => {
                const addr_hl = self.get_register_pair(.HL);
                self.b = self.b -% 1; // Decrement B
                const port_nbr = self.c; // Output (HL) to port C
                self.ports_out[port_nbr] = ram[addr_hl];
                self.set_register_pair(.HL, addr_hl + 1); // Increment HL
                self.f.bits.n = 1;
                self.f.bits.h = 0;
                self.f.bits.z = if (self.b - 1 != 0) 1 else 0; // Set parity flag if B != 0

                if (opcode == .OTIR and self.b != 0) {
                    self.pc -= 2; // repeat until B is zero
                }
            },
            // (C) ← (HL), B ← B – 1, HL ← HL – 1
            .OUTD, .OTDR => {
                const addr_hl = self.get_register_pair(.HL);
                self.b = self.b -% 1; // Decrement B
                const port_nbr = self.c; // Output (HL) to port C
                self.ports_out[port_nbr] = ram[addr_hl];
                self.set_register_pair(.HL, addr_hl - 1); // Increment HL
                self.f.bits.n = 1;
                self.f.bits.h = 0;
                self.f.bits.z = if (self.b - 1 != 0) 1 else 0; // Set parity flag if B != 0

                if (opcode == .OTDR and self.b != 0) {
                    self.pc -= 2; // repeat until B is zero
                }
            },

            else => {},
        }
    }

    // IX instructions (prefix 0xDD)
    fn ix_instructions(self: *z80, ram: []u8) void {
        const instruction = ram[self.pc];
        const opcode = @as(z80.ix_opcodes, @enumFromInt(instruction));
        self.pc = self.pc +% 1;

        switch (opcode) {
            .INC_B, .INC_C, .INC_D, .INC_E, .INC_A => self.inc_8bit(instruction, ram[0..]),
            .DEC_B, .DEC_C, .DEC_D, .DEC_E, .DEC_A => self.dec_8bit(instruction, ram[0..]),

            .LD_B_N, .LD_C_N, .LD_D_N, .LD_E_N, .LD_A_N => {
                self.load_immediate_8bit(instruction, ram[0..]);
            },

            .INC_IXH, .INC_IXL => {
                var value: u8 = undefined;
                if (opcode == .INC_IXH) {
                    value = @truncate((self.ix & 0xFF00) >> 8);
                    value = value +% 1;
                    self.ix = (self.ix & 0x00FF) | (@as(u16, value) << 8);
                } else {
                    value = @truncate(self.ix & 0x00FF);
                    value = value +% 1;
                    self.ix = (self.ix & 0xFF00) | @as(u16, value);
                }
                // Set flags
                self.set_zsp(value);
                self.f.bits.h = if ((value & 0x0F) == 0x00) 1 else 0; // Check for half-carry
                self.f.bits.n = 0; // Clear subtraction flag
            },

            .DEC_IXH, .DEC_IXL => {
                var value: u8 = undefined;
                if (opcode == .DEC_IXH) {
                    value = @truncate((self.ix & 0xFF00) >> 8);
                    value = value -% 1;
                    self.ix = (self.ix & 0x00FF) | (@as(u16, value) << 8);
                } else {
                    value = @truncate(self.ix & 0x00FF);
                    value = value -% 1;
                    self.ix = (self.ix & 0xFF00) | @as(u16, value);
                }
                // Set flags
                self.set_zsp(value);
                self.f.bits.h = if ((value & 0x0F) == 0x0F) 1 else 0; // Check for half-borrow
                self.f.bits.n = 1; // Set subtraction flag
            },

            .ADD_IX_BC, .ADD_IX_DE, .ADD_IX_IX, .ADD_IX_SP => {
                const value = switch (opcode) {
                    .ADD_IX_BC => self.get_register_pair(.BC),
                    .ADD_IX_DE => self.get_register_pair(.DE),
                    .ADD_IX_IX => self.ix,
                    .ADD_IX_SP => self.sp,
                    else => unreachable,
                };
                const IX = self.ix;
                const result = IX +% value;
                self.ix = result;
                self.f.bits.c = if (result > 0xFFFF) 1 else 0; // Set carry if overflow
                self.f.bits.h = if (((IX & 0x0FFF) + (value & 0x0FFF)) > 0x0FFF) 1 else 0; // Check for half-carry
                self.f.bits.n = 0; // Clear subtraction flag
                self.set_zsp(@truncate(result & 0xFF)); // Set ZSP based on lower byte of result
            },

            .LD_IX_NN => {
                const low_byte = ram[self.pc];
                const high_byte = ram[self.pc + 1];
                self.pc = self.pc +% 2;
                // Increment PC by 2 to account for the two bytes of the address
                self.ix = get_word_from_bytes(high_byte, low_byte);
            },

            .LD_IX__NN_ => {
                const addr = get_word_from_bytes(ram[self.pc + 1], ram[self.pc]);
                self.pc = self.pc +% 2;
                // Increment PC by 1 to account for the low byte of the address
                const low_byte = ram[addr];
                const high_byte = ram[addr + 1];
                self.ix = get_word_from_bytes(high_byte, low_byte);
            },

            .LD__NN__IX => {
                const addr = get_word_from_bytes(ram[self.pc + 1], ram[self.pc]);
                self.pc = self.pc +% 2;
                // Increment PC by 1 to account for the low byte of the address
                ram[addr] = @truncate(self.ix & 0x00FF); // Low byte
                ram[addr + 1] = @truncate((self.ix & 0xFF00) >> 8); // High byte
            },

            .LD__IXD__N => {
                const offset = ram[self.pc];
                const addr = add_offset(self.ix, offset);
                const value = ram[self.pc + 1];
                self.pc = self.pc +% 2;
                // skip offset and value
                ram[addr] = value;
            },

            .LD_B__IXD_, .LD_C__IXD_, .LD_D__IXD_, .LD_E__IXD_, .LD_H__IXD_, .LD_L__IXD_, .LD_A__IXD_ => {
                const offset = ram[self.pc];
                const addr = add_offset(self.ix, offset);
                const value = ram[addr];
                self.pc = self.pc +% 1;
                // skip offset and value
                self.set_register(@truncate((instruction & 0b00111000) >> 3), value);
            },

            .LD__IXD__B, .LD__IXD__C, .LD__IXD__D, .LD__IXD__E, .LD__IXD__H, .LD__IXD__L, .LD__IXD__A => {
                const offset = ram[self.pc];
                const addr = add_offset(self.ix, offset);
                self.pc = self.pc +% 1;
                // skip offset
                const value = self.get_register(@truncate((instruction & 0b00000111) >> 3));
                ram[addr] = value;
            },

            .INC_IX => self.ix = self.ix +% 1,
            .DEC_IX => self.ix = self.ix -% 1,

            .INC__IXD_ => {
                const offset = ram[self.pc];
                const addr = add_offset(self.ix, offset);
                const value = ram[addr];
                self.pc = self.pc +% 1;
                // skip offset
                const inc_value = value +% 1;
                ram[addr] = inc_value;
                // Set flags
                self.set_zsp(inc_value);
                self.f.bits.pv = if (value == 0x7F) 1 else 0; // Overflow if value was 0x7F
                self.f.bits.h = if ((inc_value & 0x0F) == 0x00) 1 else 0; // Check for half-carry
                self.f.bits.n = 0; // Clear subtraction flag
            },

            .DEC__IXD_ => {
                const offset = ram[self.pc];
                const addr = add_offset(self.ix, offset);
                const value = ram[addr];
                self.pc = self.pc +% 1;
                // skip offset
                const dec_value = value -% 1;
                ram[addr] = dec_value;
                // Set flags
                self.set_zsp(dec_value);
                self.f.bits.pv = if (value == 0x80) 1 else 0; // Overflow if value was 0x80
                self.f.bits.h = if ((value & 0x0F) == 0x0F) 1 else 0; // Check for half-borrow
                self.f.bits.n = 1; // Set subtraction flag
            },

            .ADD_A__IXD_, .ADC_A__IXD_ => {
                const offset = ram[self.pc];
                const addr = add_offset(self.ix, offset);
                const value = ram[addr];
                self.pc = self.pc +% 1;
                // skip offset
                const result = self.a +% value + (if (opcode == .ADC_A__IXD_) @as(u8, self.f.bits.c) else 0);
                self.f.bits.c = if (result < self.a) 1 else 0; // Set carry if overflow
                self.f.bits.h = if (((self.a & 0x0F) + (value & 0x0F)) > 0x0F) 1 else 0; // Check for half-carry
                self.f.bits.n = 0; // Clear subtraction flag
                self.a = @truncate(result);
                self.set_zsp(self.a); // Set ZSP based on result
            },

            .SUB__IXD_, .SBC_A__IXD_ => {
                const offset = ram[self.pc];
                const addr = add_offset(self.ix, offset);
                const value = ram[addr];
                self.pc = self.pc +% 1;
                // skip offset
                const borrow_in: u8 = if (opcode == .SBC_A__IXD_) @as(u8, self.f.bits.c) else 0;
                const result = self.a -% (value +% borrow_in);
                self.f.bits.c = if (self.a < (value +% borrow_in)) 1 else 0; // Set carry if underflow
                self.f.bits.h = if ((self.a & 0x0F) < ((value & 0x0F) + borrow_in)) 1 else 0; // Check for half-borrow
                self.f.bits.n = 1; // Set subtraction flag
                self.a = @truncate(result);
                self.set_zsp(self.a); // Set ZSP based on result
            },

            .AND__IXD_ => {
                const offset = ram[self.pc];
                const addr = add_offset(self.ix, offset);
                const value = ram[addr];
                self.pc = self.pc +% 1;
                // skip offset
                self.a = self.a & value;
                self.set_zsp(self.a);
                self.f.bits.c = 0;
                self.f.bits.h = 1; // AND always sets half-carry
                self.f.bits.n = 0;
            },

            .OR__IXD_ => {
                const offset = ram[self.pc];
                const addr = add_offset(self.ix, offset);
                const value = ram[addr];
                self.pc = self.pc +% 1;
                // skip offset
                self.a = self.a | value;
                self.set_zsp(self.a);
                self.f.bits.c = 0;
                self.f.bits.h = 0;
                self.f.bits.n = 0;
            },

            .XOR__IXD_ => {
                const offset = ram[self.pc];
                const addr = add_offset(self.ix, offset);
                const value = ram[addr];
                self.pc = self.pc +% 1;
                // skip offset
                self.a = self.a ^ value;
                self.set_zsp(self.a);
                self.f.bits.c = 0;
                self.f.bits.h = 0;
                self.f.bits.n = 0;
            },

            .CP__IXD_ => {
                const offset = ram[self.pc];
                const addr = add_offset(self.ix, offset);
                const value = ram[addr];
                self.pc = self.pc +% 1;
                // skip offset
                const result = self.a -% value;
                self.f.bits.c = if (self.a < value) 1 else 0; // Set carry if underflow
                self.f.bits.h = if ((self.a & 0x0F) < (value & 0x0F)) 1 else 0; // Check for half-borrow
                self.f.bits.n = 1; // Set subtraction flag
                self.set_zsp(@truncate(result)); // Set ZSP based on result
            },

            .POP_IX => {
                self.ix = self.pop(ram[0..]);
            },

            .PUSH_IX => {
                self.push(self.ix, ram[0..]);
            },

            .EX__SP_IX => {
                const temp: u16 = (@as(u16, ram[self.sp + 1]) << 8) | ram[self.sp];

                ram[self.sp] = @truncate(self.ix & 0x00FF); // Low byte
                ram[self.sp + 1] = @truncate((self.ix & 0xFF00) >> 8); // High byte
                self.ix = temp;
            },

            .JP_IX => {
                self.pc = self.ix;
            },

            .LD_SP_IX => {
                self.sp = self.ix;
            },

            .IX_BIT => {
                self.ix_iy_bit_instructions(ram[0..], 'X');
            },

            else => {
                std.debug.print("Unimplemented IX opcode: {X}\n", .{instruction});
            },
        }
    }

    // IX bit instructions (prefix 0xDD 0xCB)
    fn ix_iy_bit_instructions(self: *z80, ram: []u8, ix_iy_reg: u8) void {
        const offset = ram[self.pc];
        self.pc = self.pc +% 1;
        // skip offset
        const reg = if (ix_iy_reg == 'X') self.ix else self.iy;
        const ixd = add_offset(reg, offset);
        // get the next opcode
        const instruction = ram[self.pc];
        const opcode = @as(z80.ix_bit_opcodes, @enumFromInt(instruction));
        self.pc = self.pc +% 1;

        switch (opcode) {
            .RLC__IXD_B, .RLC__IXD_C, .RLC__IXD_D, .RLC__IXD_E, .RLC__IXD_H, .RLC__IXD_L, .RLC__IXD_, .RLC__IXD_A => {
                self.rotate_ix_iy(instruction, ram[0..], ixd, 'L', false);
            },
            .RRC__IXD_B, .RRC__IXD_C, .RRC__IXD_D, .RRC__IXD_E, .RRC__IXD_H, .RRC__IXD_L, .RRC__IXD_, .RRC__IXD_A => {
                self.rotate_ix_iy(instruction, ram[0..], ixd, 'R', false);
            },
            .RL__IXD_B, .RL__IXD_C, .RL__IXD_D, .RL__IXD_E, .RL__IXD_H, .RL__IXD_L, .RL__IXD_, .RL__IXD_A => {
                self.rotate_ix_iy(instruction, ram[0..], ixd, 'L', true);
            },
            .RR__IXD_B, .RR__IXD_C, .RR__IXD_D, .RR__IXD_E, .RR__IXD_H, .RR__IXD_L, .RR__IXD_, .RR__IXD_A => {
                self.rotate_ix_iy(instruction, ram[0..], ixd, 'R', true);
            },
            .SLA__IXD_B, .SLA__IXD_C, .SLA__IXD_D, .SLA__IXD_E, .SLA__IXD_H, .SLA__IXD_L, .SLA__IXD_, .SLA__IXD_A => {
                self.shift_ix_iy(instruction, ram[0..], ixd, "SLA");
            },
            .SLL__IXD_B, .SLL__IXD_C, .SLL__IXD_D, .SLL__IXD_E, .SLL__IXD_H, .SLL__IXD_L, .SLL__IXD_, .SLL__IXD_A => {
                self.shift_ix_iy(instruction, ram[0..], ixd, "SLL");
            },
            .SRA__IXD_B, .SRA__IXD_C, .SRA__IXD_D, .SRA__IXD_E, .SRA__IXD_H, .SRA__IXD_L, .SRA__IXD_, .SRA__IXD_A => {
                self.shift_ix_iy(instruction, ram[0..], ixd, "SRA");
            },
            .SRL__IXD_B, .SRL__IXD_C, .SRL__IXD_D, .SRL__IXD_E, .SRL__IXD_H, .SRL__IXD_L, .SRL__IXD_, .SRL__IXD_A => {
                self.shift_ix_iy(instruction, ram[0..], ixd, "SRL");
            },
            // zig fmt: off
            .BIT0__IXD0, .BIT0__IXD1, .BIT0__IXD2, .BIT0__IXD3, .BIT0__IXD4, .BIT0__IXD5, .BIT0__IXD6, .BIT0__IXD7, .BIT1__IXD8, .BIT1__IXD9, .BIT1__IXDA, .BIT1__IXDB, .BIT1__IXDC, .BIT1__IXDD, .BIT1__IXDE, .BIT1__IXDF, 
            .BIT2__IXD0, .BIT2__IXD1, .BIT2__IXD2, .BIT2__IXD3, .BIT2__IXD4, .BIT2__IXD5, .BIT2__IXD6, .BIT2__IXD7, .BIT3__IXD8, .BIT3__IXD9, .BIT3__IXDA, .BIT3__IXDB, .BIT3__IXDC, .BIT3__IXDD, .BIT3__IXDE, .BIT3__IXDF, 
            .BIT4__IXD0, .BIT4__IXD1, .BIT4__IXD2, .BIT4__IXD3, .BIT4__IXD4, .BIT4__IXD5, .BIT4__IXD6, .BIT4__IXD7, .BIT5__IXD8, .BIT5__IXD9, .BIT5__IXDA, .BIT5__IXDB, .BIT5__IXDC, .BIT5__IXDD, .BIT5__IXDE, .BIT5__IXDF, 
            .BIT6__IXD0, .BIT6__IXD1, .BIT6__IXD2, .BIT6__IXD3, .BIT6__IXD4, .BIT6__IXD5, .BIT6__IXD6, .BIT6__IXD7, .BIT7__IXD8, .BIT7__IXD9, .BIT7__IXDA, .BIT7__IXDB, .BIT7__IXDC, .BIT7__IXDD, .BIT7__IXDE, .BIT7__IXDF, 
            .RES0__IXD_B, .RES0__IXD_C, .RES0__IXD_D, .RES0__IXD_E, .RES0__IXD_H, .RES0__IXD_L, .RES0__IXD, .RES0__IXD_A, .RES1__IXD_B, .RES1__IXD_C, .RES1__IXD_D, .RES1__IXD_E, .RES1__IXD_H, .RES1__IXD_L, .RES1__IXD, 
            .RES1__IXD_A, .RES2__IXD_B, .RES2__IXD_C, .RES2__IXD_D, .RES2__IXD_E, .RES2__IXD_H, .RES2__IXD_L, .RES2__IXD, .RES2__IXD_A, .RES3__IXD_B, .RES3__IXD_C, .RES3__IXD_D, .RES3__IXD_E, .RES3__IXD_H, .RES3__IXD_L, 
            .RES3__IXD, .RES3__IXD_A, .RES4__IXD_B, .RES4__IXD_C, .RES4__IXD_D, .RES4__IXD_E, .RES4__IXD_H, .RES4__IXD_L, .RES4__IXD, .RES4__IXD_A, .RES5__IXD_B, .RES5__IXD_C, .RES5__IXD_D, .RES5__IXD_E, .RES5__IXD_H, .RES5__IXD_L,
            .RES5__IXD, .RES5__IXD_A, .RES6__IXD_B, .RES6__IXD_C, .RES6__IXD_D, .RES6__IXD_E, .RES6__IXD_H, .RES6__IXD_L, .RES6__IXD, .RES6__IXD_A, .RES7__IXD_B, .RES7__IXD_C, .RES7__IXD_D, .RES7__IXD_E, .RES7__IXD_H, .RES7__IXD_L, 
            .RES7__IXD, .RES7__IXD_A, .SET0__IXD_B, .SET0__IXD_C, .SET0__IXD_D, .SET0__IXD_E, .SET0__IXD_H, .SET0__IXD_L, .SET0__IXD, .SET0__IXD_A, .SET1__IXD_B, .SET1__IXD_C, .SET1__IXD_D, .SET1__IXD_E, .SET1__IXD_H, .SET1__IXD_L, 
            .SET1__IXD, .SET1__IXD_A, .SET2__IXD_B, .SET2__IXD_C, .SET2__IXD_D, .SET2__IXD_E, .SET2__IXD_H, .SET2__IXD_L, .SET2__IXD, .SET2__IXD_A, .SET3__IXD_B, .SET3__IXD_C, .SET3__IXD_D, .SET3__IXD_E, .SET3__IXD_H, .SET3__IXD_L, 
            .SET3__IXD, .SET3__IXD_A, .SET4__IXD_B, .SET4__IXD_C, .SET4__IXD_D, .SET4__IXD_E, .SET4__IXD_H, .SET4__IXD_L, .SET4__IXD, .SET4__IXD_A, .SET5__IXD_B, .SET5__IXD_C, .SET5__IXD_D, .SET5__IXD_E, .SET5__IXD_H, .SET5__IXD_L,
            .SET5__IXD, .SET5__IXD_A, .SET6__IXD_B, .SET6__IXD_C, .SET6__IXD_D, .SET6__IXD_E, .SET6__IXD_H, .SET6__IXD_L, .SET6__IXD, .SET6__IXD_A, .SET7__IXD_B, .SET7__IXD_C, .SET7__IXD_D, .SET7__IXD_E, .SET7__IXD_H, .SET7__IXD_L, 
            .SET7__IXD, .SET7__IXD_A  => {
                self.bit_ops_ix_iy(instruction, ram[0..], ixd);
            },
            // zig fmt: on
        }
    }

    // IY instructions (prefix 0xDD)
    fn iy_instructions(self: *z80, ram: []u8) void {
        const instruction = ram[self.pc];
        const opcode = @as(z80.iy_opcodes, @enumFromInt(instruction));
        self.pc = self.pc +% 1;

        switch (opcode) {
            .INC_B, .INC_C, .INC_D, .INC_E, .INC_A => self.inc_8bit(instruction, ram[0..]),
            .DEC_B, .DEC_C, .DEC_D, .DEC_E, .DEC_A => self.dec_8bit(instruction, ram[0..]),

            .LD_B_N, .LD_C_N, .LD_D_N, .LD_E_N, .LD_A_N => {
                self.load_immediate_8bit(instruction, ram[0..]);
            },

            .INC_IYH, .INC_IYL => {
                var value: u8 = undefined;
                if (opcode == .INC_IYH) {
                    value = @truncate((self.iy & 0xFF00) >> 8);
                    value = value +% 1;
                    self.iy = (self.iy & 0x00FF) | (@as(u16, value) << 8);
                } else {
                    value = @truncate(self.iy & 0x00FF);
                    value = value +% 1;
                    self.iy = (self.iy & 0xFF00) | @as(u16, value);
                }
                // Set flags
                self.set_zsp(value);
                self.f.bits.h = if ((value & 0x0F) == 0x00) 1 else 0; // Check for half-carry
                self.f.bits.n = 0; // Clear subtraction flag
            },

            .DEC_IYH, .DEC_IYL => {
                var value: u8 = undefined;
                if (opcode == .DEC_IYH) {
                    value = @truncate((self.iy & 0xFF00) >> 8);
                    value = value -% 1;
                    self.iy = (self.iy & 0x00FF) | (@as(u16, value) << 8);
                } else {
                    value = @truncate(self.iy & 0x00FF);
                    value = value -% 1;
                    self.iy = (self.iy & 0xFF00) | @as(u16, value);
                }
                // Set flags
                self.set_zsp(value);
                self.f.bits.h = if ((value & 0x0F) == 0x0F) 1 else 0; // Check for half-borrow
                self.f.bits.n = 1; // Set subtraction flag
            },

            .ADD_IY_BC, .ADD_IY_DE, .ADD_IY_IY, .ADD_IY_SP => {
                const value = switch (opcode) {
                    .ADD_IY_BC => self.get_register_pair(.BC),
                    .ADD_IY_DE => self.get_register_pair(.DE),
                    .ADD_IY_IY => self.iy,
                    .ADD_IY_SP => self.sp,
                    else => unreachable,
                };
                const IY = self.iy;
                const result = IY +% value;
                self.iy = result;
                self.f.bits.c = if (result > 0xFFFF) 1 else 0; // Set carry if overflow
                self.f.bits.h = if (((IY & 0x0FFF) + (value & 0x0FFF)) > 0x0FFF) 1 else 0; // Check for half-carry
                self.f.bits.n = 0; // Clear subtraction flag
                self.set_zsp(@truncate(result & 0xFF)); // Set ZSP based on lower byte of result
            },

            .LD_IY_NN => {
                const low_byte = ram[self.pc];
                const high_byte = ram[self.pc + 1];
                self.pc = self.pc +% 2;
                // Increment PC by 2 to account for the two bytes of the address
                self.iy = get_word_from_bytes(high_byte, low_byte);
            },

            .LD_IY__NN_ => {
                const addr = get_word_from_bytes(ram[self.pc + 1], ram[self.pc]);
                self.pc = self.pc +% 2;
                // Increment PC by 1 to account for the low byte of the address
                const low_byte = ram[addr];
                const high_byte = ram[addr + 1];
                self.iy = get_word_from_bytes(high_byte, low_byte);
            },

            .LD__NN__IY => {
                const addr = get_word_from_bytes(ram[self.pc + 1], ram[self.pc]);
                self.pc = self.pc +% 2;
                // Increment PC by 1 to account for the low byte of the address
                ram[addr] = @truncate(self.iy & 0x00FF); // Low byte
                ram[addr + 1] = @truncate((self.iy & 0xFF00) >> 8); // High byte
            },

            .LD__IYD__N => {
                const offset = ram[self.pc];
                const addr = add_offset(self.iy, offset);
                const value = ram[self.pc + 1];
                self.pc = self.pc +% 2;
                // skip offset and value
                ram[addr] = value;
            },

            .LD_B__IYD_, .LD_C__IYD_, .LD_D__IYD_, .LD_E__IYD_, .LD_H__IYD_, .LD_L__IYD_, .LD_A__IYD_ => {
                const offset = ram[self.pc];
                const addr = add_offset(self.iy, offset);
                const value = ram[addr];
                self.pc = self.pc +% 1;
                // skip offset and value
                self.set_register(@truncate((instruction & 0b00111000) >> 3), value);
            },

            .LD__IYD__B, .LD__IYD__C, .LD__IYD__D, .LD__IYD__E, .LD__IYD__H, .LD__IYD__L, .LD__IYD__A => {
                const offset = ram[self.pc];
                const addr = add_offset(self.iy, offset);
                self.pc = self.pc +% 1;
                // skip offset
                const value = self.get_register(@truncate((instruction & 0b00000111) >> 3));
                ram[addr] = value;
            },

            .INC_IY => self.iy = self.iy +% 1,
            .DEC_IY => self.iy = self.iy -% 1,

            .INC__IYD_ => {
                const offset = ram[self.pc];
                const addr = add_offset(self.iy, offset);
                const value = ram[addr];
                self.pc = self.pc +% 1;
                // skip offset
                const inc_value = value +% 1;
                ram[addr] = inc_value;
                // Set flags
                self.set_zsp(inc_value);
                self.f.bits.pv = if (value == 0x7F) 1 else 0; // Overflow if value was 0x7F
                self.f.bits.h = if ((inc_value & 0x0F) == 0x00) 1 else 0; // Check for half-carry
                self.f.bits.n = 0; // Clear subtraction flag
            },

            .DEC__IYD_ => {
                const offset = ram[self.pc];
                const addr = add_offset(self.iy, offset);
                const value = ram[addr];
                self.pc = self.pc +% 1;
                // skip offset
                const dec_value = value -% 1;
                ram[addr] = dec_value;
                // Set flags
                self.set_zsp(dec_value);
                self.f.bits.pv = if (value == 0x80) 1 else 0; // Overflow if value was 0x80
                self.f.bits.h = if ((value & 0x0F) == 0x0F) 1 else 0; // Check for half-borrow
                self.f.bits.n = 1; // Set subtraction flag
            },

            .ADD_A__IYD_, .ADC_A__IYD_ => {
                const offset = ram[self.pc];
                const addr = add_offset(self.iy, offset);
                const value = ram[addr];
                self.pc = self.pc +% 1;
                // skip offset
                const result = self.a +% value + (if (opcode == .ADC_A__IYD_) @as(u8, self.f.bits.c) else 0);
                self.f.bits.c = if (result < self.a) 1 else 0; // Set carry if overflow
                self.f.bits.h = if (((self.a & 0x0F) + (value & 0x0F)) > 0x0F) 1 else 0; // Check for half-carry
                self.f.bits.n = 0; // Clear subtraction flag
                self.a = @truncate(result);
                self.set_zsp(self.a); // Set ZSP based on result
            },

            .SUB__IYD_, .SBC_A__IYD_ => {
                const offset = ram[self.pc];
                const addr = add_offset(self.iy, offset);
                const value = ram[addr];
                self.pc = self.pc +% 1;
                // skip offset
                const borrow_in: u8 = if (opcode == .SBC_A__IYD_) @as(u8, self.f.bits.c) else 0;
                const result = self.a -% (value +% borrow_in);
                self.f.bits.c = if (self.a < (value +% borrow_in)) 1 else 0; // Set carry if underflow
                self.f.bits.h = if ((self.a & 0x0F) < ((value & 0x0F) + borrow_in)) 1 else 0; // Check for half-borrow
                self.f.bits.n = 1; // Set subtraction flag
                self.a = @truncate(result);
                self.set_zsp(self.a); // Set ZSP based on result
            },

            .AND__IYD_ => {
                const offset = ram[self.pc];
                const addr = add_offset(self.iy, offset);
                const value = ram[addr];
                self.pc = self.pc +% 1;
                // skip offset
                self.a = self.a & value;
                self.set_zsp(self.a);
                self.f.bits.c = 0;
                self.f.bits.h = 1; // AND always sets half-carry
                self.f.bits.n = 0;
            },

            .OR__IYD_ => {
                const offset = ram[self.pc];
                const addr = add_offset(self.iy, offset);
                const value = ram[addr];
                self.pc = self.pc +% 1;
                // skip offset
                self.a = self.a | value;
                self.set_zsp(self.a);
                self.f.bits.c = 0;
                self.f.bits.h = 0;
                self.f.bits.n = 0;
            },

            .XOR__IYD_ => {
                const offset = ram[self.pc];
                const addr = add_offset(self.iy, offset);
                const value = ram[addr];
                self.pc = self.pc +% 1;
                // skip offset
                self.a = self.a ^ value;
                self.set_zsp(self.a);
                self.f.bits.c = 0;
                self.f.bits.h = 0;
                self.f.bits.n = 0;
            },

            .CP__IYD_ => {
                const offset = ram[self.pc];
                const addr = add_offset(self.iy, offset);
                const value = ram[addr];
                self.pc = self.pc +% 1;
                // skip offset
                const result = self.a -% value;
                self.f.bits.c = if (self.a < value) 1 else 0; // Set carry if underflow
                self.f.bits.h = if ((self.a & 0x0F) < (value & 0x0F)) 1 else 0; // Check for half-borrow
                self.f.bits.n = 1; // Set subtraction flag
                self.set_zsp(@truncate(result)); // Set ZSP based on result
            },

            .POP_IY => {
                self.iy = self.pop(ram[0..]);
            },

            .PUSH_IY => {
                self.push(self.iy, ram[0..]);
            },

            .EX__SP_IY => {
                const temp: u16 = (@as(u16, ram[self.sp + 1]) << 8) | ram[self.sp];

                ram[self.sp] = @truncate(self.iy & 0x00FF); // Low byte
                ram[self.sp + 1] = @truncate((self.iy & 0xFF00) >> 8); // High byte
                self.iy = temp;
            },

            .JP_IY => {
                self.pc = self.iy;
            },

            .LD_SP_IY => {
                self.sp = self.iy;
            },

            .IY_BIT => {
                // Handle IY bit operations (0xDD 0xCB)
                self.ix_iy_bit_instructions(ram[0..], 'Y');
            },

            else => {
                std.debug.print("Unimplemented IY opcode: {X}\n", .{instruction});
            },
        }
    }

    // ********************************************************************************
    //
    //       88888    00000     88888    00000
    //      88   88  00   00   88   88  00   00
    //      88   88  00   00   88   88  00   00
    //        888    00   00     888    00   00
    //      88   88  00   00   88   88  00   00
    //      88   88  00   00   88   88  00   00
    //      88   88  00   00   88   88  00   00
    //       88888    00000     88888    00000
    //
    //-----------------------------------------------------------------------------
    // execute instruction at pc
    //
    pub fn execute_instruction(self: *z80, ram: []u8, debug: bool) void {
        var instruction: u8 = undefined;

        if (self.running_state != .running)
            return;

        //
        // handle interrupts
        //
        if (self.interrupt_req == 1 and self.inte == 1) {
            self.interrupt_req = 0; // lower interrupt req signal
            self.inte = 0; // reset INTE
            instruction = self.interrupt_vector; // opcode sent by interrupt req
        } else {
            // regular CPU cycle
            instruction = ram[self.pc];
            self.pc = self.pc +% 1;
        }

        if (debug == true) {
            const stdout = std.io.getStdOut().writer();

            stdout.print("{X:04}: {s} {X:02} {X:02} \t\t inst={X:02}, flags={X:02}, reg=[{X:02}|{X:02}|{X:02}|{X:02}|{X:02}|{X:02}|{X:02}]\n", .{ self.pc - 1, z80.opcode_names[instruction], ram[self.pc], ram[self.pc + 1], instruction, self.f.byte, self.b, self.c, self.d, self.e, self.h, self.l, self.a }) catch unreachable;
        }

        self.r = (self.r & 0b10000000) | ((self.r +% 1) & 0b01111111);
        // decode 1 byte instructions (8080 instructions)
        const opcode = @as(z80.opcodes, @enumFromInt(instruction));
        switch (opcode) {
            .NOP => {},
            // LD: move data
            .LD_B_B, .LD_B_C, .LD_B_D, .LD_B_E, .LD_B_H, .LD_B_L, .LD_B__HL_, .LD_B_A, .LD_C_B, .LD_C_C, .LD_C_D, .LD_C_E, .LD_C_H, .LD_C_L, .LD_C__HL_, .LD_C_A, .LD_D_B, .LD_D_C, .LD_D_D, .LD_D_E, .LD_D_H, .LD_D_L, .LD_D__HL_, .LD_D_A, .LD_E_B, .LD_E_C, .LD_E_D, .LD_E_E, .LD_E_H, .LD_E_L, .LD_E__HL_, .LD_E_A, .LD_H_B, .LD_H_C, .LD_H_D, .LD_H_E, .LD_H_H, .LD_H_L, .LD_H__HL_, .LD_H_A, .LD_L_B, .LD_L_C, .LD_L_D, .LD_L_E, .LD_L_H, .LD_L_L, .LD_L__HL_, .LD_L_A, .LD__HL__B, .LD__HL__C, .LD__HL__D, .LD__HL__E, .LD__HL__H, .LD__HL__L, .LD__HL__A, .LD_A_B, .LD_A_C, .LD_A_D, .LD_A_E, .LD_A_H, .LD_A_L, .LD_A__HL_, .LD_A_A => self.move_8bit(instruction, ram[0..]),
            // LXI: Load register pair immediate
            .LD_BC_NN, .LD_DE_NN, .LD_HL_NN, .LD_SP_NN => {
                self.load_immediate_16bit(opcode, ram[0..]);
            },

            // STAX: stores A into address in register pair B or D
            .LD__BC__A => {
                const addr = self.get_register_pair(.BC);
                ram[addr] = self.a;
            },
            .LD__DE__A => {
                const addr = self.get_register_pair(.DE);
                ram[addr] = self.a;
            },

            // INX: increment register pair
            .INC_BC => {
                const val = self.get_register_pair(.BC) +% 1;
                self.set_register_pair(.BC, val);
            },
            .INC_DE => {
                const val = self.get_register_pair(.DE) +% 1;
                self.set_register_pair(.DE, val);
            },
            .INC_HL => {
                const val = self.get_register_pair(.HL) +% 1;
                self.set_register_pair(.HL, val);
            },
            .INC_SP => {
                self.sp = self.sp +% 1;
            },

            // INc: increment register
            .INC_B, .INC_D, .INC_H, .INC__HL_, .INC_C, .INC_E, .INC_L, .INC_A => self.inc_8bit(instruction, ram[0..]),

            // DEC: decrement register
            .DEC_B, .DEC_D, .DEC_H, .DEC__HL_, .DEC_C, .DEC_E, .DEC_L, .DEC_A => self.dec_8bit(instruction, ram[0..]),

            // LD x, N: Move immediate to register
            .LD_B_N, .LD_C_N, .LD_D_N, .LD_E_N, .LD_H_N, .LD_L_N, .LD__HL__N, .LD_A_N => {
                self.load_immediate_8bit(instruction, ram[0..]);
            },

            // Rotate instructions
            // RLC: Rotate accumulator left w/ carry
            .RLCA => {
                const msb = (self.a & 0x80) >> 7;
                self.a = (self.a << 1) | msb;
                self.f.bits.c = @truncate(msb);
            },

            // RAL: Rotate accumulator left through carry
            .RLA => {
                const msb = (self.a & 0x80) >> 7;
                self.a = (self.a << 1) | self.f.bits.c;
                self.f.bits.c = @truncate(msb);
            },

            // RRCA: Rotate accumulator right w/ carry
            .RRCA => {
                const lsb = self.a & 0x01;
                self.a = (self.a >> 1) | (lsb << 7);
                self.f.bits.c = @truncate(lsb);
            },

            // RRA: Rotate accumulator right through carry
            .RRA => {
                const lsb = self.a & 0x01;
                self.a = (self.a >> 1) | (@as(u8, self.f.bits.c) << 7);
                self.f.bits.c = @truncate(lsb);
            },

            // DAA:
            .DAA => {
                self.daa();
            },

            // LDAX
            .LD_A__BC_ => {
                const addr = self.get_register_pair(.BC);
                self.a = ram[addr];
            },

            .LD_A__DE_ => {
                const addr = self.get_register_pair(.DE);
                self.a = ram[addr];
            },

            // DEC: Decrement register pair
            .DEC_BC, .DEC_DE, .DEC_HL, .DEC_SP => {
                if (opcode == .DEC_SP) {
                    self.sp = self.sp -% 1;
                } else {
                    const reg_pair: z80.register_pairs = switch (opcode) {
                        .DEC_BC => .BC,
                        .DEC_DE => .DE,
                        .DEC_HL => .HL,
                        else => unreachable,
                    };
                    const value = self.get_register_pair(reg_pair);
                    self.set_register_pair(reg_pair, value -% 1);
                }
            },

            // DAD: Double add register pair to HL
            .ADD_HL_BC, .ADD_HL_DE, .ADD_HL_HL, .ADD_HL_SP => {
                self.add_16(opcode);
            },

            // LD (nn), HL: Store H and L into memory address provided in operands
            .LD__NN__HL => {
                const addr = get_word_from_bytes(ram[self.pc + 1], ram[self.pc]);
                ram[addr] = self.l;
                ram[addr + 1] = self.h;
                self.pc = self.pc +% 2;
            },

            // LD (HL), NN: Load H and L from memory address provided in operands
            .LD_HL__NN_ => {
                const addr = get_word_from_bytes(ram[self.pc + 1], ram[self.pc]);
                self.l = ram[addr];
                self.h = ram[addr + 1];
                self.pc = self.pc +% 2;
            },

            // CPL: Complement accumulator
            .CPL => {
                self.a = ~self.a;
            },

            // STA: Store accumulator direct
            .LD__NN__A => {
                const addr = get_word_from_bytes(ram[self.pc + 1], ram[self.pc]);
                ram[addr] = self.a;
                self.pc = self.pc +% 2;
            },

            // STC: Set carry flag
            .SCF => {
                self.f.bits.c = 1;
            },

            // CMC: Complement carry flag
            .CCF => {
                self.f.bits.c = if (self.f.bits.c == 0) 1 else 0;
            },

            // LD A, (NN): Load accumulator direct
            .LD_A__NN_ => {
                const addr = get_word_from_bytes(ram[self.pc + 1], ram[self.pc]);
                self.a = ram[addr];
                self.pc = self.pc +% 2;
            },

            //HALT .
            .HALT => self.running_state = .halted,

            // ADD & ADC: Add with or without carry
            .ADD_A_B, .ADD_A_C, .ADD_A_D, .ADD_A_E, .ADD_A_H, .ADD_A_L, .ADD_A__HL_, .ADD_A_A => self.add_adc(instruction, ram[0..], false),
            .ADC_A_B, .ADC_A_C, .ADC_A_D, .ADC_A_E, .ADC_A_H, .ADC_A_L, .ADC_A__HL_, .ADC_A_A => self.add_adc(instruction, ram[0..], true),

            // SUB AND SBB: subtract with or without borrow
            .SUB_B, .SUB_C, .SUB_D, .SUB_E, .SUB_H, .SUB_L, .SUB__HL_, .SUB_A => self.sub_sbb(instruction, ram[0..], false),
            .SBC_A_B, .SBC_A_C, .SBC_A_D, .SBC_A_E, .SBC_A_H, .SBC_A_L, .SBC_A_A => self.sub_sbb(instruction, ram[0..], true),

            // ANA, XRA, ORA: Logical operations
            .AND_B, .AND_C, .AND_D, .AND_E, .AND_H, .AND_L, .AND__HL_, .AND_A, .XOR_B, .XOR_C, .XOR_D, .XOR_E, .XOR_H, .XOR_L, .XOR__HL_, .XOR_A, .OR_B, .OR_C, .OR_D, .OR_E, .OR_H, .OR_L, .OR__HL_, .OR_A => self.and_xor_or(instruction, ram[0..]),

            // RETs: return from subroutine
            .RET_NZ, .RET_NC, .RET_PO, .RET_PE, .RET_P, .RET_Z, .RET_C, .RET_M, .RET => {
                const condition = switch (opcode) {
                    .RET_NZ => self.f.bits.z == 0,
                    .RET_NC => self.f.bits.c == 0,
                    .RET_PO => self.f.bits.pv == 0,
                    .RET_PE => self.f.bits.pv == 1,
                    .RET_P => self.f.bits.s == 0,
                    .RET_M => self.f.bits.s == 1,
                    .RET_Z => self.f.bits.z == 1,
                    .RET_C => self.f.bits.c == 1,
                    .RET => true, // unconditional return
                    else => unreachable,
                };
                if (condition) {
                    const return_address = self.pop(ram[0..]);
                    self.pc = return_address;
                }
            },

            // POPs: Pops stack into register pairs
            .POP_BC, .POP_DE, .POP_HL, .POP_AF => {
                const reg_pair: z80.register_pairs = switch (opcode) {
                    .POP_BC => .BC,
                    .POP_DE => .DE,
                    .POP_HL => .HL,
                    .POP_AF => .AF,
                    else => unreachable,
                };
                const value = self.pop(ram[0..]);
                self.set_register_pair(reg_pair, value);
            },

            // JMPs: Jump to address
            .JP_NN, .JP_NZ_NN, .JP_Z_NN, .JP_NC_NN, .JP_C_NN, .JP_PO_NN, .JP_PE_NN, .JP_P_NN, .JP_M_NN => {
                const condition = switch (opcode) {
                    .JP_NN => true,
                    .JP_NZ_NN => self.f.bits.z == 0,
                    .JP_Z_NN => self.f.bits.z == 1,
                    .JP_NC_NN => self.f.bits.c == 0,
                    .JP_C_NN => self.f.bits.c == 1,
                    .JP_PO_NN => self.f.bits.pv == 0,
                    .JP_PE_NN => self.f.bits.pv == 1,
                    .JP_P_NN => self.f.bits.s == 0,
                    .JP_M_NN => self.f.bits.s == 1,
                    else => unreachable,
                };
                if (condition) {
                    const addr = get_word_from_bytes(ram[self.pc + 1], ram[self.pc]);
                    self.pc = addr;
                } else {
                    self.pc = self.pc +% 2;
                    // Increment PC by 2 to skip the jump address
                }
            },
            // CALLs: Call subroutine
            .CALL_NN,
            .CALL_NZ_NN,
            .CALL_Z_NN,
            .CALL_NC_NN,
            .CALL_C_NN,
            .CALL_PO_NN,
            .CALL_PE_NN,
            .CALL_P_NN,
            .CALL_M_NN,
            => {
                const condition = switch (opcode) {
                    .CALL_NN => true,
                    .CALL_NZ_NN => self.f.bits.z == 0,
                    .CALL_Z_NN => self.f.bits.z == 1,
                    .CALL_NC_NN => self.f.bits.c == 0,
                    .CALL_C_NN => self.f.bits.c == 1,
                    .CALL_PO_NN => self.f.bits.pv == 0,
                    .CALL_PE_NN => self.f.bits.pv == 1,
                    .CALL_P_NN => self.f.bits.s == 0,
                    .CALL_M_NN => self.f.bits.s == 1,
                    else => unreachable,
                };
                if (condition) {
                    const addr = get_word_from_bytes(ram[self.pc + 1], ram[self.pc]);
                    self.push(self.pc + 2, ram[0..]); // Push next instruction address
                    self.pc = addr; // Jump to subroutine address
                } else {
                    self.pc = self.pc +% 2;
                    // Increment PC by 2 to skip the call address
                }
            },

            // ACI, ADI: Add immediate with/without carry
            .ADC_A_N, .ADD_A_N => {
                const value: u8 = ram[self.pc];
                const carry_in: u8 = if (opcode == .ADC_A_N) self.f.bits.c else 0;
                const result = @as(u16, self.a) + @as(u16, value) + @as(u16, carry_in);
                const result_u8: u8 = @truncate(result);
                // Set flags
                self.set_zsp(result_u8);
                self.f.bits.c = z80.calc_carry(8, self.a, value, carry_in);
                self.f.bits.h = z80.calc_carry(4, self.a, value, carry_in);
                self.a = result_u8; // Store result in accumulator
                self.pc = self.pc +% 1;
            },

            // RSTs: push PC to stack and jump to address 0000000000EXP00
            .RST_00H, .RST_10H, .RST_20H, .RST_30H, .RST_08H, .RST_18H, .RST_28H, .RST_38H => {
                const addr = get_word_from_bytes(0x0000, instruction & 0b00111000);
                self.push(self.pc, ram[0..]); // Push current PC to stack
                self.pc = addr; // Jump to RST vector address
            },

            // PUSH: Push register pair onto stack
            .PUSH_BC, .PUSH_DE, .PUSH_HL, .PUSH_AF => {
                const reg_pair: z80.register_pairs = switch (opcode) {
                    .PUSH_BC => .BC,
                    .PUSH_DE => .DE,
                    .PUSH_HL => .HL,
                    .PUSH_AF => .AF,
                    else => unreachable,
                };
                const value = self.get_register_pair(reg_pair);
                self.push(value, ram[0..]); // Push register pair onto stack
            },

            // SUI, SBI: Subtract immediate with/without borrow
            .SUB_N, .SBC_A_N => {
                const value: u8 = ram[self.pc];
                const borrow_in: u8 = if (opcode == .SBC_A_N) self.f.bits.c else 0;
                const result = @as(u16, self.a) -% @as(u16, value) -% @as(u16, borrow_in);
                const result_u8: u8 = @truncate(result);
                // Set flags
                self.set_zsp(result_u8);
                self.f.bits.c = if (result > 0xFF) 1 else 0; // Set carry if underflow
                self.f.bits.h = if (((self.a & 0x0F) < (value & 0x0F) + borrow_in)) 1 else 0; // Check for half-borrow
                self.a = result_u8; // Store result in accumulator
                self.pc = self.pc +% 1;
                // Increment PC by 1 to account for the immediate data
            },

            // XTHL: Exchange stack top with HL
            .EX__SP__HL => {
                const hl_value = self.get_register_pair(.HL);
                const sp_value = self.pop(ram[0..]); // Pop top of stack
                self.set_register_pair(.HL, sp_value); // Set HL to stack top
                self.push(hl_value, ram[0..]); // Push old HL value onto stack
            },

            // ANI, ORI, XRI: Logical operations with immediate
            .AND_N, .OR_N, .XOR_N => {
                const value: u8 = ram[self.pc];
                switch (opcode) {
                    .AND_N => {
                        self.a = self.a & value; // AND operation
                        //self.f.bits.h = if ((self.a & 0x0F) < (value & 0x0F)) 1 else 0; // Check for half-carry
                        self.f.bits.h = if ((self.a | value) & 0x08 != 0) 1 else 0;
                    },
                    .OR_N => {
                        self.a = self.a | value; // OR operation
                        self.f.bits.h = 0; // No half-carry for OR
                    },
                    .XOR_N => {
                        self.a = self.a ^ value; // XOR operation
                        self.f.bits.h = 0; // No half-carry for XOR
                    },
                    else => unreachable,
                }
                // Set flags
                self.set_zsp(self.a);
                self.f.bits.c = 0; // No carry for logical operations
                self.pc = self.pc +% 1;
                // Increment PC by 1 to account for the immediate data
            },

            // PCHL: Load program counter from HL
            .JP__HL_ => {
                self.pc = self.get_register_pair(.HL); // Set PC to value in HL
            },

            .EX_DE_HL => {
                const hl_value = self.get_register_pair(.HL);
                const de_value = self.get_register_pair(.DE);
                self.set_register_pair(.HL, de_value);
                self.set_register_pair(.DE, hl_value);
            },

            // DI: Disable interrupts
            .DI => {
                self.iff1 = 0;
                self.iff2 = 0;
            },

            // EI: Enable interrupts
            .EI => {
                self.iff1 = 1;
                self.iff2 = 1;
            },

            // SPHL: Set stack pointer to HL
            .LD_SP_HL => {
                self.sp = self.get_register_pair(.HL); // Set stack pointer to value in HL
            },

            // CMP CPI: Compare immediate with accumulator
            .CP_N, .CP_A, .CP_B, .CP_C, .CP_D, .CP_E, .CP_H, .CP_L, .CP__HL_ => {
                var value: u8 = undefined;
                if (opcode == .CP_N) {
                    // CPI: Compare immediate
                    value = ram[self.pc];
                    self.pc = self.pc +% 1;
                    // Increment PC by 1 for CPI
                } else {
                    // CMP: Compare register or memory
                    const reg: u4 = @truncate(instruction & 0b00000111);
                    if (reg == 0x06) { // CMP M
                        const addr = self.get_register_pair(.HL);
                        value = ram[addr];
                    } else { // CMP r
                        value = self.get_register(reg);
                    }
                }
                const result = @as(u16, self.a) -% @as(u16, value);
                // Set flags
                self.set_zsp(@truncate(result));
                self.f.bits.c = if (result > 0xFF) 1 else 0; // Set carry if underflow
                //self.f.bits.h = if (((self.a & 0x0F) < (value & 0x0F))) 1 else 0; // Check for half-borrow
                self.f.bits.h = if (~((self.a ^ (result & 0xFF) ^ value) & 0x10) > 0) 1 else 0;
            },

            // OUT
            .OUT__N__A => {
                const device = ram[self.pc];
                self.pc = self.pc +% 1;

                self.ports_out[device] = self.a;
            },

            .IN_A__N_ => {
                const device = ram[self.pc];
                self.pc = self.pc +% 1;

                self.a = self.ports_in[device];
            },

            // z80 extensions
            .BIT => {
                self.bit_instructions(ram[0..]);
            },
            .MISC => {
                self.misc_instructions(ram[0..]);
            },
            .IX => {
                self.ix_instructions(ram[0..]);
            },
            .IY => {
                self.iy_instructions(ram[0..]);
            },

            else => {},
        }
    }
};

//************************************************************************************************************/

//   TTTTTTTTTTTT    EEEEEEEEEEEE     SSSSSSSSSSSS    TTTTTTTTTTTT
//   TTTTTTTTTTTT    EEE        E    SSSS        SS   TTTTTTTTTTTT
//       TTT         EEE              SS                  TTT
//       TTT         EEEEEEE           SSSSSS             TTT
//       TTT         EEE                    SSSSS         TTT
//       TTT         EEE                       SSSS       TTT
//       TTT         EEE        E     SS      SSSS        TTT
//       TTT         EEEEEEEEEEEE      SSSSSSSSSSS        TTT

//************************************************************************************************************/

// Load step test cases

// stub to debug
pub fn main() void {
    var in: [256]u8 = undefined;
    in[0x10] = 0x12; // Simulate input port 0x10 returning 0x12
    var out: [256]u8 = undefined;
    var cpu = z80{ .ports_in = in[0..], .ports_out = out[0..] };
    var ram: [0xFFFF]u8 = undefined;

    // LD (IX+D), B
    cpu.pc = 0; // Reset program counter
    cpu.ix = 0x1000;
    cpu.b = 0xCD; // B
    ram[0] = @intFromEnum(z80.opcodes.IX);
    ram[1] = @intFromEnum(z80.ix_opcodes.LD__IXD__B); // LD (IX+N), B
    ram[2] = 0x05; // D (5 offset
    cpu.execute_instruction(ram[0..], true);
    std.debug.print("Memory at 0x1005: {x:02}\n", .{ram[0x1005]});
}

test "Load" {
    var in: [256]u8 = undefined;
    var out: [256]u8 = undefined;
    var cpu = z80{ .ports_in = in[0..], .ports_out = out[0..] };

    var ram: [256]u8 = undefined;

    // MOV B, C: C -> B
    cpu.set_register(0, 0x12); // b
    cpu.set_register(1, 0x34); // c
    cpu.print_registers();
    ram[0] = @intFromEnum(z80.opcodes.LD_B_C); // MOV b, c
    cpu.execute_instruction(ram[0..], true);
    cpu.print_registers();
    try std.testing.expect(cpu.get_register(0) == 0x34); // B should now be 0x34

    // MOV C, D
    cpu.d = 0x56;
    ram[1] = @intFromEnum(z80.opcodes.LD_C_D); // MOV c, d
    cpu.execute_instruction(ram[0..], true);
    try std.testing.expect(cpu.c == 0x56); // C should now be 0x56

    // MOV M, A (using H and L)
    cpu.h = 0x00;
    cpu.l = 0x34;
    cpu.a = 0x78;
    ram[2] = @intFromEnum(z80.opcodes.LD__HL__A); // MOV M, A
    cpu.execute_instruction(ram[0..], true);
    try std.testing.expect(ram[0x0034] == 0x78); // Memory at address HL should be 0x78
}

test "Shift" {
    var in: [256]u8 = undefined;
    var out: [256]u8 = undefined;
    var cpu = z80{ .ports_in = in[0..], .ports_out = out[0..] };
    var ram: [256]u8 = undefined;

    // SLA B
    cpu.set_register(0, 0b00011000); // b
    cpu.f.bits.c = 1;
    cpu.print_registers();
    ram[0] = @intFromEnum(z80.opcodes.BIT);
    ram[1] = @intFromEnum(z80.bit_opcodes.SLA_B);
    cpu.execute_instruction(ram[0..], true);
    cpu.print_registers();
    try std.testing.expect(cpu.get_register(0) == 0b00110000); // B should now be 0x34
    try std.testing.expect(cpu.f.bits.c == 0); // Carry flag should be cleared

    // SRA C
    cpu.pc = 0; // Reset program counter
    cpu.set_register(1, 0b00011000); // c
    cpu.f.bits.c = 1; // Clear carry flag
    ram[0] = @intFromEnum(z80.opcodes.BIT);
    ram[1] = @intFromEnum(z80.bit_opcodes.SRA_C);
    cpu.execute_instruction(ram[0..], true);
    cpu.print_registers();
    try std.testing.expect(cpu.get_register(1) == 0b00001100); // C should now be 0x0C
    try std.testing.expect(cpu.f.bits.c == 0); // Carry flag should be cleared

    // SLL D
    cpu.pc = 0; // Reset program counter
    cpu.set_register(2, 0b00011000); // d
    cpu.f.bits.c = 1; // Clear carry flag
    ram[0] = @intFromEnum(z80.opcodes.BIT);
    ram[1] = @intFromEnum(z80.bit_opcodes.SLL_D);
    cpu.execute_instruction(ram[0..], true);
    cpu.print_registers();
    try std.testing.expect(cpu.get_register(2) == 0b00110001); // D should now be 0x30
    try std.testing.expect(cpu.f.bits.c == 0); // Carry flag should be cleared

    // SRL E
    cpu.pc = 0; // Reset program counter
    cpu.set_register(3, 0b00011000); // e
    cpu.f.bits.c = 1; // Clear carry flag
    ram[0] = @intFromEnum(z80.opcodes.BIT);
    ram[1] = @intFromEnum(z80.bit_opcodes.SRL_E);
    cpu.execute_instruction(ram[0..], true);
    cpu.print_registers();
    try std.testing.expect(cpu.get_register(3) == 0b00001100); // E should now be 0x0C
    try std.testing.expect(cpu.f.bits.c == 0); // Carry flag should be cleared

}

test "Rotate" {
    var in: [256]u8 = undefined;
    var out: [256]u8 = undefined;
    var cpu = z80{ .ports_in = in[0..], .ports_out = out[0..] };
    var ram: [256]u8 = undefined;

    // RLC B
    cpu.set_register(0, 0b00011000); // b
    cpu.f.bits.c = 0; // Clear carry flag
    cpu.print_registers();
    ram[0] = @intFromEnum(z80.opcodes.BIT);
    ram[1] = @intFromEnum(z80.bit_opcodes.RLC_B);
    cpu.execute_instruction(ram[0..], true);
    cpu.print_registers();
    try std.testing.expect(cpu.get_register(0) == 0b00110000); // B should now be 0x34

    // RLC (HL)
    cpu.pc = 0; // Reset program counter
    cpu.set_register_pair(.HL, 0xF0);
    cpu.f.bits.c = 1;
    ram[0xF0] = 0b00111000; // Memory at HL
    ram[0] = @intFromEnum(z80.opcodes.BIT);
    ram[1] = @intFromEnum(z80.bit_opcodes.RLC__HL_);
    cpu.execute_instruction(ram[0..], true);
    std.debug.print("Memory at 0xF0: {b:08}\n", .{ram[0xF0]});
    try std.testing.expect(ram[0xF0] == 0b01110000);

    // RL D
    cpu.pc = 0; // Reset program counter
    cpu.set_register(2, 0b10111000);
    cpu.f.bits.c = 1;
    ram[0] = @intFromEnum(z80.opcodes.BIT);
    ram[1] = @intFromEnum(z80.bit_opcodes.RL_D);
    cpu.execute_instruction(ram[0..], true);
    //std.debug.print("Memory at 0xF0: {b:08}\n", .{ram[0xF0]});
    try std.testing.expect(cpu.d == 0b01110001);
    try std.testing.expect(cpu.f.bits.c == 1); // Carry flag should be set

    // RRC E
    cpu.pc = 0; // Reset program counter
    cpu.set_register(3, 0b10111000);
    cpu.f.bits.c = 1;
    ram[0] = @intFromEnum(z80.opcodes.BIT);
    ram[1] = @intFromEnum(z80.bit_opcodes.RRC_E);
    cpu.execute_instruction(ram[0..], true);
    try std.testing.expect(cpu.e == 0b01011100); // E should now    be 0b01011100
    try std.testing.expect(cpu.f.bits.c == 0); // Carry flag should be set

    // RRC (HL)
    cpu.pc = 0; // Reset program counter
    cpu.set_register_pair(.HL, 0xF0);
    cpu.f.bits.c = 1;
    ram[0xF0] = 0b00111000; // Memory at HL
    ram[0] = @intFromEnum(z80.opcodes.BIT);
    ram[1] = @intFromEnum(z80.bit_opcodes.RRC__HL_);
    cpu.execute_instruction(ram[0..], true);
    std.debug.print("Memory at 0xF0: {b:08}\n", .{ram[0xF0]});
    try std.testing.expect(ram[0xF0] == 0b00011100); // Memory at HL should be 0
    try std.testing.expect(cpu.f.bits.c == 0); // Carry flag should be cleared

    // RR D
    cpu.pc = 0; // Reset program counter
    cpu.set_register(2, 0b10111000);
    cpu.f.bits.c = 1;
    ram[0] = @intFromEnum(z80.opcodes.BIT);
    ram[1] = @intFromEnum(z80.bit_opcodes.RR_D);
    cpu.execute_instruction(ram[0..], true);
    try std.testing.expect(cpu.d == 0b11011100); // D should now be 0b01011100
    try std.testing.expect(cpu.f.bits.c == 0); // Carry flag should be set
}

test "Bit" {
    var in: [256]u8 = undefined;
    var out: [256]u8 = undefined;
    var cpu = z80{ .ports_in = in[0..], .ports_out = out[0..] };
    var ram: [256]u8 = undefined;

    // BIT 3, B
    cpu.set_register(0, 0b00011000); // b
    cpu.f.bits.c = 0; // Clear carry flag
    cpu.print_registers();
    ram[0] = @intFromEnum(z80.opcodes.BIT);
    ram[1] = @intFromEnum(z80.bit_opcodes.BIT_3_B);
    cpu.execute_instruction(ram[0..], true);
    cpu.print_registers();
    try std.testing.expect(cpu.get_register(0) == 0b00011000); // B should remain unchanged
    try std.testing.expect(cpu.f.bits.z == 0); // Zero flag should be cleared (bit 3 is set)
    try std.testing.expect(cpu.f.bits.h == 1); // Half-carry flag should be set
    try std.testing.expect(cpu.f.bits.n == 0); // Subtraction flag should be cleared

    // BIT 4, C
    cpu.pc = 0; // Reset program counter
    cpu.set_register(1, 0b00011000); // c
    cpu.f.bits.c = 1; // Set carry flag
    ram[0] = @intFromEnum(z80.opcodes.BIT);
    ram[1] = @intFromEnum(z80.bit_opcodes.BIT_4_C);
    cpu.execute_instruction(ram[0..], true);
    cpu.print_registers();
    try std.testing.expect(cpu.get_register(1) == 0b00011000); // C should remain unchanged
    try std.testing.expect(cpu.f.bits.z == 0); // Zero flag should be set (bit 4 is set)
    try std.testing.expect(cpu.f.bits.h == 1); // Half-carry flag should be set
    try std.testing.expect(cpu.f.bits.n == 0); // Subtraction flag should be cleared

    // BIT 5, (HL)
    cpu.pc = 0; // Reset program counter
    cpu.set_register_pair(.HL, 0xF0);
    cpu.f.bits.c = 1; // Set carry flag
    ram[0xF0] = 0b00011000; // Memory at HL
    ram[0] = @intFromEnum(z80.opcodes.BIT);
    ram[1] = @intFromEnum(z80.bit_opcodes.BIT_5__HL_);
    cpu.execute_instruction(ram[0..], true);
    std.debug.print("Memory at 0xF0: {b:08}\n", .{ram[0xF0]});
    try std.testing.expect(ram[0xF0] == 0b00011000); // Memory at HL should remain unchanged
    try std.testing.expect(cpu.f.bits.z == 1); // Zero flag should be set (bit 5 is clear)
    try std.testing.expect(cpu.f.bits.h == 1); // Half-carry flag should be set
    try std.testing.expect(cpu.f.bits.n == 0); // Subtraction flag should be cleared

}

test "Reset bit" {
    var in: [256]u8 = undefined;
    var out: [256]u8 = undefined;
    var cpu = z80{ .ports_in = in[0..], .ports_out = out[0..] };
    var ram: [256]u8 = undefined;

    // RES 3, B
    cpu.set_register(0, 0b11111111); // b
    cpu.print_registers();
    ram[0] = @intFromEnum(z80.opcodes.BIT);
    ram[1] = @intFromEnum(z80.bit_opcodes.RES_3_B);
    cpu.execute_instruction(ram[0..], true);
    cpu.print_registers();
    try std.testing.expect(cpu.get_register(0) == 0b11110111); // B should now be 0b11110111

    // RES 4, C
    cpu.pc = 0; // Reset program counter
    cpu.set_register(1, 0b11111111); // c
    ram[0] = @intFromEnum(z80.opcodes.BIT);
    ram[1] = @intFromEnum(z80.bit_opcodes.RES_4_C);
    cpu.execute_instruction(ram[0..], true);
    cpu.print_registers();
    try std.testing.expect(cpu.get_register(1) == 0b11101111); // C should now be 0b11101111

    // RES 5, (HL)
    cpu.pc = 0; // Reset program counter
    cpu.set_register_pair(.HL, 0xF0);
    cpu.f.bits.c = 1; // Set carry flag
    ram[0xF0] = 0b11111111; // Memory at HL
    ram[0] = @intFromEnum(z80.opcodes.BIT);
    ram[1] = @intFromEnum(z80.bit_opcodes.RES_5__HL_);
    cpu.execute_instruction(ram[0..], true);
    std.debug.print("Memory at 0xF0: {b:08}\n", .{ram[0xF0]});
    try std.testing.expect(ram[0xF0] == 0b11011111);
}

test "Set bit" {
    var in: [256]u8 = undefined;
    var out: [256]u8 = undefined;
    var cpu = z80{ .ports_in = in[0..], .ports_out = out[0..] };
    var ram: [256]u8 = undefined;

    // SET 3, B
    cpu.set_register(0, 0b00000000); // b
    cpu.print_registers();
    ram[0] = @intFromEnum(z80.opcodes.BIT);
    ram[1] = @intFromEnum(z80.bit_opcodes.SET_3_B);
    cpu.execute_instruction(ram[0..], true);
    cpu.print_registers();
    try std.testing.expect(cpu.get_register(0) == 0b00001000); // B should now be 0b00001000

    // SET 4, C
    cpu.pc = 0; // Reset program counter
    cpu.set_register(1, 0b00000000); // c
    ram[0] = @intFromEnum(z80.opcodes.BIT);
    ram[1] = @intFromEnum(z80.bit_opcodes.SET_4_C);
    cpu.execute_instruction(ram[0..], true);
    cpu.print_registers();
    try std.testing.expect(cpu.get_register(1) == 0b00010000); // C should now be 0b00010000

    // SET 5, (HL)
    cpu.pc = 0; // Reset program counter
    cpu.set_register_pair(.HL, 0xF0);
    ram[0xF0] = 0b00000000; // Memory at HL
    ram[0] = @intFromEnum(z80.opcodes.BIT);
    ram[1] = @intFromEnum(z80.bit_opcodes.SET_5__HL_);
    cpu.execute_instruction(ram[0..], true);
    std.debug.print("Memory at 0xF0: {b:08}\n", .{ram[0xF0]});
    try std.testing.expect(ram[0xF0] == 0b00100000); // Memory at HL should now be 0b00100000
}

test "Misc" {
    var in: [256]u8 = undefined;
    in[0x10] = 0x12; // Simulate input port 0x10 returning 0x12
    var out: [256]u8 = undefined;
    var cpu = z80{ .ports_in = in[0..], .ports_out = out[0..] };
    var ram: [0xFFFF]u8 = undefined;

    // IN instructions
    cpu.pc = 0; // Reset program counter
    cpu.set_register(0, 0x12); // b
    cpu.set_register(1, 0x10); // c
    ram[0] = @intFromEnum(z80.opcodes.MISC);
    ram[1] = @intFromEnum(z80.misc_opcodes.IN_B__C_);
    cpu.execute_instruction(ram[0..], true);
    try std.testing.expect(cpu.b == 0x12);

    // OUT instructions
    cpu.pc = 0; // Reset program counter
    std.debug.print("Output port before 0x10: {x:02}\n", .{cpu.ports_out[0x10]});

    cpu.set_register(0, 0x34); // b
    cpu.set_register(1, 0x10); // c
    ram[0] = @intFromEnum(z80.opcodes.MISC);
    ram[1] = @intFromEnum(z80.misc_opcodes.OUT__C__B); // OUT (C), B
    cpu.execute_instruction(ram[0..], true);
    std.debug.print("Output port 0x10: {x:02}\n", .{cpu.ports_out[0x10]});
    try std.testing.expect(cpu.ports_out[0x10] == 0x34);

    // SBC HL, BC
    cpu.pc = 0; // Reset program counter
    cpu.set_register_pair(.BC, 0x0010); // Set BC to 0x0010
    cpu.set_register_pair(.HL, 0x0020);
    cpu.f.bits.c = 0;
    ram[0] = @intFromEnum(z80.opcodes.MISC);
    ram[1] = @intFromEnum(z80.misc_opcodes.SBC_HL_BC); // SBC HL, BC
    cpu.execute_instruction(ram[0..], true);
    try std.testing.expect(cpu.get_register_pair(.HL) == 0x0010);
    try std.testing.expect(cpu.f.bits.c == 0); // Carry flag should be cleared
    try std.testing.expect(cpu.f.bits.h == 0); // Half-carry flag should be cleared
    try std.testing.expect(cpu.f.bits.z == 0); // Zero flag should be cleared
    try std.testing.expect(cpu.f.bits.s == 0); // Sign flag should be cleared
    try std.testing.expect(cpu.f.bits.pv == 0); // Parity/Overflow flag should be cleared
    try std.testing.expect(cpu.f.bits.n == 1); // Subtraction flag should be

    cpu.pc = 0; // Reset program counter
    cpu.set_register_pair(.BC, 0x0010); // Set BC to 0x0010
    cpu.set_register_pair(.HL, 0x0020);
    cpu.f.bits.c = 1;
    ram[0] = @intFromEnum(z80.opcodes.MISC);
    ram[1] = @intFromEnum(z80.misc_opcodes.SBC_HL_BC); // SBC HL, BC
    cpu.execute_instruction(ram[0..], true);
    try std.testing.expect(cpu.get_register_pair(.HL) == 0x000F);
    try std.testing.expect(cpu.f.bits.c == 0); // Carry flag should be cleared
    try std.testing.expect(cpu.f.bits.h == 0); // Half-carry flag should be cleared
    try std.testing.expect(cpu.f.bits.z == 0); // Zero flag should be cleared
    try std.testing.expect(cpu.f.bits.s == 0); // Sign flag should be cleared
    try std.testing.expect(cpu.f.bits.pv == 1); // Parity/Overflow flag should be cleared
    try std.testing.expect(cpu.f.bits.n == 1); // Subtraction flag should be

    // ADC HL, BC
    cpu.pc = 0; // Reset program counter
    cpu.set_register_pair(.BC, 0x0010); // Set BC to 0x0010
    cpu.set_register_pair(.HL, 0x0020);
    cpu.f.bits.c = 0;
    ram[0] = @intFromEnum(z80.opcodes.MISC);
    ram[1] = @intFromEnum(z80.misc_opcodes.ADC_HL_BC); // ADC HL, BC
    cpu.execute_instruction(ram[0..], true);
    try std.testing.expect(cpu.get_register_pair(.HL) == 0x0030);
    try std.testing.expect(cpu.f.bits.c == 0); // Carry flag should be cleared
    try std.testing.expect(cpu.f.bits.h == 0); // Half-carry flag should be cleared
    try std.testing.expect(cpu.f.bits.z == 0); // Zero flag should be cleared
    try std.testing.expect(cpu.f.bits.s == 0); // Sign flag should be cleared
    try std.testing.expect(cpu.f.bits.pv == 1); // Parity/Overflow flag should be cleared
    try std.testing.expect(cpu.f.bits.n == 0); // Subtraction flag should be cleared

    // LD BC, (NN)
    cpu.pc = 0; // Reset program counter
    ram[0] = @intFromEnum(z80.opcodes.MISC);
    ram[1] = @intFromEnum(z80.misc_opcodes.LD_BC__NN_); // LD BC, (NN)
    ram[2] = 0x21; // Low byte of address
    ram[3] = 0x30; // High byte of address1
    ram[0x2130] = 0x65; // Memory at address 0x3412
    ram[0x2131] = 0x78; // Memory at address 0x3413
    cpu.execute_instruction(ram[0..], true);
    std.debug.print("BC: {x:04}\n", .{cpu.get_register_pair(.BC)});
    try std.testing.expect(cpu.get_register_pair(.BC) == 0x7865);

    // LD (NN), BC
    cpu.pc = 0; // Reset program counter
    cpu.set_register_pair(.BC, 0x4644); // Set BC to 0x1234
    ram[0] = @intFromEnum(z80.opcodes.MISC);
    ram[1] = @intFromEnum(z80.misc_opcodes.LD__NN__BC); // LD (NN), BC
    ram[2] = 0x10; // Low byte of address
    ram[3] = 0x00; // High byte of address
    cpu.execute_instruction(ram[0..], true);
    try std.testing.expect(ram[0x1000] == 0x44);
    try std.testing.expect(ram[0x1001] == 0x46);

    // RLD
    cpu.pc = 0; // Reset program counter
    cpu.set_register_pair(.HL, 0x2000); // Set HL to 0x2000
    ram[0x2000] = 0x12; // after: 24
    cpu.a = 0x34; // after: 0x31
    ram[0] = @intFromEnum(z80.opcodes.MISC);
    ram[1] = @intFromEnum(z80.misc_opcodes.RLD); // RLD
    cpu.execute_instruction(ram[0..], true);
    std.debug.print("RLD. A: {x:02}, Memory at 0x2000: {x:02}\n", .{ cpu.a, ram[0x2000] });
    try std.testing.expect(cpu.a == 0x31);
    try std.testing.expect(ram[0x2000] == 0x24);

    // RRD
    cpu.pc = 0; // Reset program counter
    cpu.set_register_pair(.HL, 0x3000); // Set HL to 0x3000
    ram[0x3000] = 0x12; // after: 41
    cpu.a = 0x34; // after: 0   x32
    ram[0] = @intFromEnum(z80.opcodes.MISC);
    ram[1] = @intFromEnum(z80.misc_opcodes.RRD); // RRD
    cpu.execute_instruction(ram[0..], true);
    std.debug.print("RRD. A: {x:02}, Memory at 0x3000: {x:02}\n", .{ cpu.a, ram[0x3000] });

    try std.testing.expect(cpu.a == 0x32);
    try std.testing.expect(ram[0x3000] == 0x41);

    // LDI
    cpu.pc = 0; // Reset program counter
    cpu.set_register_pair(.HL, 0x4000); // Set HL to 0x4000
    cpu.set_register_pair(.DE, 0x5000); // Set DE to 0x5000
    cpu.set_register_pair(.BC, 0x0003); // Set BC to 3
    ram[0x4000] = 0xAA;
    ram[0x4001] = 0xBB;
    ram[0] = @intFromEnum(z80.opcodes.MISC);
    ram[1] = @intFromEnum(z80.misc_opcodes.LDI); // LDI
    cpu.execute_instruction(ram[0..], true);
    std.debug.print("LDI. HL: {x:04}, DE: {x:04}, BC: {x:04}\n", .{ cpu.get_register_pair(.HL), cpu.get_register_pair(.DE), cpu.get_register_pair(.BC) });
    try std.testing.expect(cpu.get_register_pair(.HL) == 0x4001);
    try std.testing.expect(cpu.get_register_pair(.DE) == 0x5001);
    try std.testing.expect(cpu.get_register_pair(.BC) == 0x0002);
    try std.testing.expect(ram[0x5000] == 0xAA);
    //try std.testing.expect(ram[0x5001] == 0xBB);
    try std.testing.expect(cpu.f.bits.h == 0);
    try std.testing.expect(cpu.f.bits.pv == 1);

    // CPI
    cpu.pc = 0; // Reset program counter
    cpu.set_register_pair(.HL, 0x6000); // Set HL to 0x6000
    cpu.set_register_pair(.BC, 0x0003); // Set BC to 3
    ram[0x6000] = 0xAA;
    ram[0x6001] = 0xBB;
    cpu.a = 0xAA; // Set accumulator to 0xAA
    ram[0] = @intFromEnum(z80.opcodes.MISC);
    ram[1] = @intFromEnum(z80.misc_opcodes.CPI); // CPI
    cpu.execute_instruction(ram[0..], true);
    std.debug.print("CPI. HL: {x:04}, DE: {x:04}, BC: {x:04}, A: {x:02}\n", .{ cpu.get_register_pair(.HL), cpu.get_register_pair(.DE), cpu.get_register_pair(.BC), cpu.a });
    try std.testing.expect(cpu.get_register_pair(.HL) == 0x6001);
    try std.testing.expect(cpu.get_register_pair(.BC) == 0x0002);
    try std.testing.expect(cpu.a == 0xAA); // A should remain unchanged
    try std.testing.expect(cpu.f.bits.h == 0); // Half-carry flag should be cleared
    try std.testing.expect(cpu.f.bits.pv == 1); // Parity/Overflow flag should be set
    try std.testing.expect(cpu.f.bits.z == 0);

    // INI
    cpu.pc = 0; // Reset program counter
    cpu.c = 0x10; // Set C to 0x10 (input port)
    cpu.set_register_pair(.BC, 0x0010); // Set BC to 0x0010
    cpu.set_register_pair(.HL, 0x7000);
    cpu.ports_in[0x10] = 0x01; // Simulate input port 0x10 returning 0x01
    cpu.b = 0x0F; // Set B to 0x0F (counter)
    cpu.f.bits.c = 0;
    ram[0] = @intFromEnum(z80.opcodes.MISC);
    ram[1] = @intFromEnum(z80.misc_opcodes.INI); // INI
    cpu.execute_instruction(ram[0..], true);
    try std.testing.expect(cpu.c == 0x10);
    try std.testing.expect(cpu.get_register_pair(.HL) == 0x7001);
    try std.testing.expect(cpu.ports_in[0x10] == 0x01);
    try std.testing.expect(cpu.f.bits.z == 0); // Zero flag should be cleared

}

test "IX" {
    var in: [256]u8 = undefined;
    var out: [256]u8 = undefined;
    var cpu = z80{ .ports_in = in[0..], .ports_out = out[0..] };
    var ram: [0x6000]u8 = undefined;

    // INC_B: undocumented
    cpu.pc = 0; // Reset program counter
    cpu.b = 0x12;
    ram[0] = @intFromEnum(z80.opcodes.IX);
    ram[1] = @intFromEnum(z80.ix_opcodes.INC_B); // INC B
    cpu.execute_instruction(ram[0..], true);
    try std.testing.expect(cpu.b == 0x13);
    // DEC_C: undocumented
    cpu.pc = 0; // Reset program counter
    cpu.c = 0x34;
    ram[0] = @intFromEnum(z80.opcodes.IX);
    ram[1] = @intFromEnum(z80.ix_opcodes.DEC_C); // DEC C
    cpu.execute_instruction(ram[0..], true);
    try std.testing.expect(cpu.c == 0x33);
    // LD B, N: undocumented
    cpu.pc = 0; // Reset program counter
    ram[0] = @intFromEnum(z80.opcodes.IX);
    ram[1] = @intFromEnum(z80.ix_opcodes.LD_B_N); // LD B, N
    ram[2] = 0x56; // N
    cpu.execute_instruction(ram[0..], true);
    try std.testing.expect(cpu.b == 0x56);

    // INC_IXH
    cpu.pc = 0; // Reset program counter
    cpu.ix = 0x2000;
    ram[0] = @intFromEnum(z80.opcodes.IX);
    ram[1] = @intFromEnum(z80.ix_opcodes.INC_IXH); // INC (IX)
    cpu.execute_instruction(ram[0..], true);
    try std.testing.expect(cpu.ix == 0x2100);

    // DEC_IXL
    cpu.pc = 0; // Reset program counter
    cpu.ix = 0x2100;
    ram[0] = @intFromEnum(z80.opcodes.IX);
    ram[1] = @intFromEnum(z80.ix_opcodes.DEC_IXL); // DEC (IX)
    cpu.execute_instruction(ram[0..], true);
    std.debug.print("IX after DEC_IXL: {x:04}\n", .{cpu.ix});

    try std.testing.expect(cpu.ix == 0x21FF);

    // LD IX, NN
    cpu.pc = 0; // Reset program counter
    ram[0] = @intFromEnum(z80.opcodes.IX);
    ram[1] = @intFromEnum(z80.ix_opcodes.LD_IX_NN); // LD IX, NN
    ram[2] = 0x34; // Low byte
    ram[3] = 0x12; // High byte
    cpu.execute_instruction(ram[0..], true);
    try std.testing.expect(cpu.ix == 0x1234);

    // ADD IX, BC
    cpu.pc = 0; // Reset program counter
    cpu.ix = 0x1000;
    cpu.set_register_pair(.BC, 0x0100);
    ram[0] = @intFromEnum(z80.opcodes.IX);
    ram[1] = @intFromEnum(z80.ix_opcodes.ADD_IX_BC); // ADD IX, BC
    cpu.execute_instruction(ram[0..], true);
    try std.testing.expect(cpu.ix == 0x1100);

    // LD (NN), IX
    cpu.pc = 0; // Reset program counter
    cpu.ix = 0x5678;
    ram[0] = @intFromEnum(z80.opcodes.IX);
    ram[1] = @intFromEnum(z80.ix_opcodes.LD__NN__IX); // LD (NN), IX
    ram[2] = 0x00; // Low byte
    ram[3] = 0x20; // High byte
    cpu.execute_instruction(ram[0..], true);
    try std.testing.expect(ram[0x2000] == 0x78);
    try std.testing.expect(ram[0x2001] == 0x56);

    // LD (IXD), N
    cpu.pc = 0; // Reset program counter
    cpu.ix = 0x0000;
    ram[0] = @intFromEnum(z80.opcodes.IX);
    ram[1] = @intFromEnum(z80.ix_opcodes.LD__IXD__N); // LD IXD, N
    ram[2] = 0x7F; // D
    ram[3] = 0xBB; // N
    cpu.execute_instruction(ram[0..], true);
    std.debug.print("IX after LD IXD, N: {x:04}\n", .{cpu.ix});
    try std.testing.expect(ram[0x007F] == 0xBB);

    // LD B, (IX+N)
    cpu.pc = 0; // Reset program counter
    cpu.ix = 0x1000;
    ram[0x1005] = 0xAB; // Memory at IX + 5
    ram[0] = @intFromEnum(z80.opcodes.IX);
    ram[1] = @intFromEnum(z80.ix_opcodes.LD_B__IXD_); // LD B, (IX+N)
    ram[2] = 0x05; // N (5 offset
    cpu.execute_instruction(ram[0..], true);
    try std.testing.expect(cpu.b == 0xAB);

    // LD (IX+D), B
    cpu.pc = 0; // Reset program counter
    cpu.ix = 0x1000;
    cpu.b = 0xCD; // B
    ram[0] = @intFromEnum(z80.opcodes.IX);
    ram[1] = @intFromEnum(z80.ix_opcodes.LD__IXD__B); // LD (IX+N), B
    ram[2] = 0x05; // D (5 offset
    cpu.execute_instruction(ram[0..], true);
    std.debug.print("Memory at 0x1005: {x:02}\n", .{ram[0x1005]});
    try std.testing.expect(ram[0x1005] == 0xCD);

    // LD (IX+D), C
    cpu.pc = 0; // Reset program counter
    cpu.ix = 0x1000;
    cpu.b = 0xCE; // B
    ram[0] = @intFromEnum(z80.opcodes.IX);
    ram[1] = @intFromEnum(z80.ix_opcodes.LD__IXD__C); // LD (IX+N), B
    ram[2] = 0xFF; // D (-1 offset)
    cpu.execute_instruction(ram[0..], true);
    std.debug.print("Memory at 0x1005: {x:02}\n", .{ram[0x0FFF]});
    try std.testing.expect(ram[0x0FFF] == 0xCE);

    // INC (IX+N)
    cpu.pc = 0; // Reset program counter
    cpu.ix = 0x2000;
    ram[0x200A] = 0x7F; // Memory at IX + 10
    ram[0] = @intFromEnum(z80.opcodes.IX);
    ram[1] = @intFromEnum(z80.ix_opcodes.INC__IXD_); // INC (IX+N)
    ram[2] = 0x0A; // N (10 offset
    cpu.execute_instruction(ram[0..], true);
    try std.testing.expect(ram[0x200A] == 0x80);
    try std.testing.expect(cpu.f.bits.s == 1); // Sign flag should be set
    try std.testing.expect(cpu.f.bits.z == 0); // Zero flag should be cleared
    try std.testing.expect(cpu.f.bits.h == 1); // Half-carry flag should be set
    try std.testing.expect(cpu.f.bits.pv == 1); // Parity/Overflow flag should be set
    try std.testing.expect(cpu.f.bits.n == 0);

    // DEC (IX+N)
    cpu.pc = 0; // Reset program counter
    cpu.ix = 0x2000;
    ram[0x200A] = 0x01; // Memory at IX + 10
    ram[0] = @intFromEnum(z80.opcodes.IX);
    ram[1] = @intFromEnum(z80.ix_opcodes.DEC__IXD_); // DEC (IX+N)
    ram[2] = 0x0A; // N (10 offset
    cpu.execute_instruction(ram[0..], true);
    try std.testing.expect(ram[0x200A] == 0x00);
    try std.testing.expect(cpu.f.bits.s == 0); // Sign flag should be cleared
    try std.testing.expect(cpu.f.bits.z == 1); // Zero flag should be set
    try std.testing.expect(cpu.f.bits.h == 0); // Half-carry flag should be set
    try std.testing.expect(cpu.f.bits.pv == 0); // Parity/Overflow flag should be cleared
    try std.testing.expect(cpu.f.bits.n == 1);

    // ADD A, (IX+N)
    cpu.pc = 0; // Reset program counter
    cpu.ix = 0x1000;
    cpu.a = 0x10;
    ram[0x1005] = 0x22; // Memory at IX + 5
    ram[0] = @intFromEnum(z80.opcodes.IX);
    ram[1] = @intFromEnum(z80.ix_opcodes.ADD_A__IXD_); // ADD A, (IX+N)
    ram[2] = 0x05; // N (5 offset
    cpu.execute_instruction(ram[0..], true);
    try std.testing.expect(cpu.a == 0x32);

    // ADC A, (IX+N)
    cpu.pc = 0; // Reset program counter
    cpu.ix = 0x1000;
    cpu.a = 0x10;
    cpu.f.bits.c = 1; // Set carry flag
    ram[0x1005] = 0x22; // Memory at IX + 5
    ram[0] = @intFromEnum(z80.opcodes.IX);
    ram[1] = @intFromEnum(z80.ix_opcodes.ADC_A__IXD_); // ADD A, (IX+N)
    ram[2] = 0x05; // N (5 offset
    cpu.execute_instruction(ram[0..], true);
    try std.testing.expect(cpu.a == 0x33);

    // SUB A, (IX+N)
    cpu.pc = 0; // Reset program counter
    cpu.ix = 0x1000;
    cpu.a = 0x10;
    ram[0x1005] = 0x02; // Memory at IX + 5
    ram[0] = @intFromEnum(z80.opcodes.IX);
    ram[1] = @intFromEnum(z80.ix_opcodes.SUB__IXD_); // SUB A, (IX+N)
    ram[2] = 0x05; // N (5 offset
    cpu.execute_instruction(ram[0..], true);
    try std.testing.expect(cpu.a == 0x0E);

    // POP IX    cpu.pc = 0; // Reset program counter
    cpu.sp = 0x3000;
    ram[0x3000] = 0x34; // Low byte
    ram[0x3001] = 0x12; // High byte
    cpu.pc = 0; // Reset program counter
    ram[0] = @intFromEnum(z80.opcodes.IX);
    ram[1] = @intFromEnum(z80.ix_opcodes.POP_IX); // POP IX
    cpu.execute_instruction(ram[0..], true);
    try std.testing.expect(cpu.ix == 0x1234);
    try std.testing.expect(cpu.sp == 0x3002);

    // PUSH IX
    cpu.pc = 0; // Reset program counter
    cpu.ix = 0x5678;
    cpu.sp = 0x4000;
    ram[0] = @intFromEnum(z80.opcodes.IX);
    ram[1] = @intFromEnum(z80.ix_opcodes.PUSH_IX); // PUSH IX
    cpu.execute_instruction(ram[0..], true);
    try std.testing.expect(cpu.sp == 0x3FFE);

    try std.testing.expect(ram[0x3FFF] == 0x56);
    try std.testing.expect(ram[0x3FFE] == 0x78);

    // EX (SP), IX
    cpu.pc = 0; // Reset program counter
    cpu.ix = 0xAAAA;
    cpu.sp = 0x5000;
    ram[0x5000] = 0x34; // Low byte
    ram[0x5001] = 0x12; // High byte
    ram[0] = @intFromEnum(z80.opcodes.IX);
    ram[1] = @intFromEnum(z80.ix_opcodes.EX__SP_IX); // EX (SP), IX
    cpu.execute_instruction(ram[0..], true);
    try std.testing.expect(cpu.ix == 0x1234);
    try std.testing.expect(ram[0x5000] == 0xAA);
    try std.testing.expect(ram[0x5001] == 0xAA);

    // JP (IX)
    cpu.pc = 0; // Reset program counter
    cpu.ix = 0x2345;
    ram[0] = @intFromEnum(z80.opcodes.IX);
    ram[1] = @intFromEnum(z80.ix_opcodes.JP_IX); // JP (IX)
    cpu.execute_instruction(ram[0..], true);
    try std.testing.expect(cpu.pc == 0x2345);

    // LD SP, IX
    cpu.pc = 0; // Reset program counter
    cpu.ix = 0x3456;
    cpu.sp = 0x0000;
    ram[0] = @intFromEnum(z80.opcodes.IX);
    ram[1] = @intFromEnum(z80.ix_opcodes.LD_SP_IX); // LD SP, IX
    cpu.execute_instruction(ram[0..], true);
    try std.testing.expect(cpu.sp == 0x3456);
}

test "IY" {
    var in: [256]u8 = undefined;
    var out: [256]u8 = undefined;
    var cpu = z80{ .ports_in = in[0..], .ports_out = out[0..] };
    var ram: [0x6000]u8 = undefined;

    // INC_B: undocumented
    cpu.pc = 0; // Reset program counter
    cpu.b = 0x12;
    ram[0] = @intFromEnum(z80.opcodes.IY);
    ram[1] = @intFromEnum(z80.iy_opcodes.INC_B); // INC B
    cpu.execute_instruction(ram[0..], true);
    try std.testing.expect(cpu.b == 0x13);
    // DEC_C: undocumented
    cpu.pc = 0; // Reset program counter
    cpu.c = 0x34;
    ram[0] = @intFromEnum(z80.opcodes.IY);
    ram[1] = @intFromEnum(z80.iy_opcodes.DEC_C); // DEC C
    cpu.execute_instruction(ram[0..], true);
    try std.testing.expect(cpu.c == 0x33);
    // LD B, N: undocumented
    cpu.pc = 0; // Reset program counter
    ram[0] = @intFromEnum(z80.opcodes.IY);
    ram[1] = @intFromEnum(z80.iy_opcodes.LD_B_N); // LD B, N
    ram[2] = 0x56; // N
    cpu.execute_instruction(ram[0..], true);
    try std.testing.expect(cpu.b == 0x56);

    // INC_IYH
    cpu.pc = 0; // Reset program counter
    cpu.iy = 0x2000;
    ram[0] = @intFromEnum(z80.opcodes.IY);
    ram[1] = @intFromEnum(z80.iy_opcodes.INC_IYH); // INC (IY)
    cpu.execute_instruction(ram[0..], true);
    try std.testing.expect(cpu.iy == 0x2100);

    // DEC_IYL
    cpu.pc = 0; // Reset program counter
    cpu.iy = 0x2100;
    ram[0] = @intFromEnum(z80.opcodes.IY);
    ram[1] = @intFromEnum(z80.iy_opcodes.DEC_IYL); // DEC (IY)
    cpu.execute_instruction(ram[0..], true);
    std.debug.print("IY after DEC_IYL: {x:04}\n", .{cpu.iy});

    try std.testing.expect(cpu.iy == 0x21FF);

    // LD IY, NN
    cpu.pc = 0; // Reset program counter
    ram[0] = @intFromEnum(z80.opcodes.IY);
    ram[1] = @intFromEnum(z80.iy_opcodes.LD_IY_NN); // LD IY, NN
    ram[2] = 0x34; // Low byte
    ram[3] = 0x12; // High byte
    cpu.execute_instruction(ram[0..], true);
    try std.testing.expect(cpu.iy == 0x1234);

    // ADD IY, BC
    cpu.pc = 0; // Reset program counter
    cpu.iy = 0x1000;
    cpu.set_register_pair(.BC, 0x0100);
    ram[0] = @intFromEnum(z80.opcodes.IY);
    ram[1] = @intFromEnum(z80.iy_opcodes.ADD_IY_BC); // ADD IY, BC
    cpu.execute_instruction(ram[0..], true);
    try std.testing.expect(cpu.iy == 0x1100);

    // LD (NN), IY
    cpu.pc = 0; // Reset program counter
    cpu.iy = 0x5678;
    ram[0] = @intFromEnum(z80.opcodes.IY);
    ram[1] = @intFromEnum(z80.iy_opcodes.LD__NN__IY); // LD (NN), IY
    ram[2] = 0x00; // Low byte
    ram[3] = 0x20; // High byte
    cpu.execute_instruction(ram[0..], true);
    try std.testing.expect(ram[0x2000] == 0x78);
    try std.testing.expect(ram[0x2001] == 0x56);

    // LD (IYD), N
    cpu.pc = 0; // Reset program counter
    cpu.iy = 0x0000;
    ram[0] = @intFromEnum(z80.opcodes.IY);
    ram[1] = @intFromEnum(z80.iy_opcodes.LD__IYD__N); // LD IYD, N
    ram[2] = 0x7F; // D
    ram[3] = 0xBB; // N
    cpu.execute_instruction(ram[0..], true);
    std.debug.print("IY after LD IYD, N: {x:04}\n", .{cpu.iy});
    try std.testing.expect(ram[0x007F] == 0xBB);

    // LD B, (IY+N)
    cpu.pc = 0; // Reset program counter
    cpu.iy = 0x1000;
    ram[0x1005] = 0xAB; // Memory at IY + 5
    ram[0] = @intFromEnum(z80.opcodes.IY);
    ram[1] = @intFromEnum(z80.iy_opcodes.LD_B__IYD_); // LD B, (IY+N)
    ram[2] = 0x05; // N (5 offset
    cpu.execute_instruction(ram[0..], true);
    try std.testing.expect(cpu.b == 0xAB);

    // LD (IY+D), B
    cpu.pc = 0; // Reset program counter
    cpu.iy = 0x1000;
    cpu.b = 0xCD; // B
    ram[0] = @intFromEnum(z80.opcodes.IY);
    ram[1] = @intFromEnum(z80.iy_opcodes.LD__IYD__B); // LD (IY+N), B
    ram[2] = 0x05; // D (5 offset
    cpu.execute_instruction(ram[0..], true);
    std.debug.print("Memory at 0x1005: {x:02}\n", .{ram[0x1005]});
    try std.testing.expect(ram[0x1005] == 0xCD);

    // LD (IY+D), C
    cpu.pc = 0; // Reset program counter
    cpu.iy = 0x1000;
    cpu.b = 0xCE; // B
    ram[0] = @intFromEnum(z80.opcodes.IY);
    ram[1] = @intFromEnum(z80.iy_opcodes.LD__IYD__C); // LD (IY+N), B
    ram[2] = 0xFF; // D (-1 offset)
    cpu.execute_instruction(ram[0..], true);
    std.debug.print("Memory at 0x1005: {x:02}\n", .{ram[0x0FFF]});
    try std.testing.expect(ram[0x0FFF] == 0xCE);

    // INC (IY+N)
    cpu.pc = 0; // Reset program counter
    cpu.iy = 0x2000;
    ram[0x200A] = 0x7F; // Memory at IY + 10
    ram[0] = @intFromEnum(z80.opcodes.IY);
    ram[1] = @intFromEnum(z80.iy_opcodes.INC__IYD_); // INC (IY+N)
    ram[2] = 0x0A; // N (10 offset
    cpu.execute_instruction(ram[0..], true);
    try std.testing.expect(ram[0x200A] == 0x80);
    try std.testing.expect(cpu.f.bits.s == 1); // Sign flag should be set
    try std.testing.expect(cpu.f.bits.z == 0); // Zero flag should be cleared
    try std.testing.expect(cpu.f.bits.h == 1); // Half-carry flag should be set
    try std.testing.expect(cpu.f.bits.pv == 1); // Parity/Overflow flag should be set
    try std.testing.expect(cpu.f.bits.n == 0);

    // DEC (IY+N)
    cpu.pc = 0; // Reset program counter
    cpu.iy = 0x2000;
    ram[0x200A] = 0x01; // Memory at IY + 10
    ram[0] = @intFromEnum(z80.opcodes.IY);
    ram[1] = @intFromEnum(z80.iy_opcodes.DEC__IYD_); // DEC (IY+N)
    ram[2] = 0x0A; // N (10 offset
    cpu.execute_instruction(ram[0..], true);
    try std.testing.expect(ram[0x200A] == 0x00);
    try std.testing.expect(cpu.f.bits.s == 0); // Sign flag should be cleared
    try std.testing.expect(cpu.f.bits.z == 1); // Zero flag should be set
    try std.testing.expect(cpu.f.bits.h == 0); // Half-carry flag should be set
    try std.testing.expect(cpu.f.bits.pv == 0); // Parity/Overflow flag should be cleared
    try std.testing.expect(cpu.f.bits.n == 1);

    // ADD A, (IY+N)
    cpu.pc = 0; // Reset program counter
    cpu.iy = 0x1000;
    cpu.a = 0x10;
    ram[0x1005] = 0x22; // Memory at IY + 5
    ram[0] = @intFromEnum(z80.opcodes.IY);
    ram[1] = @intFromEnum(z80.iy_opcodes.ADD_A__IYD_); // ADD A, (IY+N)
    ram[2] = 0x05; // N (5 offset
    cpu.execute_instruction(ram[0..], true);
    try std.testing.expect(cpu.a == 0x32);

    // ADC A, (IY+N)
    cpu.pc = 0; // Reset program counter
    cpu.iy = 0x1000;
    cpu.a = 0x10;
    cpu.f.bits.c = 1; // Set carry flag
    ram[0x1005] = 0x22; // Memory at IY + 5
    ram[0] = @intFromEnum(z80.opcodes.IY);
    ram[1] = @intFromEnum(z80.iy_opcodes.ADC_A__IYD_); // ADD A, (IY+N)
    ram[2] = 0x05; // N (5 offset
    cpu.execute_instruction(ram[0..], true);
    try std.testing.expect(cpu.a == 0x33);

    // SUB A, (IY+N)
    cpu.pc = 0; // Reset program counter
    cpu.iy = 0x1000;
    cpu.a = 0x10;
    ram[0x1005] = 0x02; // Memory at IY + 5
    ram[0] = @intFromEnum(z80.opcodes.IY);
    ram[1] = @intFromEnum(z80.iy_opcodes.SUB__IYD_); // SUB A, (IY+N)
    ram[2] = 0x05; // N (5 offset
    cpu.execute_instruction(ram[0..], true);
    try std.testing.expect(cpu.a == 0x0E);

    // POP IY    cpu.pc = 0; // Reset program counter
    cpu.sp = 0x3000;
    ram[0x3000] = 0x34; // Low byte
    ram[0x3001] = 0x12; // High byte
    cpu.pc = 0; // Reset program counter
    ram[0] = @intFromEnum(z80.opcodes.IY);
    ram[1] = @intFromEnum(z80.iy_opcodes.POP_IY); // POP IY
    cpu.execute_instruction(ram[0..], true);
    try std.testing.expect(cpu.iy == 0x1234);
    try std.testing.expect(cpu.sp == 0x3002);

    // PUSH IY
    cpu.pc = 0; // Reset program counter
    cpu.iy = 0x5678;
    cpu.sp = 0x4000;
    ram[0] = @intFromEnum(z80.opcodes.IY);
    ram[1] = @intFromEnum(z80.iy_opcodes.PUSH_IY); // PUSH IY
    cpu.execute_instruction(ram[0..], true);
    try std.testing.expect(cpu.sp == 0x3FFE);

    try std.testing.expect(ram[0x3FFF] == 0x56);
    try std.testing.expect(ram[0x3FFE] == 0x78);

    // EX (SP), IY
    cpu.pc = 0; // Reset program counter
    cpu.iy = 0xAAAA;
    cpu.sp = 0x5000;
    ram[0x5000] = 0x34; // Low byte
    ram[0x5001] = 0x12; // High byte
    ram[0] = @intFromEnum(z80.opcodes.IY);
    ram[1] = @intFromEnum(z80.iy_opcodes.EX__SP_IY); // EX (SP), IY
    cpu.execute_instruction(ram[0..], true);
    try std.testing.expect(cpu.iy == 0x1234);
    try std.testing.expect(ram[0x5000] == 0xAA);
    try std.testing.expect(ram[0x5001] == 0xAA);

    // JP (IY)
    cpu.pc = 0; // Reset program counter
    cpu.iy = 0x2345;
    ram[0] = @intFromEnum(z80.opcodes.IY);
    ram[1] = @intFromEnum(z80.iy_opcodes.JP_IY); // JP (IY)
    cpu.execute_instruction(ram[0..], true);
    try std.testing.expect(cpu.pc == 0x2345);

    // LD SP, IY
    cpu.pc = 0; // Reset program counter
    cpu.iy = 0x3456;
    cpu.sp = 0x0000;
    ram[0] = @intFromEnum(z80.opcodes.IY);
    ram[1] = @intFromEnum(z80.iy_opcodes.LD_SP_IY); // LD SP, IY
    cpu.execute_instruction(ram[0..], true);
    try std.testing.expect(cpu.sp == 0x3456);
}

test "IX bit" {
    var in: [256]u8 = undefined;
    var out: [256]u8 = undefined;
    var cpu = z80{ .ports_in = in[0..], .ports_out = out[0..] };
    var ram: [0x6000]u8 = undefined;

    // RLC (IX+D), B
    cpu.pc = 0; // Reset program counter
    cpu.ix = 0x1000;
    ram[0x1005] = 0b10000001; // Memory at IX + 5
    ram[0] = @intFromEnum(z80.opcodes.IX);
    ram[1] = @intFromEnum(z80.ix_opcodes.IX_BIT);
    ram[2] = 0x05; // D (5 offset
    ram[3] = @intFromEnum(z80.ix_bit_opcodes.RLC__IXD_B);

    cpu.execute_instruction(ram[0..], true);
    try std.testing.expect(cpu.b == 0b00000011);
    try std.testing.expect(cpu.f.bits.z == 0); // Zero flag should be cleared
    try std.testing.expect(cpu.f.bits.c == 1); // Carry flag should be set
    try std.testing.expect(cpu.f.bits.h == 0); // Half-carry flag should be set
    try std.testing.expect(cpu.f.bits.n == 0); // N flag should

    // RLC (IX+D)
    cpu.pc = 0; // Reset program counter
    cpu.ix = 0x1000;
    ram[0x1005] = 0b10000001; // Memory at IX + 5
    ram[0] = @intFromEnum(z80.opcodes.IX);
    ram[1] = @intFromEnum(z80.ix_opcodes.IX_BIT);
    ram[2] = 0x05; // D (5 offset
    ram[3] = @intFromEnum(z80.ix_bit_opcodes.RLC__IXD_);

    cpu.execute_instruction(ram[0..], true);
    try std.testing.expect(ram[0x1005] == 0b00000011);

    // RRC (IX+D), C
    cpu.pc = 0; // Reset program counter
    cpu.ix = 0x1000;
    ram[0x1005] = 0b00000011; // Memory at IX + 5
    cpu.c = 0x00;
    ram[0] = @intFromEnum(z80.opcodes.IX);
    ram[1] = @intFromEnum(z80.ix_opcodes.IX_BIT);
    ram[2] = 0x05; // D (5 offset
    ram[3] = @intFromEnum(z80.ix_bit_opcodes.RRC__IXD_C);
    cpu.execute_instruction(ram[0..], true);
    try std.testing.expect(cpu.c == 0b10000001);
    try std.testing.expect(ram[0x1005] == 0b10000001);
    try std.testing.expect(cpu.f.bits.c == 1); // Carry flag should be set

    // RL (IX+D), D
    cpu.pc = 0; // Reset program counter
    cpu.ix = 0x1000;
    ram[0x1005] = 0b10000001; // Memory at IX + 5
    cpu.d = 0x00;
    cpu.f.bits.c = 1; // Set carry flag
    ram[0] = @intFromEnum(z80.opcodes.IX);
    ram[1] = @intFromEnum(z80.ix_opcodes.IX_BIT);
    ram[2] = 0x05; // D (5 offset
    ram[3] = @intFromEnum(z80.ix_bit_opcodes.RL__IXD_D);
    cpu.execute_instruction(ram[0..], true);
    try std.testing.expect(cpu.d == 0b00000011);
    try std.testing.expect(cpu.f.bits.c == 1); // Carry flag should be set

    // RL (IX+D), D
    cpu.pc = 0; // Reset program counter
    cpu.ix = 0x1000;
    ram[0x1005] = 0b10000001; // Memory at IX + 5
    cpu.d = 0x00;
    cpu.f.bits.c = 0; // Set carry flag
    ram[0] = @intFromEnum(z80.opcodes.IX);
    ram[1] = @intFromEnum(z80.ix_opcodes.IX_BIT);
    ram[2] = 0x05; // D (5 offset
    ram[3] = @intFromEnum(z80.ix_bit_opcodes.RL__IXD_D);
    cpu.execute_instruction(ram[0..], true);
    try std.testing.expect(cpu.d == 0b00000010);
    try std.testing.expect(cpu.f.bits.c == 1); // Carry flag should be set

    // SLA (IX+D), D
    cpu.pc = 0; // Reset program counter
    cpu.ix = 0x1000;
    ram[0x1005] = 0b10000001; // Memory at IX + 5
    cpu.d = 0x00;
    cpu.f.bits.c = 0; // Set carry flag
    ram[0] = @intFromEnum(z80.opcodes.IX);
    ram[1] = @intFromEnum(z80.ix_opcodes.IX_BIT);
    ram[2] = 0x05; // D (5 offset
    ram[3] = @intFromEnum(z80.ix_bit_opcodes.SLA__IXD_D);
    cpu.execute_instruction(ram[0..], true);
    try std.testing.expect(cpu.d == 0b00000010);
    try std.testing.expect(cpu.f.bits.c == 1); // Carry flag should be set
    try std.testing.expect(ram[0x1005] == 0b00000010);

    // SRA (IX+D), D
    cpu.pc = 0; // Reset program counter
    cpu.ix = 0x1000;
    ram[0x1005] = 0b10000001; // Memory at IX + 5
    cpu.d = 0x00;
    cpu.f.bits.c = 0; // Set carry flag
    ram[0] = @intFromEnum(z80.opcodes.IX);
    ram[1] = @intFromEnum(z80.ix_opcodes.IX_BIT);
    ram[2] = 0x05; // D (5 offset
    ram[3] = @intFromEnum(z80.ix_bit_opcodes.SRA__IXD_D);
    cpu.execute_instruction(ram[0..], true);
    try std.testing.expect(cpu.d == 0b11000000);
    try std.testing.expect(cpu.f.bits.c == 1); // Carry flag should be set
    try std.testing.expect(ram[0x1005] == 0b11000000);

    // SRL (IX+D), D
    cpu.pc = 0; // Reset program counter
    cpu.ix = 0x1000;
    ram[0x1005] = 0b10000001; // Memory at IX + 5
    cpu.d = 0x00;
    cpu.f.bits.c = 0; // Set carry flag
    ram[0] = @intFromEnum(z80.opcodes.IX);
    ram[1] = @intFromEnum(z80.ix_opcodes.IX_BIT);
    ram[2] = 0x05; // D (5 offset
    ram[3] = @intFromEnum(z80.ix_bit_opcodes.SRL__IXD_D);
    cpu.execute_instruction(ram[0..], true);
    try std.testing.expect(cpu.d == 0b01000000);
    try std.testing.expect(cpu.f.bits.c == 1); // Carry flag should be set
    try std.testing.expect(ram[0x1005] == 0b01000000);

    // SLL (IX+D), D (undocumented)
    cpu.pc = 0; // Reset program counter
    cpu.ix = 0x1000;
    ram[0x1005] = 0b10000001; // Memory at IX + 5
    cpu.d = 0x00;
    cpu.f.bits.c = 0; // Set carry flag
    ram[0] = @intFromEnum(z80.opcodes.IX);
    ram[1] = @intFromEnum(z80.ix_opcodes.IX_BIT);
    ram[2] = 0x05; // D (5 offset
    ram[3] = @intFromEnum(z80.ix_bit_opcodes.SLL__IXD_D);
    cpu.execute_instruction(ram[0..], true);
    try std.testing.expect(cpu.d == 0b00000011);
    try std.testing.expect(cpu.f.bits.c == 1); // Carry flag should be set
    try std.testing.expect(ram[0x1005] == 0b00000011);

    // BIT 7, (IX+D)
    cpu.pc = 0; // Reset program counter
    cpu.ix = 0x1000;
    ram[0x1005] = 0b10000001; // Memory at IX + 5
    ram[0] = @intFromEnum(z80.opcodes.IX);
    ram[1] = @intFromEnum(z80.ix_opcodes.IX_BIT);
    ram[2] = 0x05; // D (5 offset
    ram[3] = @intFromEnum(z80.ix_bit_opcodes.BIT7__IXD8);
    cpu.f.bits.z = 0; // Clear zero flag
    cpu.f.bits.h = 0; // Clear half-carry flag
    cpu.f.bits.n = 1; // Set N flag
    cpu.execute_instruction(ram[0..], true);
    try std.testing.expect(cpu.f.bits.z == 0); // Zero flag should be cleared
    try std.testing.expect(cpu.f.bits.h == 1); // Half-carry flag should be set
    try std.testing.expect(cpu.f.bits.n == 0); // N flag should

    // RES 0, (IX+D), d
    cpu.pc = 0; // Reset program counter
    cpu.ix = 0x1000;
    ram[0x1005] = 0b10000001; // Memory at IX + 5
    cpu.d = 0x00;
    ram[0] = @intFromEnum(z80.opcodes.IX);
    ram[1] = @intFromEnum(z80.ix_opcodes.IX_BIT);
    ram[2] = 0x05; // D (5 offset
    ram[3] = @intFromEnum(z80.ix_bit_opcodes.RES0__IXD_D);
    cpu.execute_instruction(ram[0..], true);
    try std.testing.expect(cpu.d == 0b10000000);
    try std.testing.expect(ram[0x1005] == 0b10000000);

    // SET 1, (IX+D), d
    cpu.pc = 0; // Reset program counter
    cpu.ix = 0x1000;
    ram[0x1005] = 0b10000000; // Memory at IX + 5
    cpu.d = 0x00;
    ram[0] = @intFromEnum(z80.opcodes.IX);
    ram[1] = @intFromEnum(z80.ix_opcodes.IX_BIT);
    ram[2] = 0x05; // D (5 offset
    ram[3] = @intFromEnum(z80.ix_bit_opcodes.SET1__IXD_D);
    cpu.execute_instruction(ram[0..], true);
    try std.testing.expect(cpu.d == 0b10000010);
    try std.testing.expect(ram[0x1005] == 0b10000010);

    // BIT 1, (IY+D)
    cpu.pc = 0; // Reset program counter
    cpu.iy = 0x1000;
    ram[0x1005] = 0b10000010; // Memory at IY + 5
    ram[0] = @intFromEnum(z80.opcodes.IY);
    ram[1] = @intFromEnum(z80.iy_opcodes.IY_BIT);
    ram[2] = 0x05; // D (5 offset
    ram[3] = @intFromEnum(z80.ix_bit_opcodes.BIT1__IXD8);
    cpu.f.bits.z = 0; // Clear zero flag
    cpu.f.bits.h = 0; // Clear half-carry flag
    cpu.f.bits.n = 1; // Set N flag
    cpu.execute_instruction(ram[0..], true);
    try std.testing.expect(cpu.f.bits.z == 0); // Zero flag should be cleared
    try std.testing.expect(cpu.f.bits.h == 1); // Half-carry flag should be set
    try std.testing.expect(cpu.f.bits.n == 0); // N flag should
}
