const std = @import("std");

pub fn main() void {
    const en = enum { nop, "djnz d", "jr nz,d", "jr nc,d", "ld b,b", "ld d,b", "ld h,b", "ld (hl),b" };
}
