/* BraAArch64.c -- Converter for AArch64 code (BCJ)
 2026 : Adapted from Bra86.c (Igor Pavlov, Public domain) for AArch64
 *
 * Converts relative branch offsets in B/BL instructions to absolute addresses
 * and back, improving compression of AArch64 executables.
 *
 * AArch64 B/BL instruction encoding:
 *   bits [31:26] = 000101 (B) or 100101 (BL)
 *   bits [25:0]  = imm26 (signed, in units of 4 bytes)
 *   Branch target = PC + sign_extend(imm26) * 4
 */

#include "Bra.h"

SizeT AArch64_Convert(Byte *data, SizeT size, UInt32 ip, int encoding)
{
  SizeT bufferPos = 0;

  if (size < 4)
    return 0;

  for (;;)
  {
    Byte *p = data + bufferPos;
    Byte *limit = data + size - 3;
    for (; p < limit; p += 4)
    {
      UInt32 instr = (UInt32)p[0] | ((UInt32)p[1] << 8) |
                     ((UInt32)p[2] << 16) | ((UInt32)p[3] << 24);
      if ((instr & 0x7C000000) == 0x14000000)
        break;
    }
    bufferPos = (SizeT)(p - data);
    if (p >= limit)
      break;

    UInt32 instr = (UInt32)p[0] | ((UInt32)p[1] << 8) |
                   ((UInt32)p[2] << 16) | ((UInt32)p[3] << 24);

    UInt32 imm26 = instr & 0x03FFFFFF;
    UInt32 ipWords = (ip + (UInt32)bufferPos) / 4;

    UInt32 newImm26;
    if (encoding)
      newImm26 = (imm26 + ipWords) & 0x03FFFFFF;
    else
      newImm26 = (imm26 - ipWords + 0x04000000) & 0x03FFFFFF;

    UInt32 newInstr = (instr & 0xFC000000) | newImm26;

    p[0] = (Byte)(newInstr);
    p[1] = (Byte)(newInstr >> 8);
    p[2] = (Byte)(newInstr >> 16);
    p[3] = (Byte)(newInstr >> 24);

    bufferPos += 4;
  }

  return bufferPos;
}
