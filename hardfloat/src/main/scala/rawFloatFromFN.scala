
/*============================================================================

This Chisel source file is part of a pre-release version of the HardFloat IEEE
Floating-Point Arithmetic Package, by John R. Hauser (with some contributions
from Yunsup Lee and Andrew Waterman, mainly concerning testing).

Copyright 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017 The Regents of the
University of California.  All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 1. Redistributions of source code must retain the above copyright notice,
    this list of conditions, and the following disclaimer.

 2. Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions, and the following disclaimer in the documentation
    and/or other materials provided with the distribution.

 3. Neither the name of the University nor the names of its contributors may
    be used to endorse or promote products derived from this software without
    specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS "AS IS", AND ANY
EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE, ARE
DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=============================================================================*/

package hardfloat

import Chisel._

class my_rawFloatFromFN(expWidth: Int, sigWidth: Int) extends chisel3.RawModule
{
	val io = IO(new Bundle {
		val in = Bits(INPUT, 32)
		val out = Output(new RawFloat(expWidth, sigWidth))
		val isZero = Bool(OUTPUT)
		val sign = Bits(OUTPUT, expWidth + sigWidth - 1)
	})
        val sign = io.in(expWidth + sigWidth - 1)
        val expIn = io.in(expWidth + sigWidth - 2, sigWidth - 1)
        val fractIn = io.in(sigWidth - 2, 0)

        val isZeroExpIn = (expIn === UInt(0))
        val isZeroFractIn = (fractIn === UInt(0))

        val normDist = countLeadingZeros(fractIn)
        val subnormFract = (fractIn<<normDist)(sigWidth - 3, 0)<<1
        val adjustedExp =
            Mux(isZeroExpIn,
                normDist ^ UInt((BigInt(1)<<(expWidth + 1)) - 1),
                expIn
            ) + (UInt(BigInt(1)<<(expWidth - 1))
                     | Mux(isZeroExpIn, UInt(2), UInt(1)))

        val isZero = isZeroExpIn && isZeroFractIn
        val isSpecial = (adjustedExp(expWidth, expWidth - 1) === UInt(3))

        io.out.isNaN  := isSpecial && ! isZeroFractIn
        io.out.isInf  := isSpecial &&   isZeroFractIn
        io.out.isZero := isZero
        io.out.sign   := sign
        io.out.sExp   := adjustedExp(expWidth, 0).zext
        io.out.sig :=
            Cat(UInt(0, 1), ! isZero, Mux(isZeroExpIn, subnormFract, fractIn))

	io.isZero := isZero
	io.sign := sign
}

object rawFloatFromFN
{
    def apply(expWidth: Int, sigWidth: Int, in: Bits) =
    {
        val sign = in(expWidth + sigWidth - 1)
        val expIn = in(expWidth + sigWidth - 2, sigWidth - 1)
        val fractIn = in(sigWidth - 2, 0)

        val isZeroExpIn = (expIn === UInt(0))
        val isZeroFractIn = (fractIn === UInt(0))

        val normDist = countLeadingZeros(fractIn)
        val subnormFract = (fractIn<<normDist)(sigWidth - 3, 0)<<1
        val adjustedExp =
            Mux(isZeroExpIn,
                normDist ^ UInt((BigInt(1)<<(expWidth + 1)) - 1),
                expIn
            ) + (UInt(BigInt(1)<<(expWidth - 1))
                     | Mux(isZeroExpIn, UInt(2), UInt(1)))

        val isZero = isZeroExpIn && isZeroFractIn
        val isSpecial = (adjustedExp(expWidth, expWidth - 1) === UInt(3))

        val out = Wire(new RawFloat(expWidth, sigWidth))
        out.isNaN  := isSpecial && ! isZeroFractIn
        out.isInf  := isSpecial &&   isZeroFractIn
        out.isZero := isZero
        out.sign   := sign
        out.sExp   := adjustedExp(expWidth, 0).zext
        out.sig :=
            Cat(UInt(0, 1), ! isZero, Mux(isZeroExpIn, subnormFract, fractIn))
        out
    }
}

