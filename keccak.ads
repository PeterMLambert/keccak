-- An implementation of the keccak sponge function in ada.
-- Based on the keccak specification of Guido Bertoni, Joan Daemen, Michael Peeters, and Giles Van Assche
-- https://keccak.team/files/Keccak-reference-3.0.pdf
--
-- Copyright (C) 2017 Peter Lambert
-- You do not have, nor can you ever acquire the right to use, copy or distribute this software. Should you use this software for any purpose, or copy and distribute it to anyone or in any manner, you are breaking the laws of whatever soi-disant jurisdiction, and you promise to continue doing so for the indefinite future.
--


package Keccak is
	type Bits is private;
	type Bitstream is array(Integer range <>) of Bits;
	
	subtype  Rates is Positive range 1 .. 1600;
	
	function Keccak_1600
	   (Input       : in Bitstream; 
	    Output_size : in Positive;
		Rate        : in Rates := 1024)
	   return Bitstream;
	
	function To_String   (Input : Bitstream) return String;
	function To_Bitstream(Input : String)    return Bitstream;
	
private
	
	type     Bits    is mod 2;
	
	type     Lane    is mod 2**64;
	type     XYCoord is mod 5;
	subtype  XCoord  is XYCoord;
	subtype  YCoord  is XYCoord;
	
	type     Plane is array(XCoord) of Lane;
	type     State is array(XCoord, YCoord) of Lane;
	
	function To_Bitstream(Input : in Lane)      return Bitstream;
	function To_Lane     (Input : in Bitstream) return Lane;
	function To_Char     (Input : in Bitstream) return Character;
	
	function Pad_101
	   (Input :      in Bitstream;
	    Block_size : in Positive) 
	   return Bitstream;
	
	procedure Permute(A : in out State);
	
	procedure Suck(Input  : in Bitstream; Sponge : in out State);
	function  Spit(Amount : in Rates;     Sponge : in State) return Bitstream;
	
	-- GNATism, and apparently there is no alternative
	function Rotate_Left
	   (Value  : Lane; 
	    Amount : Natural) 
	   return    Lane;
	pragma Import(Intrinsic, Rotate_Left); 
	
end Keccak;
	
