--
-- Copyright (C) 2017 Peter Lambert
-- You do not have, nor can you ever acquire the right to use, copy or distribute this software. Should you use this software for any purpose, or copy and distribute it to anyone or in any manner, you are breaking the laws of whatever soi-disant jurisdiction, and you promise to continue doing so for the indefinite future.
--

package body Keccak is
	
	-- Some functions for converting between types
	function To_Bitstream(Input : in Lane) return Bitstream is
		Result : Bitstream(1 .. 64) := (Others => 0);
	begin
		for K in 0 .. 63 loop
			Result(64 - K) := Bits((Input / 2**K) mod 2);
		end loop;
		return Result;
	end To_Bitstream;
	
	
	function To_Lane(Input : in Bitstream) return Lane is
		Result : Lane := 0;
	begin
		if Input'Length /= 64 then
			raise Constraint_Error with "A Lane should be made from 64 bits";
		end if;
		for K in 0 .. 63 loop
			Result := Result + Lane(Input(Input'First + K)) * 2**(63 - K);
		end loop;
		return Result;
	end To_Lane;
	
	
	function To_Char(Input : in Bitstream) return Character is
		X : Integer := 0;
	begin
		if Input'Length /= 8 then
			raise Constraint_Error with "A Character should be made from 8 bits";
		end if;
		for I in 0 .. 7 loop
			X := X + Integer(Input(Input'Last - I)) * 2**I;
		end loop;
		return Character'Val(X);
	end;
	
	
	function To_String(Input : Bitstream) return String is
		Output      : String(1 .. Input'Length / 8 + 1);
		Output_Size : Natural;
		First_Size  : Natural   := Input'Length mod 8;
		Offset      : Natural   := 8 - First_Size;
		Temp : Bitstream(1 .. 8);
		BS   : Bitstream(1 .. Input'Length + 8) := (others => 0);
		Start_Bit : Natural;
	begin
		if First_Size > 0 then
			Output_Size := Input'Length / 8 + 1;
			Start_Bit   := 1;
		else
			Output_Size := Input'Length / 8;
			Start_Bit   := 9;
		end if;
		
		BS(1 + Offset .. BS'Last) := Input;
		
		for I in 0 .. Output_Size - 1 loop
			Temp := BS(Start_Bit + I * 8 .. Start_Bit + 7 + I * 8);
			Output(1 + I) := To_Char(Temp);
		end loop;
		
		return Output(1 .. Output_Size);
	end To_String;
	
	
	function To_Bitstream(Input : String) return Bitstream is
		Result : Bitstream(1 .. Input'Length * 8);
		Temp   : Bitstream(1 .. 8);
	begin
		for I in 0 .. Input'Length - 1 loop
			for J in 0 .. 7 loop
				Temp(8 - J) := Bits((Character'Pos(Input(Input'First + I)) / 2**J) mod 2);
			end loop;
			Result(1 + 8 * I .. 8 + 8 * I) := Temp;
		end loop;
		return Result;
	end To_Bitstream;
	
	--------------------------------------------------------------------
	-- These are the permutations used in the Keccak sponge function  --
	--------------------------------------------------------------------
	
	-- Round constants for the iota function
	type Round_Constants is array(Natural range 1 .. 24) of Lane;
	RC : constant Round_Constants :=
		(
		 16#0000_0000_0000_0001#,
		 16#0000_0000_0000_8082#,
		 16#8000_0000_0000_808A#,
		 16#8000_0000_8000_8000#,
		 16#0000_0000_0000_808B#,
		 16#0000_0000_8000_0001#,
		 16#8000_0000_8000_8081#,
		 16#8000_0000_0000_8009#,
		 16#0000_0000_0000_008A#,
		 16#0000_0000_0000_0088#,
		 16#0000_0000_8000_8009#,
		 16#0000_0000_8000_000A#,
		 16#0000_0000_8000_808B#,
		 16#8000_0000_0000_008B#,
		 16#8000_0000_0000_8089#,
		 16#8000_0000_0000_8003#,
		 16#8000_0000_0000_8002#,
		 16#8000_0000_0000_0080#,
		 16#0000_0000_0000_800A#,
		 16#8000_0000_8000_000A#,
		 16#8000_0000_8000_8081#,
		 16#8000_0000_0000_8080#,
		 16#0000_0000_8000_0001#,
		 16#8000_0000_8000_8008#
		);
	
	function chi(A : in State) return State is
		Ar : State;
	begin
		for Y in YCoord loop
			for X in XCoord loop
				Ar(X, Y) := A(X, Y) xor (((not A(X + 1, Y)) and A(X + 2, Y)));
			end loop;
		end loop;
		
		return Ar;
	end chi;
	
	function iota(Round_Const : in Lane; A : in State) return State is
		Ar : State;
	begin
		for Y in YCoord loop
			for X in XCoord loop
				Ar(X, Y) := A(X, Y) xor Round_Const;
			end loop;
		end loop;
		return Ar;
	end iota;
	
	function pi(A : in State) return State is
		Ar : State;
	begin
		for X in XCoord loop
			for Y in YCoord loop
				Ar(X, Y) := A(Y, 2 * X + 3 * Y);
			end loop;
		end loop;
		return Ar;
	end pi;	
	
	function rho(A : in State) return State is
		Ar : State;
		X  : XCoord;
		Y  : YCoord;
	begin
		Ar(0, 0) := A(0, 0);
		X := 1;
		Y := 0;
		for T in 0 .. 23 loop
			Ar(X, Y) := Rotate_Left(A(X, Y), ((T + 1) * (T + 2) / 2) mod 64);
			X := Y;
			Y := 2 * X + 3 * Y;
		end loop;
		return Ar;
	end rho;
	
	function theta(A : in State) return State is
		C, D : Plane;
		Ar   : State; 
	begin
		for X in XCoord loop
			C(X) := A(X, 0);
			for Y in XYCoord range 1 .. 4 loop
				C(X) := (C(X) xor A(X, Y));
			end loop;
		end loop;
		
		for X in XCoord loop
			D(X) := C(X - 1) xor Rotate_Left(C(X + 1), 1);
			for Y in YCoord loop
				Ar(X, Y) := A(X, Y) xor D(X);
			end loop;
		end loop;
		return Ar;
	end theta;
	
	-- And this function puts all the permutations together.
	procedure Permute(A : in out State) is
	begin
		for I in RC'Range loop
			A := iota(RC(I), chi(pi(rho(theta(A)))));
		end loop;
	end Permute;
	
	--------------------------------------------------------------------------
	
	-- The padding function 
	function Pad_101(Input : in Bitstream; Block_Size : in Positive) return Bitstream is
		NumZeros : Natural;
	begin
		-- return Input + "10*1"
		NumZeros := Block_Size - (Input'Length + 2) mod Block_Size;
		if NumZeros = Block_Size then
			NumZeros := 0;
		end if;
		
		declare
			Result : Bitstream(1 .. Input'Length + 2 + NumZeros) := (others => 0);
		begin
			Result(1 .. Input'Length) := Input;
			Result(Input'Length + 1)  := 1;
			Result(Result'Last) := 1;
			
			return Result;
		end;
	end Pad_101;
	
	
	-- Suck a bitstream into a state (xor'ing bits)
	procedure Suck(Input : in Bitstream; Sponge : in out State) is
		Buffer   : Bitstream(1 .. 64);
		End_Size : Natural := Input'Length mod 64;
		End_Pos  : Natural := Input'Length /   64;
	begin
		for I in 0 .. End_Pos - 1 loop
			Buffer := Input(Buffer'First + I * 64 .. Buffer'First + (I + 1) * 64 - 1);
			Sponge(XYCoord(I / 5), XYCoord(I)) := Sponge(XYCoord(I / 5), XYCoord(I)) xor To_Lane(Buffer);
		end loop;
		if End_Size /= 0 then
			Buffer := (others => 0);
			Buffer(1 .. End_Size) := Input(Input'Last - End_Size + 1 .. Input'Last);
			Sponge(XYCoord(End_Pos / 5), XYCoord(End_Pos)) := Sponge(XYCoord(End_Pos / 5), XYCoord(End_Pos)) xor To_Lane(Buffer);
		end if;
	end Suck;
	
	-- Get the result of the state out
	function Spit(Amount : in Rates; Sponge : in State) return bitstream is
		Result   : Bitstream(1 .. Amount);
		Buffer   : Bitstream(1 .. 64);
		End_Size : Natural := Amount mod 64;
		End_Pos  : Natural := Amount /   64;
	begin
		for I in 0 .. End_Pos - 1 loop
			Buffer := To_Bitstream(Sponge(XYCoord(I / 5), XYCoord(I)));
			Result(1 + I * 64 .. (I + 1) * 64) := Buffer;
		end loop;
		if End_Size > 0 then
			Buffer := To_bitstream(Sponge(XYCoord(End_Pos / 5), XYCoord(End_Pos)));
			Result(Result'Last - End_Size + 1 .. Result'Last) := Buffer(1 .. End_Size);
		end if;
		return Result;
	end Spit;
	
	--------------------------------------------------------------------------
	-- The Keccak Sponge function
	--------------------------------------------------------------------------
	function Keccak_1600(Input       : in Bitstream; 
	                     Output_size : in Positive;
		                 Rate        : in Rates := 1024
						) return Bitstream is
		Internal_State : State := (Others => (Others => 0));
		Output   : Bitstream(1 .. Output_size) := (Others => 0);
		In_Buff  : Bitstream(1 .. Rate)		   := (Others => 0);
		End_Size : Natural := Output_Size mod Rate;
		Pad_Size : Natural;
		Pad_Mess : Bitstream(1 .. Input'Length + Rate + 1);
	begin
		-- Pad the message
		if (Input'Length + 2) mod Rate = 0 then
			Pad_Size := Input'Length + 2;
		else
			Pad_Size := Input'Length + 2 + (Rate - ((Input'Length + 2) mod Rate));
		end if;
		Pad_Mess(1 .. Pad_Size) := Pad_101(Input, Rate);
		
		-- Suck the padded message into the sponge
		for I in 0 .. (Pad_Size / Rate) - 1 loop
			In_Buff := Pad_Mess(1 + I * Rate .. (I + 1) * Rate);
			Suck(In_Buff, Internal_State);
			Permute(Internal_State);
		end loop;
		
		-- Squeeze the output out of the sponge
		if Output_Size <= Rate then
			Output := Spit(Output_Size, Internal_State);
			return Output;
		else
			Output(1 .. Rate) := Spit(Rate, Internal_State);
			for I in 1 .. (Output_Size / Rate) - 1 loop
				Permute(Internal_State);
				Output(1 + I * Rate .. (I + 1) * Rate) := Spit(Rate, Internal_State);
			end loop;
			if End_Size > 0 then
				Permute(Internal_State);
				Output(Output_Size - End_Size .. Output'Last) := Spit(End_Size, Internal_State);
			end if;
			return Output;
		end if;
		
	end Keccak_1600;
		
end Keccak;