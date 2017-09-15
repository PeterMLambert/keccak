-- OAEP for RSA

with Keccak;

package body Oaep is

	Num_Bytes : Natural := 256;
	
	function Random_String(in N : Natural) return String is
		Result : String(1 .. N);
	begin
		for I in String'Range loop
			Result(I) := Random(G);
		end loop;
		return Result;
	end Random_String;

	
	function Nul_Pad(Input : in String; Total : in Natural) return String is
		Result : String(1 .. Total) := (others => Character'Val(0));
	begin
		if Input'Length > Total then
			raise Constraint_Error with "Pad length should not be shorter than message length.";
		end if;
		Result(1 .. Input'Length) := Input;
		return Result;
	end Nul_Pad;
	
	
	function String_Xor(A : in String; B : in String) return String is
		Result : String(1 .. A'Length);
		type Char_m is mod 256;
	begin
		if A'Length /= B'Length then
			raise Constraint_Error with "Strings must be the same length to XOR.";
		end if;
		
		for I in 0 .. Result'Length - 1 loop
			Result(Result'First + I) := Character'Val(Char_m'Pos(A(A'First + I)) xor Char_m'Pos(B(B'First + I)));
		end loop;
		
		return Result;
	end String_Xor;
	
	
	function Hash(S : in String) return String is
	begin
		return Keccak.To_String(Keccak.Keccak_1600(S, Num_Bytes * 8, Keccak.Rates(1024)));
	end Hash;

	
	function Oaep_Encrypt (M : in String) return String is
		Moo     : String(1 .. Num_Bytes) := Nul_Pad(M, Num_Bytes);
		R, X, Y : String(1 .. Num_Bytes);
		C       : String(1 .. 2 * Num_Bytes);
	begin
		Random_Char.Reset(G);	
		
		R := Random_String(Num_Bytes);
		X := String_Xor(Moo, Hash(R));
		Y := String_Xor(R  , Hash(X));
		C := (X & Y);
		
		return C;
	end Oaep_Encrypt;
	
	
	function Oaep_Decrypt (C : in String) return String is
		M, X, Y, R : String(1 .. Num_Bytes);
	begin
		X := C(C'First .. C'First - 1 + Num_Bytes);
		Y := C(C'Last + 1 - Num_Bytes .. C'Last);
		R := String_Xor(Y, Hash(X));
		M := String_Xor(X, Hash(R));
		
		return M;
	end Oaep_Decrypt;
end Oaep;