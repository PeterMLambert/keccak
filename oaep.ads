package Oaep is
	
	function Oaep_Encrypt (M : in String) return String;

private
	package  Random_Char is new Ada.Numerics.Discrete_Random(Character);
	G : Random_Char.Generator;
	
	function Random_String(in N : Natural) return String;
	
	function Nul_Pad(Input : in String; Total : in Natural) return String;
	
	function String_Xor(A : in String; B : in String) return String;
	
end Oaep;