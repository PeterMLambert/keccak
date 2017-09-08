-- An example of using the Keccak package with the command line.
--
--
-- Copyright (C) 2017 Peter Lambert
-- You do not have, nor can you ever acquire the right to use, copy or distribute this software. Should you use this software for any purpose, or copy and distribute it to anyone or in any manner, you are breaking the laws of whatever soi-disant jurisdiction, and you promise to continue doing so for the indefinite future.
--

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Bounded;
with Ada.Strings.Unbounded;
with Ada.Directories;
with Ada.Direct_IO;

with Keccak;

procedure KeccakCLI is
	function Max_Arg return Integer is 
		Result : Integer := 1;
	begin
		for I in 1 .. Argument_Count loop
			if Argument(I)'Length > Result then
				Result := Argument(I)'Length;
			end if;
		end loop;
		return Result;
	end Max_Arg;
	pragma Inline(Max_Arg);
	
	package BS is new Ada.Strings.Bounded.Generic_Bounded_Length(Max => Max_Arg);
	package UBS renames Ada.Strings.Unbounded;
	
	Hex_Out : Boolean := False;
	Output_Bits : Positive;
	Rate : Keccak.Rates;
	File_Name : BS.Bounded_String;
	File_Size : Natural;
	Output : UBS.Unbounded_String;
	
	function Hex_Byte(B : Integer) return String is
        C : constant := 2 ** 4;
        H : constant array(0 .. C - 1) of Character := "0123456789ABCDEF";
        S : String(1 .. 2);
    begin
        S(1) := H(B / C);
        S(2) := H(B mod C);
        return S;
    end Hex_Byte;
	
begin
	-- Get input from command line
	case Argument_Count is
		when 3 =>
			Rate := Keccak.Rates(Integer'Value(Argument(1)));
			Output_Bits := Integer'Value(Argument(2));
			File_Name := BS.To_Bounded_String(Argument(3));
		when 4 => 
			if Argument(1) = "-x" then
				Hex_Out := True;
			end if;
			Rate := Keccak.Rates(Integer'Value(Argument(2)));
			Output_Bits := Integer'Value(Argument(3));
			File_Name := BS.To_Bounded_String(Argument(4));
		when others =>
			Put("Usage: " & Command_Name & " [-x] Rate(1 to 1600) Output_Bits File_Name");
			New_Line(2);
			return;
	end case;	
	
	-- read file for input
	if not Ada.Directories.Exists(BS.To_String(File_Name)) then
		Put_Line(Standard_Error, "Unable to find file.");
		return;
	end if;
	
	File_Size := Natural(Ada.Directories.Size(BS.To_String(File_Name)));
	
	declare
		subtype File_String    is String(1 .. File_Size);
		package File_String_IO is new Ada.Direct_IO(File_String);
		
		Input_File : File_String_IO.File_Type;
		Input      : File_String;
		
	begin
		File_String_IO.Open(Input_File, Mode => File_String_IO.In_File,
		                                Name => BS.To_String(File_Name));
		File_String_IO.Read(Input_File, Item => Input);
		File_String_IO.Close(Input_File);
		
										
		-- perform keccak_1600 on the file
		Output := UBS.To_Unbounded_String(
		           Keccak.To_String(
		            Keccak.Keccak_1600(
		             Keccak.To_Bitstream(Input),
		             Output_Bits,
		             Rate)));
		
		-- display the result
		if Hex_Out then
			for K in UBS.To_String(Output)'Range loop
				Put(Hex_Byte(Character'Pos(UBS.To_String(Output)(K))));
			end loop;
		else
			Put(UBS.To_String(Output));
		end if;
	end;
end KeccakCLI;