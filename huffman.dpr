Program huffman;

{$APPTYPE CONSOLE}

Uses
  SysUtils, Windows, Classes;

Const
  BYTES_TO_READ = 4096;

Type
  TreePointer = ^Node;
  Node = Record
    data: Byte;
    amount: Int64;
    left, right, next: TreePointer;
  end;
  TFile = File of Byte;
  TCharArr = array[Byte, 0..1] of Integer;
  TCodeTable = array[Byte, 1..2] of Word;
  TReWrArr = array[1..BYTES_TO_READ] of Byte;

// Make linked list
// Record:
// .data: Byte
// .amount: Integer
// .next: Listpointer
Procedure MakeList(x: TreePointer; Const MyArr: TCharArr);
Var
  Deleted: Boolean;
  i: Integer;
  y: TreePointer;

Begin
  y := x;
  for i := 0 to 255 do
    if MyArr[i, 1] <> 0 then
    begin
       x^.data := MyArr[i, 0];
       x^.amount := MyArr[i, 1];
       if i <> 0 then
       begin
       New(x^.next);
       x := x^.next;
       end;
    end;
  Deleted := False;
  while not Deleted do
  begin
    if y^.next^.amount = 0 then
    begin
      y^.next := nil;
      Deleted := True;
    end;
    y := y^.next;
  end;
End;

// Shell Sort function
Procedure ShellSort(Var MyArr: TCharArr);
Var
  i, Temp0, Temp1, Right, Left, Len: Integer;
  Shift: array[1..4] of Integer;
  // i - cycle variable
  // t - number of steps to sort
  // Temp - buffer variable
  // Right - right element while checking two of them
  // Left - left element while checking two of them

Begin
  Len := 256;
  // Calculating the number of steps
  Shift[4] := 1;

  // Creating the array of steps
  for i := 3 downto 1 do
    Shift[i] := 3*Shift[i+1] + 1;
  for i := 1 to 4 do
    // Performing check from the rightest possible
    // with exact step element
    for Right := Shift[i] to Len-1 do
    begin
      Temp0 := MyArr[Right, 0];
      Temp1 := MyArr[Right, 1];
      Left := Right-Shift[i];

      // Continues check till the left border
      while (Left >= 0) and (MyArr[Left, 1] > Temp1)  do
      begin
        MyArr[Left+Shift[i], 0] := MyArr[Left, 0];
        MyArr[Left+Shift[i], 1] := MyArr[Left, 1];
        Left := Left-Shift[i];
      end;
      MyArr[Left+Shift[i], 0] := Temp0;
      MyArr[Left+Shift[i], 1] := Temp1;
    end;
End;

// Creating frequency array of file bytes
// [0:N, 1:M, ..., 255:P]
// And sorting it to the type
// [X:A, Y:B, ..., Z:C],
// where A<=B<=C
Procedure MakeArr(Var ChArr: TCharArr; Const FileName: String);
Var
  i, ReadSize: Integer;
  FileSize: Int64;
  FHandle: TFileStream;
  BufferInp: TReWrArr;

Begin
  //AssignFile(FHandle, 'C:\Users\Paul\Downloads\mingw-w64-install.exe');
  //Reset(FHandle);
  for i := 0 to Length(ChArr)-1 do
  begin
    ChArr[i, 1] := 0;
    ChArr[i, 0] := i;
  end;

  FHandle := TFileStream.Create(FileName, fmOpenRead);
  repeat
    ReadSize := FHandle.Read(BufferInp, BYTES_TO_READ);
    for i := 1 to ReadSize do
    begin
      Inc(ChArr[BufferInp[i], 1]);
    end;
  until ReadSize < BYTES_TO_READ;

  FHandle.Free;
  ShellSort(ChArr);

  for i := 0 to 255 do
    writeln(ChArr[i, 0], ':', ChArr[i,1]);

  writeln('---');
End;

Function CreateTree(y: TreePointer; ynext: TreePointer): TreePointer;
Var
  Temp: TreePointer;

Begin
  New(Temp);
  Temp^.Amount := y^.amount + ynext^.amount;
  Temp^.left := y;
  Temp^.right := ynext;

  Result := Temp;
End;
{
node : [amount [Integer], data [Byte], left [node], right [node], next [nodept]]
}

// Create frequence tree (for ex.):
//                50
//         20            30(A) --------> .amount = 30, .data = Ord(A),
//     8(B)    12                        .right = nil, .left = nil
//         4(C)   8(V)
Procedure CreateFreqTree(Var TreePt: TreePointer);
Var
  Placed: Boolean;
  Tree, x, y, z: TreePointer;

Begin
  y := TreePt;
  while y^.next <> nil do
  begin
    Tree := CreateTree(y, y^.next);
    z := y^.next;
    Placed := False;
    while (z^.next <> nil) and (not Placed) do
    begin
      if z^.next^.amount > Tree^.amount then
      begin
        x := z^.next;
        z^.next := Tree;
        Tree^.next := x;
        Placed := True;
      end;
      z := z^.next;
    end;
    if not Placed then
    begin
      New(z^.next);
      z^.next := Tree;
    end;
    y := y^.next^.next;
  end;
  TreePt := y;
End;

Procedure print(x:TreePointer);
Begin
  while x^.next <> nil do
  begin
    writeln(x^.data, ':', x^.amount);
    x:=x^.next;
  end;
End;

Procedure WriteToFile(X: TreePointer);
var
  CompFile: TFile;
begin
  AssignFile(CompFile, 'out.huffm');
  Rewrite(CompFile);
  while x <> nil do
  begin
    Write(CompFile, x^.data);
    x := x^.next;
  end;
end;

Procedure PrintTree(Var t:treepointer; space:integer);
Var
  i: Integer;

begin
  if t<>nil then
  begin
    Printtree(t^.right,space+1);
    for i:=1 to space do write(' ');
    if t^.data <> 0 then
      writeln(t^.amount, ' : ', t^.data)
    else
      writeln(t^.amount);
    Printtree(t^.left,space+1);
  end;
end;

// Create Table of code to encode file
// ByteTable[$B4] = (.Code-[Word], .CodeLength-[Byte])
Procedure CreateCodeTable(Var ByteTable: TCodeTable; TreeRoot: TreePointer; Code: Word; CodeLen: Byte);
Begin
  if TreeRoot <> nil then
  begin

    if (TreeRoot^.left = nil) and (TreeRoot^.right = nil) then
    begin
      ByteTable[TreeRoot^.data, 1] := Code;
      ByteTable[TreeRoot^.data, 2] := CodeLen;
    end;
      //writeln(Code, ' ', TreeRoot^.data);
    CreateCodeTable(ByteTable, TreeRoot^.left, Code shl 1, CodeLen+1);
    CreateCodeTable(ByteTable, TreeRoot^.right, Code shl 1 + 1, CodeLen+1);
  end;

End;

Procedure CreateTreeCode(Tree: TreePointer; Var WriteArr: TReWrArr; Code, Length: Word; Var Index: Integer);
Begin
  if Tree <> nil then
  begin
    if Length = 8 then
    begin
      WriteArr[Index] := Code;
    end;
    if (Tree^.left = nil) and (Tree^.right = nil) then
    begin
      Code := ((Code shl 1) + 1) shr 8 + Tree^.data;
    end;
    CreateTreeCode(Tree^.left, WriteArr, Code shl 1, Length+1, Index);
    CreateTreeCode(Tree^.right, WriteArr, Code shl 1, Length+1, Index);
  end
  else
  begin

  end;
end;

// Creating symbol code in RemainCode [Word], saving length in RemainLen
// Needs code of the symbol - Code [Word], its length - CodeLen
// Returns True if symbol got length of 16 bits and should be placed
//         False if shouldn't be placed.
Function CreateByteCode(Var Code, CodeLen, RemainCode, RemainLen: Word): Boolean;
Var
  Temp: Word;
Begin
  if RemainLen + CodeLen <= 16 then
  begin
    Code := RemainCode shl CodeLen + Code;
    CodeLen := RemainLen + CodeLen;
    RemainCode := Code;
    RemainLen := CodeLen;
    if CodeLen = 16 then
    begin
      Result := True;
      RemainCode := 0;
      RemainLen := 0;
    end
    else
      Result := False;
  end
  else if RemainLen + CodeLen > 16 then
  begin
    Temp := (Code shl (32 - CodeLen - RemainLen)) shr (32 - CodeLen - RemainLen);
    Code := RemainCode shl (16 - RemainLen) + Code shr (CodeLen - 16 + RemainLen);
    RemainLen := CodeLen - 16 + RemainLen;
    RemainCode := Temp;
    Result := True;
  end;
End;

Procedure WriteCodeToFile(Var ByteTable: TCodeTable; Tree: TreePointer; Const FileName: String);
Var
  WriteInArr: Boolean;
  FReadHandle, FWriteHandle: TFileStream;
  F: File of Byte;
  FileNameNoExt: String;
  i, j, SlPos, DotPos, FSize: Integer;
  FileSize: Int64;
  Code, CodeLen, RemainCode, RemainLen: Word;
  BufferInp, BufferOut: TReWrArr;

Begin
  FReadHandle := TFileStream.Create(FileName, fmOpenRead);
  SlPos := 0;
  DotPos := 0;
  for i := 1 to Length(FileName) do
  begin
    if FileName[i] = '\' then
      SlPos := i;
    if FileName[i] = '.' then
      DotPos := i;
  end;
  FileNameNoExt := 'output\' + Copy(FileName, SlPos+1, DotPos-SlPos-1) + '.huffm';
  writeln(FileNAmeNoExt);
  FWriteHandle := TFileStream.Create(FileNameNoExt, fmCreate);
  FWriteHandle.Free;
  FWriteHandle := TFileStream.Create(FileNameNoExt, fmOpenWrite);

  // Creating file extension in bytes
  FileNameNoExt := Copy(FileName, DotPos+1, Length(FileNAme)-DotPos);
  SlPos := 1;
  for i := 1 to Length(FileNameNoExt) do
  begin
    BufferInp[i] := Ord(FileNameNoExt[i]);
    Inc(SlPos)
  end;
  //FWriteHandle.Write(BufferInp, SlPos-1);
  FileSize := FReadHandle.Size;
  BufferInp[1] := 0;
  SlPos := 1;
  for i := 2 to 6 do
  begin
    BufferInp[i] := FileSize;
    FileSize := FileSize shr 8;
    Inc(SlPos);
  end;
  //FSize := FWriteHandle.Write(BufferInp, 6);


  Code := 0;
  CodeLen := 0;
  CreateCodeTable(ByteTable, Tree, Code, CodeLen);

  FSize := 0;
  //CreateTreeCode(Tree, BufferInp, ByteCode, CodeLength, FSize);


  // Cycle of writing codes to output file
  // according to bytes that are met in input file.
  repeat
    // Reading some amount of bytes into array of bytes
    FSize := FReadHandle.Read(BufferInp, BYTES_TO_READ);

    RemainCode := 0;
    RemainLen := 0;
    WriteInArr := False;
    // Creating codes for the part of symbols 
    j := 1;
    for i := 1 to FSize do
    begin
      Code := ByteTable[BufferInp[i], 1];
      CodeLen := ByteTable[BufferInp[i], 2];
      WriteInArr := CreateByteCode(Code, CodeLen, RemainCode, RemainLen);
      if WriteInArr then
      begin
        BufferOut[j] := Hi(Code);   
        Inc(j);
        BufferOut[j] := Lo(Code);   
        Inc(j);
        WriteInArr := False;
      end;
    end;
    FSize := FWriteHandle.Write(BufferOut, j);
  until FSize < 2048;
  FReadHandle.Free;
  FWriteHandle.Free;
End;

Var
  FName: String;
  First: TreePointer;
  i: Integer;
  Time0, Time1: Cardinal;
  FileLen: Int64;
  CharArr: TCharArr;
  CodeTable: TCodeTable;

Begin
   //     'D:\Documents\uni\OAun\work\cp\code\ex.txt'       ''
  FName :=  'C:\Users\Paul\Downloads\mingw-w64-install.exe';

  // Creating first element of list
  // .data - byte containing [Byte]
  // .amount - amount of such bytes in file [Integer]
  // .next - pointer on the next element
  New(First);

  // Creating frequency array of file bytes
  // [0:N, 1:M, ..., 255:P]
  Time0 := GetTickCount();
  MakeArr(CharArr, FName);

  // Creating list of bytes from array
  MakeList(First, CharArr);
  print(first);

  // Creating Binary Tree from list
  CreateFreqTree(first);

  //InsertTree(Root, CharArr);
  PrintTree(first, 1);

  // Creation of table of codes that
  // are equivalent to exact bytes

  WriteCodeToFile(CodeTable, First, FName);

  Time1 := GetTickCount();

  writeln((Time1-Time0)/1000:3:3);

  FileLen := 0;
  for i := Low(CodeTable) to High(CodeTable) do
  begin
    if CodeTable[i,2] <> 0 then
    begin
    write(IntToHex(i, 2), ': ');
    writeln(IntToHex(CodeTable[i, 1], 2) + ', ', CodeTable[i, 2]);
    FileLen := FileLen + (CodeTable[i,2]-1)*CharArr[i, 1];
    end;
  end;
  writeln(FileLen/8:10:2);
  //WriteToFile(First^);
  readln;
End.
