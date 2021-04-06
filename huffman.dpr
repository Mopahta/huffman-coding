Program huffman;

{$APPTYPE CONSOLE}

Uses
  SysUtils;

Type
  TreePointer = ^Node;
  Node = Record
    data: Byte;
    amount: Integer;
    left, right, next: TreePointer;
  end;

  TFile = File of Byte;
  TCharArr = array of array of Integer;

Var
  FullFile, CompFile: TFile;
  S: Byte;
  First: TreePointer;
  Root: TreePointer;
  FileHandle, FSize, i, Min: Integer;
  CharArr: TCharArr;

// Make linked list
// Record:
// .data: Byte
// .amount: Integer
// .next: Listpointer
Procedure MakeList(x: TreePointer; Const MyArr: TCharArr);
Var
  i: Integer;

Begin
  for i := 0 to 255 do
    if MyArr[i, 1] <> 0 then
    begin
       x^.data := MyArr[i, 0];
       x^.amount := MyArr[i, 1];
       New(x^.next);
       x := x^.next;
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
Procedure MakeArr(Var ChArr: TCharArr);
Var
  i, FSize: Integer;
  S: Byte;
  FHandle: TFile;

Begin
  SetLength(ChArr, 256, 2);
  AssignFile(FHandle, 'l1.exe');
  Reset(FHandle);
  FSize := FileSize(FHandle);
  for i := 0 to Length(ChArr)-1 do
  begin
    ChArr[i, 1] := 0;
    ChArr[i, 0] := i;
  end;
  for i := 1 to FSize do
  begin
    Read(FHandle, S);
    Inc(ChArr[S, 1]);
  end;
  ShellSort(ChArr);
  for i := 0 to 255 do
    writeln(ChArr[i, 0], ':', ChArr[i,1]);

  writeln('---');
End;

Function CreateTree(y: TreePointer; ynext: TreePointer): TreePointer;
Var
  Temp, a, b: TreePointer;

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


Procedure CreateFreqTree(Var TreePt: TreePointer);
Var
  Placed: Boolean;
  i: Integer;
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

  {for i := 1 to 255 do { TODO: change so that it would work depending on number of elements in list}
  {begin
    Tree := CreateTree(y, y^.next);
    Placed := False;
    x := y;
    while (x^.next <> nil) and (not Placed) do
    begin
      x := x^.next;
      if x^.amount>Tree^.Amount then
      begin
        z := x^.next;
        New(x^.next);
        x^.next^.tree := tree;
        x^.next^.amount := tree.Amount;
        x^.next^.next := z;
        Placed := True;
      end;
    end;
    if not Placed then
    begin
      New(x^.next);
      x^.next^.amount := tree.Amount;
      x^.next^.tree := tree;
    end;


    y := y^.next^.next;
  end;
  ListPt := y;      }
End;

Procedure print(x:TreePointer);
Begin
  while x^.amount <> 0 do
  begin
    writeln(x^.data, ':', x^.amount);
    x:=x^.next;
  end;
End;

{Function FindMin(List: ListPointer; Var PrevMin: Integer): Integer;
Var
  i, TempMin, Count, CountIter: Integer;
  Found: Boolean;
begin
  Found := False;
  TempMin := List^.amount;
  CountIter := 0;
  while (List^.amount <> 0) and (not Found) do
  begin
    if (List^.amount > PrevMin) and (Count = 0) then
    begin
      PrevMin := List^.amount;
      Found := True;
    end
    else
      Dec(Count);
    Inc(CountIter);
    List := List^.next;
  end;
  FindMin := PrevMin;
end;

Procedure CreateTree(List: ListPointer; Var Tree: TreePointer);
Begin

End;

Procedure InsertTree(Var TreePt: TreePointer; ChArr: TCharArr);
Var
  i: Integer;
Begin
  for i := 0 to 255 do
  begin
    if (TreePt = nil) and (ChArr[i, 1] <> 0)then
    begin
      New(TreePt);
      TreePt^.Data := ChArr[i, 0];
      TreePt^.Left := nil;
      TreePt^.Right := nil;
    end
    {else if (ChArr[i, 1] <= TreePt^.Data) and (ChArr[i, 1] <> 0) then
      InsertTree(TreePt^.left, ChArr)
    else if ChArr[i, 1] <> 0 then
      InsertTree(TreePt^.right, ListPt);
  end;
End;  }

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
begin
  if t<>nil then
  begin
    Printtree(t^.right,space+1);
    for i:=1 to space do write(' ');
    writeln(t^.amount);
    Printtree(t^.left,space+1);
  end;
end;

Begin
  // Creating first element of list
  // .data - byte containing [Byte]
  // .amount - amount of such bytes in file [Integer]
  // .next - pointer on the next element
  New(First);

  // Creating frequency array of file bytes
  // [0:N, 1:M, ..., 255:P]
  MakeArr(CharArr);
  MakeList(First, CharArr);
  print(first);
  CreateFreqTree(first);
  //InsertTree(Root, CharArr);
  //PrintTree(first.tree, 1);


  //WriteToFile(First^);
  readln;
End.
