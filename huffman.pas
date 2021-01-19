unit huffman;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;


type CharMap = record
  c: char;
  count: LongInt;
  end;

type tCharRepo = array of CharMap;

type PCharRepo = ^tCharRepo;

type
PHuffmanNode = ^HuffmanNode;
HuffmanNode = record
  identifier: string;
  parent: PHuffmanNode;
  size: integer;
  right: boolean;
end;

type HuffmanNodeLevel = array of PHuffmanNode;

type CharBinaryMap = record
  c: string;
  binary: string;
end;

type HufmanBinaryTable = array of CharBinaryMap;

type

  { TForm1 }

  TForm1 = class(TForm)
    BEncode: TButton;
    ECharQuery: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    LCharCount: TLabel;
    LCharQueryResult: TLabel;
    MOutput: TMemo;
    MInput: TMemo;
    procedure BEncodeClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure displayGlobalCharCount();
    procedure displayQueryCharCount();
    procedure displayHuffmanResult(huffmanTable: HufmanBinaryTable; input: string);
  private

  public

  end;

  { # Gobal variables # }
var
  Form1: TForm1;
  charCount: LongInt;

implementation

{$R *.lfm}

{ TForm1 }

function countChars(const input: string): LongInt;
begin
   countChars:= Length(input);
end;

function countChar(const c: char; text: string): LongInt;
var accumulator: LongInt;
    textLength: LongInt;
    i: LongInt;
begin
   accumulator:= 0;
   textLength:= Length(text);

   for i:= 0 to textLength do
   begin
     if (text.Chars[i] = c) then
     begin
       accumulator:= accumulator + 1;
     end;
   end;

   countChar:= accumulator;

end;

function inArray(const c: char; var arr: array of CharMap): LongInt;
var i: LongInt;
    found: LongInt;
begin
   found:= -1;
   if (Length(arr) > 0) then
   begin
     for i:= 0 to Length(arr) - 1 do
     begin
          if (arr[i].c = c) then
          begin
           found:= i;
          end;
     end;
   end;
     inArray:= found;
end;

procedure sortArray(var arr: array of CharMap);
var iterations: integer;
      i: integer;
      j: integer;
      tmp: CharMap;
begin
   iterations:= 0;
   for j:= 0 to Length(arr) - 1 do
   begin
         for i:= 0 to (Length(arr) - 2 - j) do
         begin
              if arr[i].count > arr[i+1].count then
              begin
               // swap
               tmp:= arr[i];
               arr[i]:= arr[i+1];
               arr[i+1]:= tmp;
               iterations:= iterations + 1;
              end;
         end;
   end;
end;

procedure sortLevel(var arr: HuffmanNodeLevel);
var iterations: integer;
      i: integer;
      j: integer;
      tmp: PHuffmanNode;
begin
   iterations:= 0;
   for j:= 0 to Length(arr) - 1 do
   begin
         for i:= 0 to (Length(arr) - 2 - j) do
         begin
              if arr[i]^.size > arr[i+1]^.size then
              begin
               // swap
               tmp:= arr[i];
               arr[i]:= arr[i+1];
               arr[i+1]:= tmp;
               iterations:= iterations + 1;
              end;
         end;
   end;
end;

function reverse(const str: string): string;
var
  i, j: Integer;
begin
  j := length(str);
  setlength(reverse, j);
  for i := 1 to j do
    reverse[i] := str[j - i + 1];
end;

function generateCharRepo(input: string): PCharRepo;
var charRepo: PCharRepo;
    charCount: LongInt;
    cMap: CharMap;
    i: LongInt;
    position: LongInt;
begin
   new(charRepo);
   charCount:= 0;
   SetLength(charRepo^, charCount);
   for i:= 1 to Length(input) do
   begin
        if (inArray(input[i], charRepo^) = -1) then
        begin
         charCount:= charCount + 1;
         setLength(charRepo^, charCount);

         cMap.c := input[i];
         cMap.count:= 1;
         charRepo^[charCount - 1]:= cMap;
        end
        else begin
             position := inArray(input[i], charRepo^);
             charRepo^[position].count := charRepo^[position].count + 1;
        end;
   end;

   generateCharRepo:= charRepo;
end;


function repoToTreeLeaves(repo: PCharRepo): HuffmanNodeLevel;
var i: integer;
    level: HuffmanNodeLevel;
    levelLength: LongInt;
    node: PHuffmanNode;
begin
   levelLength := Length(repo^);
   SetLength(level, levelLength);
   for i:= 0 to Length(repo^) - 1 do
   begin
        new(node);
        node^.identifier:= '' + repo^[i].c;
        node^.size:= repo^[i].count;
        node^.parent:=nil;
        level[i] := node;
   end;
   repoToTreeLeaves:= level;
end;


function createHuffmanTreeFromRepo(charRepo: PCharRepo): HuffmanNodeLevel;
var connectionPool: HuffmanNodeLevel;
    oldConnectionPool: HuffmanNodeLevel;
    ground: HuffmanNodeLevel;
    node: PHuffmanNode;
    child1: PHuffmanNode;
    child2: PHuffmanNode;
    newLength: LongInt;
    i: integer;
begin
      ground:= repoToTreeLeaves(charRepo);
      sortLevel(ground);
      connectionPool:= ground;

      while (Length(connectionPool) > 1) do begin
           child1:= connectionPool[0];
           child2:= connectionPool[1];

           new(node);
           node^.identifier:= child1^.identifier + child2^.identifier;
           node^.size:= child1^.size + child2^.size;
           node^.parent:= nil;

           child1^.parent:= node;
           child1^.right:= false;

           child2^.parent:= node;
           child2^.right:= true;

           oldConnectionPool:= Copy(connectionPool);

           newLength:= Length(connectionPool) - 1;
           SetLength(connectionPool, 0);
           SetLength(connectionPool, newLength);

           for i:= 0 to newLength - 1 do
           begin
                connectionPool[i]:= oldConnectionPool[i + 2];
           end;

           connectionPool[newLength - 1]:= node;

           sortLevel(connectionPool);

      end;

      createHuffmanTreeFromRepo:= ground;
end;

function createBinaryTableFromHuffmanTreeLeaves(leaves: HuffmanNodeLevel): HufmanBinaryTable;
var i: integer;
    table: HufmanBinaryTable;
    tableSize: integer;
    tmpPath: string;
    parent: PHuffmanNode;
begin
    tableSize := Length(leaves);
    SetLength(table, tableSize);
    for i:= 0 to Length(leaves) - 1 do
    begin
         table[i].c:= leaves[i]^.identifier;
         parent := leaves[i];
         tmpPath:= '';
         while (parent <> nil) do begin
              if (parent^.right) then
              begin
               tmpPath:= tmpPath + '1';
              end
              else
              begin
                   tmpPath:= tmpPath + '0';
              end;
              parent := parent^.parent;
         end;
         table[i].binary:= reverse(tmpPath);
    end;

    createBinaryTableFromHuffmanTreeLeaves := table;
end;

function findBinaryForChar(c: char; table: HufmanBinaryTable): string;
var i: integer;
    found: string;
begin
   found:= '!ERROR!';
   for i:= 0 to Length(table) - 1 do
   begin
        if (table[i].c = '' + c) then begin
         found:= table[i].binary;;
        end;
   end;
   findBinaryForChar:= found;
end;

procedure TForm1.displayHuffmanResult(huffmanTable: HufmanBinaryTable; input: string);
var i: Integer;
    output: string;
    lines: TStringArray;
begin
   output:= '';
   for i:= 0 to Length(huffmanTable) - 1 do begin
       output:= output + '"' + huffmanTable[i].c + '"' +': ' + huffmanTable[i].binary + '\n';
   end;

   for i:= 1 to Length(input) do
   begin
       output+=  findBinaryForChar(input[i], huffmanTable) + '    ';
   end;

   lines:= output.Split('\n');
   MOutput.Clear;
   for I:= 0 to Length(lines) - 1 do
   begin
       MOutput.Lines.Add(lines[i]);
   end;
end;

procedure TForm1.displayGlobalCharCount();
begin
   LCharCount.Caption:= IntToStr(countChars(MInput.Lines.Text));
end;

procedure TForm1.displayQueryCharCount();
begin
   if ((Length(ECharQuery.Text) = 0) OR (Length(MInput.Lines.Text) = 0)) then
   begin
     LCharQueryResult.Caption:= ': -';
   end
   else
   begin
     LCharQueryResult.Caption := IntToStr(countChar(ECharQuery.Text[1], MInput.Lines.Text));
   end;
end;

procedure displayCharCounts();
begin
end;

procedure TForm1.BEncodeClick(Sender: TObject);
var charRepo: PCharRepo;
    nodes: HuffmanNodeLevel;
    table: HufmanBinaryTable;
begin
     Self.displayGlobalCharCount();
     Self.displayQueryCharCount();

     charRepo := generateCharRepo(MInput.Lines.Text);
     nodes:= createHuffmanTreeFromRepo(charRepo);

     table:= createBinaryTableFromHuffmanTreeLeaves(nodes);

     Self.displayHuffmanResult(table, MInput.Lines.Text);

     // cleanup
     dispose(charRepo);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin

end;

end.

