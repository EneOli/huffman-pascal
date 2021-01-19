unit huffmanUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    BEncode: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    LCharCount: TLabel;
    MInput: TMemo;
    procedure BEncodeClick(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
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

procedure displayCharCount();
begin
end;

procedure TForm1.BEncodeClick(Sender: TObject);
var chars: LongInt;
begin
   chars:= countChars(MInput.Lines.Text);
   LCharCount.Caption:= IntToStr(chars);
end;

procedure TForm1.Edit1Change(Sender: TObject);
begin

end;

end.

