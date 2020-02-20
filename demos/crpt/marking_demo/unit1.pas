unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, DividerBevel;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    DividerBevel1: TDividerBevel;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    procedure Button1Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation
uses Math;

{$R *.lfm}

function MakeCRPTCode(APrefix:Word; AGTIN:string; ASerial:string):string;
var
  A, B:array [1..32] of Byte;
  W2: QWord;
  i: Integer;
begin
  Result:='';
  FillChar(A, SizeOf(A), 0);
  W2:=StrToQWord(AGTIN);
  Move(APrefix, B, 2);
  for i:=1 to 2 do A[i]:=B[3-i];
  Move(W2, B, 6);
  for i:=1 to 6 do A[2+i]:=B[7-i];
  for i:=1 to Min(Length(ASerial), 24) do A[8 + i]:=Ord(ASerial[i]);

  for i:=1 to Length(ASerial) + 8  do
  begin
    if Result<>'' then Result:=Result + ' ';
    Result:=Result + IntToHex(A[i], 2);
  end;
end;

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  Edit5.Text:=MakeCRPTCode(StrToInt('$' + Edit1.Text), Edit2.Text, Edit3.Text);
end;

end.
//44 4D 02 A3 35 7F 8A B6 4D 64 45 66 78 3A 58 70 36 59 46 64 37

