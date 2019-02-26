unit Unit2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, DividerBevel, ETradeDoc,
  ClientExchangeFile;

type

  { TExchInfoFrame }

  TExchInfoFrame = class(TFrame)
    Button2: TButton;
    DividerBevel2: TDividerBevel;
    Edit1: TEdit;
    ETradeDoc1: TETradeDoc;
    Label2: TLabel;
    Memo1: TMemo;
    procedure Button2Click(Sender: TObject);
  private
    procedure DumpClientExchangeFile(P:TClientExchangeFile; APrefix: string);
    procedure DumpParticipantsInformationEx(P:TParticipantsInformationEx; APrefix: string);
    procedure DumpSenderExchangeInformationEx(P:TSenderExchangeInformationEx; APrefix: string);

    procedure WriteLogI(S:string; AParams:array of const);
  public
    constructor Create(TheOwner: TComponent); override;
  end;

implementation
uses LazFileUtils;

{$R *.lfm}

{ TExchInfoFrame }

procedure TExchInfoFrame.Button2Click(Sender: TObject);
var
  D: TClientExchangeFile;
begin
  D:=ETradeDoc1.LoadClientExchangeFile(Edit1.Text);
  DumpClientExchangeFile(D, 'ОБМЕН');
  D.Free;
end;

procedure TExchInfoFrame.DumpClientExchangeFile(P: TClientExchangeFile;
  APrefix: string);
begin
  if not Assigned(P) then Exit;
  APrefix:=APrefix + '.';
  DumpParticipantsInformationEx(P.ParticipantsInformationEx, APrefix + 'ParticipantsInformationEx');
  //property Document:TExchangeDocument read FDocument;
end;

procedure TExchInfoFrame.DumpParticipantsInformationEx(
  P: TParticipantsInformationEx; APrefix: string);
begin
  if not Assigned(P) then Exit;
  APrefix:=APrefix + '.';
  WriteLogI(APrefix + 'SenderInfo = %s', [P.SenderInfo]);
  WriteLogI(APrefix + 'RecipientInfo = %s', [P.RecipientInfo]);
  DumpSenderExchangeInformationEx(P.SenderExchangeInformationEx, APrefix + 'SenderExchangeInformationEx');

end;

procedure TExchInfoFrame.DumpSenderExchangeInformationEx(
  P: TSenderExchangeInformationEx; APrefix: string);
begin
  if not Assigned(P) then Exit;
  APrefix:=APrefix + '.';

  WriteLogI(APrefix + 'FullName = %s', [P.FullName]);
  WriteLogI(APrefix + 'Inn = %s', [P.Inn]);
  WriteLogI(APrefix + 'IdentifierSenderOperator = %s', [P.IdentifierSenderOperator]);
end;

procedure TExchInfoFrame.WriteLogI(S: string; AParams: array of const);
begin
  Memo1.Lines.Add(Format(S, AParams));
end;

constructor TExchInfoFrame.Create(TheOwner: TComponent);
var
  S: String;
begin
  inherited Create(TheOwner);

  Memo1.Lines.Clear;

  S:=AppendPathDelim(ExtractFileDir(ParamStr(0))) + AppendPathDelim('data');
  Edit1.Text:=S + 'exch1.xml';
end;

end.

