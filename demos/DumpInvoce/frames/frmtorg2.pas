unit frmTorg2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls;

type

  { TfrmTorg2Frame }

  TfrmTorg2Frame = class(TFrame)
    Button1: TButton;
    Button2: TButton;
    CLabel: TLabel;
    procedure Button1Click(Sender: TObject);
  private

  public

  end;

implementation
uses et_torg2;

{$R *.lfm}

{ TfrmTorg2Frame }

procedure TfrmTorg2Frame.Button1Click(Sender: TObject);
var
  F: TTorg2ExchangeFile;
  FN: String;
begin
  FN:='DP_PRIRASXPRIN'+'MARK'+'_'+'2610001001'+'_'+'2610001002'+'_'+'2019'+'11'+'20'+'ED914F28-C18C-48F4-999C-5CA294E76B9F';
  F:=TTorg2ExchangeFile.Create;
  F.FileID:=FN;
  F.FormatVersion:='5.01';
  F.ApplicationVersion:='DemoTST_1.0';

  F.ParticipantsInformation.RecipientInfo:='001';
  F.ParticipantsInformation.SenderInfo:='002';
  F.ParticipantsInformation.SellerExchangeInformation.FullName:='Оператор ЭДО №1';
  F.ParticipantsInformation.SellerExchangeInformation.Inn:='2610001003';
  F.ParticipantsInformation.SellerExchangeInformation.IdentifierSenderOperator:='881';

  F.Document.KND:='1175014';

  F.SaveToFile('/tmp/'+FN+'.xml');
  F.Free;
end;

end.

