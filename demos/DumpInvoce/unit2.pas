unit Unit2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, DividerBevel, ETradeDoc,
  ClientExchangeFile, ExchangeDocument, ExchangeInformation;

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
    procedure DumpExchangeDocument(P:TExchangeDocument; APrefix: string);
    procedure DumpExchangeFileIdentificatorSeller(P:TExchangeFileIdentificatorSeller; APrefix: string);
    procedure DumpExchangeInformation(P:TExchangeInformation; APrefix: string);
    procedure DumpAcceptanceInformation(P:TAcceptanceInformation; APrefix: string);
    procedure DumpOperationCode(P:TOperationCode; APrefix: string);
    procedure DumpCargoPerson(P:TCargoPerson; APrefix: string);
    procedure DumpCustomerEmployee(P:TCustomerEmployee; APrefix: string);
    procedure DumpExchangePerson(P:TExchangePerson; APrefix: string);
    procedure DumpExchangeOtherIssuer(P:TExchangeOtherIssuer; APrefix: string);
    procedure DumpAacceptanceLegalEntityEmployee(P:TAacceptanceLegalEntityEmployee; APrefix: string);
    procedure DumpIndividualAcceptanceGoods(P:TIndividualAcceptanceGoods; APrefix: string);
    procedure DumpAdditionalInfoId4(P:TAdditionalInfoId4; APrefix: string);
    procedure DumpExchangeTextInfoList(P:TExchangeTextInfoList; APrefix: string);
    procedure DumpExchangeTextInfo(P:TExchangeTextInfo; APrefix: string);

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
  DumpExchangeDocument(P.Document, APrefix + 'Document');
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

procedure TExchInfoFrame.DumpExchangeDocument(P: TExchangeDocument;
  APrefix: string);
begin
  if not Assigned(P) then Exit;
  APrefix:=APrefix + '.';

  WriteLogI(APrefix + 'KND = %s', [P.KND]);
  WriteLogI(APrefix + 'DateCreate = %s', [P.DateCreate]);
  WriteLogI(APrefix + 'TimeCreate = %s', [P.TimeCreate]);
  WriteLogI(APrefix + 'DocumentCreator = %s', [P.DocumentCreator]);
  WriteLogI(APrefix + 'DocumentCreatorBase = %s', [P.DocumentCreatorBase]);
  DumpExchangeFileIdentificatorSeller(P.ExchangeFileIdentificatorSeller, APrefix + 'ExchangeFileIdentificatorSeller');
  DumpExchangeInformation(P.ExchangeInformation, APrefix + 'ExchangeInformation');
  //property TreasuryInformation:TTreasuryInformation read FTreasuryInformation;
  //property ExchangeSigner:TExchangeSignerList read FSigner;
end;

procedure TExchInfoFrame.DumpExchangeFileIdentificatorSeller(
  P: TExchangeFileIdentificatorSeller; APrefix: string);
begin
  if not Assigned(P) then Exit;
  APrefix:=APrefix + '.';
  WriteLogI(APrefix + 'FileName = %s', [P.FileName]);
  WriteLogI(APrefix + 'DateCreate = %s', [P.DateCreate]);
  WriteLogI(APrefix + 'TimeCreate = %s', [P.TimeCreate]);
  WriteLogI(APrefix + 'Sign = %s', [P.Sign]);
end;

procedure TExchInfoFrame.DumpExchangeInformation(P: TExchangeInformation;
  APrefix: string);
begin
  if not Assigned(P) then Exit;
  APrefix:=APrefix + '.';
  WriteLogI(APrefix + 'DocumentName = %s', [P.DocumentName]);
  WriteLogI(APrefix + 'DocumentFunction = %s', [P.DocumentFunction]);
  WriteLogI(APrefix + 'DocumentNumber = %s', [P.DocumentNumber]);
  WriteLogI(APrefix + 'DocumentDate = %s', [P.DocumentDate]);
  WriteLogI(APrefix + 'OperationCode = %s', [P.OperationCode]);
  DumpAcceptanceInformation(P.AcceptanceInformation, APrefix + 'AcceptanceInformation');
  DumpAdditionalInfoId4(P.AdditionalInfoId, APrefix + 'AdditionalInfoId');
end;

procedure TExchInfoFrame.DumpAcceptanceInformation(P: TAcceptanceInformation;
  APrefix: string);
begin
  if not Assigned(P) then Exit;
  APrefix:=APrefix + '.';
  WriteLogI(APrefix + 'OperationContent = %s', [P.OperationContent]);
  WriteLogI(APrefix + 'AcceptanceDate = %s', [P.AcceptanceDate]);
  DumpOperationCode(P.OperationCode, APrefix + 'OperationCode');
  DumpCargoPerson(P.CargoPerson, APrefix + 'CargoPerson');
end;

procedure TExchInfoFrame.DumpOperationCode(P: TOperationCode; APrefix: string);
begin
  if not Assigned(P) then Exit;
  APrefix:=APrefix + '.';
  WriteLogI(APrefix + 'ItogCode = %s', [P.ItogCode]);
  WriteLogI(APrefix + 'DocumentName = %s', [P.DocumentName]);
  WriteLogI(APrefix + 'DocumentCode = %s', [P.DocumentCode]);
  WriteLogI(APrefix + 'DocumentNumber = %s', [P.DocumentNumber]);
  WriteLogI(APrefix + 'DocumentDate = %s', [P.DocumentDate]);
  WriteLogI(APrefix + 'FileID = %s', [P.FileID]);
end;

procedure TExchInfoFrame.DumpCargoPerson(P: TCargoPerson; APrefix: string);
begin
  if not Assigned(P) then Exit;
  APrefix:=APrefix + '.';
  DumpCustomerEmployee(P.CustomerEmployee, APrefix + 'CustomerEmployee');
  DumpExchangeOtherIssuer(P.OtherIssuer, APrefix + 'OtherIssuer');
end;

procedure TExchInfoFrame.DumpCustomerEmployee(P: TCustomerEmployee;
  APrefix: string);
begin
  if not Assigned(P) then Exit;
  APrefix:=APrefix + '.';
  WriteLogI(APrefix + 'Position = %s', [P.Position]);
  WriteLogI(APrefix + 'OtherInfo = %s', [P.OtherInfo]);
  WriteLogI(APrefix + 'SignerPowersBase = %s', [P.SignerPowersBase]);
  DumpExchangePerson(P.Person, APrefix + 'Person');
end;

procedure TExchInfoFrame.DumpExchangePerson(P: TExchangePerson; APrefix: string
  );
begin
  if not Assigned(P) then Exit;
  APrefix:=APrefix + '.';
  WriteLogI(APrefix + 'Surname = %s', [P.Surname]);
  WriteLogI(APrefix + 'FirstName = %s', [P.FirstName]);
  WriteLogI(APrefix + 'Patronymic = %s', [P.Patronymic]);
end;

procedure TExchInfoFrame.DumpExchangeOtherIssuer(P: TExchangeOtherIssuer;
  APrefix: string);
begin
  if not Assigned(P) then Exit;
  APrefix:=APrefix + '.';
  DumpAacceptanceLegalEntityEmployee(P.LegalEntityEmployee, APrefix + 'LegalEntityEmployee');
  DumpIndividualAcceptanceGoods(P.IndividualAcceptanceGoods, APrefix + 'IndividualAcceptanceGoods');
end;

procedure TExchInfoFrame.DumpAacceptanceLegalEntityEmployee(
  P: TAacceptanceLegalEntityEmployee; APrefix: string);
begin
  if not Assigned(P) then Exit;
  APrefix:=APrefix + '.';
  WriteLogI(APrefix + 'Position = %s', [P.Position]);
  WriteLogI(APrefix + 'OtherInfo = %s', [P.OtherInfo]);
  WriteLogI(APrefix + 'Organization = %s', [P.Organization]);
  WriteLogI(APrefix + 'OrgSignerPowersBase = %s', [P.OrgSignerPowersBase]);
  WriteLogI(APrefix + 'SignerPowersBase = %s', [P.SignerPowersBase]);
  DumpExchangePerson(P.Person, APrefix + 'Person');
end;

procedure TExchInfoFrame.DumpIndividualAcceptanceGoods(
  P: TIndividualAcceptanceGoods; APrefix: string);
begin
  if not Assigned(P) then Exit;
  APrefix:=APrefix + '.';
  WriteLogI(APrefix + 'SignerPowersBase = %s', [P.SignerPowersBase]);
  WriteLogI(APrefix + 'OtherInfo = %s', [P.OtherInfo]);
  DumpExchangePerson(P.Person, APrefix + 'Person');
end;

procedure TExchInfoFrame.DumpAdditionalInfoId4(P: TAdditionalInfoId4;
  APrefix: string);
begin
  if not Assigned(P) then Exit;
  APrefix:=APrefix + '.';
  WriteLogI(APrefix + 'FileID = %s', [P.FileID]);
  DumpExchangeTextInfoList(P.TextInfo, APrefix + 'TextInfo');
end;

procedure TExchInfoFrame.DumpExchangeTextInfoList(P: TExchangeTextInfoList;
  APrefix: string);
var
  i: Integer;
begin
  if not Assigned(P) then Exit;
  for i:=0 to P.Count-1 do
    DumpExchangeTextInfo(P[i], Format(APrefix + '[%d]', [i]));
end;

procedure TExchInfoFrame.DumpExchangeTextInfo(P: TExchangeTextInfo;
  APrefix: string);
begin
  if not Assigned(P) then Exit;
  APrefix:=APrefix + '.';

  WriteLogI(APrefix + 'ID = %s', [P.ID]);
  WriteLogI(APrefix + 'Value = %s', [P.Value]);
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

