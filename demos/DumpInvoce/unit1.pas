unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  ETradeDoc, DividerBevel,
  InvoceExchangeFile, InvoceDocument, InvoiceItem, TransferInfo, Signer;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    DividerBevel1: TDividerBevel;
    Edit1: TEdit;
    ETradeDoc1: TETradeDoc;
    Label1: TLabel;
    Memo1: TMemo;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    //invoce
    procedure DumpExchangeFile(D: TExchangeFile; APrefix: string);
    procedure DumpParticipantsInformation(P: TParticipantsInformation; APrefix: string);
    procedure DumpSellerExchangeInformation(P: TSellerExchangeInformation; APrefix: string);
    procedure DumpInvoceDocument(P: TInvoceDocument; APrefix: string);
    procedure DumpInvoiceInformation(P:TInvoiceInformation; APrefix: string);
    procedure DumpInvoiceRevisionInfo(P:TInvoiceRevisionInfo; APrefix: string);
    procedure DumpInvoiceItems(P:TInvoiceItems; APrefix: string);
    procedure DumpInvoiceItemList(P:TInvoiceItemList; APrefix: string);
    procedure DumpTotalForPay(P:TTotalForPay; APrefix: string);
    procedure DumpTransferInfo(P:TTransferInfo; APrefix: string);
    procedure DumpSigners(P:TSignerList; APrefix: string);


    procedure WriteLogI(S:string; AParams:array of const);
  public

  end;

var
  Form1: TForm1;

implementation
uses LazFileUtils, LazUTF8;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  S: String;
begin
  Memo1.Lines.Clear;

  S:=AppendPathDelim(ExtractFileDir(ParamStr(0))) + AppendPathDelim('data');
  Edit1.Text:=S + 'invoce1.xml';
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  D: TExchangeFile;
begin
  D:=ETradeDoc1.LoadInvoce(Edit1.Text);
  DumpExchangeFile(D, 'СФ');
  D.Free;
end;

procedure TForm1.DumpExchangeFile(D: TExchangeFile; APrefix: string);
begin
  if not Assigned(D) then Exit;
  APrefix:=APrefix + '.';
  WriteLogI(APrefix + 'FileID = %s', [D.FileID]);
  WriteLogI(APrefix + 'FormatVersion = %s', [D.FormatVersion]);
  WriteLogI(APrefix + 'ApplicationVersion = %s', [D.ApplicationVersion]);
  DumpParticipantsInformation(D.ParticipantsInformation, APrefix+ 'ParticipantsInformation');
  DumpInvoceDocument(D.Document, APrefix+ 'Document');
end;

procedure TForm1.DumpParticipantsInformation(P: TParticipantsInformation;
  APrefix: string);
begin
  if not Assigned(P) then Exit;
  APrefix:=APrefix + '.';
  WriteLogI(APrefix + 'SenderInfo = %s', [P.SenderInfo]);
  WriteLogI(APrefix + 'RecipientInfo = %s', [P.RecipientInfo]);
  DumpSellerExchangeInformation(P.SellerExchangeInformation, APrefix + 'SellerExchangeInformation');
end;

procedure TForm1.DumpSellerExchangeInformation(P: TSellerExchangeInformation;
  APrefix: string);
begin
  if not Assigned(P) then Exit;
  APrefix:=APrefix + '.';
  WriteLogI(APrefix + 'FullName = %s', [P.FullName]);
  WriteLogI(APrefix + 'Inn = %s', [P.Inn]);
  WriteLogI(APrefix + 'IdentifierSenderOperator = %s', [P.IdentifierSenderOperator]);
end;

procedure TForm1.DumpInvoceDocument(P: TInvoceDocument; APrefix: string);
begin
  if not Assigned(P) then Exit;
  APrefix:=APrefix + '.';
  WriteLogI(APrefix + 'KND = %s', [P.KND]);
  WriteLogI(APrefix + 'DocFunction = %s', [P.DocFunction]);
  WriteLogI(APrefix + 'DocumentNameEL = %s', [P.DocumentNameEL]);
  WriteLogI(APrefix + 'DocumentName = %s', [P.DocumentName]);
  WriteLogI(APrefix + 'InvoceDateCreate = %s', [P.InvoceDateCreate]);
  WriteLogI(APrefix + 'InvoceTimeCreate = %s', [P.InvoceTimeCreate]);
  WriteLogI(APrefix + 'DocumentCreator = %s', [P.DocumentCreator]);
  WriteLogI(APrefix + 'DocumentCreatorBase = %s', [P.DocumentCreatorBase]);
  WriteLogI(APrefix + 'InformationAvailabilityStructureAdditionalInfo = %s', [P.InformationAvailabilityStructureAdditionalInfo]);
  DumpInvoiceInformation(P.InvoiceInformation, APrefix + 'InvoiceInformation');
  DumpInvoiceItems(P.InvoiceItems, APrefix + 'InvoiceItems');
  DumpTransferInfo(P.TransferInfo, APrefix + 'TransferInfo');
  DumpSigners(P.Signers, APrefix + 'Signers');

end;

procedure TForm1.DumpInvoiceInformation(P: TInvoiceInformation; APrefix: string
  );
begin
  if not Assigned(P) then Exit;
  APrefix:=APrefix + '.';
  WriteLogI(APrefix + 'DocumentNumber = %s', [P.DocumentNumber]);
  WriteLogI(APrefix + 'DocumentDate = %s', [P.DocumentDate]);
  WriteLogI(APrefix + 'CurrencyCode = %s', [P.CurrencyCode]);
  DumpInvoiceRevisionInfo(P.InvoiceRevisionInfo, APrefix + 'InvoiceRevisionInfo');
end;

procedure TForm1.DumpInvoiceRevisionInfo(P: TInvoiceRevisionInfo;
  APrefix: string);
begin
  if not Assigned(P) then Exit;
  APrefix:=APrefix + '.';
  WriteLogI(APrefix + 'InvoiceRevisionDate = %s', [P.InvoiceRevisionDate]);
  WriteLogI(APrefix + 'InvoiceRevisionNumber = %d', [P.InvoiceRevisionNumber]);
  WriteLogI(APrefix + 'InvoiceRevisionDateDef = %s', [P.InvoiceRevisionDateDef]);
  WriteLogI(APrefix + 'InvoiceRevisionNumberDef = %s', [P.InvoiceRevisionNumberDef]);
end;

procedure TForm1.DumpInvoiceItems(P: TInvoiceItems; APrefix: string);
begin
  if not Assigned(P) then Exit;
  APrefix:=APrefix + '.';
  DumpInvoiceItemList(P.InvoiceItemList, APrefix + 'InvoiceItemList');
  DumpTotalForPay(P.TotalForPay, APrefix + 'TotalForPay');
end;

procedure TForm1.DumpInvoiceItemList(P: TInvoiceItemList; APrefix: string);
var
  i: Integer;
  Itm: TInvoiceItem;
begin
  if not Assigned(P) then Exit;
  APrefix:=APrefix + '.';
  for i:=0 to P.Count-1 do
  begin
    Itm:=P[i];
  end;
end;

procedure TForm1.DumpTotalForPay(P: TTotalForPay; APrefix: string);
begin

end;

procedure TForm1.DumpTransferInfo(P: TTransferInfo; APrefix: string);
begin

end;

procedure TForm1.DumpSigners(P: TSignerList; APrefix: string);
begin

end;

procedure TForm1.WriteLogI(S: string; AParams: array of const);
begin
  Memo1.Lines.Add(Format(S, AParams));
end;

end.

